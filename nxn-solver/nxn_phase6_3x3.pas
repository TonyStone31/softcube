unit nxn_phase6_3x3;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nxn_cubedefs, nxn_facecube, nxn_util, nxn_min2phase;

// Extract virtual 3x3 state from an NxN cube (after centers + edges solved)
// Returns a 54-char URFDLB facelet string suitable for min2phase
function Extract3x3State(fc: FaceletCube): string;

// Call the min2phase solver (pure Pascal, no external binary)
// Returns the move sequence string (or empty for solved cube)
// Returns '!ERROR' if cube state is invalid (parity issue)
function CallMin2Phase(const faceletStr: string; const solverPath: string;
  maxLength: integer = 23; timeLimitMs: integer = 5000): string;

// Apply PLL parity fix algorithm for even cubes: r2 U2 r2 Uw2 r2 u2
// This swaps two paired edges to fix parity without breaking centers
procedure ApplyParityFix(fc: FaceletCube);

// Fix inner row displacement after Phase 5.
// Inner-only moves (move(U, orbit)) don't affect the 3x3 state,
// so this can safely be called between Phase 5 and Phase 6.
// Returns the notation string for compensation moves, and updates moveCount.
function FixInnerRowDisplacement(fc: FaceletCube; var moveCount: integer): string;

implementation

function ColorIndexToFaceChar(c: ColorIndex): char;
begin
  case c of
    UCol: Result := 'U';
    DCol: Result := 'D';
    RCol: Result := 'R';
    LCol: Result := 'L';
    FCol: Result := 'F';
    BCol: Result := 'B';
  else
    Result := '-';
  end;
end;

function Extract3x3State(fc: FaceletCube): string;
var
  n, mid, edgeOrb: integer;
  // Virtual 3x3 facelet array: [face 0..5][row 0..2][col 0..2]
  virt: array[0..5, 0..2, 0..2] of ColorIndex;
  kFace, face3x3Row, face3x3Col: integer;
  outStr: string;

begin
  n := fc.size;

  if Odd(n) then
    mid := (n - 1) div 2
  else
    mid := n div 2 - 1; // for even cubes, use any center facelet

  // For edges, use orbit 1 instead of mid. On virtual cubes (even→odd padding),
  // the padding orbit (mid) gets scrambled by face rotations in earlier phases.
  // Orbit 1 is a real orbit with correctly paired edge data after Phase 5.
  // On genuine odd cubes, orbit 1 is also valid (all orbits are paired).
  edgeOrb := 1;

  // For each of the 6 Kociemba faces (U=0,D=1,R=2,L=3,F=4,B=5):
  // Extract corners, edges, and center

  for kFace := 0 to 5 do
  begin
    // Center: [mid, mid] - invariant under face rotations (fixed point)
    virt[kFace, 1, 1] := fc.faceCols[kFace, mid, mid];

    // Corners: map 3x3 positions to NxN positions
    virt[kFace, 0, 0] := fc.faceCols[kFace, 0, 0];
    virt[kFace, 0, 2] := fc.faceCols[kFace, 0, n - 1];
    virt[kFace, 2, 0] := fc.faceCols[kFace, n - 1, 0];
    virt[kFace, 2, 2] := fc.faceCols[kFace, n - 1, n - 1];

    // Edges: use orbit 1 position (first real orbit)
    virt[kFace, 0, 1] := fc.faceCols[kFace, 0, edgeOrb];
    virt[kFace, 1, 0] := fc.faceCols[kFace, edgeOrb, 0];
    virt[kFace, 1, 2] := fc.faceCols[kFace, edgeOrb, n - 1];
    virt[kFace, 2, 1] := fc.faceCols[kFace, n - 1, edgeOrb];
  end;

  // Debug: dump edges at different orbits
  for edgeOrb := 1 to n div 2 do
  begin
    outStr := Format('Edges orb%d: ', [edgeOrb]);
    for kFace := 0 to 5 do
      outStr := outStr + Format('%d:t=%s l=%s r=%s b=%s ',
        [kFace,
         ColorIndexToFaceChar(fc.faceCols[kFace, 0, edgeOrb]),
         ColorIndexToFaceChar(fc.faceCols[kFace, edgeOrb, 0]),
         ColorIndexToFaceChar(fc.faceCols[kFace, edgeOrb, n-1]),
         ColorIndexToFaceChar(fc.faceCols[kFace, n-1, edgeOrb])]);
    WriteLnVerbose(outStr);
  end;
  edgeOrb := 1; // restore

  // Build URFDLB string: iterate faces in URFDLB order
  // URFDLB order: U(pos 0), R(pos 1), F(pos 2), D(pos 3), L(pos 4), B(pos 5)
  // Kociemba internal: U=0, D=1, R=2, L=3, F=4, B=5
  SetLength(outStr, 54);
  for face3x3Row := 0 to 5 do // URFDLB face index
  begin
    kFace := URFDLBToKociemba[face3x3Row]; // Map to Kociemba face
    for face3x3Col := 0 to 8 do // 9 facelets per face
      outStr[face3x3Row * 9 + face3x3Col + 1] :=
        ColorIndexToFaceChar(virt[kFace, face3x3Col div 3, face3x3Col mod 3]);
  end;

  Result := outStr;
end;

function FixInnerRowDisplacement(fc: FaceletCube; var moveCount: integer): string;
var
  n, mid, orbit, k, fix, dReps, i: integer;
  col: ColorIndex;
  notation: string;
  prefix: integer;
begin
  Result := '';
  moveCount := 0;
  n := fc.size;
  mid := (n - 1) div 2;

  for orbit := 1 to n - 2 do
  begin
    // Skip center orbit for odd cubes (it holds the 3x3 center/edge facelets)
    if Odd(n) and (orbit = mid) then
      Continue;

    // Check displacement by reading R face center at (orbit, mid)
    // After correct Phase 4, R face centers should all be RCol.
    // move(U, orbit) cycles: F<-R<-B<-L<-F
    // So after k quarter-turns: R gets B(k=1), L(k=2), F(k=3)
    col := fc.faceCols[Ord(R), orbit, mid];

    case col of
      RCol: k := 0;
      BCol: k := 1;
      LCol: k := 2;
      FCol: k := 3;
    else
      begin
        WriteLnVerbose(Format('WARNING: Orbit %d has unexpected color %d at R[%d,%d]',
          [orbit, Ord(col), orbit, mid]));
        k := 0;
      end;
    end;

    if k = 0 then
      Continue;

    fix := (4 - k) mod 4;

    WriteLnVerbose(Format('Orbit %d: R[%d,%d]=%s, displacement=%d, fix=%d x move(U,%d)',
      [orbit, orbit, mid, ColorIndexStrings[col], k, fix, orbit]));

    // Apply compensation moves
    for i := 1 to fix do
      fc.move(U, orbit);
    Inc(moveCount, fix);

    // Generate notation
    notation := '';
    if orbit <= mid then
    begin
      // U-side inner orbit: use U notation directly
      prefix := orbit + 1;
      case fix of
        1: notation := IntToStr(prefix) + 'U';
        2: notation := IntToStr(prefix) + 'U2';
        3: notation := IntToStr(prefix) + 'U''';
      end;
    end
    else
    begin
      // D-side inner orbit: convert to D notation
      // k moves of move(U, orbit) = (3k mod 4) moves of move(D, n-1-orbit)
      prefix := n - orbit;
      dReps := (3 * fix) mod 4;
      case dReps of
        1: notation := IntToStr(prefix) + 'D';
        2: notation := IntToStr(prefix) + 'D2';
        3: notation := IntToStr(prefix) + 'D''';
      end;
    end;

    if Result <> '' then
      Result := Result + ' ';
    Result := Result + notation;
  end;

  if Result <> '' then
    WriteLnVerbose('Inner row displacement fix: ' + Result +
      Format(' (%d moves)', [moveCount]));
end;

procedure ApplyParityFix(fc: FaceletCube);
// PLL parity fix: r2 U2 r2 Uw2 r2 u2
// r = inner R slice (slice 1 from R)
// u = inner U slice (slice 1 from U)
// Uw = outer U + inner U together
var
  i: integer;
begin
  WriteLnVerbose('Applying PLL parity fix: 2R2 U2 2R2 U2 2U2 2R2 2U2');
  // r2: move(R, 1) twice
  for i := 1 to 2 do fc.move(R, 1);
  // U2: move(U, 0) twice
  for i := 1 to 2 do fc.move(U, 0);
  // r2: move(R, 1) twice
  for i := 1 to 2 do fc.move(R, 1);
  // Uw2: move(U, 0) twice + move(U, 1) twice
  for i := 1 to 2 do fc.move(U, 0);
  for i := 1 to 2 do fc.move(U, 1);
  // r2: move(R, 1) twice
  for i := 1 to 2 do fc.move(R, 1);
  // u2: move(U, 1) twice
  for i := 1 to 2 do fc.move(U, 1);
end;

function CallMin2Phase(const faceletStr: string; const solverPath: string;
  maxLength: integer; timeLimitMs: integer): string;
begin
  Result := '';

  // Check if it's already solved
  if faceletStr = 'UUUUUUUUURRRRRRRRRFFFFFFFFFDDDDDDDDDLLLLLLLLLBBBBBBBBB' then
  begin
    WriteLnVerbose('Cube is already solved, no Phase 6 moves needed.');
    Exit('');
  end;

  WriteLnVerbose('Calling min2phase (built-in): ' + faceletStr);

  // Use pure Pascal min2phase solver (no external binary needed)
  Result := SolveMin2Phase(faceletStr, maxLength, timeLimitMs);

  WriteLnVerbose('min2phase result: ' + Result);
end;

end.
