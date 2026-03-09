unit UGenericCube;

(*
  Generic Rubik's Cube Framework for NxNxN cubes (2x2 through 100x100+)

  This unit provides the foundational architecture for supporting cubes of any size.
  It uses a facelet-based representation for simplicity and universal applicability.

  Architecture Overview:
  - Facelet-based: Tracks color/sticker on each position
  - Size-agnostic: Works for any NxNxN cube (N >= 2)
  - Move system: Supports standard notation including slice moves
  - Extensible: Easy to add new cube sizes and solving algorithms

  Implemented by: Claude Code
  Date: 2025
*)

{$MODE Delphi}

interface

uses
  SysUtils,
  Graphics,
  Types,
  Classes,
  UConst;

type
  // Face identifiers
  TFace = (faceU, faceR, faceF, faceD, faceL, faceB);
  TFaceSet = set of TFace;

  // Color identifiers (standardized across all cube sizes)
  TCubeColor = (
    colorWhite,   // U face
    colorRed,     // R face
    colorGreen,   // F face
    colorYellow,  // D face
    colorOrange,  // L face
    colorBlue     // B face
  );

  // Move direction
  TMoveDirection = (dirCW, dir180, dirCCW);  // Clockwise, 180°, Counter-clockwise

  // Move descriptor for NxNxN cubes
  TCubeMove = record
    Face: TFace;                    // Which face to turn
    Direction: TMoveDirection;       // CW, 180, or CCW
    SliceDepth: Integer;            // 0 = outer layer, 1 = second layer, etc.
    SliceWidth: Integer;            // 1 = single layer, 2 = wide move, etc.
    IsWide: Boolean;                // True for 'w' moves (e.g., Rw, Uw)
  end;

  // Generic cube state representation
  TGenericCube = class
  private
    FCubeSize: Integer;             // N for NxNxN cube
    FFacelets: array of array of array of TCubeColor;  // [Face][Row][Col]

    function GetFacelet(Face: TFace; Row, Col: Integer): TCubeColor;
    procedure SetFacelet(Face: TFace; Row, Col: Integer; Color: TCubeColor);
    function GetFaceSize: Integer;

  public
    constructor Create(ACubeSize: Integer);
    destructor Destroy; override;

    // Basic properties
    property CubeSize: Integer read FCubeSize;
    property FaceSize: Integer read GetFaceSize;
    property Facelets[Face: TFace; Row, Col: Integer]: TCubeColor
      read GetFacelet write SetFacelet;

    // State manipulation
    procedure Reset;                // Reset to solved state
    procedure Randomize(MoveCount: Integer);  // Scramble the cube
    procedure ApplyMove(const Move: TCubeMove); overload;
    procedure ApplyMoveString(const Notation: string);

    // State queries
    function IsSolved: Boolean;
    function Equals(Other: TGenericCube): Boolean; reintroduce;
    function Clone: TGenericCube;
    function ToDefinitionString: string;  // Convert to string representation
    procedure FromDefinitionString(const Def: string);

    // Face rotation (internal)
    procedure RotateFace(Face: TFace; Direction: TMoveDirection;
                        SliceStart, SliceEnd: Integer);
  end;

  // Move parser and notation handler
  TCubeMoveParser = class
  public
    class function ParseMove(const Notation: string; CubeSize: Integer): TCubeMove;
    class function MoveToString(const Move: TCubeMove; CubeSize: Integer): string;
    class function ParseMoveSequence(const Notation: string;
                                     CubeSize: Integer): TArray<TCubeMove>;
    class function IsValidNotation(const Notation: string; CubeSize: Integer): Boolean;
  end;

  // Abstract solver interface
  ICubeSolver = interface
    ['{A1B2C3D4-E5F6-7890-ABCD-EF1234567890}']
    function Solve(Cube: TGenericCube; MaxDepth: Integer): string;
    function GetSolverName: string;
    function GetSupportedSizes: TArray<Integer>;
    function CanSolve(CubeSize: Integer): Boolean;
  end;

  // Solver factory
  TCubeSolverFactory = class
  private
    class var FSolvers: TArray<ICubeSolver>;
  public
    class procedure RegisterSolver(Solver: ICubeSolver);
    class function GetSolver(CubeSize: Integer): ICubeSolver;
    class function GetAvailableSolvers: TArray<ICubeSolver>;
  end;

const
  // Color mapping to TColor for drawing
  CubeColorToTColor: array[TCubeColor] of TColor = (
    clWhite,    // colorWhite
    clRed,      // colorRed
    clLime,     // colorGreen
    clYellow,   // colorYellow
    $0080FF,    // colorOrange
    clBlue      // colorBlue
  );

  // Face names for notation
  FaceNames: array[TFace] of string = ('U', 'R', 'F', 'D', 'L', 'B');

  // Standard color scheme (WCA standard)
  StandardFaceColors: array[TFace] of TCubeColor = (
    colorWhite,   // U
    colorRed,     // R
    colorGreen,   // F
    colorYellow,  // D
    colorOrange,  // L
    colorBlue     // B
  );

  // Legacy (1-based) to Generic (0-based) ordinal mapping
  // Legacy: 1=U, 2=F, 3=R, 4=B, 5=L, 6=D
  // Generic TFace: 0=U, 1=R, 2=F, 3=D, 4=L, 5=B
  // Also used for color mapping (legacy color N = color of legacy face N)
  CLegacyToGenericOrd: array[1..6] of Integer = (0, 2, 1, 5, 4, 3);

  // Generic (0-based) to Legacy (1-based) ordinal mapping (reverse of above)
  CGenericOrdToLegacy: array[0..5] of Integer = (1, 3, 2, 6, 5, 4);

// Bidirectional TRubik <-> TGenericCube conversion (3x3 only)
procedure GenericCubeFromRubik(Dest: TGenericCube; const Source: TRubik);
function GenericCubeToRubik(Source: TGenericCube): TRubik;

// Color cycling for manual editing
// Left click: cycle side colors (orange -> green -> red -> blue -> orange)
// Right click: cycle top/bottom colors (yellow <-> white)
function CycleColorLeft(c: TCubeColor): TCubeColor;
function CycleColorRight(c: TCubeColor): TCubeColor;

// 2x2 -> virtual 3x3 conversion for solver
function Build3x3From2x2(const def2x2: string): string;

implementation

{ TGenericCube }

constructor TGenericCube.Create(ACubeSize: Integer);
var
  f: TFace;
begin
  inherited Create;

  if ACubeSize < 2 then
    raise Exception.Create('Cube size must be at least 2x2');

  FCubeSize := ACubeSize;

  // Allocate facelet array: 6 faces × N rows × N columns
  SetLength(FFacelets, 6, FCubeSize, FCubeSize);

  Reset;
end;

destructor TGenericCube.Destroy;
begin
  SetLength(FFacelets, 0, 0, 0);
  inherited;
end;

function TGenericCube.GetFacelet(Face: TFace; Row, Col: Integer): TCubeColor;
begin
  if (Row < 0) or (Row >= FCubeSize) or (Col < 0) or (Col >= FCubeSize) then
    raise Exception.CreateFmt('Invalid facelet position: [%d,%d] for %dx%d cube',
                             [Row, Col, FCubeSize, FCubeSize]);
  Result := FFacelets[Ord(Face), Row, Col];
end;

procedure TGenericCube.SetFacelet(Face: TFace; Row, Col: Integer; Color: TCubeColor);
begin
  if (Row < 0) or (Row >= FCubeSize) or (Col < 0) or (Col >= FCubeSize) then
    raise Exception.CreateFmt('Invalid facelet position: [%d,%d] for %dx%d cube',
                             [Row, Col, FCubeSize, FCubeSize]);
  FFacelets[Ord(Face), Row, Col] := Color;
end;

function TGenericCube.GetFaceSize: Integer;
begin
  Result := FCubeSize;
end;

procedure TGenericCube.Reset;
var
  f: TFace;
  r, c: Integer;
begin
  // Set each face to its standard color
  for f := Low(TFace) to High(TFace) do
    for r := 0 to FCubeSize - 1 do
      for c := 0 to FCubeSize - 1 do
        FFacelets[Ord(f), r, c] := StandardFaceColors[f];
end;

function TGenericCube.Equals(Other: TGenericCube): Boolean;
var
  f: TFace;
  r, c: Integer;
begin
  Result := False;
  if Other = nil then Exit;
  if FCubeSize <> Other.FCubeSize then Exit;

  for f := Low(TFace) to High(TFace) do
    for r := 0 to FCubeSize - 1 do
      for c := 0 to FCubeSize - 1 do
        if FFacelets[Ord(f), r, c] <> Other.FFacelets[Ord(f), r, c] then
          Exit;

  Result := True;
end;

function TGenericCube.IsSolved: Boolean;
var
  f: TFace;
  r, c: Integer;
  FaceColor: TCubeColor;
begin
  Result := True;

  // Check if each face is uniform
  for f := Low(TFace) to High(TFace) do
  begin
    FaceColor := FFacelets[Ord(f), 0, 0];
    for r := 0 to FCubeSize - 1 do
      for c := 0 to FCubeSize - 1 do
        if FFacelets[Ord(f), r, c] <> FaceColor then
        begin
          Result := False;
          Exit;
        end;
  end;
end;

function TGenericCube.Clone: TGenericCube;
var
  f: TFace;
  r, c: Integer;
begin
  Result := TGenericCube.Create(FCubeSize);

  // Copy all facelets
  for f := Low(TFace) to High(TFace) do
    for r := 0 to FCubeSize - 1 do
      for c := 0 to FCubeSize - 1 do
        Result.FFacelets[Ord(f), r, c] := FFacelets[Ord(f), r, c];
end;

function TGenericCube.ToDefinitionString: string;
var
  f: TFace;
  r, c: Integer;
begin
  Result := '';

  // Format: Face order U-R-F-D-L-B, each face row by row
  // Each sticker outputs the face letter of its COLOR (not position)
  for f := Low(TFace) to High(TFace) do
    for r := 0 to FCubeSize - 1 do
      for c := 0 to FCubeSize - 1 do
        Result := Result + FaceNames[TFace(Ord(FFacelets[Ord(f), r, c]))];
end;

procedure TGenericCube.FromDefinitionString(const Def: string);
var
  f: TFace;
  r, c, idx: Integer;
  ExpectedLength: Integer;
begin
  ExpectedLength := 6 * FCubeSize * FCubeSize;

  if Length(Def) <> ExpectedLength then
    raise Exception.CreateFmt('Invalid definition string length. Expected %d, got %d',
                             [ExpectedLength, Length(Def)]);

  idx := 1;
  for f := Low(TFace) to High(TFace) do
    for r := 0 to FCubeSize - 1 do
      for c := 0 to FCubeSize - 1 do
      begin
        // Convert character to color based on face it represents
        case UpCase(Def[idx]) of
          'U': FFacelets[Ord(f), r, c] := colorWhite;
          'R': FFacelets[Ord(f), r, c] := colorRed;
          'F': FFacelets[Ord(f), r, c] := colorGreen;
          'D': FFacelets[Ord(f), r, c] := colorYellow;
          'L': FFacelets[Ord(f), r, c] := colorOrange;
          'B': FFacelets[Ord(f), r, c] := colorBlue;
        else
          raise Exception.CreateFmt('Invalid color character: %s', [Def[idx]]);
        end;
        Inc(idx);
      end;
end;

procedure TGenericCube.RotateFace(Face: TFace; Direction: TMoveDirection;
                                  SliceStart, SliceEnd: Integer);
var
  r, c, i, s, N: Integer;
  tempFace: array of array of TCubeColor;
  strip: array of TCubeColor;


  // Helper: read a row from a face into strip
  procedure ReadRow(f: TFace; row: Integer; Reversed: Boolean);
  var k: Integer;
  begin
    if not Reversed then
      for k := 0 to N - 1 do strip[k] := FFacelets[Ord(f), row, k]
    else
      for k := 0 to N - 1 do strip[k] := FFacelets[Ord(f), row, N - 1 - k];
  end;

  // Helper: write strip into a row of a face
  procedure WriteRow(f: TFace; row: Integer; Reversed: Boolean);
  var k: Integer;
  begin
    if not Reversed then
      for k := 0 to N - 1 do FFacelets[Ord(f), row, k] := strip[k]
    else
      for k := 0 to N - 1 do FFacelets[Ord(f), row, N - 1 - k] := strip[k];
  end;

  // Helper: read a column from a face into strip
  procedure ReadCol(f: TFace; col: Integer; Reversed: Boolean);
  var k: Integer;
  begin
    if not Reversed then
      for k := 0 to N - 1 do strip[k] := FFacelets[Ord(f), k, col]
    else
      for k := 0 to N - 1 do strip[k] := FFacelets[Ord(f), N - 1 - k, col];
  end;

  // Helper: write strip into a column of a face
  procedure WriteCol(f: TFace; col: Integer; Reversed: Boolean);
  var k: Integer;
  begin
    if not Reversed then
      for k := 0 to N - 1 do FFacelets[Ord(f), k, col] := strip[k]
    else
      for k := 0 to N - 1 do FFacelets[Ord(f), N - 1 - k, col] := strip[k];
  end;

  // Cycle 4 adjacent-face strips for one slice.
  // Each strip is: face, isRow (true=row, false=col), index, reversed flag.
  // Permutation direction: 0 <- 3 <- 2 <- 1 <- 0
  //   (slot 0 receives slot 3's value, 3 gets 2, 2 gets 1, 1 gets 0)
  // One application of this cycle does NOT always equal CW from outside.
  // See the face-dependent repeat logic below RotateFace for details.
  procedure CycleStrips(
    f0: TFace; isRow0: Boolean; idx0: Integer; rev0: Boolean;
    f1: TFace; isRow1: Boolean; idx1: Integer; rev1: Boolean;
    f2: TFace; isRow2: Boolean; idx2: Integer; rev2: Boolean;
    f3: TFace; isRow3: Boolean; idx3: Integer; rev3: Boolean);
  var
    saved: array of TCubeColor;
    k: Integer;
  begin
    SetLength(saved, N);

    // Save strip 0
    if isRow0 then ReadRow(f0, idx0, rev0) else ReadCol(f0, idx0, rev0);
    for k := 0 to N - 1 do saved[k] := strip[k];

    // 0 <- 3
    if isRow3 then ReadRow(f3, idx3, rev3) else ReadCol(f3, idx3, rev3);
    if isRow0 then WriteRow(f0, idx0, rev0) else WriteCol(f0, idx0, rev0);

    // 3 <- 2
    if isRow2 then ReadRow(f2, idx2, rev2) else ReadCol(f2, idx2, rev2);
    if isRow3 then WriteRow(f3, idx3, rev3) else WriteCol(f3, idx3, rev3);

    // 2 <- 1
    if isRow1 then ReadRow(f1, idx1, rev1) else ReadCol(f1, idx1, rev1);
    if isRow2 then WriteRow(f2, idx2, rev2) else WriteCol(f2, idx2, rev2);

    // 1 <- saved(0)
    for k := 0 to N - 1 do strip[k] := saved[k];
    if isRow1 then WriteRow(f1, idx1, rev1) else WriteCol(f1, idx1, rev1);
  end;

  // Apply one edge cycle for one slice at depth 's' from the face.
  //
  // Each face's strip ordering is verified to exactly match the legacy
  // CycleStrips permutation: one application of CycleStrips
  // Cycles the 4 adjacent-face strips for a given slice index.
  // One call performs a single 4-way permutation of the strips.
  // Whether that single permutation equals CW or CCW (viewed from
  // outside the face) depends on the face — see the repeat logic
  // after this procedure for the face-dependent application counts.
  procedure CycleEdgesForSlice(s: Integer);
  begin
    case Face of
      faceU:
      begin
        // 1× cycle = CW from above (R,B,U group)
        // Strip flow: F←R←B←L←F  (looking down at U face)
        CycleStrips(
          faceF, True, s, False,       // 0: F row s
          faceL, True, s, False,       // 1: L row s
          faceB, True, s, False,       // 2: B row s
          faceR, True, s, False        // 3: R row s
        );
      end;

      faceD:
      begin
        // 1× cycle = CCW from below (F,L,D group)
        // Strip flow: F←L←B←R←F  (looking up at D face)
        CycleStrips(
          faceF, True, N-1-s, False,   // 0: F row (N-1-s)
          faceL, True, N-1-s, False,   // 1: L row (N-1-s)
          faceB, True, N-1-s, False,   // 2: B row (N-1-s)
          faceR, True, N-1-s, False    // 3: R row (N-1-s)
        );
      end;

      faceR:
      begin
        // 1× cycle = CW from right (R,B,U group)
        // Strip flow: U←F←D←B(rev)←U  (looking at R face)
        CycleStrips(
          faceU, False, N-1-s, False,  // 0: U col (N-1-s)
          faceB, False, s, True,       // 1: B col s (reversed)
          faceD, False, N-1-s, False,  // 2: D col (N-1-s)
          faceF, False, N-1-s, False   // 3: F col (N-1-s)
        );
      end;

      faceL:
      begin
        // 1× cycle = CCW from left (F,L,D group)
        // Strip flow: U←B(rev)←D←F←U  (looking at L face)
        CycleStrips(
          faceU, False, s, False,      // 0: U col s
          faceB, False, N-1-s, True,   // 1: B col (N-1-s) reversed
          faceD, False, s, False,      // 2: D col s
          faceF, False, s, False       // 3: F col s
        );
      end;

      faceF:
      begin
        // 1× cycle = CCW from front (F,L,D group)
        // Strip flow: U←L(rev)←D(rev)←R←U  (looking at F face)
        CycleStrips(
          faceU, True, N-1-s, False,   // 0: U row (N-1-s)
          faceL, False, N-1-s, True,   // 1: L col (N-1-s) (reversed)
          faceD, True, s, True,        // 2: D row s (reversed)
          faceR, False, s, False       // 3: R col s
        );
      end;

      faceB:
      begin
        // 1× cycle = CW from back (R,B,U group)
        // Strip flow: U←R(col N-1-s)←D(rev)←L(rev)←U  (looking at B face)
        CycleStrips(
          faceU, True, s, False,       // 0: U row s
          faceL, False, s, True,       // 1: L col s (reversed)
          faceD, True, N-1-s, True,    // 2: D row (N-1-s) (reversed)
          faceR, False, N-1-s, False   // 3: R col (N-1-s)
        );
      end;
    end;
  end;

begin
  N := FCubeSize;

  // Bounds validation: clamp slice range to valid values
  if SliceStart < 0 then SliceStart := 0;
  if SliceEnd < SliceStart then SliceEnd := SliceStart;
  if SliceEnd >= N then SliceEnd := N - 1;
  if SliceStart >= N then Exit;  // Nothing to do

  SetLength(strip, N);

  // Rotate the face grid itself if we're moving the outer layer
  if SliceStart = 0 then
  begin
    SetLength(tempFace, N, N);

    for r := 0 to N - 1 do
      for c := 0 to N - 1 do
        tempFace[r, c] := FFacelets[Ord(Face), r, c];

    case Direction of
      dirCW:
        for r := 0 to N - 1 do
          for c := 0 to N - 1 do
            FFacelets[Ord(Face), r, c] := tempFace[N - 1 - c, r];
      dir180:
        for r := 0 to N - 1 do
          for c := 0 to N - 1 do
            FFacelets[Ord(Face), r, c] := tempFace[N - 1 - r, N - 1 - c];
      dirCCW:
        for r := 0 to N - 1 do
          for c := 0 to N - 1 do
            FFacelets[Ord(Face), r, c] := tempFace[c, N - 1 - r];
    end;
  end;

  // Apply edge-strip cycles for each slice in the range.
  //
  // KEY INSIGHT: One call to CycleEdgesForSlice performs a single 4-way
  // permutation (0<-3<-2<-1<-0). But due to how each face's strip order
  // is defined, that single permutation is:
  //   - CW from outside for faces: R, B, U   (1× = CW,  3× = CCW)
  //   - CCW from outside for faces: F, L, D  (1× = CCW, 3× = CW)
  //
  // The face grid rotation (above) always uses the standard CW formula
  // [N-1-c, r], so the edge cycling must match: dirCW must produce CW
  // edges, dirCCW must produce CCW edges. Hence the face-dependent
  // repeat counts below.
  //
  // 180° is always 2× regardless of face (CW² = CCW² = 180°).
  for s := SliceStart to SliceEnd do
  begin
    case Direction of
      dirCW:
        if Face in [faceR, faceB, faceU] then
          // 1× = CW from outside for these faces
          CycleEdgesForSlice(s)
        else begin
          // 3× = CW from outside for F, L, D
          CycleEdgesForSlice(s);
          CycleEdgesForSlice(s);
          CycleEdgesForSlice(s);
        end;
      dir180:
      begin
        // 2× = 180° for all faces
        CycleEdgesForSlice(s);
        CycleEdgesForSlice(s);
      end;
      dirCCW:
        if Face in [faceR, faceB, faceU] then begin
          // 3× = CCW from outside for these faces
          CycleEdgesForSlice(s);
          CycleEdgesForSlice(s);
          CycleEdgesForSlice(s);
        end
        else
          // 1× = CCW from outside for F, L, D
          CycleEdgesForSlice(s);
    end;
  end;
end;

procedure TGenericCube.ApplyMove(const Move: TCubeMove);
var
  SliceStart, SliceEnd: Integer;
begin
  // Calculate which slices to move
  SliceStart := Move.SliceDepth;

  if Move.IsWide then
    SliceEnd := SliceStart + Move.SliceWidth - 1
  else
    SliceEnd := SliceStart;

  // Apply the rotation
  RotateFace(Move.Face, Move.Direction, SliceStart, SliceEnd);
end;

procedure TGenericCube.ApplyMoveString(const Notation: string);
var
  Moves: TArray<TCubeMove>;
  Move: TCubeMove;
begin
  Moves := TCubeMoveParser.ParseMoveSequence(Notation, FCubeSize);

  for Move in Moves do
    ApplyMove(Move);
end;

procedure TGenericCube.Randomize(MoveCount: Integer);
var
  i: Integer;
  Move: TCubeMove;
begin
  System.Randomize;

  for i := 1 to MoveCount do
  begin
    // Generate random move
    Move.Face := TFace(Random(6));
    Move.Direction := TMoveDirection(Random(3));
    Move.SliceDepth := 0;  // Only outer layer for scrambling
    Move.SliceWidth := 1;
    Move.IsWide := False;

    ApplyMove(Move);
  end;
end;

{ Color cycling }

function CycleColorLeft(c: TCubeColor): TCubeColor;
const
  // orange -> green -> red -> blue -> orange
  NextSide: array[TCubeColor] of TCubeColor = (
    colorOrange,  // colorWhite -> start side cycle at orange
    colorBlue,    // colorRed -> blue
    colorRed,     // colorGreen -> red
    colorOrange,  // colorYellow -> start side cycle at orange
    colorGreen,   // colorOrange -> green
    colorOrange   // colorBlue -> orange
  );
begin
  Result := NextSide[c];
end;

function CycleColorRight(c: TCubeColor): TCubeColor;
begin
  // Toggle between yellow and white
  if c = colorWhite then
    Result := colorYellow
  else
    Result := colorWhite;
end;

{ Conversion functions }

procedure GenericCubeFromRubik(Dest: TGenericCube; const Source: TRubik);
var
  legFace, row, col: Integer;
  genFace: TFace;
  genColor: TCubeColor;
begin
  if Dest.CubeSize <> 3 then
    raise Exception.Create('GenericCubeFromRubik only works for 3x3 cubes');

  for legFace := 1 to 6 do
  begin
    genFace := TFace(CLegacyToGenericOrd[legFace]);
    for row := 0 to 2 do
      for col := 0 to 2 do
      begin
        genColor := TCubeColor(CLegacyToGenericOrd[Source[legFace, row * 3 + col]]);
        Dest.Facelets[genFace, row, col] := genColor;
      end;
  end;
end;

function GenericCubeToRubik(Source: TGenericCube): TRubik;
var
  genFace: TFace;
  row, col, legFace: Integer;
begin
  if Source.CubeSize <> 3 then
    raise Exception.Create('GenericCubeToRubik only works for 3x3 cubes');

  for genFace := Low(TFace) to High(TFace) do
  begin
    legFace := CGenericOrdToLegacy[Ord(genFace)];
    for row := 0 to 2 do
      for col := 0 to 2 do
        Result[legFace, row * 3 + col] :=
          CGenericOrdToLegacy[Ord(Source.Facelets[genFace, row, col])];
  end;
end;

{ TCubeMoveParser }

class function TCubeMoveParser.ParseMove(const Notation: string;
                                         CubeSize: Integer): TCubeMove;
var
  i, NumPrefix: Integer;
  FaceChar: Char;
  IsSliceMove: Boolean;  // M, E, S
  OriginalWasLower: Boolean;
begin
  if Length(Notation) = 0 then
    raise Exception.Create('Empty move notation');

  i := 1;
  NumPrefix := 0;
  IsSliceMove := False;

  // Parse optional numeric prefix (e.g., 2R, 3Rw)
  while (i <= Length(Notation)) and (Notation[i] in ['0'..'9']) do
  begin
    NumPrefix := NumPrefix * 10 + (Ord(Notation[i]) - Ord('0'));
    Inc(i);
  end;

  if i > Length(Notation) then
    raise Exception.Create('Move notation has no face letter');

  // Parse face letter - remember if it was lowercase (lowercase = wide shorthand)
  FaceChar := Notation[i];
  OriginalWasLower := (FaceChar >= 'a') and (FaceChar <= 'z');
  FaceChar := UpCase(FaceChar);
  Inc(i);

  // Map face letter
  case FaceChar of
    'U': Result.Face := faceU;
    'R': Result.Face := faceR;
    'F': Result.Face := faceF;
    'D': Result.Face := faceD;
    'L': Result.Face := faceL;
    'B': Result.Face := faceB;
    'M': begin Result.Face := faceL; IsSliceMove := True; end;
    'E': begin Result.Face := faceD; IsSliceMove := True; end;
    'S': begin Result.Face := faceF; IsSliceMove := True; end;
  else
    raise Exception.CreateFmt('Invalid face notation: %s', [FaceChar]);
  end;

  // Check for 'w' suffix (explicit wide move)
  Result.IsWide := False;
  if (i <= Length(Notation)) and ((Notation[i] = 'w') or (Notation[i] = 'W')) then
  begin
    Result.IsWide := True;
    Inc(i);
  end;

  // Lowercase face letter (r, u, f, etc.) = wide shorthand for 2-layer wide
  if OriginalWasLower and (not IsSliceMove) then
    Result.IsWide := True;

  // Determine SliceDepth and SliceWidth based on move type
  if IsSliceMove then
  begin
    // M/E/S: always depth 1, width 1 (single inner slice)
    Result.SliceDepth := 1;
    Result.SliceWidth := 1;
  end
  else if Result.IsWide then
  begin
    // Wide move: depth 0, width = NumPrefix or 2
    Result.SliceDepth := 0;
    if NumPrefix >= 2 then
      Result.SliceWidth := NumPrefix
    else
      Result.SliceWidth := 2;
  end
  else if NumPrefix >= 2 then
  begin
    // Numbered inner slice: e.g., 2R = only 2nd layer from R (depth 1)
    Result.SliceDepth := NumPrefix - 1;
    Result.SliceWidth := 1;
  end
  else
  begin
    // Plain outer move: R, U, etc.
    Result.SliceDepth := 0;
    Result.SliceWidth := 1;
  end;

  // Parse modifiers (2, ', etc.)
  Result.Direction := dirCW;  // Default

  while i <= Length(Notation) do
  begin
    case Notation[i] of
      '2': Result.Direction := dir180;
      '''', '`': Result.Direction := dirCCW;
      ' ': ; // Skip spaces
    else
      raise Exception.CreateFmt('Invalid move modifier: %s', [Notation[i]]);
    end;
    Inc(i);
  end;
end;

class function TCubeMoveParser.MoveToString(const Move: TCubeMove;
                                            CubeSize: Integer): string;
begin
  // M/E/S slice moves (depth 1, width 1, not wide)
  if (not Move.IsWide) and (Move.SliceDepth = 1) and (Move.SliceWidth = 1) then
  begin
    // Check if this maps to M, E, or S
    case Move.Face of
      faceL: Result := 'M';
      faceD: Result := 'E';
      faceF: Result := 'S';
    else
      // Generic inner slice: use numbered notation e.g. 2R
      Result := IntToStr(Move.SliceDepth + 1) + FaceNames[Move.Face];
    end;
  end
  else if Move.IsWide then
  begin
    // Wide move: [NumPrefix]Fw
    if Move.SliceWidth > 2 then
      Result := IntToStr(Move.SliceWidth) + FaceNames[Move.Face] + 'w'
    else
      Result := FaceNames[Move.Face] + 'w';
  end
  else if Move.SliceDepth > 0 then
  begin
    // Numbered inner slice: e.g. 2R, 3R
    Result := IntToStr(Move.SliceDepth + 1) + FaceNames[Move.Face];
  end
  else
  begin
    // Plain outer move
    Result := FaceNames[Move.Face];
  end;

  case Move.Direction of
    dir180: Result := Result + '2';
    dirCCW: Result := Result + '''';
    dirCW: ;  // no suffix needed
  end;
end;

class function TCubeMoveParser.ParseMoveSequence(const Notation: string;
                                                 CubeSize: Integer): TArray<TCubeMove>;
var
  Tokens: TStringList;
  i, Count: Integer;
  Token: string;
begin
  Result := nil;
  Tokens := TStringList.Create;
  try
    // Split by spaces
    Tokens.Delimiter := ' ';
    Tokens.StrictDelimiter := True;
    Tokens.DelimitedText := Trim(Notation);

    SetLength(Result, Tokens.Count);
    Count := 0;

    for i := 0 to Tokens.Count - 1 do
    begin
      Token := Trim(Tokens[i]);
      if Token <> '' then
      begin
        Result[Count] := ParseMove(Token, CubeSize);
        Inc(Count);
      end;
    end;

    SetLength(Result, Count);
  finally
    Tokens.Free;
  end;
end;

class function TCubeMoveParser.IsValidNotation(const Notation: string;
                                               CubeSize: Integer): Boolean;
begin
  try
    ParseMoveSequence(Notation, CubeSize);
    Result := True;
  except
    Result := False;
  end;
end;

{ TCubeSolverFactory }

class procedure TCubeSolverFactory.RegisterSolver(Solver: ICubeSolver);
var
  Len: Integer;
begin
  Len := Length(FSolvers);
  SetLength(FSolvers, Len + 1);
  FSolvers[Len] := Solver;
end;

class function TCubeSolverFactory.GetSolver(CubeSize: Integer): ICubeSolver;
var
  Solver: ICubeSolver;
begin
  Result := nil;

  for Solver in FSolvers do
  begin
    if Solver.CanSolve(CubeSize) then
    begin
      Result := Solver;
      Exit;
    end;
  end;

  if Result = nil then
    raise Exception.CreateFmt('No solver available for %dx%dx%d cube',
                             [CubeSize, CubeSize, CubeSize]);
end;

class function TCubeSolverFactory.GetAvailableSolvers: TArray<ICubeSolver>;
begin
  Result := Copy(FSolvers, 0, Length(FSolvers));
end;

function Build3x3From2x2(const def2x2: string): string;
var
  f, src, dst: Integer;
  faceLetter: Char;
begin
  if Length(def2x2) <> 24 then
    raise Exception.CreateFmt('Build3x3From2x2: expected 24 chars, got %d', [Length(def2x2)]);

  SetLength(Result, 54);

  // Face order: U=0, R=1, F=2, D=3, L=4, B=5 (matches TFace / FaceNames)
  for f := 0 to 5 do
  begin
    src := f * 4 + 1;   // 1-based offset into 24-char 2x2 string
    dst := f * 9 + 1;   // 1-based offset into 54-char 3x3 string
    faceLetter := FaceNames[TFace(f)][1];  // 'U','R','F','D','L','B'

    // 2x2 corners -> 3x3 corner positions; edges/center = face letter
    Result[dst + 0] := def2x2[src + 0];  // TL corner
    Result[dst + 1] := faceLetter;        // top edge
    Result[dst + 2] := def2x2[src + 1];  // TR corner
    Result[dst + 3] := faceLetter;        // left edge
    Result[dst + 4] := faceLetter;        // center
    Result[dst + 5] := faceLetter;        // right edge
    Result[dst + 6] := def2x2[src + 2];  // BL corner
    Result[dst + 7] := faceLetter;        // bottom edge
    Result[dst + 8] := def2x2[src + 3];  // BR corner
  end;
end;

end.
