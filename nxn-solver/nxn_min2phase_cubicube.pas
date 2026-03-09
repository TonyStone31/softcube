unit nxn_min2phase_cubicube;
// CubieCube coordinate methods for the Two-Phase Algorithm
// Ported from Herbert Kociemba's CubeExplorer (CubiCube.pas)

{$mode objfpc}{$H+}

interface

uses nxn_min2phase_defs;

// --- Raw coordinate getters ---

// Corner orientation coordinate (0..2186)
function GetCornOriCoord(const cc: TCubieCube): word;
// Set corner orientations from coordinate
procedure SetCornOriCoord(var cc: TCubieCube; w: word);

// Edge orientation coordinate (0..2047)
function GetEdgeOriCoord(const cc: TCubieCube): word;
// Set edge orientations from coordinate
procedure SetEdgeOriCoord(var cc: TCubieCube; w: word);

// UDSlice coordinate (0..494) - which 4 edges are in UD slice (unordered)
function GetUDSliceCoord(const cc: TCubieCube): word;
// Set UD slice edges from coordinate
procedure SetUDSliceCoord(var cc: TCubieCube; w: word);

// UDSliceSorted coordinate (0..11879) - which 4 edges in UD slice + their order
function GetUDSliceSortedCoord(const cc: TCubieCube): word;
// Set from coordinate
procedure SetUDSliceSortedCoord(var cc: TCubieCube; w: word);

// Corner permutation coordinate (0..40319)
function GetCornPermCoord(const cc: TCubieCube): word;
// Set from coordinate
procedure SetCornPermCoord(var cc: TCubieCube; w: word);

// Phase 2 edge permutation coordinate (0..40319) - permutation of 8 non-slice edges
function GetPhase2EdgePermCoord(const cc: TCubieCube): word;
// Set from coordinate
procedure SetPhase2EdgePermCoord(var cc: TCubieCube; w: word);

// Full edge permutation coordinate (0..479001599) - all 12 edges
function GetEdgePermCoord(const cc: TCubieCube): integer;
procedure SetEdgePermCoord(var cc: TCubieCube; w: integer);

// Parity checks
function CornParityEven(const cc: TCubieCube): boolean;
function EdgeParityEven(const cc: TCubieCube): boolean;

// Convert 54-char facelet string (URFDLB) to CubieCube
// Returns true on success, false on invalid input
// On failure, FaceletParseError contains detail
function FaceletsToCubieCube(const facelets: string; out cc: TCubieCube): boolean;
var FaceletParseError: string;

// Convert CubieCube to 54-char facelet string
function CubieCubeToFacelets(const cc: TCubieCube): string;

implementation

uses SysUtils;

function GetCornOriCoord(const cc: TCubieCube): word;
var
  co: integer;
  s: word;
begin
  s := 0;
  for co := 0 to 6 do  // last corner orientation determined by others
    s := 3 * s + cc.corn[co].o;
  Result := s;
end;

procedure SetCornOriCoord(var cc: TCubieCube; w: word);
var
  co: integer;
  parity: shortint;
begin
  parity := 0;
  for co := 6 downto 0 do
  begin
    cc.corn[co].o := w mod 3;
    parity := parity + w mod 3;
    w := w div 3;
  end;
  parity := parity mod 3;
  case parity of
    0: cc.corn[7].o := 0;
    1: cc.corn[7].o := 2;
    2: cc.corn[7].o := 1;
  end;
end;

function GetEdgeOriCoord(const cc: TCubieCube): word;
var
  ed: integer;
  s: word;
begin
  s := 0;
  for ed := 0 to 10 do
    s := 2 * s + cc.edge[ed].o;
  Result := s;
end;

procedure SetEdgeOriCoord(var cc: TCubieCube; w: word);
var
  ed: integer;
  parity: shortint;
begin
  parity := 0;
  for ed := 10 downto 0 do
  begin
    cc.edge[ed].o := w mod 2;
    parity := parity + w mod 2;
    w := w div 2;
  end;
  cc.edge[11].o := parity mod 2;
end;

function GetUDSliceCoord(const cc: TCubieCube): word;
var
  s: word;
  k, n: integer;
  occupied: array[0..11] of boolean;
  ed: integer;
begin
  for n := 0 to 11 do occupied[n] := false;
  for ed := 0 to 11 do
    if cc.edge[ed].e >= 8 then  // FR=8, FL=9, BL=10, BR=11
      occupied[ed] := true;

  s := 0;
  k := 3;
  n := 11;
  while k >= 0 do
  begin
    if occupied[n] then
      Dec(k)
    else
      s := s + Cnk(n, k);
    Dec(n);
  end;
  Result := s;
end;

procedure SetUDSliceCoord(var cc: TCubieCube; w: word);
var
  n, k, v: integer;
  udSliceEdge, ed, i: integer;
  occupied: array[0..11] of boolean;
begin
  for n := 0 to 11 do occupied[n] := false;
  n := 11;
  k := 3;
  while k >= 0 do
  begin
    v := Cnk(n, k);
    if w < v then
    begin
      Dec(k);
      occupied[n] := true;
    end
    else
      w := w - v;
    Dec(n);
  end;
  udSliceEdge := 8; // FR
  for ed := 0 to 11 do
    if occupied[ed] then
    begin
      // Swap: find where udSliceEdge currently is
      for i := 0 to 11 do
        if cc.edge[i].e = udSliceEdge then
        begin
          cc.edge[i].e := cc.edge[ed].e;
          break;
        end;
      cc.edge[ed].e := udSliceEdge;
      if udSliceEdge < 11 then Inc(udSliceEdge);
    end;
end;

function GetUDSliceSortedCoord(const cc: TCubieCube): word;
var
  j, k, s, x: integer;
  i: integer;
  e: byte;
  arr: array[0..3] of byte;
begin
  j := 0;
  for i := 0 to 11 do
  begin
    e := cc.edge[i].e;
    if (e >= 8) and (e <= 11) then
    begin
      arr[j] := e;
      Inc(j);
    end;
  end;

  x := 0;
  for j := 3 downto 1 do
  begin
    s := 0;
    for k := j - 1 downto 0 do
      if arr[k] > arr[j] then Inc(s);
    x := (x + s) * j;
  end;
  Result := GetUDSliceCoord(cc) * 24 + x;
end;

procedure SetUDSliceSortedCoord(var cc: TCubieCube; w: word);
var
  x: word;
  order: array[0..3] of integer;
  used: array[8..11] of boolean;
  i, m: integer;
  k, j: integer;
  e: byte;
begin
  x := w mod 24;
  SetUDSliceCoord(cc, w div 24);

  for j := 8 to 11 do used[j] := false;
  for i := 0 to 3 do
  begin
    order[i] := x mod (i + 1);
    x := x div (i + 1);
  end;

  for i := 3 downto 0 do
  begin
    k := 11;
    while used[k] do Dec(k);
    while order[i] > 0 do
    begin
      Dec(order[i]);
      repeat Dec(k); until not used[k];
    end;

    m := -1;
    for j := 0 to 11 do
    begin
      e := cc.edge[j].e;
      if (e >= 8) and (e <= 11) then Inc(m);
      if m = i then
      begin
        cc.edge[j].e := k;
        used[k] := true;
        break;
      end;
    end;
  end;
end;

function GetCornPermCoord(const cc: TCubieCube): word;
var
  i, j: integer;
  x, s: integer;
begin
  x := 0;
  for i := 7 downto 1 do
  begin
    s := 0;
    for j := i - 1 downto 0 do
      if cc.corn[j].c > cc.corn[i].c then Inc(s);
    x := (x + s) * i;
  end;
  Result := x;
end;

procedure SetCornPermCoord(var cc: TCubieCube; w: word);
var
  used: array[0..7] of boolean;
  order: array[0..7] of integer;
  i, k: integer;
begin
  for i := 0 to 7 do
  begin
    used[i] := false;
    order[i] := w mod (i + 1);
    w := w div (i + 1);
  end;
  for i := 7 downto 0 do
  begin
    k := 7;
    while used[k] do Dec(k);
    while order[i] > 0 do
    begin
      Dec(order[i]);
      repeat Dec(k); until not used[k];
    end;
    cc.corn[i].c := k;
    used[k] := true;
  end;
end;

function GetPhase2EdgePermCoord(const cc: TCubieCube): word;
var
  i, j: integer;
  x, s: integer;
begin
  x := 0;
  for i := 7 downto 1 do  // only edges 0..7 (UR..DB)
  begin
    s := 0;
    for j := i - 1 downto 0 do
      if cc.edge[j].e > cc.edge[i].e then Inc(s);
    x := (x + s) * i;
  end;
  Result := x;
end;

procedure SetPhase2EdgePermCoord(var cc: TCubieCube; w: word);
var
  used: array[0..7] of boolean;
  order: array[0..7] of integer;
  i, k: integer;
begin
  for i := 0 to 7 do
  begin
    used[i] := false;
    order[i] := w mod (i + 1);
    w := w div (i + 1);
  end;
  for i := 7 downto 0 do
  begin
    k := 7;
    while used[k] do Dec(k);
    while order[i] > 0 do
    begin
      Dec(order[i]);
      repeat Dec(k); until not used[k];
    end;
    cc.edge[i].e := k;
    used[k] := true;
  end;
  // Fill UD-slice edges to identity
  for i := 8 to 11 do cc.edge[i].e := i;
end;

function GetEdgePermCoord(const cc: TCubieCube): integer;
var
  i, j: integer;
  x, s: integer;
begin
  x := 0;
  for i := 11 downto 1 do
  begin
    s := 0;
    for j := i - 1 downto 0 do
      if cc.edge[j].e > cc.edge[i].e then Inc(s);
    x := (x + s) * i;
  end;
  Result := x;
end;

procedure SetEdgePermCoord(var cc: TCubieCube; w: integer);
var
  used: array[0..11] of boolean;
  order: array[0..11] of integer;
  i, k: integer;
begin
  for i := 0 to 11 do
  begin
    used[i] := false;
    order[i] := w mod (i + 1);
    w := w div (i + 1);
  end;
  for i := 11 downto 0 do
  begin
    k := 11;
    while used[k] do Dec(k);
    while order[i] > 0 do
    begin
      Dec(order[i]);
      repeat Dec(k); until not used[k];
    end;
    cc.edge[i].e := k;
    used[k] := true;
  end;
end;

function CornParityEven(const cc: TCubieCube): boolean;
var
  i, j, s: integer;
begin
  s := 0;
  for i := 7 downto 1 do
    for j := i - 1 downto 0 do
      if cc.corn[j].c > cc.corn[i].c then Inc(s);
  Result := not Odd(s);
end;

function EdgeParityEven(const cc: TCubieCube): boolean;
var
  i, j, s: integer;
begin
  s := 0;
  for i := 11 downto 1 do
    for j := i - 1 downto 0 do
      if cc.edge[j].e > cc.edge[i].e then Inc(s);
  Result := not Odd(s);
end;

function FaceletsToCubieCube(const facelets: string; out cc: TCubieCube): boolean;
var
  facelet: array[0..53] of byte; // color index 0..5
  i, j, ori: integer;
  col1, col2: byte;
begin
  Result := false;
  FaceletParseError := '';
  if Length(facelets) <> 54 then
  begin
    FaceletParseError := 'length=' + IntToStr(Length(facelets)) + ' (expected 54)';
    exit;
  end;

  CubieIdentity(cc);

  // Parse facelet string to color indices
  for i := 0 to 53 do
  begin
    case facelets[i + 1] of
      'U': facelet[i] := 0;
      'R': facelet[i] := 1;
      'F': facelet[i] := 2;
      'D': facelet[i] := 3;
      'L': facelet[i] := 4;
      'B': facelet[i] := 5;
    else
      begin
        FaceletParseError := 'invalid char "' + facelets[i + 1] + '" at position ' + IntToStr(i);
        exit;
      end;
    end;
  end;

  // Determine corner positions and orientations
  for i := 0 to 7 do
  begin
    // Find the orientation (which facelet has U or D color)
    ori := -1;
    for j := 0 to 2 do
      if (facelet[CF[i, j]] = 0) or (facelet[CF[i, j]] = 3) then  // U or D color
      begin
        ori := j;
        break;
      end;
    if ori < 0 then
    begin
      FaceletParseError := 'corner ' + IntToStr(i) + ': no U/D color at positions ' +
        IntToStr(CF[i,0]) + ',' + IntToStr(CF[i,1]) + ',' + IntToStr(CF[i,2]) +
        ' (colors ' + IntToStr(facelet[CF[i,0]]) + ',' + IntToStr(facelet[CF[i,1]]) +
        ',' + IntToStr(facelet[CF[i,2]]) + ')';
      exit;
    end;

    // Get the other two colors to identify the corner
    col1 := facelet[CF[i, (ori + 1) mod 3]];
    col2 := facelet[CF[i, (ori + 2) mod 3]];

    // Find which corner this is
    cc.corn[i].c := 255; // sentinel
    for j := 0 to 7 do
      if (col1 = CCI[j, 1]) and (col2 = CCI[j, 2]) then
      begin
        cc.corn[i].c := j;
        cc.corn[i].o := ori;
        break;
      end;
    if cc.corn[i].c = 255 then
    begin
      FaceletParseError := 'corner ' + IntToStr(i) + ': unknown corner with colors ' +
        IntToStr(facelet[CF[i,0]]) + ',' + IntToStr(facelet[CF[i,1]]) + ',' +
        IntToStr(facelet[CF[i,2]]);
      exit;
    end;
  end;

  // Determine edge positions and orientations
  for i := 0 to 11 do
  begin
    cc.edge[i].e := 255; // sentinel
    for j := 0 to 11 do
    begin
      if (facelet[EF[i, 0]] = ECI[j, 0]) and (facelet[EF[i, 1]] = ECI[j, 1]) then
      begin
        cc.edge[i].e := j;
        cc.edge[i].o := 0;
        break;
      end
      else if (facelet[EF[i, 0]] = ECI[j, 1]) and (facelet[EF[i, 1]] = ECI[j, 0]) then
      begin
        cc.edge[i].e := j;
        cc.edge[i].o := 1;
        break;
      end;
    end;
    if cc.edge[i].e = 255 then
    begin
      FaceletParseError := 'edge ' + IntToStr(i) + ': unknown edge with colors ' +
        IntToStr(facelet[EF[i,0]]) + ',' + IntToStr(facelet[EF[i,1]]) +
        ' at positions ' + IntToStr(EF[i,0]) + ',' + IntToStr(EF[i,1]);
      exit;
    end;
  end;

  Result := true;
end;

function CubieCubeToFacelets(const cc: TCubieCube): string;
var
  facelet: array[0..53] of byte;
  i, n, j: integer;
  faceChars: array[0..5] of char;
begin
  faceChars[0] := 'U'; faceChars[1] := 'R'; faceChars[2] := 'F';
  faceChars[3] := 'D'; faceChars[4] := 'L'; faceChars[5] := 'B';

  // Set center facelets
  for i := 0 to 5 do
    facelet[i * 9 + 4] := i;

  // Set corner facelets
  for i := 0 to 7 do
    for n := 0 to 2 do
      facelet[CF[i, n]] := CCI[cc.corn[i].c, (n + 3 - cc.corn[i].o) mod 3];

  // Set edge facelets
  for j := 0 to 11 do
    for n := 0 to 1 do
      facelet[EF[j, n]] := ECI[cc.edge[j].e, (n + cc.edge[j].o) mod 2];

  SetLength(Result, 54);
  for i := 0 to 53 do
    Result[i + 1] := faceChars[facelet[i]];
end;

end.
