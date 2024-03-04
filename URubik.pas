unit URubik;

(*
  --------------------------------------------------------------------------------
  Original Source Code obtained from: https://codes-sources.commentcamarche.net/source/53132-rubik-s-cube

  Modifications made by: Tony Stone
  Date of Modification: 2/25/2024

  This source code is made available under the terms and conditions outlined in the
  General Conditions of Use of the CodeS-SourceS.CommentCaMarche.net website.
  The terms of use can be found at: https://codes-sources.commentcamarche.net/contents/1-conditions-generales-d-utilisation

  The original work is being modified while respecting the applicable terms of use
  as specified on the CodeS-SourceS.CommentCaMarche.net website.

  Please refer to the above link for detailed information on the usage rights and
  limitations according to the General Conditions of Use.
  --------------------------------------------------------------------------------
*)

{$MODE Delphi}

interface

uses
  graphics,
  StrUtils,
  SysUtils,
  UConst,
  UDraw;

procedure LFDstringCorrection(var s: string);
function CubeToDefinitionString(cube: TRubik): string;
procedure RotateFace(var cube: TUnitRubik; face, rotation: integer);
procedure ApplyFormula(var cube: TRubik; s: string);
function findEdge(cube: TFaceRubik; c: integer; sens: byte = 0): integer;
function findCorner(cube: TFaceRubik; a: integer): integer;
procedure placeWhiteEdges(var cube: TRubik);
procedure placeWhiteCorners(var cube: TRubik);
procedure placeSecondLayerEdges(var cube: TRubik);
procedure PlaceYellowEdges(var cube: TRubik);
procedure OrientYellowEdges(var cube: TRubik);
procedure PlaceYellowCorners(var cube: TRubik);
procedure OrientYellowCorners(var cube: TRubik);
function FilterMoves(var s: string; cube: TFaceRubik; can: tcanvas): string;
function CountMoves(s: string): integer;
function VerifyCube(original: TFaceRubik; var s: string): boolean;

var
  solu: string = '';

implementation

procedure LFDstringCorrection(var s: string);
{ #note -oTonyStone : Yes this is very hackish.  It was currently the easiest solution for me to
invert the rotation directions for the Left Front and Down faces.  A proper solution I think may
be to adjust the rotations in the RotateFace procedure in the future. }
var
  i: Integer;
  newS: string;
begin
  newS := ''; // Initialize the new string
  i := 1;
  while i <= Length(s) do
  begin
    if (i < Length(s)) and (s[i + 1] = '''') then
    begin
      // Check for L', D', F' and convert them to L, D, F
      case s[i] of
        'L', 'D', 'F': newS := newS + s[i]; // Drop the '
      else
        newS := newS + s[i] + s[i + 1]; // Keep other moves as they are
      end;
      Inc(i, 2); // Skip the next character as it's already processed
    end
    else if (i < Length(s)) and (s[i + 1] = '2') then
    begin
      newS := newS + s[i] + s[i + 1];
      Inc(i, 2); // Skip the next character as it's already processed
    end
    else
    begin
      // Handle the last character or any character not followed by ' or 2
      case s[i] of
        'L', 'D', 'F': newS := newS + s[i] + '''';
      else
        newS := newS + s[i];
      end;
      Inc(i);
    end;
  end;
  s := newS; // Update the original string
end;

function CubeToDefinitionString(cube: TRubik): string;
var
  faceOrder: array[1..6] of integer; // Array to define the custom order of faces
  face, i: integer;
  faceName: string;
begin
  Result := '';
  // remap so string matches expected input in other solvers
  faceOrder[1] := 1; // UP
  faceOrder[2] := 3; // FRONT
  faceOrder[3] := 2; // RIGHT
  faceOrder[4] := 6; // BACK
  faceOrder[5] := 5; // LEFT
  faceOrder[6] := 4; // DOWN

  for face := 1 to 6 do
  begin
    // Use faceOrder array to access the faces in the desired order
    for i := 0 to 8 do
    begin
      faceName := FACE_NAMES[cube[faceOrder[face], i]];
      Result := Result + faceName;
    end;
  end;
end;

//UUUUUUUUULLLLLLLLLFFFFFFFFFRRRRRRRRRBBBBBBBBBDDDDDDDDD
//BDBBURBRFRBUFLFFBBLULUFULDRURRLRUDFDDDUBBLLFRULDDDLFRF

function CountMoves(s: string): integer;
var
  i, j: integer;
begin
  Result := 0;
  s := AnsiUpperCase(s);
  i := 1;
  while i <= length(s) do
  begin
    j := 1;
    if (i < length(s)) and (s[i + 1] = '''') then j := 3;
    if (i < length(s)) and (s[i + 1] = '2') then j := 2;
    case s[i] of
      'L', 'R', 'B', 'F', 'U', 'D': Inc(Result);
    end;
    if j > 1 then Inc(i, 2)
    else
      Inc(i);
  end;
end;
         // French still... I think it is just comparing two cubes.  Will rename
function MemeCubes(c1, c2: TRubik): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to 53 do if TLinRubik(c1)[i] <> TLinRubik(c2)[i] then exit;
  Result := True;
end;

procedure RotateFace(var cube: TUnitRubik; face, rotation: integer);
var
  i, j: integer;
  tmp: TUnitRubik;
begin
  if rotation = 0 then exit;
  if rotation > 1 then RotateFace(cube, face, rotation - 1);

  tmp := cube;

  case face of // Faces that rotate "naturally" with the given logic
    CUBE_TOP: // face  blanche
    begin     // White Face
      for j := 0 to 2 do for i := 0 to 2 do tmp[1, j, i] := cube[1, 2 - i, j];
      for i := 0 to 2 do tmp[2, 0, i] := cube[3, 0, i];
      for i := 0 to 2 do tmp[3, 0, i] := cube[4, 0, i];
      for i := 0 to 2 do tmp[4, 0, i] := cube[5, 0, i];
      for i := 0 to 2 do tmp[5, 0, i] := cube[2, 0, i];
    end;

    CUBE_BACK: // face 1 bleu
    begin      // Blue Face
      for j := 0 to 2 do for i := 0 to 2 do tmp[4, j, i] := cube[4, 2 - i, j];
      for i := 0 to 2 do tmp[1, 0, i] := cube[3, i, 2];
      for i := 0 to 2 do tmp[3, i, 2] := cube[6, 2, 2 - i];
      for i := 0 to 2 do tmp[6, 2, 2 - i] := cube[5, 2 - i, 0];
      for i := 0 to 2 do tmp[5, 2 - i, 0] := cube[1, 0, i];
    end;

    CUBE_RIGHT: // face 4 rouge
    begin       // Red Face
      for j := 0 to 2 do for i := 0 to 2 do tmp[3, j, i] := cube[3, 2 - i, j];
      for i := 0 to 2 do tmp[1, i, 2] := cube[2, i, 2];
      for i := 0 to 2 do tmp[2, i, 2] := cube[6, i, 2];
      for i := 0 to 2 do tmp[6, i, 2] := cube[4, 2 - i, 0];
      for i := 0 to 2 do tmp[4, 2 - i, 0] := cube[1, i, 2];
    end;
  end;

  case face of
    CUBE_BOTTOM: // face  jaune
    begin        // Yellow Face
      //Orginal before I flipped directions.
      for j := 0 to 2 do for i := 0 to 2 do tmp[6, j, i] := cube[6, i, 2 - j];
      for i := 0 to 2 do tmp[2, 2, i] := cube[3, 2, i];
      for i := 0 to 2 do tmp[3, 2, i] := cube[4, 2, i];
      for i := 0 to 2 do tmp[4, 2, i] := cube[5, 2, i];
      for i := 0 to 2 do tmp[5, 2, i] := cube[2, 2, i];
    end;
    CUBE_LEFT: // Orange Face - Adjusted for counter-clockwise rotation
    begin
      for j := 0 to 2 do for i := 0 to 2 do tmp[5, j, i] := cube[5, i, 2 - j];
      for i := 0 to 2 do tmp[1, i, 0] := cube[2, i, 0];
      for i := 0 to 2 do tmp[2, i, 0] := cube[6, i, 0];
      for i := 0 to 2 do tmp[6, i, 0] := cube[4, 2 - i, 2];
      for i := 0 to 2 do tmp[4, 2 - i, 2] := cube[1, i, 0];
    end;

    CUBE_FRONT: // face 3 vert
    begin       // Face Green
      for j := 0 to 2 do for i := 0 to 2 do tmp[2, j, i] := cube[2, i, 2 - j];
      for i := 0 to 2 do tmp[1, 2, i] := cube[3, i, 0];
      for i := 0 to 2 do tmp[3, i, 0] := cube[6, 0, 2 - i];
      for i := 0 to 2 do tmp[6, 0, 2 - i] := cube[5, 2 - i, 2];
      for i := 0 to 2 do tmp[5, 2 - i, 2] := cube[1, 2, i];
    end;
  end;

  // not into this yet but its something....
  case face of
    CUBE_CENTER_HORIZONTAL: // Between CUBE_TOP and CUBE_BOTTOM
    begin
      for i := 0 to 2 do tmp[2, 1, i] := cube[3, 1, i];
      for i := 0 to 2 do tmp[3, 1, i] := cube[4, 1, i];
      for i := 0 to 2 do tmp[4, 1, i] := cube[5, 1, i];
      for i := 0 to 2 do tmp[5, 1, i] := cube[2, 1, i];
    end;
    CUBE_CENTER_DEPTH: // Between CUBE_FRONT and CUBE_BACK
    begin
      for i := 0 to 2 do tmp[1, 1, i] := cube[3, i, 1];
      for i := 0 to 2 do tmp[3, i, 1] := cube[6, 1, 2 - i];
      for i := 0 to 2 do tmp[6, 1, 2 - i] := cube[5, 2 - i, 1];
      for i := 0 to 2 do tmp[5, 2 - i, 1] := cube[1, 1, i];
    end;
    CUBE_CENTER_VERTICAL: // Between CUBE_LEFT and CUBE_RIGHT
    begin
      for i := 0 to 2 do tmp[1, i, 1] := cube[2, i, 1];
      for i := 0 to 2 do tmp[2, i, 1] := cube[6, i, 1];
      for i := 0 to 2 do tmp[6, i, 1] := cube[4, 2 - i, 1];
      for i := 0 to 2 do tmp[4, 2 - i, 1] := cube[1, i, 1];
    end;
  end;
  cube := tmp;
end;



procedure ApplyFormula(var cube: TRubik; s: string);
var
  i, j: integer;
begin
  s := AnsiUpperCase(s);
  solu := solu + s + '/';
  i := 1;
  while i <= length(s) do
  begin
    j := 1;
    if (i < length(s)) and (s[i + 1] = '''') then j := 3;
    if (i < length(s)) and (s[i + 1] = '2') then j := 2;
    case s[i] of
      'L': RotateFace(TUnitRubik(cube), CUBE_LEFT, j);
      'R': RotateFace(TUnitRubik(cube), CUBE_RIGHT, j);
      'B': RotateFace(TUnitRubik(cube), CUBE_BACK, j);
      'F': RotateFace(TUnitRubik(cube), CUBE_FRONT, j);
      'U': RotateFace(TUnitRubik(cube), CUBE_TOP, j);
      'D': RotateFace(TUnitRubik(cube), CUBE_BOTTOM, j);
    end;
    if j > 1 then Inc(i, 2)
    else
      Inc(i);
  end;
end;

function RotateEdge(cube: TFaceRubik; c, ref: integer): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to 23 do if c = EDGE_POSITIONS[i, 4] then
    begin
      if cube[EDGE_POSITIONS[i, 0], EDGE_POSITIONS[i, 1]] = ref then Result := 1
      else if cube[EDGE_POSITIONS[i, 2], EDGE_POSITIONS[i, 3]] = ref then Result := 2;
    end;
end;

// Recherche du coté c dans cube
function findEdge(cube: TFaceRubik; c: integer; sens: byte = 0): integer;
var
  i: integer;
  c1, c2: integer;
begin
  Result := 0;
  case sens of
    0:
    begin
      for i := 0 to 23 do if c = EDGE_POSITIONS[i, 4] then
        begin
          c1 := TargetCubeState[EDGE_POSITIONS[i, 0], EDGE_POSITIONS[i, 1]];
          c2 := TargetCubeState[EDGE_POSITIONS[i, 2], EDGE_POSITIONS[i, 3]];
        end;

      for i := 0 to 23 do
        if (cube[EDGE_POSITIONS[i, 0], EDGE_POSITIONS[i, 1]] = c1) and
          (cube[EDGE_POSITIONS[i, 2], EDGE_POSITIONS[i, 3]] = c2) then
          Result := EDGE_POSITIONS[i, 4];
    end;
    1:
    begin
      for i := 0 to 23 do if c = EDGE_POSITIONS[i, 4] then
        begin
          c1 := cube[EDGE_POSITIONS[i, 0], EDGE_POSITIONS[i, 1]];
          c2 := cube[EDGE_POSITIONS[i, 2], EDGE_POSITIONS[i, 3]];
        end;

      for i := 0 to 23 do
        if (TargetCubeState[EDGE_POSITIONS[i, 0], EDGE_POSITIONS[i, 1]] = c1) and
          (TargetCubeState[EDGE_POSITIONS[i, 2], EDGE_POSITIONS[i, 3]] = c2) then Result := EDGE_POSITIONS[i, 4];
    end
  end;
end;

function findCorner(cube: TFaceRubik; a: integer): integer;
var
  i: integer;
  c1, c2, c3: integer;
begin
  Result := 0;
  for i := 0 to 23 do
    if a = CORNER_POSITION[i, 6] then
    begin
      c1 := TargetCubeState[CORNER_POSITION[i, 0], CORNER_POSITION[i, 1]];
      c2 := TargetCubeState[CORNER_POSITION[i, 2], CORNER_POSITION[i, 3]];
      c3 := TargetCubeState[CORNER_POSITION[i, 4], CORNER_POSITION[i, 5]];
    end;

  for i := 0 to 23 do
    if (cube[CORNER_POSITION[i, 0], CORNER_POSITION[i, 1]] = c1) and
      (cube[CORNER_POSITION[i, 2], CORNER_POSITION[i, 3]] = c2) and
      (cube[CORNER_POSITION[i, 4], CORNER_POSITION[i, 5]] = c3) then Result := CORNER_POSITION[i, 6];
end;

function VerifyCube(original: TFaceRubik; var s: string): boolean;
begin
  Result := False;
  s := '';
  if findEdge(original, 12) = 0 then s := s + 'The White/Green side is missing.' + #13#10;
  if findEdge(original, 13) = 0 then s := s + 'The White/Red side is missing.' + #13#10;
  if findEdge(original, 14) = 0 then s := s + 'The White/Blue side is missing' + #13#10;
  if findEdge(original, 15) = 0 then s := s + 'The White/Orange side is missing.' + #13#10;

  if findEdge(original, 23) = 0 then s := s + 'The Green/Red side is missing' + #13#10;
  if findEdge(original, 34) = 0 then s := s + 'The Red/Blue side is missing' + #13#10;
  if findEdge(original, 45) = 0 then s := s + 'The Blue/Orange side is missing' + #13#10;
  if findEdge(original, 52) = 0 then s := s + 'The Orange/Green side is missing' + #13#10;


  if findEdge(original, 62) = 0 then s := s + 'The Yellow/Green side is missing' + #13#10;
  if findEdge(original, 63) = 0 then s := s + 'The Yellow/Red side is missing' + #13#10;
  if findEdge(original, 64) = 0 then s := s + 'The Yellow/Blue side is missing' + #13#10;
  if findEdge(original, 65) = 0 then s := s + 'The Yellow/Orange side is missing' + #13#10;

  if findCorner(original, 123) = 0 then s := s + 'The White/Green/Red corner is missing' + #13#10;
  if findCorner(original, 134) = 0 then s := s + 'The White/Red/Blue corner is missing' + #13#10;
  if findCorner(original, 145) = 0 then s := s + 'The White/Blue/Orange corner is missing' + #13#10;
  if findCorner(original, 152) = 0 then s := s + 'The White/Orange/Green corner is missing' + #13#10;


  if findCorner(original, 632) = 0 then s := s + 'The Yellow/Green/Red corner is missing' + #13#10;
  if findCorner(original, 643) = 0 then s := s + 'The Yellow/Red/Blue corner is missing' + #13#10;
  if findCorner(original, 654) = 0 then s := s + 'The Yellow/Blue/Orange corner is missing' + #13#10;
  if findCorner(original, 625) = 0 then s := s + 'The Yellow/Orange/Green corner is missing' + #13#10;


  Result := s = '';
end;

// Placements des cotés du centre blanc
// Placement of the sides of the white center
procedure placeWhiteEdges(var cube: TRubik);
begin
  // coté blanc/vert
  // white/green side
  case findEdge(cube, 12) of
    12: ;
    13: ApplyFormula(cube, 'U');
    14: ApplyFormula(cube, 'U2');
    15: ApplyFormula(cube, 'U''');
    21: ApplyFormula(cube, 'F''RU');
    23: ApplyFormula(cube, 'RU');
    25: ApplyFormula(cube, 'LU''');
    26: ApplyFormula(cube, 'FRU');
    31: ApplyFormula(cube, 'R''F');
    32: ApplyFormula(cube, 'F');
    34: ApplyFormula(cube, 'BU2');
    36: ApplyFormula(cube, 'RF');
    41: ApplyFormula(cube, 'UR''F');
    43: ApplyFormula(cube, 'R''U');
    45: ApplyFormula(cube, 'L''U''');
    46: ApplyFormula(cube, 'BR''U');
    51: ApplyFormula(cube, 'L''F''');
    52: ApplyFormula(cube, 'F''');
    54: ApplyFormula(cube, 'B''U2');
    56: ApplyFormula(cube, 'LF''');
    62: ApplyFormula(cube, 'F2');
    63: ApplyFormula(cube, 'DF2');
    64: ApplyFormula(cube, 'B2U2');
    65: ApplyFormula(cube, 'L2U''');
  end;

  // Placement du coté Blanc/Rouge avec sauvegarde de la position Blanc/Vert
  // Placement of the White/Red side with saving of the White/Green position

  case findEdge(cube, 13) of
    13: ;
    14: ApplyFormula(cube, 'FUF''');
    15: ApplyFormula(cube, 'FU2F''');
    23: ApplyFormula(cube, 'R');
    25: ApplyFormula(cube, 'U2LU2');
    26: ApplyFormula(cube, 'D''R''U''BU');
    31: ApplyFormula(cube, 'RU''BU');
    32: ApplyFormula(cube, 'UFU''');
    34: ApplyFormula(cube, 'U''BU');
    36: ApplyFormula(cube, 'R''U''BU');
    41: ApplyFormula(cube, 'B''R''');
    43: ApplyFormula(cube, 'R''');
    45: ApplyFormula(cube, 'B2R''');
    46: ApplyFormula(cube, 'BR''');
    51: ApplyFormula(cube, 'L''UF''U''');
    52: ApplyFormula(cube, 'UF''U''');
    54: ApplyFormula(cube, 'L2UF''U''');
    56: ApplyFormula(cube, 'LUF''U''');
    62: ApplyFormula(cube, 'D''R2');
    63: ApplyFormula(cube, 'R2');
    64: ApplyFormula(cube, 'DR2');
    65: ApplyFormula(cube, 'D2R2');
  end;

  // Placement du coté Blanc/Bleu avec sauvegarde des positions Blanc/Vert et Blanc/Rouge
  // Placement of the White/Blue side with saving of the White/Green and White/Red positions

  case findEdge(cube, 14) of
    14: ;
    15: ApplyFormula(cube, 'LU''L''U');
    23: ApplyFormula(cube, 'URU''');
    25: ApplyFormula(cube, 'U''LU');
    26: ApplyFormula(cube, 'DL''B''');
    32: ApplyFormula(cube, 'U2FU2');
    34: ApplyFormula(cube, 'B');
    36: ApplyFormula(cube, 'UR''U''B');
    41: ApplyFormula(cube, 'B''UR''U''');
    43: ApplyFormula(cube, 'UR''U''');
    45: ApplyFormula(cube, 'U''L''U');
    46: ApplyFormula(cube, 'BUR''U''');
    51: ApplyFormula(cube, 'LB''');
    52: ApplyFormula(cube, 'L2B''');
    54: ApplyFormula(cube, 'B''');
    56: ApplyFormula(cube, 'L''B''');
    62: ApplyFormula(cube, 'D2B2');
    63: ApplyFormula(cube, 'D''B2');
    64: ApplyFormula(cube, 'B2');
    65: ApplyFormula(cube, 'DB2');
  end;

  // Placement du dernier coté Blanc/Orange avec sauvegarde des positions Blanc/Vert, Blanc/Rouge et Blanc/Bleu
  // Placement of the last White/Orange side with saving of the White/Green, White/Red, and White/Blue positions

  case findEdge(cube, 15) of
    15: ;
    23: ApplyFormula(cube, 'U2RU2');
    25: ApplyFormula(cube, 'L');
    26: ApplyFormula(cube, 'U''F''UL');
    32: ApplyFormula(cube, 'U''FU');
    34: ApplyFormula(cube, 'UBU''');
    36: ApplyFormula(cube, 'DU''F''UL');
    43: ApplyFormula(cube, 'U2R''U2');
    45: ApplyFormula(cube, 'L''');
    46: ApplyFormula(cube, 'UB''U''L''');
    51: ApplyFormula(cube, 'L''U''F''U');
    52: ApplyFormula(cube, 'U''F''U');
    54: ApplyFormula(cube, 'UB''U''');
    56: ApplyFormula(cube, 'LU''F''U');
    62: ApplyFormula(cube, 'DL2');
    63: ApplyFormula(cube, 'D2L2');
    64: ApplyFormula(cube, 'D''L2');
    65: ApplyFormula(cube, 'L2');
  end;
end;

// Placements des angles du centre blanc
// Placement of the corners of the white center

procedure placeWhiteCorners(var cube: TRubik);
begin

  // Placement de l'angle Blanc/Vert/Rouge
  // Placement of the White/Green/Red corner


  case (findCorner(cube, 123)) of
    123: ;
    231: ApplyFormula(cube, 'F''D''FDRFR''F''');
    312: ApplyFormula(cube, 'R''DRD''FRF''R''');
    134: ApplyFormula(cube, 'B''DBRFR''F''');
    341: ApplyFormula(cube, 'RD''R''D2RFR''F''');
    413: ApplyFormula(cube, 'B''DBFRF''R''');
    145: ApplyFormula(cube, 'LD2L''RFR''F''');
    451: ApplyFormula(cube, 'BD2B''RFR''F''');
    514: ApplyFormula(cube, 'LD2L''FRF''R''');
    152: ApplyFormula(cube, 'L''D''LFRF''R''');
    521: ApplyFormula(cube, 'L''D''LRFR''F''');
    215: ApplyFormula(cube, 'FDF''D2FRF''R''');
    263: ApplyFormula(cube, 'RFR''F''');
    364: ApplyFormula(cube, 'DRFR''F''');
    465: ApplyFormula(cube, 'D2RFR''F''');
    562: ApplyFormula(cube, 'D''RFR''F''');
    326: ApplyFormula(cube, 'FRF''R''');
    436: ApplyFormula(cube, 'DFRF''R''');
    546: ApplyFormula(cube, 'D2FRF''R''');
    256: ApplyFormula(cube, 'D''FRF''R''');
    632: ApplyFormula(cube, 'F''DFD2RFR''F''');
    643: ApplyFormula(cube, 'DF''DFD2RFR''F''');
    654: ApplyFormula(cube, 'D2F''DFD2RFR''F''');
    625: ApplyFormula(cube, 'D''F''DFD2RFR''F''');
  end;

  // Placement de l'angle Blanc/Rouge/Bleu avec sauvegarde de la position Blanc/Vert/Rouge
  // Placement of the White/Red/Blue corner with saving of the White/Green/Red position

  case (findCorner(cube, 134)) of
    134: ;
    341: ApplyFormula(cube, 'RD''R''DBR''B''R');
    413: ApplyFormula(cube, 'B''DBD''R''BRB''');
    145: ApplyFormula(cube, 'LDL''BR''B''R');
    451: ApplyFormula(cube, 'BD''B''D2BR''B''R');
    514: ApplyFormula(cube, 'LDL''R''BRB''');
    152: ApplyFormula(cube, 'L''D2LR''BRB''');
    521: ApplyFormula(cube, 'L''D2LBR''B''R');
    215: ApplyFormula(cube, 'FD2F''R''BRB''');
    263: ApplyFormula(cube, 'D''BR''B''R');
    364: ApplyFormula(cube, 'BR''B''R');
    465: ApplyFormula(cube, 'DBR''B''R');
    562: ApplyFormula(cube, 'D2BR''B''R');
    326: ApplyFormula(cube, 'D''R''BRB''');
    436: ApplyFormula(cube, 'R''BRB''');
    546: ApplyFormula(cube, 'DR''BRB''');
    256: ApplyFormula(cube, 'D2R''BRB''');
    632: ApplyFormula(cube, 'D''RDR''D2BR''B''R');
    643: ApplyFormula(cube, 'RDR''D2BR''B''R');
    654: ApplyFormula(cube, 'DRDR''D2BR''B''R');
    625: ApplyFormula(cube, 'D2RDR''D2BR''B''R');
  end;

  // Placement de l'angle Blanc/Bleu/Orange avec sauvegarde des positions Blanc/Vert/Rouge et Blanc/Rouge/Bleu
  // Placement of the White/Blue/Orange corner with saving of the White/Green/Red and White/Red/Blue positions

  case (findCorner(cube, 145)) of
    145: ;
    451: ApplyFormula(cube, 'BD''B''DL''B''LB');
    514: ApplyFormula(cube, 'LDL''D''B''L''BL');
    152: ApplyFormula(cube, 'FDF''L''B''LB');
    521: ApplyFormula(cube, 'L''D''LD2L''B''LB');
    215: ApplyFormula(cube, 'FDF''B''L''BL');
    263: ApplyFormula(cube, 'D2L''B''LB');
    364: ApplyFormula(cube, 'D''L''B''LB');
    465: ApplyFormula(cube, 'L''B''LB');
    562: ApplyFormula(cube, 'DL''B''LB');
    326: ApplyFormula(cube, 'D2B''L''BL');
    436: ApplyFormula(cube, 'D''B''L''BL');
    546: ApplyFormula(cube, 'B''L''BL');
    256: ApplyFormula(cube, 'DB''L''BL');
    632: ApplyFormula(cube, 'D2LD''L''D2B''L''BL');
    643: ApplyFormula(cube, 'D''LD''L''D2B''L''BL');
    654: ApplyFormula(cube, 'LD''L''D2B''L''BL');
    625: ApplyFormula(cube, 'DLD''L''D2B''L''BL');
  end;

  // Placement de l'angle Blanc/Orange/Vert avec sauvegarde des positions Blanc/Vert/Rouge, Blanc/Rouge/Bleu et Blanc/Bleu/Orange
  // Placement of the White/Orange/Green corner with saving of the White/Green/Red, White/Red/Blue, and White/Blue/Orange positions

  case (findCorner(cube, 152)) of
    152: ;
    521: ApplyFormula(cube, 'L''D''LDF''LFL''');
    215: ApplyFormula(cube, 'FDF''D''LF''L''F');
    263: ApplyFormula(cube, 'DF''LFL''');
    364: ApplyFormula(cube, 'D2F''LFL''');
    465: ApplyFormula(cube, 'D''F''LFL''');
    562: ApplyFormula(cube, 'F''LFL''');
    326: ApplyFormula(cube, 'DLF''L''F');
    436: ApplyFormula(cube, 'D2LF''L''F');
    546: ApplyFormula(cube, 'D''LF''L''F');
    256: ApplyFormula(cube, 'LF''L''F');
    632: ApplyFormula(cube, 'DFD''F''D2LF''L''F');
    643: ApplyFormula(cube, 'D2FD''F''D2LF''L''F');
    654: ApplyFormula(cube, 'D''FD''F''D2LF''L''F');
    625: ApplyFormula(cube, 'FD''F''D2LF''L''F');
  end;
end;


procedure placeSecondLayerEdges(var cube: TRubik);
begin
  // Placement du coté Vert/Rouge
  // Placement of the Green/Red side

  case findEdge(cube, 23) of
    23: ;
    32: ApplyFormula(cube, 'R''DRD''F''D''FDR''D2RD2F''D''F');
    34: ApplyFormula(cube, 'B''DBD''RD''R''D''F''D2FD2R''DR');
    43: ApplyFormula(cube, 'B''DBD''RD''R''D2R''D2RD2F''D''F');
    45: ApplyFormula(cube, 'LDL''D''BD''B''F''D2FD2R''DR');
    54: ApplyFormula(cube, 'LDL''D''BD''B''D''R''D2RD2F''D''F');
    52: ApplyFormula(cube, 'FDF''D''L''D''LDF''D2FD2R''DR');
    25: ApplyFormula(cube, 'FDF''D''L''D''LR''D2RD2F''D''F');
    26: ApplyFormula(cube, 'D''R''D2RD2F''D''F');
    62: ApplyFormula(cube, 'F''D2FD2R''DR');
    36: ApplyFormula(cube, 'R''D2RD2F''D''F');
    63: ApplyFormula(cube, 'DF''D2FD2R''DR');
    46: ApplyFormula(cube, 'DR''D2RD2F''D''F');
    64: ApplyFormula(cube, 'D2F''D2FD2R''DR');
    56: ApplyFormula(cube, 'D2R''D2RD2F''D''F');
    65: ApplyFormula(cube, 'D''F''D2FD2R''DR');
  end;

  // Placement du coté Rouge/Bleu avec sauvegarde de la position Vert/Rouge
  // Placement of the Red/Blue side with saving of the Green/Red position

  case findEdge(cube, 34) of
    34: ;
    43: ApplyFormula(cube, 'B''DBD''RD''R''DB''D2BD2RD''R''');
    45: ApplyFormula(cube, 'LDL''D''BD''B''D''RD2R''D2B''DB');
    54: ApplyFormula(cube, 'LDL''D''BD''B''D2B''D2BD2RD''R''');
    52: ApplyFormula(cube, 'FDF''D''L''D''LRD2R''D2B''DB');
    25: ApplyFormula(cube, 'FDF''D''L''D''LD''B''D2BD2RD''R''');
    26: ApplyFormula(cube, 'D2B''D2BD2RD''R''');
    62: ApplyFormula(cube, 'D''RD2R''D2B''DB');
    36: ApplyFormula(cube, 'D''B''D2BD2RD''R''');
    63: ApplyFormula(cube, 'RD2R''D2B''DB');
    46: ApplyFormula(cube, 'B''D2BD2RD''R''');
    64: ApplyFormula(cube, 'DRD2R''D2B''DB');
    56: ApplyFormula(cube, 'DB''D2BD2RD''R''');
    65: ApplyFormula(cube, 'D2RD2R''D2B''DB');
  end;

  // Placement du coté Bleu/Orange avec sauvegarde des positions Vert/Rouge et Rouge/Bleu
  // Placement of the Blue/Orange side with saving of the Green/Red and Red/Blue positions

  case findEdge(cube, 45) of
    45: ;
    54: ApplyFormula(cube, 'LDL''D''BD''B''DLD2L''D2BD''B''');
    52: ApplyFormula(cube, 'FDF''D''L''D''LD''BD2B''D2LDL''');
    25: ApplyFormula(cube, 'FDF''D''L''D''LD2LD2L''D2BD''B''');
    26: ApplyFormula(cube, 'DLD2L''D2BD''B''');
    62: ApplyFormula(cube, 'D2BD2B''D2LDL''');
    36: ApplyFormula(cube, 'D2LD2L''D2BD''B''');
    63: ApplyFormula(cube, 'D''BD2B''D2LDL''');
    46: ApplyFormula(cube, 'D''LD2L''D2BD''B''');
    64: ApplyFormula(cube, 'BD2B''D2LDL''');
    56: ApplyFormula(cube, 'LD2L''D2BD''B''');
    65: ApplyFormula(cube, 'DBD2B''D2LDL''');
  end;

  // Placement du coté Orange/Vert avec sauvegarde des positions Vert/Rouge, Rouge/Bleu et Bleu/Orange
  // Placement of the Orange/Green side with saving of the Green/Red, Red/Blue, and Blue/Orange positions

  case findEdge(cube, 52) of
    52: ;
    25: ApplyFormula(cube, 'FDF''D''L''D''LDFD2F''D2L''D''L');
    26: ApplyFormula(cube, 'FD2F''D2L''D''L');
    62: ApplyFormula(cube, 'DL''D2LD2FDF''');
    36: ApplyFormula(cube, 'DFD2F''D2L''D''L');
    63: ApplyFormula(cube, 'D2L''D2LD2FDF''');
    46: ApplyFormula(cube, 'D2FD2F''D2L''D''L');
    64: ApplyFormula(cube, 'D''L''D2LD2FDF''');
    56: ApplyFormula(cube, 'D''FD2F''D2L''D''L');
    65: ApplyFormula(cube, 'L''D2LD2FDF''');
  end;
end;

const
  QuoiFaire0: array[1..24, 1..7] of integer =
    ((2345, 0, 0, 0, 0, 0, 0), (2354, 4, 1, 1, 4, 9, 0), (2435, 2, 1, 4, 0, 0, 0),
    (2453, 3, 4, 1, 1, 0, 0), (2534, 3, 9, 4, 1, 0, 0), (2543, 3, 4, 9, 4, 0, 0), (3245, 2, 4, 1, 0, 0, 0),
    (3254, 4, 4, 9, 4, 9, 0), (3425, 3, 9, 4, 9, 0, 0), (3452, 1, 1, 0, 0, 0, 0), (3524, 3, 4, 4, 1, 0, 0),
    (3542, 3, 1, 1, 4, 0, 0), (4235, 4, 1, 4, 4, 1, 0), (4253, 2, 9, 4, 0, 0, 0), (4325, 5, 4, 9, 4, 1, 1),
    (4352, 2, 4, 4, 0, 0, 0), (4523, 2, 1, 1, 0, 0, 0), (4532, 2, 4, 9, 0, 0, 0), (5234, 1, 9, 0, 0, 0, 0),
    (5243, 3, 1, 4, 9, 0, 0), (5324, 1, 4, 0, 0, 0, 0), (5342, 4, 9, 4, 1, 1, 0), (5423, 3, 1, 4, 4, 0, 0),
    (5432, 4, 4, 9, 4, 1, 0));
  //4 2->4 3->3 4->5 5->2
  //9 2->3 3->4 4->5 5->2
  //1 2<-3 3<-4 4<-5 5<-2

procedure PlaceYellowEdges(var cube: TRubik);
var
  c2, c3, c4, c5: integer;
begin
  c2 := RotateEdge(cube, 62, 6);
  c3 := RotateEdge(cube, 63, 6);
  c4 := RotateEdge(cube, 64, 6);
  c5 := RotateEdge(cube, 65, 6);

  // 1 = coin dans le bon sens
  // 1 = corner in the correct orientation




  case c2 * 1000 + c3 * 100 + c4 * 10 + c5 of
    2222: begin
      ApplyFormula(cube, 'RF''D''FDR''/D2/RD''F''DFR''');
    end;
    2121: begin
      ApplyFormula(cube, 'D/RF''D''FDR''');
    end;
    1212: begin
      ApplyFormula(cube, 'RF''D''FDR''');
    end;   //
    1122: begin
      ApplyFormula(cube, 'D2/RD''F''DFR''');
    end;
    1221: begin
      ApplyFormula(cube, 'D/RD''F''DFR''');
    end;
    2211: begin
      ApplyFormula(cube, 'RD''F''DFR''');
    end;   //
    2112: begin
      ApplyFormula(cube, 'D''/RD''F''DFR''');
    end;  //
    1111: ; // rien, c'est OK  // nothing, it's OK

    else
      solu := solu + 'Error';
  end;
end;

const //from french translates to Actions... may rename to MoveActions ?
  QuoiFaire1: array[1..24, 1..7] of integer =
    ((2345, 0, 0, 0, 0, 0, 0), (2354, 4, 1, 1, 4, 9, 0), (2435, 2, 1, 4, 0, 0, 0),
    (2453, 3, 4, 1, 1, 0, 0), (2534, 3, 9, 4, 1, 0, 0), (2543, 3, 4, 9, 4, 0, 0), (3245, 2, 4, 1, 0, 0, 0),
    (3254, 4, 4, 9, 4, 9, 0), (3425, 3, 9, 4, 9, 0, 0), (3452, 1, 1, 0, 0, 0, 0), (3524, 3, 4, 4, 1, 0, 0),
    (3542, 3, 1, 1, 4, 0, 0), (4235, 4, 1, 4, 4, 1, 0), (4253, 2, 9, 4, 0, 0, 0), (4325, 5, 4, 9, 4, 1, 1),
    (4352, 2, 4, 4, 0, 0, 0), (4523, 2, 1, 1, 0, 0, 0), (4532, 2, 4, 9, 0, 0, 0), (5234, 1, 9, 0, 0, 0, 0),
    (5243, 3, 1, 4, 9, 0, 0), (5324, 1, 4, 0, 0, 0, 0), (5342, 4, 9, 4, 1, 1, 0), (5423, 3, 1, 4, 4, 0, 0),
    (5432, 4, 4, 9, 4, 1, 0));
  //4 2->4 3->3 4->5 5->2
  //9 2->3 3->4 4->5 5->2
  //1 2<-3 3<-4 4<-5 5<-2


procedure OrientYellowEdges(var cube: TRubik);
var
  c2, c3, c4, c5: integer;
  i, n: integer;
begin
  c2 := findEdge(cube, 62) mod 10;
  c3 := findEdge(cube, 63) mod 10;
  c4 := findEdge(cube, 64) mod 10;
  c5 := findEdge(cube, 65) mod 10;
  n := 0;

  for i := 1 to 24 do if QuoiFaire1[i, 1] = c2 * 1000 + c3 * 100 + c4 * 10 + c5 then n := i;
  if n < 1 then exit;
  for i := 1 to QuoiFaire1[n, 2] do
  begin
    case QuoiFaire1[n, i + 2] of
      4: ApplyFormula(cube, 'F''D2FDF''DF');
      9: ApplyFormula(cube, 'D''');
      1: ApplyFormula(cube, 'D');
    end;
  end;
end;


const
  QuoiFaire2: array[1..12, 1..4] of integer =
    ((1234, 0, 0, 0), (1342, 2, 2, 2), (1423, 1, 2, 0)
    , (2143, 2, 4, 1), (2314, 2, 3, 3), (2431, 2, 4, 4)
    , (3124, 1, 3, 0), (3241, 2, 1, 1), (3412, 2, 4, 3)
    , (4132, 1, 4, 0), (4213, 1, 1, 0), (4321, 2, 3, 4));

procedure PlaceYellowCorners(var cube: TRubik);
var
  c1, c2, c3, c4: integer;
  i, n: integer;
begin
  c1 := findCorner(cube, 625);
  c2 := findCorner(cube, 632);
  c3 := findCorner(cube, 643);
  c4 := findCorner(cube, 654);
  case c1 of
    625, 652, 265, 256, 562, 526: c1 := 1;
    632, 623, 362, 326, 263, 236: c1 := 2;
    643, 634, 463, 436, 364, 346: c1 := 3;
    654, 645, 564, 546, 465, 456: c1 := 4;
  end;
  case c2 of
    625, 652, 265, 256, 562, 526: c2 := 1;
    632, 623, 362, 326, 263, 236: c2 := 2;
    643, 634, 463, 436, 364, 346: c2 := 3;
    654, 645, 564, 546, 465, 456: c2 := 4;
  end;
  case c3 of
    625, 652, 265, 256, 562, 526: c3 := 1;
    632, 623, 362, 326, 263, 236: c3 := 2;
    643, 634, 463, 436, 364, 346: c3 := 3;
    654, 645, 564, 546, 465, 456: c3 := 4;
  end;
  case c4 of
    625, 652, 265, 256, 562, 526: c4 := 1;
    632, 623, 362, 326, 263, 236: c4 := 2;
    643, 634, 463, 436, 364, 346: c4 := 3;
    654, 645, 564, 546, 465, 456: c4 := 4;
  end;

  n := 0;
  for i := 1 to 12 do if QuoiFaire2[i, 1] = c1 * 1000 + c2 * 100 + c3 * 10 + c4 then n := i;
  if n < 1 then exit;
  for i := 1 to QuoiFaire2[n, 2] do
  begin
    case QuoiFaire2[n, i + 2] of
      1: ApplyFormula(cube, 'B''D''F''DBD''FD');
      2: ApplyFormula(cube, 'R''D''L''DRD''LD');
      3: ApplyFormula(cube, 'FD''BDF''D''B''D');
      4: ApplyFormula(cube, 'LD''RDL''D''R''D');
    end;
  end;
end;

//



const
  QuoiFaire3: array[1..27, 1..5] of integer =
    (
    (1111, 0, 0, 0, 0), (1123, 2, 4, 4, 0), (1132, 1, 4, 0, 0),
    (1213, 2, 2, 5, 0), (1222, 3, 3, 3, 4), (1231, 2, 3, 3, 0),
    (1312, 2, 3, 4, 0), (1321, 1, 3, 0, 0), (1333, 3, 3, 4, 4),
    (2113, 1, 5, 0, 0), (2122, 3, 4, 4, 5), (2131, 2, 4, 5, 0),
    (2212, 3, 2, 5, 5), (2221, 3, 2, 2, 3), (2233, 3, 3, 3, 5),
    (2311, 2, 2, 2, 0), (2323, 2, 3, 5, 0), (2332, 3, 2, 2, 4),
    (3112, 2, 5, 5, 0), (3121, 2, 2, 3, 0), (3133, 3, 4, 5, 5),
    (3211, 1, 2, 0, 0), (3223, 3, 2, 4, 4), (3232, 2, 2, 4, 0),
    (3313, 3, 2, 2, 5), (3322, 3, 3, 5, 5), (3331, 3, 2, 3, 3));

{
c1 625 256 562
c2 632 326 263
c3 643 436 364
c4 654 546 465
}

procedure OrientYellowCorners(var cube: TRubik);
var
  c1, c2, c3, c4: integer;
  i, n: integer;
begin
  c1 := findCorner(cube, 625);
  c2 := findCorner(cube, 632);
  c3 := findCorner(cube, 643);
  c4 := findCorner(cube, 654);
  case c1 of
    625: c1 := 1;
    256: c1 := 3;
    562: c1 := 2;
  end;
  case c2 of
    632: c2 := 1;
    326: c2 := 3;
    263: c2 := 2;
  end;
  case c3 of
    643: c3 := 1;
    436: c3 := 3;
    364: c3 := 2;
  end;
  case c4 of
    654: c4 := 1;
    546: c4 := 3;
    465: c4 := 2;
  end;

  n := 0;
  for i := 1 to 27 do if QuoiFaire3[i, 1] = c1 * 1000 + c2 * 100 + c3 * 10 + c4 then n := i;
  for i := 1 to 27 do if QuoiFaire3[i, 1] = c1 * 1000 + c2 * 100 + c3 * 10 + c4 then n := i;

  if n < 1 then exit;
  for i := 1 to QuoiFaire3[n, 2] do
  begin
    case QuoiFaire3[n, i + 2] of
      2: ApplyFormula(cube, 'F''D2FDF''DFB''D2BD''B''D''B');
      3: ApplyFormula(cube, 'RD2R''DRDR''LD2L''D''LD''L''');
      4: ApplyFormula(cube, 'BD2B''DBDB''FD2F''D''FD''F''');
      5: ApplyFormula(cube, 'L''D2LDL''DLR''D2RD''R''D''R');
    end;
  end;

end;




function RubikToStr(c: TLinRubik): string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to 53 do Result := Result + IntToStr(c[i]);
end;

// Supressions de deux mouvements opposés consécutifs
// Removal of two consecutive opposite movements

function FilterMoves(var s: string; cube: TFaceRubik; can: tcanvas): string;
var
  tmp: TLinRubik;
  tab: array of string;
  i, j, n, p: integer;
  f: string;
begin
  Result := '';
  can.FillRect(can.ClipRect);
  for i := 0 to ConvertNumber - 1 do s := AnsiReplaceText(s, StrFrom_To[i * 2], StrFrom_To[i * 2 + 1]);
  tmp := TLinRubik(cube);
  n := CountMoves(s);
  setlength(tab, n + 1);
  p := 1;
  tab[0] := RubikToStr(tmp);
  for j := 0 to 53 do can.Pixels[0, j] := C_COLOR[tmp[j]];
  for i := 1 to n do
  begin
    f := s[p];
    Inc(p);
    if (p <= length(s)) and (s[p] = '''') then
    begin
      f := f + '''';
      Inc(p);
    end;
    if (p <= length(s)) and (s[p] = '2') then
    begin
      f := f + '2';
      Inc(p);
    end;
    ApplyFormula(tfacerubik(tmp), f);
    for j := 0 to 53 do can.Pixels[i, j] := C_COLOR[tmp[j]];
    tab[i] := RubikToStr(tmp);
  end;
  for j := n downto 1 do
    for i := 0 to j - 1 do
      if tab[j] = tab[i] then Result := Result + '(' + IntToStr(j) + '/' + IntToStr(i) + ')';
end;

end.
