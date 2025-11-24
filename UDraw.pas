unit UDraw;

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
  Classes,
  Controls,
  Forms,
  Graphics,
  LCLIntf,
  LCLType,
  LMessages,
  Messages,
  Math,
  SysUtils,
  Variants,
  Dialogs,
  ExtCtrls,
  Menus,
  Spin,
  StdCtrls,
  strutils,
  UConst,
  BGRABitmap,
  BGRABitmapTypes;

procedure SetCubeletColor(var cube: TRubik; pt: tpoint; colindex: integer);
function GetCubeletColor(const cube: TRubik; pt: TPoint): integer;
//procedure SetColor3D(var cube3D: TCube3D; const pt: TPoint; const colindex: integer);
function PointInPolygon(point: TPoint; const polygon: array of TPoint): boolean;

procedure Rotate3d(var C: TCube3D; rx, ry, rz: single);
procedure Rotate3dFace(var C: TCube3D; face: integer; rotation: single);
procedure DrawCube3d(Bitmap: TBGRABitmap; RubikCube: TRubik; C3D: TCube3D);
procedure DrawCube(P: TPaintBox; c: TRubik);
procedure DrawMarchingAnts(Bitmap: TBGRABitmap; const Points: array of TPointF;
  Offset: Integer; LineWidth: Single);

// Easing functions for smooth animations
function EaseInOutQuad(t: Double): Double;
function EaseInOutSine(t: Double): Double;
function EaseInOutCubic(t: Double): Double;

type
  T2DArray = array[0..3] of tpoint;

  TPolyOrder = record
    order: integer;
    z: single;
    color: tcolor;
    pt: T2DArray;
  end;

var
  CurrentCubeState: TRubik;
  TargetCubeState: TRubik;
  SelectedColor: integer = 0;
  Cube3D: TCube3D;
  axeX: T3dPoint = (-1, 0, 0);
  axeY: T3dPoint = (0, -1, 0);
  axeZ: T3dPoint = (0, 0, -1);
  CubeySize: integer = 30;

  PolyOrder: array[0..269] of TPolyOrder;

  // Lighting control - set to False to disable lighting
  UseLighting: Boolean = True;

  // Keyboard face selection - which of the 3 visible faces is selected
  KeyboardFaceSelectMode: Boolean = False;
  SelectedVisibleFace: Integer = 0;  // Index into VisibleFaces array (0, 1, or 2)
  MarchingAntsOffset: Integer = 0;   // Animation offset for marching ants
  FaceHighlightPhase: Single = 0;    // 0 to 2*Pi for pulsing effect

  // Track which 3 faces are currently visible to the user
  // Face indices: 1=Top, 2=Front, 3=Right, 4=Back, 5=Left, 6=Bottom
  // VisibleFaces[0] = top-ish face, [1] = front-ish face, [2] = right-ish face
  VisibleFaces: array[0..2] of Integer = (1, 2, 3);  // Default: Top, Front, Right
  ViewRotationY: Integer = 0;  // Track Y rotation in 90-degree increments (0, 1, 2, 3)
  ViewFlipped: Boolean = False;  // Track if cube has been flipped 180° (W key)

const
  C_FACE_GRID_POS: array[1..6, 0..2, 0..2] of TPoint =
    (
    // White face (top, above Green)              U 1
    (((x: 3; y: 0), (x: 4; y: 0), (x: 5; y: 0)),
    ((x: 3; y: 1), (x: 4; y: 1), (x: 5; y: 1)),   //1?
    ((x: 3; y: 2), (x: 4; y: 2), (x: 5; y: 2)))   //we need these to be in the numerical order of other
    ,                                              //program so we can pass state to solvers
    // Green face (center) Front                  F 3
    (((x: 3; y: 3), (x: 4; y: 3), (x: 5; y: 3)),
    ((x: 3; y: 4), (x: 4; y: 4), (x: 5; y: 4)),   //2?
    ((x: 3; y: 5), (x: 4; y: 5), (x: 5; y: 5)))
    ,
    // Red face (right of Green)                  R  2
    (((x: 6; y: 3), (x: 7; y: 3), (x: 8; y: 3)),
    ((x: 6; y: 4), (x: 7; y: 4), (x: 8; y: 4)),   //3?
    ((x: 6; y: 5), (x: 7; y: 5), (x: 8; y: 5)))
    ,
    // Blue face (far right)                      B 6
    (((x: 9; y: 3), (x: 10; y: 3), (x: 11; y: 3)),
    ((x: 9; y: 4), (x: 10; y: 4), (x: 11; y: 4)), //4?
    ((x: 9; y: 5), (x: 10; y: 5), (x: 11; y: 5)))
    ,
    //ORANGE  face (left-center)                  L 5
    (((x: 0; y: 3), (x: 1; y: 3), (x: 2; y: 3)),
    ((x: 0; y: 4), (x: 1; y: 4), (x: 2; y: 4)),   //5?
    ((x: 0; y: 5), (x: 1; y: 5), (x: 2; y: 5)))
    ,
    // Yellow face (bottom, below Green)          D 4
    (((x: 3; y: 6), (x: 4; y: 6), (x: 5; y: 6)),
    ((x: 3; y: 7), (x: 4; y: 7), (x: 5; y: 7)),   //6?
    ((x: 3; y: 8), (x: 4; y: 8), (x: 5; y: 8)))
    );

  CFacePlace: array[1..6, 0..8] of TPoint = (
    // White face (top, above Green)           U
    ((x: 3; y: 0), (x: 4; y: 0), (x: 5; y: 0),
    (x: 3; y: 1), (x: 4; y: 1), (x: 5; y: 1),    //1?
    (x: 3; y: 2), (x: 4; y: 2), (x: 5; y: 2))
    ,
    // Green face (center)                     F
    ((x: 3; y: 3), (x: 4; y: 3), (x: 5; y: 3),
    (x: 3; y: 4), (x: 4; y: 4), (x: 5; y: 4),    //2?
    (x: 3; y: 5), (x: 4; y: 5), (x: 5; y: 5))
    ,
    // Red face (right of Green)                R
    ((x: 6; y: 3), (x: 7; y: 3), (x: 8; y: 3),
    (x: 6; y: 4), (x: 7; y: 4), (x: 8; y: 4),    //3?
    (x: 6; y: 5), (x: 7; y: 5), (x: 8; y: 5))
    ,
    // Blue face (far right)                     B
    ((x: 9; y: 3), (x: 10; y: 3), (x: 11; y: 3),
    (x: 9; y: 4), (x: 10; y: 4), (x: 11; y: 4),  //4?
    (x: 9; y: 5), (x: 10; y: 5), (x: 11; y: 5))
    ,
    // Orange face (left of Green)                L
    ((x: 0; y: 3), (x: 1; y: 3), (x: 2; y: 3),
    (x: 0; y: 4), (x: 1; y: 4), (x: 2; y: 4),    //5?
    (x: 0; y: 5), (x: 1; y: 5), (x: 2; y: 5))
    ,
    // Yellow face (bottom, below Green)           D
    ((x: 3; y: 6), (x: 4; y: 6), (x: 5; y: 6),
    (x: 3; y: 7), (x: 4; y: 7), (x: 5; y: 7),    //6?
    (x: 3; y: 8), (x: 4; y: 8), (x: 5; y: 8))
    );

implementation

procedure DrawCube(P: TPaintBox; c: TRubik);
var
  i, j, k: integer;
  tmp: TBitmap;
  centerPoint: TPoint;
  SpaceForWidth, SpaceForHeight, face: integer;
  faceName: string;
begin
  tmp := TBitmap.Create;
  try
    tmp.Width := P.Width;
    tmp.Height := P.Height;
    tmp.Canvas.Brush.Color := clAppWorkspace;
    tmp.Canvas.FillRect(P.ClientRect);

    SpaceForWidth := P.Width div 12; // Available space per cube, horizontally
    SpaceForHeight := P.Height div 9; // Available space per cube, vertically


    CubeySize := Min(SpaceForWidth, SpaceForHeight);

    // Optionally draw a black outline around each cube face for clarity
    tmp.Canvas.Brush.Color := clSilver;//clMenuBar; // Outline color
    // Drawing outlines for each cube face

    // Background for Orange face
    tmp.Canvas.Rectangle(0 * CubeySize + 2, 3 * CubeySize + 2, 3 * CubeySize - 4, 6 * CubeySize - 4);

    //Background for Green face
    tmp.Canvas.Rectangle(3 * CubeySize + 2, 3 * CubeySize + 2, 6 * CubeySize - 4, 6 * CubeySize - 4);

    //Background for Red face
    tmp.Canvas.Rectangle(6 * CubeySize + 2, 3 * CubeySize + 2, 9 * CubeySize - 4, 6 * CubeySize - 4);

    //Background for Blue face
    tmp.Canvas.Rectangle(9 * CubeySize + 2, 3 * CubeySize + 2, 12 * CubeySize - 4, 6 * CubeySize - 4);

    //Background for White face
    tmp.Canvas.Rectangle(3 * CubeySize + 2, 0 * CubeySize + 2, 6 * CubeySize - 4, 3 * CubeySize - 4);

    //Background for Yellow face
    tmp.Canvas.Rectangle(3 * CubeySize + 2, 6 * CubeySize + 2, 6 * CubeySize - 4, 9 * CubeySize - 4);

    // Fill cube faces with appropriate colors
    for i := 1 to 6 do
    begin
      for j := 0 to 2 do
      begin
        for k := 0 to 2 do
        begin
          tmp.Canvas.Brush.Color := C_COLOR[TUnitRubik(c)[i, j, k]];
          faceName := FACE_NAMES[TUnitRubik(c)[i, j, k]];

          tmp.Canvas.Rectangle(C_FACE_GRID_POS[i, j, k].x * CubeySize + 0,
            C_FACE_GRID_POS[i, j, k].Y * CubeySize + 0,
            C_FACE_GRID_POS[i, j, k].x * CubeySize + CubeySize - 2,
            C_FACE_GRID_POS[i, j, k].Y * CubeySize + CubeySize - 2);

          centerPoint := C_FACE_GRID_POS[i, j, k];
          faceName := faceName + IntToStr(TFaceRubik(c)[i, j])
          + IntToStr(C_FACE_GRID_POS[i, j, k].x);
          // Constructs name like U1, L2, etc.
          // Needs more thought

          tmp.Canvas.Font.Size := 7;
          tmp.Canvas.Font.Color := clBlack;

          //Draw face name in the center of each cubelet
          //tmp.Canvas.TextOut(
          //C_FACE_GRID_POS[i, j, k].x * CubeySize + (CubeySize div 2) - (tmp.Canvas.TextWidth(faceName) div 2),
          //C_FACE_GRID_POS[i, j, k].Y * CubeySize + (CubeySize div 2) - (tmp.Canvas.TextHeight(faceName) div 2),
          //faceName);
        end;
      end;
    end;

    P.Canvas.Draw(0, 0, tmp);
  finally
    tmp.Free;
  end;
end;

function GetCubeletColor(const cube: TRubik; pt: TPoint): integer;
var
  i, j: integer;
begin
  Result := -1; // Default value indicating no color found
  for i := 1 to 6 do
    for j := 0 to 8 do
      if j <> 4 then // Skipping the center piece as it's fixed
      begin
        if PtInRect(Rect(CFacePlace[i, j].x * CubeySize, CFacePlace[i, j].Y * CubeySize,
          CFacePlace[i, j].x * CubeySize + CubeySize - 3, CFacePlace[i, j].Y * CubeySize +
          CubeySize - 3), pt) then
        begin
          Result := cube[i, j]; // Return the color index at the clicked position
          Exit; // Exit as soon as the color is found
        end;
      end;
end;

procedure SetCubeletColor(var cube: TRubik; pt: tpoint; colindex: integer);
var
  i, j: integer;
begin
  for i := 1 to 6 do for j := 0 to 8 do
      if j <> 4 then
      begin
        if PtInRect(Rect(CFacePlace[i, j].x * CubeySize, CFacePlace[i, j].Y * CubeySize,
          CFacePlace[i, j].x * CubeySize + CubeySize - 3, CFacePlace[i, j].Y * CubeySize +
          CubeySize - 3), pt) then
        begin
          cube[i, j] := colindex;
        end;
      end;
end;

function PointInPolygon(point: TPoint; const polygon: array of TPoint): boolean;
var
  windingNumber: integer;
  i, j: integer;
  xi, yi, xj, yj: integer;
begin
  windingNumber := 0;
  j := High(polygon);

  // Iterate through each edge of the polygon
  for i := Low(polygon) to High(polygon) do
  begin
    xi := polygon[i].X;
    yi := polygon[i].Y;
    xj := polygon[j].X;
    yj := polygon[j].Y;

    // Check if the point is within the vertical range of the edge
    if (yi <= point.Y) then
    begin
      if (yj > point.Y) and ((point.Y - yi) * (xj - xi) > (point.X - xi) * (yj - yi)) then
        Inc(windingNumber);
    end
    else
    begin
      if (yj <= point.Y) and ((point.Y - yi) * (xj - xi) < (point.X - xi) * (yj - yi)) then
        Dec(windingNumber);
    end;

    j := i; // Move to the next edge
  end;

  Result := windingNumber <> 0;
end;




function Pt3dTo2D(x, y, z: single; dx, dy, scalingFactor: integer): TPoint;
begin
  z := z + 10; // Adjusts depth positioning
  Result.x := Round((x * scalingFactor / z) * 20) + dx;
  Result.y := -Round((y * scalingFactor / z) * 20) + dy; // Negative to flip Y-axis for screen coordinates
end;

procedure QuickSortPolyOrder(var Arr: array of TPolyOrder; iLo, iHi: Integer);
var
  Lo, Hi: Integer;
  Pivot: Single;
  T: TPolyOrder;
begin
  Lo := iLo;
  Hi := iHi;
  Pivot := Arr[(Lo + Hi) div 2].z;

  repeat
    // Find elements to swap (sorting by z descending - highest z first)
    while (Arr[Lo].order <> -1) and (Arr[Lo].z > Pivot) do Inc(Lo);
    while (Arr[Hi].order <> -1) and (Arr[Hi].z < Pivot) do Dec(Hi);

    // Skip inactive elements
    while (Lo <= Hi) and (Arr[Lo].order = -1) do Inc(Lo);
    while (Lo <= Hi) and (Arr[Hi].order = -1) do Dec(Hi);

    if Lo <= Hi then
    begin
      // Swap elements
      T := Arr[Lo];
      Arr[Lo] := Arr[Hi];
      Arr[Hi] := T;
      Inc(Lo);
      Dec(Hi);
    end;
  until Lo > Hi;

  if Hi > iLo then QuickSortPolyOrder(Arr, iLo, Hi);
  if Lo < iHi then QuickSortPolyOrder(Arr, Lo, iHi);
end;

function CalculateFaceNormal(const p1, p2, p3: T3dPoint): T3dPoint;
var
  u, v: T3dPoint;
  len: single;
begin
  // Calculate two edge vectors
  u[0] := p2[0] - p1[0];
  u[1] := p2[1] - p1[1];
  u[2] := p2[2] - p1[2];

  v[0] := p3[0] - p1[0];
  v[1] := p3[1] - p1[1];
  v[2] := p3[2] - p1[2];

  // Cross product to get normal
  Result[0] := u[1] * v[2] - u[2] * v[1];
  Result[1] := u[2] * v[0] - u[0] * v[2];
  Result[2] := u[0] * v[1] - u[1] * v[0];

  // Normalize
  len := sqrt(Result[0] * Result[0] + Result[1] * Result[1] + Result[2] * Result[2]);
  if len > 0.0001 then
  begin
    Result[0] := Result[0] / len;
    Result[1] := Result[1] / len;
    Result[2] := Result[2] / len;
  end;
end;

// ============================================================================
// Easing Functions - Make animations feel natural and realistic
// ============================================================================

function EaseInOutQuad(t: Double): Double;
// Quadratic easing - smooth acceleration and deceleration
// Slow start → Fast middle → Slow end
// Best for most animations - feels natural
begin
  if t < 0.5 then
    Result := 2 * t * t
  else
    Result := 1 - Power(-2 * t + 2, 2) / 2;
end;

function EaseInOutSine(t: Double): Double;
// Sine-based easing - very smooth and gentle
// Slower acceleration/deceleration than quad
// Good for subtle, elegant movements
begin
  Result := -(Cos(Pi * t) - 1) / 2;
end;

function EaseInOutCubic(t: Double): Double;
// Cubic easing - more dramatic acceleration
// Very slow start → Very fast middle → Very slow end
// Good for emphasizing movement
begin
  if t < 0.5 then
    Result := 4 * t * t * t
  else
    Result := 1 - Power(-2 * t + 2, 3) / 2;
end;

function ApplyLighting(baseColor: TColor; normal: T3dPoint): TBGRAPixel;
const
  // Light from upper-left-front (like desk lamp)
  LIGHT_X = -0.5;
  LIGHT_Y = -0.7;
  LIGHT_Z = -0.5;
  AMBIENT = 0.75;  // Minimum brightness (75% - brighter base!)
  DIFFUSE = 0.50;  // Maximum additional brightness (50% - balanced)
  SPECULAR = 1.2;  // Glossy highlight strength (120% for SUPER shiny!)
  SHININESS = 12;  // How tight the highlight is (lower = larger, more visible)
var
  dotProduct, specularIntensity: single;
  brightness: single;
  r, g, b: byte;
  lightLen, normalLen: single;
  lightDir, viewDir, reflection: T3dPoint;
  reflectDot: single;
begin
  // Normalize light direction
  lightLen := sqrt(LIGHT_X*LIGHT_X + LIGHT_Y*LIGHT_Y + LIGHT_Z*LIGHT_Z);
  lightDir[0] := LIGHT_X / lightLen;
  lightDir[1] := LIGHT_Y / lightLen;
  lightDir[2] := LIGHT_Z / lightLen;

  // Normalize normal vector
  normalLen := sqrt(normal[0]*normal[0] + normal[1]*normal[1] + normal[2]*normal[2]);
  if normalLen > 0 then
  begin
    normal[0] := normal[0] / normalLen;
    normal[1] := normal[1] / normalLen;
    normal[2] := normal[2] / normalLen;
  end;

  // Calculate dot product (how much face points toward light)
  dotProduct := -(normal[0] * lightDir[0] + normal[1] * lightDir[1] + normal[2] * lightDir[2]);

  // Clamp to [0, 1] - negative means facing away from light
  if dotProduct < 0 then dotProduct := 0;
  if dotProduct > 1 then dotProduct := 1;

  // Calculate final brightness: ambient + diffuse * angle
  brightness := AMBIENT + DIFFUSE * dotProduct;

  // Specular highlight (glossy reflection)
  // View direction from face to camera (camera at 0,0,-∞ looking toward +Z)
  viewDir[0] := 0;
  viewDir[1] := 0;
  viewDir[2] := -1;

  // Calculate reflection vector: R = 2(N·L)N - L
  reflection[0] := 2 * dotProduct * normal[0] + lightDir[0];
  reflection[1] := 2 * dotProduct * normal[1] + lightDir[1];
  reflection[2] := 2 * dotProduct * normal[2] + lightDir[2];

  // Specular intensity: (R·V)^shininess
  reflectDot := -(reflection[0] * viewDir[0] + reflection[1] * viewDir[1] + reflection[2] * viewDir[2]);
  if reflectDot < 0 then reflectDot := 0;
  if reflectDot > 1 then reflectDot := 1;

  // Apply shininess (power function for tight highlight)
  specularIntensity := Power(reflectDot, SHININESS) * SPECULAR;

  // Add specular to brightness
  brightness := brightness + specularIntensity;

  // Darken back faces for depth perception
  // Faces pointing away from camera (negative Z normal) are darker
  if normal[2] < 0 then
    brightness := brightness * 0.75;  // 25% darker for back faces

  // Apply brightness to color components (can exceed 1.0 for highlights!)
  r := Red(baseColor);
  g := Green(baseColor);
  b := Blue(baseColor);

  // Clamp to 255 max for over-bright highlights
  Result.red := Min(255, Round(r * brightness));
  Result.green := Min(255, Round(g * brightness));
  Result.blue := Min(255, Round(b * brightness));
  Result.alpha := 255;
end;

procedure DrawCube3d(Bitmap: TBGRABitmap; RubikCube: TRubik; C3D: TCube3D);
var
  i, j, layer: integer;
  pt: T2DArray;
  dx, dy, BaseScale, ScalingFactor: integer;
  ux, uy: single;
  vx, vy: single;
  bgra_pts: array[0..3] of TPointF;
  shrunk_pts: array[0..3] of TPointF;
  centerX, centerY, shrinkFactor: single;
  normal: T3dPoint;
  litColor: TBGRAPixel;
  nextJ: integer;
  edgeDx, edgeDy: single;
  // Variables for face selection highlighting
  faceIdx: Integer;
  cornerPolys: array[0..3] of Integer;
  faceCorners: array[0..3] of TPointF;
  cornerCubelets: array[0..3] of Integer;
  polyIdx, cubeletIdx, ptIdx, selJ: Integer;
  minX, minY, maxX, maxY: Single;
  foundCorners: Boolean;
  selectedFaceStart, selectedFaceEnd: Integer;
  isSelectedFacePoly: Boolean;
  highlightAmount: Single;
  highlightColor: TBGRAPixel;
begin
  // Clear the bitmap with background color
  Bitmap.Fill(ColorToBGRA(clAppWorkspace));
  dx := Bitmap.Width div 2;
  dy := Bitmap.Height div 2;

  ScalingFactor := Min(Bitmap.Width, Bitmap.Height) div 24;

  for i := 0 to 269 do
  begin
    PolyOrder[i].order := -1;
    for j := 0 to 3 do pt[j] := Pt3dTo2D(C3D[i, j + 1, 0], C3D[i, j + 1, 1], C3D[i, j + 1, 2], dx, dy, ScalingFactor);
    ux := pt[0].X - pt[1].X;
    uy := pt[0].y - pt[1].y;
    vx := pt[2].X - pt[1].X;
    vy := pt[2].y - pt[1].y;

    // ne dessine pas les faces arrières
    // does not draw the back faces
    //if ux*vy-uy*vx>=0 then continue;


    PolyOrder[i].pt := pt;
    PolyOrder[i].z := (C3D[i, 1, 2] + C3D[i, 2, 2] + C3D[i, 3, 2] + C3D[i, 4, 2]) / 4;
    PolyOrder[i].order := i;
    if i mod 5 = 4 then PolyOrder[i].color := C_COLOR[RubikCube[(i div 5) div 9 + 1, ((i div 5) mod 9)]]
    else
      PolyOrder[i].color := clblack;
    // les faces arrières sont noirs
    // the back faces are black
    if ux * vy - uy * vx >= 0 then PolyOrder[i].color := clblack;
  end;

  // tri des polygones à dessiner par ordre Z
  // sorting of polygons to be drawn by Z order (highest z first)
  QuickSortPolyOrder(PolyOrder, 0, 269);

  // Calculate selected face for highlighting
  if KeyboardFaceSelectMode then
  begin
    // Use the currently visible faces array
    faceIdx := VisibleFaces[SelectedVisibleFace];
  end
  else
  begin
    faceIdx := 0;
  end;

  // affichage des polygones
  // display of polygons with lighting
  for i := 0 to 269 do
    if PolyOrder[i].order <> -1 then
    begin
      // Convert TPoint array to TPointF for anti-aliasing
      for j := 0 to 3 do
      begin
        bgra_pts[j].x := PolyOrder[i].pt[j].x;
        bgra_pts[j].y := PolyOrder[i].pt[j].y;
      end;

      // Check if this polygon belongs to a cubelet that rotates with the selected face
      // C3dFaceRotation[faceIdx, 0..20] contains the 21 cubelet indices that rotate
      // Each cubelet has 5 polygons, so polygon index = cubelet * 5 + (0..4)
      isSelectedFacePoly := False;
      if faceIdx > 0 then
      begin
        cubeletIdx := PolyOrder[i].order div 5;  // Which cubelet does this polygon belong to?
        for selJ := 0 to 20 do
        begin
          if C3dFaceRotation[faceIdx, selJ] = cubeletIdx then
          begin
            isSelectedFacePoly := True;
            Break;
          end;
        end;
      end;

      // Skip lighting for black (back) faces
      if PolyOrder[i].color = clBlack then
      begin
        Bitmap.FillPolyAntialias(bgra_pts, ColorToBGRA(clBlack));
      end
      else if UseLighting then
      begin
        // Calculate face normal from 3D points
        normal := CalculateFaceNormal(
          C3D[PolyOrder[i].order, 1],
          C3D[PolyOrder[i].order, 2],
          C3D[PolyOrder[i].order, 3]
        );

        // Apply lighting to base color
        litColor := ApplyLighting(PolyOrder[i].color, normal);

        // Draw with lit color (specular highlights already baked in!)
        Bitmap.FillPolyAntialias(bgra_pts, litColor);

        // Draw marching ants outline for selected face cubelets
        if isSelectedFacePoly then
        begin
          DrawMarchingAnts(Bitmap, bgra_pts, MarchingAntsOffset, 3.0);
        end;

        // Directional bevel edges for raised sticker appearance
        // Draw highlight on top-left edges and shadow on bottom-right edges
        for j := 0 to 3 do
        begin
          nextJ := (j + 1) mod 4;
          edgeDx := bgra_pts[nextJ].x - bgra_pts[j].x;
          edgeDy := bgra_pts[nextJ].y - bgra_pts[j].y;

          // Determine if edge is more top-left or bottom-right
          // Top-left edges: going left (edgeDx < 0) or going up (edgeDy < 0)
          // Bottom-right edges: going right (edgeDx > 0) or going down (edgeDy > 0)

          if (edgeDx < 0) or (edgeDy < 0) then
          begin
            // Top or left edge - draw bright highlight
            Bitmap.DrawLineAntialias(
              Round(bgra_pts[j].x), Round(bgra_pts[j].y),
              Round(bgra_pts[nextJ].x), Round(bgra_pts[nextJ].y),
              ColorToBGRA(clWhite, 100), 1.5
            );
          end
          else if (edgeDx > 0) or (edgeDy > 0) then
          begin
            // Bottom or right edge - draw dark shadow
            Bitmap.DrawLineAntialias(
              Round(bgra_pts[j].x), Round(bgra_pts[j].y),
              Round(bgra_pts[nextJ].x), Round(bgra_pts[nextJ].y),
              ColorToBGRA(clBlack, 80), 1.5
            );
          end;
        end;

        // Simple subtle edge darkening (just edges, not gradient)
        Bitmap.DrawPolygonAntialias(bgra_pts, ColorToBGRA(clBlack, 35), 3.5);

        // Bright inner glow for cubelet separation
        Bitmap.DrawPolygonAntialias(bgra_pts, ColorToBGRA(clWhite, 150), 1.0);

        // Strong black outline for definition
        Bitmap.DrawPolygonAntialias(bgra_pts, ColorToBGRA(clBlack, 160), 2.0);
      end
      else
      begin
        // No lighting - just draw flat colors
        Bitmap.FillPolyAntialias(bgra_pts, ColorToBGRA(PolyOrder[i].color));

        // Simple edge darkening
        Bitmap.DrawPolygonAntialias(bgra_pts, ColorToBGRA(clBlack, 35), 3.5);

        // Bright inner glow for separation
        Bitmap.DrawPolygonAntialias(bgra_pts, ColorToBGRA(clWhite, 150), 1.0);

        Bitmap.DrawPolygonAntialias(bgra_pts, ColorToBGRA(clBlack, 160), 2.0);
      end;
    end;

  // No need to Draw or Free - TBGRAVirtualScreen handles it!
  // Face highlighting is now done inline during polygon drawing above
end;


function RotationPoint(p: t3dpoint; vect: t3dpoint; r: single): t3dpoint;
var
  s, u, v, w, x, y, z: single;
  co, si: single;
begin
  u := vect[0];
  v := vect[1];
  w := vect[2];
  x := p[0];
  y := p[1];
  z := p[2];
  s := u * x + v * y + w * z;
  co := cos(r);
  si := sin(r);
  Result[0] := (u * s) * (1 - co) + x * co + (-w * y + v * z) * si;
  Result[1] := (v * s) * (1 - co) + y * co + (+w * x - u * z) * si;
  Result[2] := (w * s) * (1 - co) + z * co + (-v * x + u * y) * si;
end;

procedure Rotate3dFace(var C: TCube3D; face: integer; rotation: single);
var
  i, j, k, f, ii, jj: integer;
  tmp: tcube3d;
  axe: t3dpoint;
begin
  case face of
    1: axe := axey;
    6: axe := axey;
    2: axe := axez;
    4: axe := axez;
    3: axe := axex;
    5: axe := axex;
  end;
  for i := 0 to 20 do for j := 0 to 4 do
    begin
      k := C3dFaceRotation[face, i] * 5 + j;
      for jj := 1 to 4 do c[k, jj] := RotationPoint(c[k, jj], axe, rotation);
    end;
end;

procedure Rotate3d(var C: TCube3D; rx, ry, rz: single);
var
  i, j: integer;
  px1, py1, pz1: single;
begin
  axex := RotationPoint(axex, REFERENCE_X, rx);
  axex := RotationPoint(axex, REFERENCE_Y, ry);
  axex := RotationPoint(axex, REFERENCE_Z, rz);
  axey := RotationPoint(axey, REFERENCE_X, rx);
  axey := RotationPoint(axey, REFERENCE_Y, ry);
  axey := RotationPoint(axey, REFERENCE_Z, rz);
  axez := RotationPoint(axez, REFERENCE_X, rx);
  axez := RotationPoint(axez, REFERENCE_Y, ry);
  axez := RotationPoint(axez, REFERENCE_Z, rz);

  for i := 0 to 269 do
    for j := 1 to 4 do
    begin
      c[i, j] := RotationPoint(c[i, j], REFERENCE_X, rx);
      c[i, j] := RotationPoint(c[i, j], REFERENCE_Y, ry);
      c[i, j] := RotationPoint(c[i, j], REFERENCE_Z, rz);
    end;
end;

procedure DrawMarchingAnts(Bitmap: TBGRABitmap; const Points: array of TPointF;
  Offset: Integer; LineWidth: Single);
// Draws a marching ants selection border around a polygon
// Points: array of corner points defining the polygon
// Offset: animation offset (0-15) for the marching effect
// LineWidth: thickness of the selection line
const
  DASH_LENGTH = 8;  // Length of each dash segment
  GAP_LENGTH = 8;   // Length of gap between dashes
  PATTERN_LENGTH = 16; // Total pattern length (DASH_LENGTH + GAP_LENGTH)
var
  i, nextI: Integer;
  dx, dy, segLen, traveled, remaining: Single;
  startX, startY, endX, endY: Single;
  drawX, drawY, stepX, stepY: Single;
  patternPos: Integer;
  inDash: Boolean;
  dashStartX, dashStartY: Single;
begin
  if Length(Points) < 3 then Exit;

  // Draw each edge of the polygon
  for i := 0 to High(Points) do
  begin
    nextI := (i + 1) mod Length(Points);

    startX := Points[i].x;
    startY := Points[i].y;
    endX := Points[nextI].x;
    endY := Points[nextI].y;

    dx := endX - startX;
    dy := endY - startY;
    segLen := Sqrt(dx * dx + dy * dy);

    if segLen < 1 then Continue;

    // Normalize direction
    stepX := dx / segLen;
    stepY := dy / segLen;

    // Walk along the edge drawing dashes
    traveled := 0;
    drawX := startX;
    drawY := startY;

    // Calculate initial pattern position based on offset
    patternPos := Offset mod PATTERN_LENGTH;
    inDash := patternPos < DASH_LENGTH;

    if inDash then
    begin
      dashStartX := drawX;
      dashStartY := drawY;
    end;

    while traveled < segLen do
    begin
      // How far until pattern state changes?
      if inDash then
        remaining := DASH_LENGTH - (patternPos mod PATTERN_LENGTH)
      else
        remaining := PATTERN_LENGTH - (patternPos mod PATTERN_LENGTH);

      // Don't go past end of segment
      if traveled + remaining > segLen then
        remaining := segLen - traveled;

      // Move along segment
      traveled := traveled + remaining;
      drawX := startX + stepX * traveled;
      drawY := startY + stepY * traveled;
      patternPos := (patternPos + Round(remaining)) mod PATTERN_LENGTH;

      // If we were in a dash, draw it
      if inDash then
      begin
        // Draw black outline FIRST (thicker, underneath)
        Bitmap.DrawLineAntialias(dashStartX, dashStartY, drawX, drawY,
          ColorToBGRA(clBlack, 255), LineWidth + 3);
        // Draw white dash on top
        Bitmap.DrawLineAntialias(dashStartX, dashStartY, drawX, drawY,
          ColorToBGRA(clWhite, 255), LineWidth);
      end;

      // Toggle dash/gap state
      inDash := not inDash;
      if inDash then
      begin
        dashStartX := drawX;
        dashStartY := drawY;
      end;
    end;

    // If we ended in a dash, draw the final segment
    if inDash then
    begin
      // Draw black outline FIRST (thicker, underneath)
      Bitmap.DrawLineAntialias(dashStartX, dashStartY, endX, endY,
        ColorToBGRA(clBlack, 255), LineWidth + 3);
      // Draw white dash on top
      Bitmap.DrawLineAntialias(dashStartX, dashStartY, endX, endY,
        ColorToBGRA(clWhite, 255), LineWidth);
    end;
  end;
end;

end.
