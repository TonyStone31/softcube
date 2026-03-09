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
  Math,
  SysUtils,
  Dialogs,
  ExtCtrls,
  Menus,
  Spin,
  StdCtrls,
  strutils,
  UConst,
  UGenericCube,
  BGRABitmap,
  BGRABitmapTypes;

function PointInPolygon(point: TPoint; const polygon: array of TPoint): boolean;

procedure DrawCube(P: TPaintBox; c: TRubik);
procedure DrawCubeN(P: TPaintBox; cube: TGenericCube);
procedure DrawMarchingAnts(Bitmap: TBGRABitmap; const Points: array of TPointF;
  Offset: Integer; LineWidth: Single);

// NxN 3D support
function GenerateNxNCube3D(N: Integer): TDynCube3D;
procedure InitNxN3D(N: Integer);
procedure DrawCube3dN(Bitmap: TBGRABitmap; cube: TGenericCube; const C3D: TDynCube3D);
procedure Rotate3dN(var C: TDynCube3D; rx, ry, rz: single);
procedure Rotate3dFaceN(var C: TDynCube3D; N, faceOrd: Integer; rotation: single);
function GetSliceCubeletIndices(N, faceOrd, sliceDepth: Integer): TArray<Integer>;
procedure Rotate3dSliceN(var C: TDynCube3D; N, faceOrd: Integer;
  SliceStart, SliceEnd: Integer; rotation: single);

// NxN hit-testing for color setting
function HitTestCubeN(P: TPaintBox; N: Integer; pt: TPoint;
  out Face: TFace; out Row, Col: Integer): Boolean;
function HitTestCube3dN(pt: TPoint; N: Integer;
  out Face: TFace; out Row, Col: Integer): Boolean;

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
  CubeySize: integer = 30;

  // Lighting control - set to False to disable lighting
  UseLighting: Boolean = True;

  // Keyboard face selection - which of the 3 visible faces is selected
  KeyboardFaceSelectMode: Boolean = False;
  SelectedVisibleFace: Integer = 0;  // Index into VisibleFaces array (0, 1, or 2)
  SelectedSliceDepth: Integer = 0;   // 0 = outer layer, 1 = first inner, etc.
  MarchingAntsOffset: Integer = 0;   // Animation offset for marching ants
  FaceHighlightPhase: Single = 0;    // 0 to 2*Pi for pulsing effect

  // Track which 3 faces are currently visible to the user
  // Face indices: 1=Top, 2=Front, 3=Right, 4=Back, 5=Left, 6=Bottom
  // VisibleFaces[0] = top-ish face, [1] = front-ish face, [2] = right-ish face
  VisibleFaces: array[0..2] of Integer = (1, 2, 3);  // Default: Top, Front, Right
  ViewRotationY: Integer = 0;  // Track Y rotation in 90-degree increments (0, 1, 2, 3)
  ViewFlipped: Boolean = False;  // Track if cube has been flipped 180° (W key)

  // Key repeat detection - prevent held key from spamming moves
  LastArrowKeyDown: Word = 0;
  ArrowKeyIsHeld: Boolean = False;

const
  // Move queue for buffering keyboard inputs during animations
  MAX_MOVE_QUEUE = 10;

type
  TMoveQueueItem = record
    Face: Integer;      // 0-based face index for ManualRotateFace
    Clockwise: Boolean;
    // Slice parameters (for NxN inner/wide moves)
    GenFace: TFace;
    GenDir: TMoveDirection;
    SliceStart: Integer;
    SliceEnd: Integer;
    UseSlice: Boolean;  // True = use GenFace/GenDir/Slice params
  end;

var
  MoveQueue: array[0..MAX_MOVE_QUEUE-1] of TMoveQueueItem;
  MoveQueueCount: Integer = 0;

  // Mouse wheel throttle for color cycling
  LastWheelColorTick: QWord = 0;

  // NxN 3D state
  Cube3DN: TDynCube3D;
  PolyOrderN: array of TPolyOrder;
  NxNFaceRotation: array[0..5] of TArray<Integer>;  // [generic face ord] -> cubelet indices
  NxNCubeSize: Integer = 0;

  // ---- 2D Unfolded View Grid Positions ----
  //
  // C_FACE_GRID_POS maps each face's [row][col] to a grid position (x, y) in
  // the standard cross-shaped unfolded layout. Grid units (not pixels) - multiply
  // by CubeySize for actual pixel coordinates.
  //
  // The 2D layout on a 12-wide x 9-tall grid:
  //
  //              col: 3  4  5
  //   row 0-2:        [ U  ]           (face 1: Top/White)
  //
  //   col: 0  1  2    3  4  5    6  7  8    9 10 11
  //   row 3-5:  [ L  ]  [ F  ]  [ R  ]  [ B  ]
  //             (face 5) (face 2) (face 3) (face 4)
  //
  //              col: 3  4  5
  //   row 6-8:        [ D  ]           (face 6: Bottom/Yellow)
  //
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

var
  // Local axis vectors - track cube orientation for face rotation animation
  axeX: T3dPoint = (-1, 0, 0);
  axeY: T3dPoint = (0, -1, 0);
  axeZ: T3dPoint = (0, 0, -1);


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

// Draw a 2D unfolded view of any NxN cube using TGenericCube.
// Same cross layout as DrawCube but dynamically sized:
//
//              [ U ]
//   [ L ][ F ][ R ][ B ]
//              [ D ]
//
procedure DrawCubeN(P: TPaintBox; cube: TGenericCube);
var
  N, cellSize, gx, gy, row, col: Integer;
  tmp: TBitmap;
  f: TFace;
  faceColor: TColor;
  faceOffX, faceOffY: Integer;
begin
  N := cube.CubeSize;
  tmp := TBitmap.Create;
  try
    tmp.Width := P.Width;
    tmp.Height := P.Height;
    tmp.Canvas.Brush.Color := clAppWorkspace;
    tmp.Canvas.FillRect(P.ClientRect);

    // Calculate cell size to fit the 4N x 3N grid
    cellSize := Min(P.Width div (4 * N), P.Height div (3 * N));
    if cellSize < 2 then cellSize := 2;

    // Draw each face
    for f := Low(TFace) to High(TFace) do
    begin
      // Grid offset for each face in cell units
      // Layout: U at (N,0), L at (0,N), F at (N,N), R at (2N,N), B at (3N,N), D at (N,2N)
      case f of
        faceU: begin faceOffX := N;     faceOffY := 0;     end;
        faceR: begin faceOffX := 2 * N; faceOffY := N;     end;
        faceF: begin faceOffX := N;     faceOffY := N;     end;
        faceD: begin faceOffX := N;     faceOffY := 2 * N; end;
        faceL: begin faceOffX := 0;     faceOffY := N;     end;
        faceB: begin faceOffX := 3 * N; faceOffY := N;     end;
      end;

      // Draw background for face
      tmp.Canvas.Brush.Color := clSilver;
      tmp.Canvas.Rectangle(
        faceOffX * cellSize + 2, faceOffY * cellSize + 2,
        (faceOffX + N) * cellSize - 4, (faceOffY + N) * cellSize - 4);

      // Draw each sticker
      for row := 0 to N - 1 do
        for col := 0 to N - 1 do
        begin
          gx := faceOffX + col;
          gy := faceOffY + row;
          faceColor := CubeColorToTColor[cube.Facelets[f, row, col]];
          tmp.Canvas.Brush.Color := faceColor;
          tmp.Canvas.Rectangle(
            gx * cellSize, gy * cellSize,
            gx * cellSize + cellSize - 2, gy * cellSize + cellSize - 2);
        end;
    end;

    P.Canvas.Draw(0, 0, tmp);
  finally
    tmp.Free;
  end;
end;

function HitTestCubeN(P: TPaintBox; N: Integer; pt: TPoint;
  out Face: TFace; out Row, Col: Integer): Boolean;
var
  cellSize: Integer;
  gridX, gridY: Integer;
  faceOffX, faceOffY: Integer;
  f: TFace;
  localCol, localRow: Integer;
  pixX, pixY: Integer;
begin
  Result := False;
  cellSize := Min(P.Width div (4 * N), P.Height div (3 * N));
  if cellSize < 2 then cellSize := 2;

  gridX := pt.X div cellSize;
  gridY := pt.Y div cellSize;

  // Check each face's bounding box (same layout as DrawCubeN)
  for f := Low(TFace) to High(TFace) do
  begin
    case f of
      faceU: begin faceOffX := N;     faceOffY := 0;     end;
      faceR: begin faceOffX := 2 * N; faceOffY := N;     end;
      faceF: begin faceOffX := N;     faceOffY := N;     end;
      faceD: begin faceOffX := N;     faceOffY := 2 * N; end;
      faceL: begin faceOffX := 0;     faceOffY := N;     end;
      faceB: begin faceOffX := 3 * N; faceOffY := N;     end;
    end;

    if (gridX >= faceOffX) and (gridX < faceOffX + N) and
       (gridY >= faceOffY) and (gridY < faceOffY + N) then
    begin
      localCol := gridX - faceOffX;
      localRow := gridY - faceOffY;

      // Verify pixel is within the sticker (not in the 2px gap)
      pixX := pt.X - (faceOffX + localCol) * cellSize;
      pixY := pt.Y - (faceOffY + localRow) * cellSize;
      if (pixX < cellSize - 2) and (pixY < cellSize - 2) then
      begin
        Face := f;
        Row := localRow;
        Col := localCol;
        Result := True;
      end;
      Exit;
    end;
  end;
end;

function HitTestCube3dN(pt: TPoint; N: Integer;
  out Face: TFace; out Row, Col: Integer): Boolean;
var
  i, polyCount: Integer;
  closestZ: Single;
  clickedIndex: Integer;
  cubeletIdx, NSq, faceOrd, stickerIdx: Integer;
begin
  Result := False;
  polyCount := Length(PolyOrderN);
  if polyCount = 0 then Exit;

  clickedIndex := -1;
  closestZ := -1e30;

  for i := 0 to polyCount - 1 do
  begin
    if (PolyOrderN[i].order <> -1) and (PolyOrderN[i].color <> clBlack) then
    begin
      if PointInPolygon(pt, PolyOrderN[i].pt) then
      begin
        if PolyOrderN[i].z > closestZ then
        begin
          clickedIndex := PolyOrderN[i].order;
          closestZ := PolyOrderN[i].z;
        end;
      end;
    end;
  end;

  if clickedIndex <> -1 then
  begin
    NSq := N * N;
    cubeletIdx := clickedIndex div 5;
    faceOrd := cubeletIdx div NSq;
    stickerIdx := cubeletIdx mod NSq;
    Row := stickerIdx div N;
    Col := stickerIdx mod N;

    if (faceOrd >= 0) and (faceOrd <= 5) then
    begin
      Face := TFace(faceOrd);
      Result := True;
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




// Project a 3D point to 2D screen coordinates using perspective projection.
// dx, dy = screen center offset; scalingFactor = zoom level.
// The Z+10 shifts the cube away from the camera to prevent division issues.
// Y is negated because screen Y grows downward but 3D Y grows upward.
function Pt3dTo2D(x, y, z: single; dx, dy, scalingFactor: integer): TPoint;
begin
  z := z + 10;
  Result.x := Round((x * scalingFactor / z) * 20) + dx;
  Result.y := -Round((y * scalingFactor / z) * 20) + dy;
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


// Rodrigues' rotation formula: rotate point p around axis vect by angle r (radians).
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

// ============================================================================
// NxN Dynamic 3D Cube Support
// ============================================================================
//
// For any NxN cube, we dynamically generate the 3D polygon mesh that the
// legacy VIEW_OF_3D_CUBE provides for 3x3. The cube spans -3.0 to +3.0 on
// each axis (same as legacy). Each sticker has 5 polygons: 4 side bevels + 1
// top colored face, giving 6*N*N*5 total polygons.
//
// Face geometry is defined by:
//   origin  - corner of cell (row=0, col=0)
//   colVec  - unit vector in the column-increasing direction
//   rowVec  - unit vector in the row-increasing direction
//   normVec - outward-facing normal (direction sticker is raised)

type
  TFaceGeom = record
    origin: T3dPoint;
    colVec: T3dPoint;
    rowVec: T3dPoint;
    normVec: T3dPoint;
  end;

const
  CUBE_HALF = 3.0;

  // Face geometry definitions - verified against legacy VIEW_OF_3D_CUBE
  FaceGeoms: array[0..5] of TFaceGeom = (
    // faceU (0): Y=+3 base, stickers in X-Z plane
    (origin: (-3, +3, +3); colVec: (1, 0, 0); rowVec: (0, 0, -1); normVec: (0, 1, 0)),
    // faceR (1): X=+3 base, stickers in Y-Z plane
    (origin: (+3, +3, -3); colVec: (0, 0, 1); rowVec: (0, -1, 0); normVec: (1, 0, 0)),
    // faceF (2): Z=-3 base, stickers in X-Y plane
    (origin: (-3, +3, -3); colVec: (1, 0, 0); rowVec: (0, -1, 0); normVec: (0, 0, -1)),
    // faceD (3): Y=-3 base, stickers in X-Z plane
    (origin: (-3, -3, -3); colVec: (1, 0, 0); rowVec: (0, 0, 1); normVec: (0, -1, 0)),
    // faceL (4): X=-3 base, stickers in Y-Z plane
    (origin: (-3, +3, +3); colVec: (0, 0, -1); rowVec: (0, -1, 0); normVec: (-1, 0, 0)),
    // faceB (5): Z=+3 base, stickers in X-Y plane
    (origin: (+3, +3, +3); colVec: (-1, 0, 0); rowVec: (0, -1, 0); normVec: (0, 0, 1))
  );

function GenerateNxNCube3D(N: Integer): TDynCube3D;
var
  cellSize, gap, raise_: Single;
  totalPolys, polyIdx: Integer;
  faceIdx, row, col, k: Integer;
  fg: TFaceGeom;
  B, T: array[0..3] of T3dPoint;
  co, ro: T3dPoint; // cell origin
begin
  cellSize := 2 * CUBE_HALF / N;
  gap := 0.05 * cellSize;    // 5% inset, matches legacy 0.1 for N=3
  raise_ := 0.05 * cellSize; // 5% raise, matches legacy 0.1 for N=3

  totalPolys := 6 * N * N * 5;
  SetLength(Result, totalPolys);
  polyIdx := 0;

  for faceIdx := 0 to 5 do
  begin
    fg := FaceGeoms[faceIdx];

    for row := 0 to N - 1 do
      for col := 0 to N - 1 do
      begin
        // Cell origin for this sticker
        for k := 0 to 2 do
          co[k] := fg.origin[k] + col * cellSize * fg.colVec[k]
                                 + row * cellSize * fg.rowVec[k];

        // 4 base corners: B0(origin), B1(+col), B2(+col+row), B3(+row)
        for k := 0 to 2 do
        begin
          B[0][k] := co[k];
          B[1][k] := co[k] + cellSize * fg.colVec[k];
          B[2][k] := co[k] + cellSize * fg.colVec[k] + cellSize * fg.rowVec[k];
          B[3][k] := co[k] + cellSize * fg.rowVec[k];
        end;

        // 4 raised corners: inset by gap toward center, raised by raise_ along normal
        for k := 0 to 2 do
        begin
          T[0][k] := B[0][k] + gap * fg.colVec[k] + gap * fg.rowVec[k] + raise_ * fg.normVec[k];
          T[1][k] := B[1][k] - gap * fg.colVec[k] + gap * fg.rowVec[k] + raise_ * fg.normVec[k];
          T[2][k] := B[2][k] - gap * fg.colVec[k] - gap * fg.rowVec[k] + raise_ * fg.normVec[k];
          T[3][k] := B[3][k] + gap * fg.colVec[k] - gap * fg.rowVec[k] + raise_ * fg.normVec[k];
        end;

        // Side 0: B0, B1, T1, T0
        Result[polyIdx, 1] := B[0];
        Result[polyIdx, 2] := B[1];
        Result[polyIdx, 3] := T[1];
        Result[polyIdx, 4] := T[0];
        Inc(polyIdx);

        // Side 1: B1, B2, T2, T1
        Result[polyIdx, 1] := B[1];
        Result[polyIdx, 2] := B[2];
        Result[polyIdx, 3] := T[2];
        Result[polyIdx, 4] := T[1];
        Inc(polyIdx);

        // Side 2: B2, B3, T3, T2
        Result[polyIdx, 1] := B[2];
        Result[polyIdx, 2] := B[3];
        Result[polyIdx, 3] := T[3];
        Result[polyIdx, 4] := T[2];
        Inc(polyIdx);

        // Side 3: B3, B0, T0, T3
        Result[polyIdx, 1] := B[3];
        Result[polyIdx, 2] := B[0];
        Result[polyIdx, 3] := T[0];
        Result[polyIdx, 4] := T[3];
        Inc(polyIdx);

        // Top face (the colored sticker): T0, T1, T2, T3
        Result[polyIdx, 1] := T[0];
        Result[polyIdx, 2] := T[1];
        Result[polyIdx, 3] := T[2];
        Result[polyIdx, 4] := T[3];
        Inc(polyIdx);
      end;
  end;
end;

// Generate face rotation cubelet mappings for NxN cube.
// For each face, stores the indices of all cubelets that rotate with it:
//   N^2 cubelets on the face + N cubelets on each of 4 adjacent faces.
// Cubelet index = Ord(face) * N^2 + row * N + col.
procedure GenerateNxNFaceRotationMap(N: Integer);
var
  faceOrd, r, c, idx: Integer;
  NSq: Integer;

  procedure AddOnFace(faceOrd: Integer);
  var i: Integer;
  begin
    for i := 0 to NSq - 1 do
    begin
      NxNFaceRotation[faceOrd][idx] := faceOrd * NSq + i;
      Inc(idx);
    end;
  end;

  procedure AddRow(adjFace, row: Integer);
  var cc: Integer;
  begin
    for cc := 0 to N - 1 do
    begin
      NxNFaceRotation[faceOrd][idx] := adjFace * NSq + row * N + cc;
      Inc(idx);
    end;
  end;

  procedure AddCol(adjFace, col: Integer);
  var rr: Integer;
  begin
    for rr := 0 to N - 1 do
    begin
      NxNFaceRotation[faceOrd][idx] := adjFace * NSq + rr * N + col;
      Inc(idx);
    end;
  end;

begin
  NSq := N * N;

  for faceOrd := 0 to 5 do
  begin
    SetLength(NxNFaceRotation[faceOrd], NSq + 4 * N);
    idx := 0;

    // All cubelets on the face itself
    AddOnFace(faceOrd);

    // Adjacent cubelets depend on which face
    case TFace(faceOrd) of
      faceU: begin // Adj: F row 0, R row 0, B row 0, L row 0
        AddRow(Ord(faceF), 0);
        AddRow(Ord(faceR), 0);
        AddRow(Ord(faceB), 0);
        AddRow(Ord(faceL), 0);
      end;
      faceD: begin // Adj: F row N-1, R row N-1, B row N-1, L row N-1
        AddRow(Ord(faceF), N - 1);
        AddRow(Ord(faceR), N - 1);
        AddRow(Ord(faceB), N - 1);
        AddRow(Ord(faceL), N - 1);
      end;
      faceR: begin // Adj: U col N-1, F col N-1, D col N-1, B col 0
        AddCol(Ord(faceU), N - 1);
        AddCol(Ord(faceF), N - 1);
        AddCol(Ord(faceD), N - 1);
        AddCol(Ord(faceB), 0);
      end;
      faceL: begin // Adj: U col 0, F col 0, D col 0, B col N-1
        AddCol(Ord(faceU), 0);
        AddCol(Ord(faceF), 0);
        AddCol(Ord(faceD), 0);
        AddCol(Ord(faceB), N - 1);
      end;
      faceF: begin // Adj: U row N-1, R col 0, D row 0, L col N-1
        AddRow(Ord(faceU), N - 1);
        AddCol(Ord(faceR), 0);
        AddRow(Ord(faceD), 0);
        AddCol(Ord(faceL), N - 1);
      end;
      faceB: begin // Adj: U row 0, R col N-1, D row N-1, L col 0
        AddRow(Ord(faceU), 0);
        AddCol(Ord(faceR), N - 1);
        AddRow(Ord(faceD), N - 1);
        AddCol(Ord(faceL), 0);
      end;
    end;
  end;
end;

// Initialize NxN 3D state: generate geometry and face rotation maps
procedure InitNxN3D(N: Integer);
begin
  NxNCubeSize := N;
  Cube3DN := GenerateNxNCube3D(N);
  SetLength(PolyOrderN, Length(Cube3DN));
  GenerateNxNFaceRotationMap(N);
  // Reset rotation axes so SetInitialCubeView starts from clean state
  axeX[0] := -1; axeX[1] := 0; axeX[2] := 0;
  axeY[0] := 0;  axeY[1] := -1; axeY[2] := 0;
  axeZ[0] := 0;  axeZ[1] := 0;  axeZ[2] := -1;
end;

// Draw NxN cube in 3D using painter's algorithm with lighting.
// Same visual style as DrawCube3d but works with dynamic polygon arrays
// and reads sticker colors from TGenericCube.
procedure DrawCube3dN(Bitmap: TBGRABitmap; cube: TGenericCube; const C3D: TDynCube3D);
const
  // Map legacy 1-based face index to TFace ordinal for NxNFaceRotation lookup
  // Legacy: 1=U, 2=F, 3=R, 4=B, 5=L, 6=D
  // TFace:  0=U, 1=R, 2=F, 3=D, 4=L, 5=B
  LegacyFaceToGenericOrd: array[1..6] of Integer = (0, 2, 1, 5, 4, 3);
var
  i, j, N, polyCount: Integer;
  pt: T2DArray;
  dx, dy, ScalingFactor: Integer;
  ux, uy, vx, vy: Single;
  bgra_pts: array[0..3] of TPointF;
  normal: T3dPoint;
  litColor: TBGRAPixel;
  nextJ: Integer;
  edgeDx, edgeDy: Single;
  cubeletIdx, faceOrd, stickerIdx, row, col: Integer;
  NSq: Integer;
  stickerColor: TColor;
  isSelectedFacePoly: Boolean;
  selFaceOrd, selJ: Integer;
  selFaceLegacy: Integer;
  selSliceIndices: TArray<Integer>;
begin
  N := cube.CubeSize;
  NSq := N * N;
  polyCount := Length(C3D);

  if polyCount = 0 then Exit;
  if Length(PolyOrderN) <> polyCount then
    SetLength(PolyOrderN, polyCount);

  Bitmap.Fill(ColorToBGRA(clAppWorkspace));
  dx := Bitmap.Width div 2;
  dy := Bitmap.Height div 2;
  ScalingFactor := Min(Bitmap.Width, Bitmap.Height) div 24;

  // Build draw list: project all polygons to 2D and determine colors
  for i := 0 to polyCount - 1 do
  begin
    PolyOrderN[i].order := -1;
    for j := 0 to 3 do
      pt[j] := Pt3dTo2D(C3D[i, j + 1, 0], C3D[i, j + 1, 1], C3D[i, j + 1, 2],
                         dx, dy, ScalingFactor);

    ux := pt[0].X - pt[1].X;
    uy := pt[0].Y - pt[1].Y;
    vx := pt[2].X - pt[1].X;
    vy := pt[2].Y - pt[1].Y;

    PolyOrderN[i].pt := pt;
    PolyOrderN[i].z := (C3D[i, 1, 2] + C3D[i, 2, 2] + C3D[i, 3, 2] + C3D[i, 4, 2]) / 4;
    PolyOrderN[i].order := i;

    // Color: top face (i mod 5 = 4) gets sticker color, sides are black
    if i mod 5 = 4 then
    begin
      cubeletIdx := i div 5;
      faceOrd := cubeletIdx div NSq;
      stickerIdx := cubeletIdx mod NSq;
      row := stickerIdx div N;
      col := stickerIdx mod N;
      stickerColor := CubeColorToTColor[cube.Facelets[TFace(faceOrd), row, col]];
      PolyOrderN[i].color := stickerColor;
    end
    else
      PolyOrderN[i].color := clBlack;

    // Back-face culling
    if ux * vy - uy * vx >= 0 then
      PolyOrderN[i].color := clBlack;
  end;

  // Sort by Z depth (painter's algorithm)
  QuickSortPolyOrder(PolyOrderN, 0, polyCount - 1);

  // Calculate selected face/slice for marching ants highlighting
  selFaceOrd := -1;
  selSliceIndices := nil;
  if KeyboardFaceSelectMode then
  begin
    selFaceLegacy := VisibleFaces[SelectedVisibleFace];  // 1-based legacy face
    if (selFaceLegacy >= 1) and (selFaceLegacy <= 6) then
    begin
      selFaceOrd := LegacyFaceToGenericOrd[selFaceLegacy];
      selSliceIndices := GetSliceCubeletIndices(N, selFaceOrd, SelectedSliceDepth);
    end;
  end;

  // Render all polygons
  for i := 0 to polyCount - 1 do
    if PolyOrderN[i].order <> -1 then
    begin
      for j := 0 to 3 do
      begin
        bgra_pts[j].x := PolyOrderN[i].pt[j].x;
        bgra_pts[j].y := PolyOrderN[i].pt[j].y;
      end;

      // Check if this polygon belongs to a cubelet on the selected slice
      isSelectedFacePoly := False;
      if (selFaceOrd >= 0) and (selSliceIndices <> nil) then
      begin
        cubeletIdx := PolyOrderN[i].order div 5;
        for selJ := 0 to Length(selSliceIndices) - 1 do
        begin
          if selSliceIndices[selJ] = cubeletIdx then
          begin
            isSelectedFacePoly := True;
            Break;
          end;
        end;
      end;

      if PolyOrderN[i].color = clBlack then
      begin
        Bitmap.FillPolyAntialias(bgra_pts, ColorToBGRA(clBlack));
      end
      else if UseLighting then
      begin
        normal := CalculateFaceNormal(
          C3D[PolyOrderN[i].order, 1],
          C3D[PolyOrderN[i].order, 2],
          C3D[PolyOrderN[i].order, 3]
        );
        litColor := ApplyLighting(PolyOrderN[i].color, normal);
        Bitmap.FillPolyAntialias(bgra_pts, litColor);

        // Draw marching ants outline for selected face cubelets
        if isSelectedFacePoly then
          DrawMarchingAnts(Bitmap, bgra_pts, MarchingAntsOffset, 3.0);

        // Directional bevel edges
        for j := 0 to 3 do
        begin
          nextJ := (j + 1) mod 4;
          edgeDx := bgra_pts[nextJ].x - bgra_pts[j].x;
          edgeDy := bgra_pts[nextJ].y - bgra_pts[j].y;
          if (edgeDx < 0) or (edgeDy < 0) then
            Bitmap.DrawLineAntialias(
              Round(bgra_pts[j].x), Round(bgra_pts[j].y),
              Round(bgra_pts[nextJ].x), Round(bgra_pts[nextJ].y),
              ColorToBGRA(clWhite, 100), 1.5)
          else if (edgeDx > 0) or (edgeDy > 0) then
            Bitmap.DrawLineAntialias(
              Round(bgra_pts[j].x), Round(bgra_pts[j].y),
              Round(bgra_pts[nextJ].x), Round(bgra_pts[nextJ].y),
              ColorToBGRA(clBlack, 80), 1.5);
        end;

        Bitmap.DrawPolygonAntialias(bgra_pts, ColorToBGRA(clBlack, 35), 3.5);
        Bitmap.DrawPolygonAntialias(bgra_pts, ColorToBGRA(clWhite, 150), 1.0);
        Bitmap.DrawPolygonAntialias(bgra_pts, ColorToBGRA(clBlack, 160), 2.0);
      end
      else
      begin
        Bitmap.FillPolyAntialias(bgra_pts, ColorToBGRA(PolyOrderN[i].color));

        // Draw marching ants outline for selected face cubelets (no lighting)
        if isSelectedFacePoly then
          DrawMarchingAnts(Bitmap, bgra_pts, MarchingAntsOffset, 3.0);

        Bitmap.DrawPolygonAntialias(bgra_pts, ColorToBGRA(clBlack, 35), 3.5);
        Bitmap.DrawPolygonAntialias(bgra_pts, ColorToBGRA(clWhite, 150), 1.0);
        Bitmap.DrawPolygonAntialias(bgra_pts, ColorToBGRA(clBlack, 160), 2.0);
      end;
    end;
end;

// Rotate the entire NxN cube geometry around world axes.
// Also updates axeX/Y/Z so face rotations remain correct.
procedure Rotate3dN(var C: TDynCube3D; rx, ry, rz: single);
var
  i, j, polyCount: Integer;
begin
  polyCount := Length(C);
  if polyCount = 0 then Exit;

  // Update local axis vectors so face rotations use correct axes
  axeX := RotationPoint(axeX, REFERENCE_X, rx);
  axeX := RotationPoint(axeX, REFERENCE_Y, ry);
  axeX := RotationPoint(axeX, REFERENCE_Z, rz);
  axeY := RotationPoint(axeY, REFERENCE_X, rx);
  axeY := RotationPoint(axeY, REFERENCE_Y, ry);
  axeY := RotationPoint(axeY, REFERENCE_Z, rz);
  axeZ := RotationPoint(axeZ, REFERENCE_X, rx);
  axeZ := RotationPoint(axeZ, REFERENCE_Y, ry);
  axeZ := RotationPoint(axeZ, REFERENCE_Z, rz);

  for i := 0 to polyCount - 1 do
    for j := 1 to 4 do
    begin
      C[i, j] := RotationPoint(C[i, j], REFERENCE_X, rx);
      C[i, j] := RotationPoint(C[i, j], REFERENCE_Y, ry);
      C[i, j] := RotationPoint(C[i, j], REFERENCE_Z, rz);
    end;
end;

// Rotate one face's cubelets for NxN animation.
// faceOrd: 0-5 = TFace enum value (faceU..faceB)
// rotation: angle in radians (positive = CW from outside)
procedure Rotate3dFaceN(var C: TDynCube3D; N, faceOrd: Integer; rotation: single);
var
  i, j, k, cubeletIdx, polyIdx: Integer;
  axe: T3dPoint;
  rotIndices: TArray<Integer>;
begin
  if (faceOrd < 0) or (faceOrd > 5) then Exit;
  if Length(NxNFaceRotation[faceOrd]) = 0 then Exit;

  // Determine rotation axis
  case TFace(faceOrd) of
    faceU, faceD: axe := axeY;
    faceR, faceL: axe := axeX;
    faceF, faceB: axe := axeZ;
  end;

  rotIndices := NxNFaceRotation[faceOrd];

  // Rotate all 5 polygons of each affected cubelet
  for i := 0 to Length(rotIndices) - 1 do
  begin
    cubeletIdx := rotIndices[i];
    for j := 0 to 4 do
    begin
      polyIdx := cubeletIdx * 5 + j;
      if polyIdx < Length(C) then
        for k := 1 to 4 do
          C[polyIdx, k] := RotationPoint(C[polyIdx, k], axe, rotation);
    end;
  end;
end;

// Get cubelet polygon indices for a specific slice depth of a face.
// Depth 0: the face's own N² cubelets + 4×N adjacent cubelets (same as NxNFaceRotation)
// Depth > 0: only 4×N adjacent cubelets at that depth (no face cubelets)
function GetSliceCubeletIndices(N, faceOrd, sliceDepth: Integer): TArray<Integer>;
var
  NSq, idx, cc: Integer;

  procedure AddRow(adjFace, row: Integer);
  var k: Integer;
  begin
    for k := 0 to N - 1 do
    begin
      Result[idx] := adjFace * NSq + row * N + k;
      Inc(idx);
    end;
  end;

  procedure AddCol(adjFace, col: Integer);
  var k: Integer;
  begin
    for k := 0 to N - 1 do
    begin
      Result[idx] := adjFace * NSq + k * N + col;
      Inc(idx);
    end;
  end;

begin
  Result := nil;
  NSq := N * N;
  idx := 0;

  // Bounds validation
  if (N < 2) or (faceOrd < 0) or (faceOrd > 5) or (sliceDepth < 0) or (sliceDepth >= N) then
    Exit;

  if sliceDepth = 0 then
  begin
    // Outer layer: face cubelets + 4×N adjacent
    SetLength(Result, NSq + 4 * N);

    // Face's own cubelets
    for cc := 0 to NSq - 1 do
    begin
      Result[idx] := faceOrd * NSq + cc;
      Inc(idx);
    end;
  end
  else
  begin
    // Inner slice: only 4×N adjacent cubelets
    SetLength(Result, 4 * N);
  end;

  // Adjacent cubelets at this depth
  case TFace(faceOrd) of
    faceU: begin
      AddRow(Ord(faceF), sliceDepth);
      AddRow(Ord(faceR), sliceDepth);
      AddRow(Ord(faceB), sliceDepth);
      AddRow(Ord(faceL), sliceDepth);
    end;
    faceD: begin
      AddRow(Ord(faceF), N - 1 - sliceDepth);
      AddRow(Ord(faceR), N - 1 - sliceDepth);
      AddRow(Ord(faceB), N - 1 - sliceDepth);
      AddRow(Ord(faceL), N - 1 - sliceDepth);
    end;
    faceR: begin
      AddCol(Ord(faceU), N - 1 - sliceDepth);
      AddCol(Ord(faceF), N - 1 - sliceDepth);
      AddCol(Ord(faceD), N - 1 - sliceDepth);
      AddCol(Ord(faceB), sliceDepth);
    end;
    faceL: begin
      AddCol(Ord(faceU), sliceDepth);
      AddCol(Ord(faceF), sliceDepth);
      AddCol(Ord(faceD), sliceDepth);
      AddCol(Ord(faceB), N - 1 - sliceDepth);
    end;
    faceF: begin
      AddRow(Ord(faceU), N - 1 - sliceDepth);
      AddCol(Ord(faceR), sliceDepth);
      AddRow(Ord(faceD), sliceDepth);
      AddCol(Ord(faceL), N - 1 - sliceDepth);
    end;
    faceB: begin
      AddRow(Ord(faceU), sliceDepth);
      AddCol(Ord(faceR), N - 1 - sliceDepth);
      AddRow(Ord(faceD), N - 1 - sliceDepth);
      AddCol(Ord(faceL), sliceDepth);
    end;
  end;
end;

// Rotate cubelets for one or more slices (inner-slice / wide move animation).
// Loops SliceStart..SliceEnd, gets indices for each, rotates all cubelet polygons.
// Uses a processed set to avoid double-rotating cubelets shared between slices.
procedure Rotate3dSliceN(var C: TDynCube3D; N, faceOrd: Integer;
  SliceStart, SliceEnd: Integer; rotation: single);
var
  s, i, j, k, cubeletIdx, polyIdx, polyCount: Integer;
  axe: T3dPoint;
  indices: TArray<Integer>;
  processed: array of Boolean;
begin
  if (faceOrd < 0) or (faceOrd > 5) then Exit;
  if (N < 2) or (SliceStart < 0) then Exit;

  // Clamp slice range to valid values
  if SliceEnd >= N then SliceEnd := N - 1;
  if SliceStart >= N then Exit;

  polyCount := Length(C) div 5;
  SetLength(processed, polyCount);
  for i := 0 to polyCount - 1 do
    processed[i] := False;

  // Determine rotation axis
  case TFace(faceOrd) of
    faceU, faceD: axe := axeY;
    faceR, faceL: axe := axeX;
    faceF, faceB: axe := axeZ;
  end;

  for s := SliceStart to SliceEnd do
  begin
    indices := GetSliceCubeletIndices(N, faceOrd, s);

    for i := 0 to Length(indices) - 1 do
    begin
      cubeletIdx := indices[i];
      if (cubeletIdx < 0) or (cubeletIdx >= polyCount) then Continue;
      if processed[cubeletIdx] then Continue;
      processed[cubeletIdx] := True;

      // Rotate all 5 polygons of this cubelet
      for j := 0 to 4 do
      begin
        polyIdx := cubeletIdx * 5 + j;
        if polyIdx < Length(C) then
          for k := 1 to 4 do
            C[polyIdx, k] := RotationPoint(C[polyIdx, k], axe, rotation);
      end;
    end;
  end;
end;

end.
