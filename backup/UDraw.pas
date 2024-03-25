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
  UConst;

procedure SetColor2D(var cube: TRubik; pt: tpoint; colindex: integer);
function GetColor2D(const cube: TRubik; pt: TPoint): integer;
procedure Rotate3d(var C: TCube3D; rx, ry, rz: single);
procedure Rotate3dFace(var C: TCube3D; face: integer; rotation: single);
procedure DrawCube3d(P: TPaintBox; c: TRubik; C3D: TCube3D);
procedure DrawCube(P: TPaintBox; c: TRubik);

var
  CurrentCubeState: TRubik;
  TargetCubeState: TRubik;
  SelectedColor: integer = 0;
  Cube3D: TCube3D;
  axeX: T3dPoint = (-1, 0, 0);
  axeY: T3dPoint = (0, -1, 0);
  axeZ: T3dPoint = (0, 0, -1);
  CubeySize: integer = 30;

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
          faceName := faceName
          + IntToStr(TFaceRubik(c)[i, j])
          //+ IntToStr(C[i, j, k].x);
          // Constructs name like U1, L2, etc.
          // Needs more thought
                                             ;
          tmp.Canvas.Font.Size := 7;
          tmp.Canvas.Font.Color := clBlack;

           //Draw face name in the center of each cubelet
          tmp.Canvas.TextOut(
          C_FACE_GRID_POS[i, j, k].x * CubeySize + (CubeySize div 2) - (tmp.Canvas.TextWidth(faceName) div 2),
          C_FACE_GRID_POS[i, j, k].Y * CubeySize + (CubeySize div 2) - (tmp.Canvas.TextHeight(faceName) div 2),
          faceName);
        end;
      end;
    end;

    P.Canvas.Draw(0, 0, tmp);
  finally
    tmp.Free;
  end;
end;

function GetColor2D(const cube: TRubik; pt: TPoint): integer;
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

procedure SetColor2D(var cube: TRubik; pt: tpoint; colindex: integer);
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

function PointInPolygon(point: TPoint; const polygon: array of TPoint): Boolean;
var
  i, j: Integer;
  inside: Boolean;
begin
  inside := False;
  j := High(polygon);
  for i := Low(polygon) to High(polygon) do
  begin
    if (((polygon[i].Y > point.Y) <> (polygon[j].Y > point.Y)) and
       (point.X < (polygon[j].X - polygon[i].X) * (point.Y - polygon[i].Y) / (polygon[j].Y - polygon[i].Y) + polygon[i].X)) then
      inside := not inside;
    j := i;
  end;
  Result := inside;
end;

function GetClickedCubie(x, y: Integer; const Polygons: array of TPolygon): TCubie;
var
  i: Integer;
begin
  // Initialize result to indicate no cubie was found
  Result.CubieIndex := -1;

  for i := Low(Polygons) to High(Polygons) do
  begin
    if PointInPolygon(Point(x, y), Polygons[i].Vertices) then
    begin
      // We found the polygon that was clicked, now map it back to a cubie
      Result := Polygons[i].Cubie; // This assumes you have a way to map polygons to cubies
      Break;
    end;
  end;
end;


// RENDU 3D RENDU 3D RENDU 3D RENDU 3D RENDU 3D RENDU 3D RENDU 3D RENDU 3D RENDU
// 3D RENDERING 3D RENDERING 3D RENDERING 3D RENDERING 3D RENDERING 3D RENDERING 3D RENDERING 3D RENDERING 3D

type
  T2DArray = array[0..3] of tpoint;

  TPolyOrder = record
    order: integer;
    z: single;
    color: tcolor;
    pt: T2DArray;
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

function Pt3dTo2D(x, y, z: single; dx, dy, scalingFactor: integer): TPoint;
begin
  z := z + 10; // Adjusts depth positioning
  Result.x := Round((x * scalingFactor / z) * 20) + dx;
  Result.y := -Round((y * scalingFactor / z) * 20) + dy; // Negative to flip Y-axis for screen coordinates
end;

var
  PolyOrder: array[0..269] of TPolyOrder;

procedure DrawCube3d(P: TPaintBox; c: TRubik; C3D: TCube3D);
var
  i, j: integer;
  pt: T2DArray;
  dx, dy, BaseScale, ScalingFactor: integer;
  ux, uy: single;
  vx, vy: single;
  tmp: tbitmap;
  polytmp: TPolyOrder;
begin
  tmp := tbitmap.Create;
  tmp.Width := p.Width;
  tmp.Height := p.Height;
  tmp.Canvas.Brush.color := clAppWorkspace;
  tmp.canvas.FillRect(p.ClientRect);
  dx := tmp.Width div 2;
  dy := tmp.Height div 2;

  ScalingFactor := Min(P.Width, P.Height) div 24;

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
    if i mod 5 = 4 then PolyOrder[i].color := C_COLOR[c[(i div 5) div 9 + 1, ((i div 5) mod 9)]]
    else
      PolyOrder[i].color := clblack;
    // les faces arrières sont noirs
    // the back faces are black
    if ux * vy - uy * vx >= 0 then PolyOrder[i].color := clblack;
  end;

  // tri des polygones à dessiner par ordre Z
  // sorting of polygons to be drawn by Z order
  for i := 0 to 268 do if PolyOrder[i].order <> -1 then
      for j := i + 1 to 269 do if PolyOrder[j].order <> -1 then
          if PolyOrder[j].z > PolyOrder[i].z then
          begin
            polytmp := PolyOrder[j];
            PolyOrder[j] := PolyOrder[i];
            PolyOrder[i] := polytmp;
          end;

  // affichage des polygones
  // display of polygons
  for i := 0 to 269 do
    if PolyOrder[i].order <> -1 then
    begin
      tmp.canvas.Brush.color := PolyOrder[i].color;
      tmp.canvas.Polygon(PolyOrder[i].pt);
    end;

  p.Canvas.Draw(0, 0, tmp);
  tmp.Free;
end;

end.
