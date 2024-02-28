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
  SysUtils,
  Variants,
  Dialogs,
  ExtCtrls,
  Menus,
  Spin,
  StdCtrls,
  strutils,
  UConst;

procedure PlaceColorClicker(var cube: TRubik; pt: tpoint; colindex: integer);
function GetCubeyColor(const cube: TRubik; pt: TPoint): integer;
procedure Rotate3d(var C: TCube3D; rx, ry, rz: single);
procedure Rotate3dFace(var C: TCube3D; face: integer; rotation: single);
procedure DrawCube3d(P: TPaintBox; c: TRubik; C3D: TCube3D);
procedure DrawCube(P: TPaintBox; c: TRubik);
procedure PlaceColor(var cube: TRubik; pt: tpoint);

var
  original: TRubik;
  destination: TRubik;
  SelectedColor: integer = 0;
  Cube3D: TCube3D;
  axeX: T3dPoint = (-1, 0, 0);
  axeY: T3dPoint = (0, -1, 0);
  axeZ: T3dPoint = (0, 0, -1);



implementation


function GetCubeyColor(const cube: TRubik; pt: TPoint): integer;
var
  i, j: integer;
begin
  Result := -1; // Default value indicating no color found
  for i := 1 to 6 do
    for j := 0 to 8 do
      if j <> 4 then // Skipping the center piece as it's fixed
      begin
        if PtInRect(Rect(CFacePlace[i, j].x * C_CUBE_SIZE, CFacePlace[i, j].Y * C_CUBE_SIZE,
          CFacePlace[i, j].x * C_CUBE_SIZE + C_CUBE_SIZE - 3, CFacePlace[i, j].Y * C_CUBE_SIZE +
          C_CUBE_SIZE - 3), pt) then
        begin
          Result := cube[i, j]; // Return the color index at the clicked position
          Exit; // Exit as soon as the color is found
        end;
      end;
end;

procedure PlaceColor(var cube: TRubik; pt: tpoint);
var
  i, j: integer;
begin
  for i := 1 to 6 do for j := 0 to 8 do
      if j <> 4 then
      begin
        if PtInRect(Rect(CFacePlace[i, j].x * C_CUBE_SIZE, CFacePlace[i, j].Y * C_CUBE_SIZE,
          CFacePlace[i, j].x * C_CUBE_SIZE + C_CUBE_SIZE - 3, CFacePlace[i, j].Y * C_CUBE_SIZE +
          C_CUBE_SIZE - 3), pt) then
        begin
          cube[i, j] := SelectedColor;
        end;
      end;
end;

procedure PlaceColorClicker(var cube: TRubik; pt: tpoint; colindex: integer);
var
  i, j: integer;
begin
  for i := 1 to 6 do for j := 0 to 8 do
      if j <> 4 then
      begin
        if PtInRect(Rect(CFacePlace[i, j].x * C_CUBE_SIZE, CFacePlace[i, j].Y * C_CUBE_SIZE,
          CFacePlace[i, j].x * C_CUBE_SIZE + C_CUBE_SIZE - 3, CFacePlace[i, j].Y * C_CUBE_SIZE +
          C_CUBE_SIZE - 3), pt) then
        begin
          cube[i, j] := colindex;
        end;
      end;
end;

procedure DrawCube(P: TPaintBox; c: TRubik);
var
  i, j, k: integer;
  tmp: TBitmap;
begin
  tmp := TBitmap.Create;
  try
    tmp.Width := P.Width;
    tmp.Height := P.Height;
    tmp.Canvas.Brush.Color := clSilver;
    tmp.Canvas.FillRect(P.ClientRect);

    // Optionally draw a black outline around each cube face for clarity
    tmp.Canvas.Brush.Color := clBlack; // Outline color
    // Drawing outlines for each cube face

    // Background for Orange face
    tmp.Canvas.Rectangle(0 * C_CUBE_SIZE + 2, 3 * C_CUBE_SIZE + 2, 3 * C_CUBE_SIZE - 4, 6 * C_CUBE_SIZE - 4);

    //Background for Green face
    tmp.Canvas.Rectangle(3 * C_CUBE_SIZE + 2, 3 * C_CUBE_SIZE + 2, 6 * C_CUBE_SIZE - 4, 6 * C_CUBE_SIZE - 4);

    //Background for Red face
    tmp.Canvas.Rectangle(6 * C_CUBE_SIZE + 2, 3 * C_CUBE_SIZE + 2, 9 * C_CUBE_SIZE - 4, 6 * C_CUBE_SIZE - 4);

    //Background for Blue face
    tmp.Canvas.Rectangle(9 * C_CUBE_SIZE + 2, 3 * C_CUBE_SIZE + 2, 12 * C_CUBE_SIZE - 4, 6 * C_CUBE_SIZE - 4);

    //Background for White face
    tmp.Canvas.Rectangle(3 * C_CUBE_SIZE + 2, 0 * C_CUBE_SIZE + 2, 6 * C_CUBE_SIZE - 4, 3 * C_CUBE_SIZE - 4);

    //Background for Yellow face
    tmp.Canvas.Rectangle(3 * C_CUBE_SIZE + 2, 6 * C_CUBE_SIZE + 2, 6 * C_CUBE_SIZE - 4, 9 * C_CUBE_SIZE - 4);

    // Fill cube faces with appropriate colors
    for i := 1 to 6 do
    begin
      for j := 0 to 2 do
      begin
        for k := 0 to 2 do
        begin
          tmp.Canvas.Brush.Color := C_COLOR[TUnitRubik(c)[i, j, k]];
          tmp.Canvas.Rectangle(C_FACE_GRID_POS[i, j, k].x * C_CUBE_SIZE + 1,
            C_FACE_GRID_POS[i, j, k].Y * C_CUBE_SIZE + 1,
            C_FACE_GRID_POS[i, j, k].x * C_CUBE_SIZE + C_CUBE_SIZE - 3,
            C_FACE_GRID_POS[i, j, k].Y * C_CUBE_SIZE + C_CUBE_SIZE - 3);
        end;
      end;
    end;

    P.Canvas.Draw(0, 0, tmp);
  finally
    tmp.Free;
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

function Pt3dTo2D(x, y, z: single; dx, dy: integer): tpoint;
begin
  z := z + 10;
  Result.x := round((x * 8 / z) * 20) + dx;
  Result.y := -round((y * 8 / z) * 20) + dy;
end;

var
  PolyOrder: array[0..269] of TPolyOrder;

procedure DrawCube3d(P: TPaintBox; c: TRubik; C3D: TCube3D);
var
  i, j: integer;
  pt: T2DArray;
  dx, dy: integer;
  ux, uy: single;
  vx, vy: single;
  tmp: tbitmap;
  polytmp: TPolyOrder;
begin
  tmp := tbitmap.Create;
  tmp.Width := p.Width;
  tmp.Height := p.Height;
  tmp.Canvas.Brush.color := clbtnface;
  tmp.canvas.FillRect(p.ClientRect);
  dx := tmp.Width div 2;
  dy := tmp.Height div 2;

  for i := 0 to 269 do
  begin
    PolyOrder[i].order := -1;
    for j := 0 to 3 do pt[j] := Pt3dTo2D(C3D[i, j + 1, 0], C3D[i, j + 1, 1], C3D[i, j + 1, 2], dx, dy);
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
