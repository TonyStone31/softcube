program scramble_gen;

{$mode objfpc}{$H+}

uses
  SysUtils, nxn_cubedefs, nxn_facecube, nxn_util;

var
  cubeSize, numMoves, i, slc, pow: integer;
  fc: FaceletCube;
  a: Axis;
  outStr: string;
  n2: integer;
  face, kFace, row, col: integer;
begin
  if ParamCount < 1 then
  begin
    WriteLn(StdErr, 'Usage: scramble_gen <size> [num_moves]');
    Halt(1);
  end;

  cubeSize := StrToIntDef(ParamStr(1), 5);
  numMoves := StrToIntDef(ParamStr(2), 30);

  Randomize;
  fc := FaceletCube.Create(cubeSize);
  try
    // Initialize solved state: face i gets color i
    // Kociemba order: U=0, D=1, R=2, L=3, F=4, B=5
    for face := 0 to 5 do
      for row := 0 to cubeSize - 1 do
        for col := 0 to cubeSize - 1 do
          fc.faceCols[face, row, col] := ColorIndex(face);

    // Apply random moves
    // Only use slices 0..cubeSize div 2 - 1 to avoid moving center facelets
    // (middle slice on odd cubes changes face identity)
    for i := 1 to numMoves do
    begin
      a := Axis(Random(6)); // U, D, R, L, F, B
      slc := Random(cubeSize div 2); // 0..size/2-1 (avoid middle slice)
      pow := Random(3) + 1; // 1, 2, or 3
      for n2 := 1 to pow do
        fc.move(a, slc);
    end;

    // Output URFDLB facelet string
    outStr := BuildFaceletString(cubeSize, fc.faceCols);
    WriteLn(outStr);
  finally
    fc.Free;
  end;
end.
