program test_min2phase;
{$mode objfpc}{$H+}

uses SysUtils, nxn_min2phase_defs, nxn_min2phase_cubicube, nxn_min2phase_coord,
     nxn_min2phase_search, nxn_min2phase;

var
  cc: TCubieCube;
  ok: boolean;
  facelets, result_str: string;
  i: integer;
  fsCoord: integer;
begin
  // Test 1: Apply U move to solved cube and convert back
  WriteLn('=== Test 1: U move roundtrip ===');
  CubieIdentity(cc);
  CubieMove(cc, 0); // U
  facelets := CubieCubeToFacelets(cc);
  WriteLn('After U: ', facelets);

  // Test 2: Parse that back
  ok := FaceletsToCubieCube(facelets, cc);
  WriteLn('Parse ok: ', ok);
  WriteLn('CornPerm: ', GetCornPermCoord(cc));
  WriteLn('CornOri: ', GetCornOriCoord(cc));
  WriteLn('EdgeOri: ', GetEdgeOriCoord(cc));
  WriteLn('UDSlice: ', GetUDSliceCoord(cc));
  WriteLn('CornParity: ', CornParityEven(cc));
  WriteLn('EdgeParity: ', EdgeParityEven(cc));

  // Test 3: Solve U move
  WriteLn('');
  WriteLn('=== Test 2: Init and solve ===');
  InitMin2Phase;
  result_str := SolveMin2Phase(facelets);
  WriteLn('Solution for U: ', result_str);

  // Test 4: Solve R2 U' F
  WriteLn('');
  WriteLn('=== Test 3: R2 U'' F ===');
  CubieIdentity(cc);
  CubieMove(cc, 1); CubieMove(cc, 1); // R2
  CubieMove(cc, 0); CubieMove(cc, 0); CubieMove(cc, 0); // U'
  CubieMove(cc, 2); // F
  facelets := CubieCubeToFacelets(cc);
  WriteLn('Facelets: ', facelets);
  result_str := SolveMin2Phase(facelets);
  WriteLn('Solution: ', result_str);

  // Test 5: Test the problematic scramble
  WriteLn('');
  WriteLn('=== Test 4: problematic scramble ===');
  facelets := 'DRLUUBFBRBLURRLRUBLRDDFDLFUFUFFDBRDUBRUFLLFDDBFLUBLRBD';
  WriteLn('Facelets: ', facelets);

  ok := FaceletsToCubieCube(facelets, cc);
  WriteLn('Parse ok: ', ok);
  if ok then
  begin
    WriteLn('CornPerm: ', GetCornPermCoord(cc));
    WriteLn('CornOri: ', GetCornOriCoord(cc));
    WriteLn('EdgeOri: ', GetEdgeOriCoord(cc));
    WriteLn('UDSlice: ', GetUDSliceCoord(cc));
    WriteLn('CornParity: ', CornParityEven(cc));
    WriteLn('EdgeParity: ', EdgeParityEven(cc));
    fsCoord := ComputeFlipUDSliceSymCoord(cc);
    WriteLn('FlipUDSliceSymCoord: ', fsCoord);
    if fsCoord >= 0 then
    begin
      WriteLn('  classIdx: ', fsCoord shr 4);
      WriteLn('  sym: ', fsCoord and 15);
    end;
  end;

  result_str := SolveMin2Phase(facelets);
  WriteLn('Solution: ', result_str);
end.
