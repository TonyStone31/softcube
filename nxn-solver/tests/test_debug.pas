program test_debug;
{$mode objfpc}{$H+}

uses SysUtils, nxn_min2phase_defs, nxn_min2phase_cubicube, nxn_min2phase_coord,
     nxn_min2phase_search, nxn_min2phase;

var
  cc: TCubieCube;
  ok: boolean;
  facelets, result_str: string;
  fsCoord, cpCoord, udsCoord: integer;
  sym, classIdx: integer;
  edge8: integer;
  prodEdge: TEdgeCubie;
  ccConj: TCubieCube;
  rlSorted, fbSorted: integer;
  prunVal: integer;
begin
  // Init tables
  Flush(Output); WriteLn('Initializing...');
  InitMin2Phase;
  Flush(Output); WriteLn('Tables ready.');

  // Test: U move
  Flush(Output); WriteLn('');
  Flush(Output); WriteLn('=== U move ===');
  CubieIdentity(cc);
  CubieMove(cc, 0); // U
  facelets := CubieCubeToFacelets(cc);
  Flush(Output); WriteLn('Facelets: ', facelets); Flush(Output);

  ok := FaceletsToCubieCube(facelets, cc);
  Flush(Output); WriteLn('Parse ok: ', ok); Flush(Output);

  // Phase 1 coords
  fsCoord := ComputeFlipUDSliceSymCoord(cc);
  Flush(Output); WriteLn('FlipSliceSymCoord: ', fsCoord);
  if fsCoord >= 0 then
  begin
    classIdx := fsCoord shr 4;
    sym := fsCoord and 15;
    Flush(Output); WriteLn('  classIdx=', classIdx, ' sym=', sym);
    Flush(Output); WriteLn('  twist=', GetCornOriCoord(cc));
    prunVal := GetPrun2Bit(PruningPhase1,
      Int64(classIdx) * N_TWIST + TwistConj[GetCornOriCoord(cc), sym]);
    Flush(Output); WriteLn('  Phase1 prun mod3=', prunVal);
  end;

  // Phase 2 coords
  Flush(Output); WriteLn('  cornPerm=', GetCornPermCoord(cc));
  udsCoord := GetUDSliceSortedCoord(cc);
  Flush(Output); WriteLn('  udSliceSorted=', udsCoord);

  // Edge8Perm via conjugation
  EdgeMult(EdgeSym[16], cc.edge, prodEdge);
  EdgeMult(prodEdge, EdgeSym[InvIdx[16]], ccConj.edge);
  rlSorted := GetUDSliceSortedCoord(ccConj);
  Flush(Output); WriteLn('  rlSliceSorted=', rlSorted);

  EdgeMult(EdgeSym[16], ccConj.edge, prodEdge);
  EdgeMult(prodEdge, EdgeSym[InvIdx[16]], ccConj.edge);
  fbSorted := GetUDSliceSortedCoord(ccConj);
  Flush(Output); WriteLn('  fbSliceSorted=', fbSorted);

  edge8 := GetEdge8Perm[rlSorted, fbSorted mod 24];
  Flush(Output); WriteLn('  edge8Perm=', edge8);

  // Phase 2 pruning
  cpCoord := CornPermRawToSym[GetCornPermCoord(cc)];
  Flush(Output); WriteLn('  cornPermSym: classIdx=', cpCoord shr 4, ' sym=', cpCoord and 15);
  if edge8 < N_UD_EDGES_PERM then
  begin
    prunVal := GetPrun2Bit(PruningPhase2,
      Int64(cpCoord shr 4) * N_UD_EDGES_PERM +
      Edge8PermConj[edge8, cpCoord and 15]);
    Flush(Output); WriteLn('  Phase2 prun mod3=', prunVal);
  end;

  // Actually solve
  Flush(Output); WriteLn('');
  Flush(Output); WriteLn('Solving U move...');
  result_str := SolveMin2Phase(facelets);
  Flush(Output); WriteLn('Result: [', result_str, ']');

  // Test: solved cube
  Flush(Output); WriteLn('');
  Flush(Output); WriteLn('=== Solved cube ===');
  facelets := 'UUUUUUUUURRRRRRRRRFFFFFFFFFDDDDDDDDDLLLLLLLLLBBBBBBBBB';
  result_str := SolveMin2Phase(facelets);
  Flush(Output); WriteLn('Result: [', result_str, ']');
end.
