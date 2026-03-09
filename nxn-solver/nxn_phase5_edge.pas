unit nxn_phase5_edge;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nxn_cubedefs, nxn_facecube, nxn_util;

// Phase 1-4: Center solving orchestration (ported from main.pas / udthreaded.pas)
function SolvePhase1(fc: FaceletCube; out moveCount: integer): string;
function SolvePhase2(fc: FaceletCube; out moveCount: integer): string;
function SolvePhase3(fc: FaceletCube; out moveCount: integer): string;
function SolvePhase4(fc: FaceletCube; out moveCount: integer): string;

// Phase 5: Edge pairing using freeslice method
function SolvePhase5(fc: FaceletCube; out moveCount: integer): string;

implementation

uses nxn_phase1_tables, nxn_phase2_tables, nxn_phase3_tables, nxn_phase4_tables;

// Sequential version of the threaded Phase 1 oblique orbit solve
// Ported from makeUDCenter.Execute + SearchUDCenter in udthreaded.pas
procedure SearchUDCenterSeq(fc: FaceletCube; ccx, slx, ccy, sly, togo: integer);
var
  mv: moves;
  sc1: SymCoord32;
  syms: UInt8;
  n, altccx, altccy, altslx, altsly, key: integer;
  newccx, newccy, newslx, newsly: integer;
begin
  if togo = 10 then
  begin
    sc1 := UDCentCoordToSymCoord[ccx];
    syms := sc1.sym;
    n := 0;
    while (syms and (1 shl n)) = 0 do
      Inc(n);
    altccy := UDCentCoordSymTransform[ccy, n];
    altslx := UDBrick256CoordSymTransform[slx, n];
    key := (sc1.c_idx shl 8) + altslx;

    if UDStates10Table[key].used = 0 then
      Exit;
    if findLowerIndexUDStates10(key, altccy) <> -1 then
      Exit;

    sc1 := UDCentCoordToSymCoord[ccy];
    syms := sc1.sym;
    n := 0;
    while (syms and (1 shl n)) = 0 do
      Inc(n);
    altccx := UDCentCoordSymTransform[ccx, n];
    altsly := UDBrick256CoordSymTransform[sly, n];
    key := (sc1.c_idx shl 8) + altsly;

    if UDStates10Table[key].used = 0 then
      Exit;
    if findLowerIndexUDStates10(key, altccx) <> -1 then
      Exit;
  end;

  if ((UDCentBrick256Prun[B_24_8 * slx + ccx] > togo) or
    (UDCentBrick256Prun[B_24_8 * sly + ccy] > togo)) then
    Exit;

  if togo = 0 then
  begin
    fc.found := True;
  end
  else
  begin
    if stopProgram then
      Exit;

    mv := InitMove;
    while True do
    begin
      if fc.mvIdx = 0 then
        mv := nextMovePhase1[NoMove, mv]
      else
        mv := nextMovePhase1[fc.fxymoves[fc.mvIdx - 1], mv];
      if (mv < xU1) and not UDfaceMoveAllowed[slx, Ord(mv)] then
        continue;

      if mv = NoMove then
        Exit
      else
      begin
        case mv of
          fU1..fB3:
          begin
            newccx := UDCenterMove[ccx, Ord(mv)];
            newccy := UDCenterMove[ccy, Ord(mv)];
            newslx := slx;
            newsly := sly;
          end;
          xU1..xB3:
          begin
            newccx := UDCenterMove[ccx, Ord(mv)];
            newslx := UDBrick256Move[slx, Ord(mv)];
            newccy := UDCenterMove[ccy, Ord(mv) + 18];
            newsly := UDBrick256Move[sly, Ord(mv) + 18];
          end;
          yU1..yB3:
          begin
            newccx := UDCenterMove[ccx, Ord(mv)];
            newslx := UDBrick256Move[slx, Ord(mv)];
            newccy := UDCenterMove[ccy, Ord(mv) - 18];
            newsly := UDBrick256Move[sly, Ord(mv) - 18];
          end;
        end;

        fc.fxymoves[fc.mvIdx] := mv;
        Inc(fc.mvIdx);
        SearchUDCenterSeq(fc, newccx, newslx, newccy, newsly, togo - 1);

        if fc.found then
          Exit;
        Dec(fc.mvIdx);
      end;
    end;
  end;
end;

// Sequential oblique orbit solver for Phase 1
procedure SolveUDCenterObliqueSeq(fc: FaceletCube; x, y: integer);
var
  togo, idx: integer;
  tmpfc: FaceletCube;
begin
  tmpfc := FaceletCube.Create(fc);
  try
    togo := 0;
    tmpfc.found := False;
    for idx := Low(tmpfc.fxymoves) to High(tmpfc.fxymoves) do
      tmpfc.fxymoves[idx] := InitMove;

    while (tmpfc.found = False) and (togo <= 25) do
    begin
      tmpfc.mvIdx := 0;
      SearchUDCenterSeq(tmpfc, tmpfc.Phase1CenterCoord(x, y),
        tmpfc.Phase1Brick256Coord(x, y),
        tmpfc.Phase1CenterCoord(y, x),
        tmpfc.Phase1Brick256Coord(y, x), togo);
      Inc(togo);
    end;

    // Copy result back
    fc.mvIdx := tmpfc.mvIdx;
    for idx := 0 to tmpfc.mvIdx - 1 do
      fc.fxymoves[idx] := tmpfc.fxymoves[idx];
  finally
    tmpfc.Free;
  end;
end;

// ===== Phase 1: UD centers to UD faces =====
function SolvePhase1(fc: FaceletCube; out moveCount: integer): string;
var
  i, j, totalSteps, stepsDone: integer;
  phaseStart, lastReport: QWord;
begin
  moveCount := 0;
  fc.moveLog := '';

  // Create tables
  TSWriteLn('Phase 1: Loading UD center tables...');
  WriteProgress('Phase1', 'Loading/Creating', 0, 0);
  WriteLnVerbose('Creating Phase 1 tables...');
  createNextMovePhase1Table;
  createUDBrick256CoordSymTransTable;
  createUDFaceMoveAllowedTable;
  createUDCenterCoordToSymCoordTable;
  createUDCenterCoordSymTransTable;
  createUDCenterMoveTable;
  createUDBrick256MoveTable;
  createUDXCrossMoveTable;
  createDistanceTable;
  createUDPlusCross1PruningTable;
  createUDCentXBrick256CoordPruningTable;
  createUDXCrossPruningTable;
  // Oblique orbit distance table - needed for 7x7+ (takes hours to generate first time)
  if fc.size >= 7 then
    createUDCentersSlice10;
  WriteLnVerbose('Phase 1 tables created.');

  phaseStart := GetTickCount64;
  lastReport := phaseStart;

  // Count total steps: plus-cross + oblique pairs + x-cross
  totalSteps := 0;
  if Odd(fc.size) then
    Inc(totalSteps, fc.size div 2 - 1);  // plus-cross
  for i := fc.size div 2 - 2 downto 1 do
    for j := i + 1 to fc.size div 2 - 1 do
      Inc(totalSteps);  // oblique
  Inc(totalSteps, fc.size div 2 - 1);  // x-cross
  stepsDone := 0;

  // Plus-cross
  WriteLnVerbose('Phase 1 +cross:');
  if Odd(fc.size) then
    for i := 1 to fc.size div 2 - 1 do
    begin
      if fc.MakeUDPlusCross1(i) then
      begin
        Inc(moveCount, fc.mvIdx);
        fc.printMoves(i, fc.size div 2);
        fc.applyMoves(i, fc.size div 2);
      end;
      Inc(stepsDone);
      if (GetTickCount64 - lastReport >= 5000) then
      begin
        TSWriteLn(Format('Phase 1: UD centers +cross %d/%d, %d moves, %s elapsed',
          [stepsDone, totalSteps, moveCount, FormatMs(GetTickCount64 - phaseStart)]));
        lastReport := GetTickCount64;
      end;
    end;

  // Oblique orbits (sequential, no threads)
  WriteLnVerbose('Phase 1 oblique orbits:');
  for i := fc.size div 2 - 2 downto 1 do
    for j := i + 1 to fc.size div 2 - 1 do
    begin
      if stopProgram then
        Exit('');
      if (GetTickCount64 - lastReport >= 5000) then
      begin
        TSWriteLn(Format('Phase 1: UD centers oblique orbit (%d,%d), step %d/%d, %d moves, %s elapsed',
          [i, j, stepsDone, totalSteps, moveCount, FormatMs(GetTickCount64 - phaseStart)]));
        lastReport := GetTickCount64;
      end;
      SolveUDCenterObliqueSeq(fc, i, j);
      Inc(moveCount, fc.mvIdx);
      fc.printMoves(i, j);
      fc.applyMoves(i, j);
      Inc(stepsDone);
    end;

  // X-cross
  WriteLnVerbose('Phase 1 xcross:');
  for i := 1 to fc.size div 2 - 1 do
  begin
    if fc.MakeUDXCross(i) then
    begin
      Inc(moveCount, fc.mvIdx);
      fc.printMoves(i, i);
      fc.applyMoves(i, i);
    end;
    Inc(stepsDone);
    if (GetTickCount64 - lastReport >= 5000) then
    begin
      TSWriteLn(Format('Phase 1: UD centers x-cross %d/%d, %d moves, %s elapsed',
        [stepsDone, totalSteps, moveCount, FormatMs(GetTickCount64 - phaseStart)]));
      lastReport := GetTickCount64;
    end;
  end;

  Result := Trim(fc.moveLog);
end;

// ===== Phase 2: FB centers to FB faces =====
function SolvePhase2(fc: FaceletCube; out moveCount: integer): string;
var
  i, j, totalSteps, stepsDone: integer;
  phaseStart, lastReport: QWord;
begin
  moveCount := 0;
  fc.moveLog := '';

  TSWriteLn('Phase 2: Loading FB center tables...');
  WriteProgress('Phase2', 'Loading/Creating', 0, 0);
  WriteLnVerbose('Creating Phase 2 tables...');
  createNextMovePhase2Table;
  createFBCenterMoveTable;
  createFBSliceMoveTable;
  createFBFaceMoveAllowedTable;
  createFBPlusCrossPruningTable;
  createFBFullCenterSliceCoordPruningTable;
  createFBXCrossMoveTable;
  createFBXCrossPruningTable;
  WriteLnVerbose('Phase 2 tables created.');

  phaseStart := GetTickCount64;
  lastReport := phaseStart;
  totalSteps := 0;
  Inc(totalSteps, fc.size div 2 - 1);
  for i := fc.size div 2 - 2 downto 1 do
    for j := i + 1 to fc.size div 2 - 1 do
      Inc(totalSteps);
  Inc(totalSteps, fc.size div 2 - 1);
  stepsDone := 0;

  // Plus-cross
  WriteLnVerbose('Phase 2 +cross:');
  for i := 1 to fc.size div 2 - 1 do
  begin
    if fc.MakeFBPlusCross(i) then
    begin
      Inc(moveCount, fc.mvIdx);
      fc.printMoves(i, fc.size div 2);
      fc.applyMoves(i, fc.size div 2);
    end;
    Inc(stepsDone);
    if (GetTickCount64 - lastReport >= 5000) then
    begin
      TSWriteLn(Format('Phase 2: FB centers +cross %d/%d, %d moves, %s elapsed',
        [stepsDone, totalSteps, moveCount, FormatMs(GetTickCount64 - phaseStart)]));
      lastReport := GetTickCount64;
    end;
  end;

  // Oblique orbits
  WriteLnVerbose('Phase 2 oblique orbits:');
  for i := fc.size div 2 - 2 downto 1 do
    for j := i + 1 to fc.size div 2 - 1 do
    begin
      if (GetTickCount64 - lastReport >= 5000) then
      begin
        TSWriteLn(Format('Phase 2: FB centers oblique orbit (%d,%d), step %d/%d, %d moves, %s elapsed',
          [i, j, stepsDone, totalSteps, moveCount, FormatMs(GetTickCount64 - phaseStart)]));
        lastReport := GetTickCount64;
      end;
      if fc.MakeFBFullCenter(i, j) then
      begin
        Inc(moveCount, fc.mvIdx);
        fc.printMoves(i, j);
        fc.applyMoves(i, j);
      end;
      Inc(stepsDone);
    end;

  // X-cross
  WriteLnVerbose('Phase 2 xcross:');
  for i := 1 to fc.size div 2 - 1 do
  begin
    if fc.MakeFBXCross(i) then
    begin
      Inc(moveCount, fc.mvIdx);
      fc.printMoves(i, i);
      fc.applyMoves(i, i);
    end;
    Inc(stepsDone);
    if (GetTickCount64 - lastReport >= 5000) then
    begin
      TSWriteLn(Format('Phase 2: FB centers x-cross %d/%d, %d moves, %s elapsed',
        [stepsDone, totalSteps, moveCount, FormatMs(GetTickCount64 - phaseStart)]));
      lastReport := GetTickCount64;
    end;
  end;

  Result := Trim(fc.moveLog);
end;

// ===== Phase 3: RLFB centers to their faces =====
function SolvePhase3(fc: FaceletCube; out moveCount: integer): string;
var
  i, j, totalSteps, stepsDone: integer;
  phaseStart, lastReport: QWord;
begin
  moveCount := 0;
  fc.moveLog := '';

  TSWriteLn('Phase 3: Loading RLFB center tables...');
  WriteProgress('Phase3', 'Loading/Creating', 0, 0);
  WriteLnVerbose('Creating Phase 3 tables...');
  createNextMovePhase3Table;
  createPh3RLFBCenterMoveTable;
  createPh3RLFBXCrossMoveTable;
  createPh3Brick702CoordToSymCoordTable;
  createPh3RLFBCenterCoordSymTransTable;
  createPh3RLFBXCrossPruningTable;
  createPh3RLFBPlusCrossPruningTable;
  // Oblique pruning table needed for 7x7+ (size >= 7 means oblique pairs exist)
  if fc.size div 2 - 2 >= 1 then
    createPh3Brick702RLFBCentPruningTable;
  createDistanceTable;
  WriteLnVerbose('Phase 3 tables created.');

  phaseStart := GetTickCount64;
  lastReport := phaseStart;
  totalSteps := 0;
  Inc(totalSteps, fc.size div 2 - 1);
  for i := fc.size div 2 - 2 downto 1 do
    for j := i + 1 to fc.size div 2 - 1 do
      Inc(totalSteps);
  Inc(totalSteps, fc.size div 2 - 1);
  stepsDone := 0;

  // Plus-cross
  WriteLnVerbose('Phase 3 +cross:');
  for i := 1 to fc.size div 2 - 1 do
  begin
    if fc.MakePh3RLFBPlusCross(i) then
    begin
      Inc(moveCount, fc.mvIdx);
      fc.printMoves(i, fc.size div 2);
      fc.applyMoves(i, fc.size div 2);
    end;
    Inc(stepsDone);
    if (GetTickCount64 - lastReport >= 5000) then
    begin
      TSWriteLn(Format('Phase 3: RLFB centers +cross %d/%d, %d moves, %s elapsed',
        [stepsDone, totalSteps, moveCount, FormatMs(GetTickCount64 - phaseStart)]));
      lastReport := GetTickCount64;
    end;
  end;

  // Oblique orbits
  WriteLnVerbose('Phase 3 oblique orbits:');
  for i := fc.size div 2 - 2 downto 1 do
    for j := i + 1 to fc.size div 2 - 1 do
    begin
      if (GetTickCount64 - lastReport >= 5000) then
      begin
        TSWriteLn(Format('Phase 3: RLFB centers oblique orbit (%d,%d), step %d/%d, %d moves, %s elapsed',
          [i, j, stepsDone, totalSteps, moveCount, FormatMs(GetTickCount64 - phaseStart)]));
        lastReport := GetTickCount64;
      end;
      if fc.MakePh3Cent702(i, j) then
      begin
        Inc(moveCount, fc.mvIdx);
        fc.printMoves(i, j);
        fc.applyMoves(i, j);
      end;
      Inc(stepsDone);
    end;

  // X-cross
  WriteLnVerbose('Phase 3 xcross:');
  for i := 1 to fc.size div 2 - 1 do
  begin
    if fc.MakePh3XCross(i) then
    begin
      Inc(moveCount, fc.mvIdx);
      fc.printMoves(i, i);
      fc.applyMoves(i, i);
    end;
    Inc(stepsDone);
    if (GetTickCount64 - lastReport >= 5000) then
    begin
      TSWriteLn(Format('Phase 3: RLFB centers x-cross %d/%d, %d moves, %s elapsed',
        [stepsDone, totalSteps, moveCount, FormatMs(GetTickCount64 - phaseStart)]));
      lastReport := GetTickCount64;
    end;
  end;

  Result := Trim(fc.moveLog);
end;

// ===== Phase 4: UD centers to their faces =====
function SolvePhase4(fc: FaceletCube; out moveCount: integer): string;
var
  i, j, totalSteps, stepsDone: integer;
  phaseStart, lastReport: QWord;
begin
  moveCount := 0;
  fc.moveLog := '';

  TSWriteLn('Phase 4: Loading UD center tables...');
  WriteProgress('Phase4', 'Loading/Creating', 0, 0);
  WriteLnVerbose('Creating Phase 4 tables...');
  createNextMovePhase4Table;
  createPhase4RLFBBrickMoveTable;
  createPhase4UDBrickMoveTable;
  createPh4CenterMoveTable;
  createPhase4UDXCrossMoveTable;
  createPh4UDPlusCrossPruningTable;
  createPh4UDCentBrickPruningTable;
  createPh4UDXCrossPruningTable;
  WriteLnVerbose('Phase 4 tables created.');

  phaseStart := GetTickCount64;
  lastReport := phaseStart;
  totalSteps := 0;
  Inc(totalSteps, fc.size div 2 - 1);
  for i := fc.size div 2 - 2 downto 1 do
    for j := i + 1 to fc.size div 2 - 1 do
      Inc(totalSteps);
  Inc(totalSteps, fc.size div 2 - 1);
  stepsDone := 0;

  // Plus-cross
  WriteLnVerbose('Phase 4 +cross:');
  for i := 1 to fc.size div 2 - 1 do
  begin
    if fc.MakePh4UDPlusCross(i) then
    begin
      Inc(moveCount, fc.mvIdx);
      fc.printMoves(i, fc.size div 2);
      fc.applyMoves(i, fc.size div 2);
    end;
    Inc(stepsDone);
    if (GetTickCount64 - lastReport >= 5000) then
    begin
      TSWriteLn(Format('Phase 4: UD centers +cross %d/%d, %d moves, %s elapsed',
        [stepsDone, totalSteps, moveCount, FormatMs(GetTickCount64 - phaseStart)]));
      lastReport := GetTickCount64;
    end;
  end;

  // Oblique orbits
  WriteLnVerbose('Phase 4 oblique orbits:');
  for i := fc.size div 2 - 2 downto 1 do
    for j := i + 1 to fc.size div 2 - 1 do
    begin
      if (GetTickCount64 - lastReport >= 5000) then
      begin
        TSWriteLn(Format('Phase 4: UD centers oblique orbit (%d,%d), step %d/%d, %d moves, %s elapsed',
          [i, j, stepsDone, totalSteps, moveCount, FormatMs(GetTickCount64 - phaseStart)]));
        lastReport := GetTickCount64;
      end;
      if fc.MakePh4UDCenters(i, j) then
      begin
        Inc(moveCount, fc.mvIdx);
        fc.printMoves(i, j);
        fc.applyMoves(i, j);
      end;
      Inc(stepsDone);
    end;

  // X-cross
  WriteLnVerbose('Phase 4 xcross:');
  for i := 1 to fc.size div 2 - 1 do
  begin
    if fc.MakePh4XCross(i) then
    begin
      Inc(moveCount, fc.mvIdx);
      fc.printMoves(i, i);
      fc.applyMoves(i, i);
    end;
    Inc(stepsDone);
    if (GetTickCount64 - lastReport >= 5000) then
    begin
      TSWriteLn(Format('Phase 4: UD centers x-cross %d/%d, %d moves, %s elapsed',
        [stepsDone, totalSteps, moveCount, FormatMs(GetTickCount64 - phaseStart)]));
      lastReport := GetTickCount64;
    end;
  end;

  Result := Trim(fc.moveLog);
end;

// ===== Phase 5: Edge pairing using freeslice method =====
function CountTotalBadEdges(fc: FaceletCube): integer;
var
  i: integer;
  e: Edge;
begin
  Result := 0;
  for i := 1 to edgemx - 1 do
    for e := UR to BR do
    begin
      if fc.ecls[i, edgeidx[e, 0]] <> fc.ecls[edgemx, edgeidx[e, 0]] then
        Inc(Result);
      if fc.ecls[i, edgeidx[e, 1]] <> fc.ecls[edgemx, edgeidx[e, 1]] then
        Inc(Result);
    end;
end;

function SolvePhase5(fc: FaceletCube; out moveCount: integer): string;
var
  x, numOrbits: integer;
begin
  moveCount := 0;

  if fc.size <= 3 then
  begin
    Result := '';
    Exit;
  end;

  // For virtual (even→odd) cubes, the padding orbit (edgemx = mid) has face colors.
  // getEdgeCluster reads these as a valid identity permutation (each edge maps to
  // itself). Phase 5 pairs orbit 1 against this reference using face moves (which
  // affect all orbits equally) and inner moves at orbit 1 (which only affect orbit 1).
  // The zero-net-displacement constraint ensures inner moves cancel out, so orbit 3
  // (the other real wing orbit) ends up matching orbit 1.

  edgemx := fc.size div 2;
  SetLength(fc.ecls, edgemx + 1, 24);

  for x := 1 to edgemx do
    fc.getEdgeCluster(x);

  fc.ns := 0;
  fc.moveLog := '';

  WriteLnVerbose('Phase 5 - Freeslice to pair edges:');
  fc.fallbackAttempt := 0;

  numOrbits := edgemx - 1;
  fc.ph5TotalSlots := 12 * numOrbits * 2;  // 12 edges × orbits × 2 slots each
  fc.ph5InitialBad := CountTotalBadEdges(fc);
  fc.ph5StartTick := GetTickCount64;
  fc.ph5LastTick := fc.ph5StartTick;

  if fc.ph5InitialBad > 0 then
  begin
    TSWriteLn(Format('Phase 5: Pairing %d edge orbits (%d/%d slots need fixing)',
      [numOrbits, fc.ph5InitialBad, fc.ph5TotalSlots]));
  end;

  while fc.MakeFLEdge do
    ;  // progress is reported from inside MakeFLEdge

  moveCount := fc.ns;
  WriteLnVerbose(Format('Phase 5: %d moves', [moveCount]));

  Result := Trim(fc.moveLog);
end;

end.
