unit nxn_min2phase_search;
// IDA* two-phase search for 3x3 Rubik's Cube
// Uses full-depth pruning tables for exact depth bounds

{$mode objfpc}{$H+}

interface

uses nxn_min2phase_defs, nxn_min2phase_cubicube, nxn_min2phase_coord;

const
  MAX_DEPTH = 31;
  DEFAULT_MAX_LENGTH = 23;
  DEFAULT_TIME_LIMIT = 5000; // 5 seconds

var
  SearchStatusPrefix: string = '';  // Set to e.g. '3x3 phase: ' for NxN solves
  SearchSolutionPrefix: string = '';  // Phases 1-5 moves prepended to SOLUTION: output for NxN

function SolveSearch(const facelets: string; maxLength: integer = DEFAULT_MAX_LENGTH;
  timeLimitMs: integer = DEFAULT_TIME_LIMIT): string;

implementation

uses SysUtils, nxn_util;

type
  TSearchNode = record
    axis: integer;
    power: integer;
    flipSliceIdx: integer;
    flipSliceSym: integer;
    twist: integer;
    prun: integer;
    cornPerm: integer;
    udSliceSorted: integer;
    edge8Perm: integer;
  end;

function GetPhase1Depth(classIdx, twist, sym: integer): integer;
begin
  Result := PruningPhase1[Int64(classIdx) * N_TWIST + TwistConj[twist, sym]];
  if Result = 255 then Result := MAX_DEPTH;
end;

function GetPhase2Depth(cornPermRaw, edge8Perm: integer): integer;
var
  x, sym, symCIdx: integer;
begin
  x := CornPermRawToSym[cornPermRaw];
  sym := x and 15;
  symCIdx := x shr 4;
  Result := PruningPhase2[Int64(symCIdx) * N_UD_EDGES_PERM + Edge8PermConj[edge8Perm, sym]];
  if Result = 255 then Result := MAX_DEPTH;
end;

procedure ComputePhase2Coords(const cc: TCubieCube;
  out edge8Perm, udSliceSorted: integer);
var
  prodEdge: TEdgeCubie;
  ccConj: TCubieCube;
  rlSorted, fbSorted: integer;
begin
  udSliceSorted := GetUDSliceSortedCoord(cc);
  EdgeMult(EdgeSym[16], cc.edge, prodEdge);
  EdgeMult(prodEdge, EdgeSym[InvIdx[16]], ccConj.edge);
  rlSorted := GetUDSliceSortedCoord(ccConj);
  EdgeMult(EdgeSym[16], ccConj.edge, prodEdge);
  EdgeMult(prodEdge, EdgeSym[InvIdx[16]], ccConj.edge);
  fbSorted := GetUDSliceSortedCoord(ccConj);
  edge8Perm := GetEdge8Perm[rlSorted, fbSorted mod 24];
end;

function SolveSearch(const facelets: string; maxLength: integer; timeLimitMs: integer): string;
var
  cc, ccAfterP1, ccTmp: TCubieCube;
  n: array[0..MAX_DEPTH + 1] of TSearchNode;
  p2n: array[0..MAX_DEPTH + 1] of TSearchNode;
  depth1, r_depth1: integer;
  depth2, r_depth2, maxLen2: integer;
  np, np1: ^TSearchNode;
  p2np, p2np1: ^TSearchNode;
  m, x: integer;
  sym, symCIdx: integer;
  found: boolean;
  bestLen: integer;
  bestMoves: string;
  p2result: integer;
  i, mv, temp: integer;
  childDepth: integer;
  p1LastAxis: integer;
  p2e8, p2uds: integer;
  p1Len: integer;
  fsCoord: integer;
  startTick: QWord;
label turn1, incAxis1, checkAxis1, left1;
label p2start, p2turn, p2incPower, p2incAxis, p2checkAxis, p2left, p2done;

  function BuildSolutionString(d1, d2: integer): string;
  var
    si, smv: integer;
  begin
    Result := '';
    for si := 0 to d1 do
    begin
      if Result <> '' then Result := Result + ' ';
      smv := 3 * n[si].axis + n[si].power - 1;
      Result := Result + MoveNames[smv];
    end;
    for si := 0 to d2 do
    begin
      if Result <> '' then Result := Result + ' ';
      smv := 3 * p2n[si].axis + p2n[si].power - 1;
      Result := Result + MoveNames[smv];
    end;
  end;

begin
  Result := '';
  if not TablesReady then exit;

  startTick := GetTickCount64;

  if facelets = 'UUUUUUUUURRRRRRRRRFFFFFFFFFDDDDDDDDDLLLLLLLLLBBBBBBBBB' then
    exit;

  if not FaceletsToCubieCube(facelets, cc) then
  begin
    Result := '!ERROR: ' + FaceletParseError;
    exit;
  end;

  if CornParityEven(cc) <> EdgeParityEven(cc) then
  begin
    Result := '!ERROR: parity mismatch';
    exit;
  end;

  fsCoord := ComputeFlipUDSliceSymCoord(cc);
  if fsCoord < 0 then
  begin
    Result := '!ERROR: invalid flip/slice';
    exit;
  end;
  n[0].flipSliceIdx := fsCoord shr 4;
  n[0].flipSliceSym := fsCoord and 15;
  n[0].twist := GetCornOriCoord(cc);
  n[0].prun := GetPhase1Depth(n[0].flipSliceIdx, n[0].twist, n[0].flipSliceSym);
  n[0].cornPerm := GetCornPermCoord(cc);
  n[0].axis := 0;
  n[0].power := 0;

  bestLen := MAX_DEPTH + 1;  // Accept any length initially, improve within time limit
  bestMoves := '';
  found := false;

  // ====== Special case: cube already in G1 (phase 1 solved) ======
  if n[0].prun = 0 then
  begin
    ComputePhase2Coords(cc, p2e8, p2uds);

    if (n[0].cornPerm = 0) and (p2e8 = 0) and (p2uds = 0) then
      exit; // Already solved

    if p2e8 >= N_UD_EDGES_PERM then
    begin
      Result := '!ERROR: invalid edge8Perm';
      exit;
    end;

    p1Len := 0;
    p1LastAxis := -1;
    p2n[0].cornPerm := n[0].cornPerm;
    p2n[0].edge8Perm := p2e8;
    p2n[0].udSliceSorted := p2uds;
    p2n[0].prun := GetPhase2Depth(p2n[0].cornPerm, p2n[0].edge8Perm);
    p2n[0].axis := 0;
    p2n[0].power := 0;
    goto p2start;
  end;

  // ====== Phase 1 IDA* ======
  depth1 := 0;
  r_depth1 := 0;
  np := @n[0];

  while true do
  begin
    Inc(np^.power);
    if np^.power > 3 then goto incAxis1;

    if depth1 <> r_depth1 then
    begin
      np1 := np; Dec(np1);
      if np^.axis = np1^.axis then goto incAxis1;
      if (np^.axis <= 2) and (np^.axis + 3 = np1^.axis) then goto incAxis1;
    end;

turn1:
    np1 := np; Inc(np1);
    m := 3 * np^.axis + np^.power - 1;

    np1^.twist := TwistMove[np^.twist, m];
    x := FlipSliceMove[np^.flipSliceIdx, SymMoveTable[np^.flipSliceSym, m]];
    np1^.flipSliceSym := nxn_min2phase_coord.SymMult[x and 15, np^.flipSliceSym];
    np1^.flipSliceIdx := x shr 4;

    // Full-depth pruning: direct table lookup
    childDepth := GetPhase1Depth(np1^.flipSliceIdx, np1^.twist, np1^.flipSliceSym);
    np1^.prun := childDepth;

    // Prune: child needs more moves than remaining depth allows
    if childDepth > r_depth1 then continue;

    np1^.cornPerm := CornPermMove[np^.cornPerm, m];

    if r_depth1 = 0 then
    begin
      // ====== Enter Phase 2 ======
      p1Len := depth1 + 1;
      p1LastAxis := n[depth1].axis;

      // Pre-check with corner permutation
      temp := PruningCornPerm[n[p1Len].cornPerm];
      maxLen2 := bestLen - p1Len;
      if temp > maxLen2 + 2 then goto left1;
      if temp > maxLen2 + 1 then goto incAxis1;
      if temp > maxLen2 then continue;

      // Compute UDSliceSorted through phase 1 moves
      n[0].udSliceSorted := GetUDSliceSortedCoord(cc);
      for i := 0 to depth1 do
      begin
        mv := 3 * n[i].axis + n[i].power - 1;
        n[i + 1].udSliceSorted := UDSliceSortedMove[n[i].udSliceSorted, mv];
      end;

      // Compute Edge8Perm via cubie-level
      ccAfterP1 := cc;
      for i := 0 to depth1 do
      begin
        ccTmp := ccAfterP1;
        CornMult(ccTmp.corn, CornerCubieMove[n[i].axis], ccAfterP1.corn);
        EdgeMult(ccTmp.edge, EdgeCubieMove[n[i].axis], ccAfterP1.edge);
        if n[i].power >= 2 then
        begin
          ccTmp := ccAfterP1;
          CornMult(ccTmp.corn, CornerCubieMove[n[i].axis], ccAfterP1.corn);
          EdgeMult(ccTmp.edge, EdgeCubieMove[n[i].axis], ccAfterP1.edge);
        end;
        if n[i].power >= 3 then
        begin
          ccTmp := ccAfterP1;
          CornMult(ccTmp.corn, CornerCubieMove[n[i].axis], ccAfterP1.corn);
          EdgeMult(ccTmp.edge, EdgeCubieMove[n[i].axis], ccAfterP1.edge);
        end;
      end;

      ComputePhase2Coords(ccAfterP1, p2e8, p2uds);
      n[p1Len].edge8Perm := p2e8;

      if p2e8 >= N_UD_EDGES_PERM then continue;

      // Check if already solved
      if (p2e8 = 0) and (n[p1Len].udSliceSorted = 0) and (n[p1Len].cornPerm = 0) then
      begin
        bestLen := p1Len;
        bestMoves := BuildSolutionString(depth1, -1);
        found := true;
        if SearchSolutionPrefix <> '' then
          TSWriteLn(Format('SOLUTION: %s%d moves|%s %s', [SearchStatusPrefix, bestLen, SearchSolutionPrefix, bestMoves]))
        else
          TSWriteLn(Format('SOLUTION: %s%d moves|%s', [SearchStatusPrefix, bestLen, bestMoves]));
        Result := bestMoves;
        exit;
      end;

      childDepth := GetPhase2Depth(n[p1Len].cornPerm, n[p1Len].edge8Perm);
      if childDepth > maxLen2 then continue;

      p2n[0].cornPerm := n[p1Len].cornPerm;
      p2n[0].edge8Perm := n[p1Len].edge8Perm;
      p2n[0].udSliceSorted := n[p1Len].udSliceSorted;
      p2n[0].prun := childDepth;
      p2n[0].axis := 0;
      p2n[0].power := 0;

      // Fall through to phase 2 search
p2start:
      depth2 := 0;
      r_depth2 := 0;
      p2np := @p2n[0];

      // Phase 2 IDA* loop
      while true do
      begin
        Inc(p2np^.power);
        if (p2np^.axis <> 0) and (p2np^.axis <> 3) then
        begin
          if p2np^.power < 2 then p2np^.power := 2;
          if p2np^.power > 2 then goto p2incAxis;
        end;
        if p2np^.power > 3 then goto p2incAxis;

p2turn:
        if depth2 <> r_depth2 then
        begin
          p2np1 := p2np; Dec(p2np1);
          if p2np^.axis = p2np1^.axis then goto p2incAxis;
          if (p2np^.axis <= 2) and (p2np^.axis + 3 = p2np1^.axis) then goto p2incAxis;
        end;

        // Apply move
        p2np1 := p2np; Inc(p2np1);
        mv := 3 * p2np^.axis + p2np^.power - 1;

        p2np1^.cornPerm := CornPermMove[p2np^.cornPerm, mv];
        p2np1^.edge8Perm := Edge8PermMove[p2np^.edge8Perm, mv];

        if p2np1^.edge8Perm >= N_UD_EDGES_PERM then goto p2incAxis;

        // Phase 2 pruning: full-depth lookup
        childDepth := GetPhase2Depth(p2np1^.cornPerm, p2np1^.edge8Perm);
        p2np1^.prun := childDepth;

        if childDepth > r_depth2 then goto p2incPower;

        // Check if solved
        if r_depth2 = 0 then
        begin
          // Verify UDSliceSorted = 0
          temp := p2n[0].udSliceSorted;
          for i := 0 to depth2 do
          begin
            mv := 3 * p2n[i].axis + p2n[i].power - 1;
            temp := UDSliceSortedMove[temp, mv];
          end;
          if temp <> 0 then goto p2incPower;

          // Boundary check: first p2 move must not conflict with last p1 move
          if p1LastAxis >= 0 then
          begin
            if p2n[0].axis = p1LastAxis then goto p2incPower;
            if (p1LastAxis <= 2) and (p2n[0].axis = p1LastAxis + 3) then goto p2incPower;
          end;

          // Solution found!
          p2result := p1Len + depth2 + 1;
          if p2result < bestLen then
          begin
            bestLen := p2result;
            bestMoves := BuildSolutionString(p1Len - 1, depth2);
            found := true;
            if SearchSolutionPrefix <> '' then
            begin
              if bestLen <= maxLength then
                TSWriteLn(Format('SOLUTION: %s%d moves (target: %d)|%s %s', [SearchStatusPrefix, bestLen, maxLength, SearchSolutionPrefix, bestMoves]))
              else
                TSWriteLn(Format('SOLUTION: %s%d moves (searching for <= %d)|%s %s', [SearchStatusPrefix, bestLen, maxLength, SearchSolutionPrefix, bestMoves]));
            end
            else
            begin
              if bestLen <= maxLength then
                TSWriteLn(Format('SOLUTION: %s%d moves (target: %d)|%s', [SearchStatusPrefix, bestLen, maxLength, bestMoves]))
              else
                TSWriteLn(Format('SOLUTION: %s%d moves (searching for <= %d)|%s', [SearchStatusPrefix, bestLen, maxLength, bestMoves]));
            end;
          end;
          goto p2incPower;
        end;

        // Go deeper
        Dec(r_depth2);
        Inc(p2np);
        p2np^.axis := 0;
        goto p2checkAxis;

p2incPower:
        continue;

p2incAxis:
        if p2np^.axis >= 5 then goto p2left;
        Inc(p2np^.axis);
p2checkAxis:
        if depth2 <> r_depth2 then
        begin
          p2np1 := p2np; Dec(p2np1);
          if p2np^.axis = p2np1^.axis then goto p2incAxis;
          if (p2np^.axis <= 2) and (p2np^.axis + 3 = p2np1^.axis) then goto p2incAxis;
        end;
        p2np^.power := 0;
        continue;

p2left:
        if depth2 = r_depth2 then
        begin
          maxLen2 := bestLen - p1Len;
          if depth2 >= maxLen2 - 1 then goto p2done;
          Inc(depth2);
          Inc(r_depth2);
          p2np^.axis := 0;
          p2np^.power := 1;
          goto p2turn;
        end
        else
        begin
          Dec(p2np);
          Inc(r_depth2);
        end;
      end;
p2done:
      // End of phase 2 search

      // If this was the G1 special case (p1Len=0), we're done
      if p1Len = 0 then
      begin
        if found then Result := bestMoves
        else Result := '!ERROR: no solution found';
        exit;
      end;

      // Stop if: solution meets target length, OR time is up
      // When maxLength < DEFAULT_MAX_LENGTH, skip the depth1+3 heuristic to search harder
      if found and ((bestLen <= maxLength) or
        ((maxLength >= DEFAULT_MAX_LENGTH) and (bestLen <= depth1 + 3)) or
        ((timeLimitMs > 0) and (GetTickCount64 - startTick >= QWord(timeLimitMs)))) then
      begin
        Result := bestMoves;
        exit;
      end;

      continue; // back to phase 1 incPower
    end;

    // Go deeper in phase 1
    Dec(r_depth1);
    Inc(np);
    np^.axis := 0;
    goto checkAxis1;

incAxis1:
    if np^.axis >= 5 then goto left1;
    Inc(np^.axis);
checkAxis1:
    if depth1 <> r_depth1 then
    begin
      np1 := np; Dec(np1);
      if np^.axis = np1^.axis then goto incAxis1;
      if (np^.axis <= 2) and (np^.axis + 3 = np1^.axis) then goto incAxis1;
    end;
    np^.power := 0;
    continue;

left1:
    if depth1 = r_depth1 then
    begin
      if found then
      begin
        TSWriteLn(Format('SEARCH: %sbest so far %d moves, searching depth %d...', [SearchStatusPrefix, bestLen, depth1 + 1]));
      end;
      // Can't improve: phase1 depth already >= bestLen-1
      if depth1 >= bestLen - 1 then
      begin
        if found then Result := bestMoves
        else Result := '!ERROR: no solution found';
        exit;
      end;
      // Default mode: stop early once a solution is found
      if found and (maxLength >= DEFAULT_MAX_LENGTH) then
      begin
        Result := bestMoves;
        exit;
      end;
      // Quality mode: stop only when target met or time exceeded
      if found and (bestLen <= maxLength) then
      begin
        Result := bestMoves;
        exit;
      end;
      // Time limit: give up if searching too long
      if (timeLimitMs > 0) and (GetTickCount64 - startTick >= QWord(timeLimitMs * 2)) then
      begin
        if found then Result := bestMoves
        else Result := '!ERROR: time limit exceeded';
        exit;
      end;
      Inc(depth1);
      Inc(r_depth1);
      np^.axis := 0;
      np^.power := 1;
      goto turn1;
    end
    else
    begin
      Dec(np);
      Inc(r_depth1);
    end;
  end;
end;

end.
