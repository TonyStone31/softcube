program nxn_solver;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, nxn_cubedefs, nxn_util, nxn_facecube,
  nxn_phase1_tables, nxn_phase2_tables, nxn_phase3_tables, nxn_phase4_tables,
  nxn_phase5_edge, nxn_phase6_3x3,
  nxn_min2phase_defs, nxn_min2phase_cubicube, nxn_min2phase_coord,
  nxn_min2phase_search, nxn_min2phase;

{$I version.inc}

var
  cubeSize: integer;
  faceletStr: string;
  solverPath: string;
  tableDirArg: string;
  fc: FaceletCube;
  allMoves: string;
  startTime: TDateTime;
  elapsedMs: int64;
  i: integer;
  selfTest: boolean;
  generateTables: boolean;
  solveMaxLength: integer = 23;
  solveTimeLimitMs: integer = 5000;

// Verify the solution by applying moves to a fresh copy of the original cube
function VerifySolution(const origFaceStr: string; origSize: integer;
  const moveStr: string): boolean;
var
  vfc: FaceletCube;
  tokens: TStringList;
  token: string;
  k, numPrefix, p, reps: integer;
  faceChar: char;
  ax: Axis;
  sliceIdx: integer;
  face, row, col: integer;
begin
  Result := False;
  vfc := FaceletCube.Create(origSize);
  try
    if not ParseFaceletString(origFaceStr, origSize, vfc.faceCols) then
    begin
      WriteLn(StdErr, 'VERIFY: failed to parse original facelet string');
      Exit;
    end;

    tokens := TStringList.Create;
    try
      tokens.Delimiter := ' ';
      tokens.StrictDelimiter := True;
      tokens.DelimitedText := Trim(moveStr);

      for k := 0 to tokens.Count - 1 do
      begin
        token := Trim(tokens[k]);
        if token = '' then Continue;

        // Parse numeric prefix
        p := 1;
        numPrefix := 0;
        while (p <= Length(token)) and (token[p] in ['0'..'9']) do
        begin
          numPrefix := numPrefix * 10 + (Ord(token[p]) - Ord('0'));
          Inc(p);
        end;
        if p > Length(token) then
        begin
          WriteLn(StdErr, 'VERIFY: bad move token: ', token);
          Exit;
        end;

        // Parse face letter
        faceChar := UpCase(token[p]);
        Inc(p);

        case faceChar of
          'U': ax := U;
          'D': ax := D;
          'R': ax := R;
          'L': ax := L;
          'F': ax := F;
          'B': ax := B;
        else
          WriteLn(StdErr, 'VERIFY: bad face letter in: ', token);
          Exit;
        end;

        // Determine slice index
        if numPrefix >= 2 then
          sliceIdx := numPrefix - 1
        else
          sliceIdx := 0;

        // Parse direction: default CW (1 rep), ' = CCW (3 reps), 2 = 180 (2 reps)
        reps := 1;
        while p <= Length(token) do
        begin
          case token[p] of
            '2': reps := 2;
            '''', '`': reps := 3;
          end;
          Inc(p);
        end;

        // Apply the move
        for p := 1 to reps do
          vfc.move(ax, sliceIdx);

        // Debug: print state periodically
        if Verbose then
          WriteLn(StdErr, Format('VERIFY after move %d (%s): %s',
            [k, token, BuildFaceletString(origSize, vfc.faceCols)]));
      end;
    finally
      tokens.Free;
    end;

    // Check if solved: every facelet should match its face's center color
    for face := 0 to 5 do
      for row := 0 to origSize - 1 do
        for col := 0 to origSize - 1 do
          if vfc.faceCols[face, row, col] <> vfc.faceCols[face, origSize div 2, origSize div 2] then
          begin
            WriteLn(StdErr, Format('VERIFY FAILED: face %d pos [%d,%d] = %d, expected %d',
              [face, row, col, Ord(vfc.faceCols[face, row, col]),
               Ord(vfc.faceCols[face, origSize div 2, origSize div 2])]));
            WriteLn(StdErr, 'VERIFY: final state: ' + BuildFaceletString(origSize, vfc.faceCols));
            Exit;
          end;

    Result := True;
  finally
    vfc.Free;
  end;
end;

procedure PrintUsage;
begin
  WriteLn(StdErr, 'Usage: nxn_solver <size> <facelet_string> [options]');
  WriteLn(StdErr, '');
  WriteLn(StdErr, '  size:           Cube size (2, 3, 4, 5, ...)');
  WriteLn(StdErr, '  facelet_string: 6*N^2 chars in URFDLB order');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Options:');
  WriteLn(StdErr, '  --verbose             Print progress info to stderr');
  WriteLn(StdErr, '  --table-dir <path>    Directory for table files (default: data/ next to executable)');
  WriteLn(StdErr, '  --max-length <n>      Max solution length for 3x3 phase (default: 23)');
  WriteLn(StdErr, '  --time-limit <ms>     Time limit in ms for 3x3 solver (default: 5000)');
  WriteLn(StdErr, '');
  WriteLn(StdErr, 'Commands:');
  WriteLn(StdErr, '  --generate-tables <size>  Generate all tables for given cube size');
  WriteLn(StdErr, '  --check-tables <size>     Check which tables exist for given cube size');
  WriteLn(StdErr, '  --selftest                Run built-in tests');
  Halt(1);
end;

// Generate a scrambled facelet string by applying moves to a solved cube
function GenerateScramble(n: integer; const scrambleMoves: string): string;
var
  tmpFc: FaceletCube;
  tokens: TStringList;
  token: string;
  k, p, numPrefix, reps: integer;
  faceChar: char;
  ax: Axis;
  sliceIdx: integer;
begin
  tmpFc := FaceletCube.Create(n);
  try
    // Apply scramble moves
    tokens := TStringList.Create;
    try
      tokens.Delimiter := ' ';
      tokens.StrictDelimiter := True;
      tokens.DelimitedText := Trim(scrambleMoves);
      for k := 0 to tokens.Count - 1 do
      begin
        token := Trim(tokens[k]);
        if token = '' then Continue;
        p := 1;
        numPrefix := 0;
        while (p <= Length(token)) and (token[p] in ['0'..'9']) do
        begin
          numPrefix := numPrefix * 10 + (Ord(token[p]) - Ord('0'));
          Inc(p);
        end;
        if p > Length(token) then Continue;
        faceChar := UpCase(token[p]);
        Inc(p);
        case faceChar of
          'U': ax := U; 'D': ax := D; 'R': ax := R;
          'L': ax := L; 'F': ax := F; 'B': ax := B;
        else Continue;
        end;
        if numPrefix >= 2 then sliceIdx := numPrefix - 1
        else sliceIdx := 0;
        reps := 1;
        while p <= Length(token) do
        begin
          case token[p] of '2': reps := 2; '''', '`': reps := 3; end;
          Inc(p);
        end;
        for p := 1 to reps do
          tmpFc.move(ax, sliceIdx);
      end;
    finally
      tokens.Free;
    end;
    Result := BuildFaceletString(n, tmpFc.faceCols);
  finally
    tmpFc.Free;
  end;
end;

// Check which table files are needed for a given cube size and whether they exist
procedure CheckTablesForSize(n: integer);
var
  virtualSize: integer;
  allTables: array of string;
  tableDesc: array of string;
  tableSizes: array of int64;
  tableCount, idx: integer;
  totalNeeded, totalExisting: int64;
  missingCount: integer;
  fi: TSearchRec;

  procedure AddTable(const name, desc: string; approxBytes: int64);
  begin
    if idx >= tableCount then Exit;
    allTables[idx] := name;
    tableDesc[idx] := desc;
    tableSizes[idx] := approxBytes;
    Inc(idx);
  end;

begin
  if n <= 3 then
    virtualSize := n
  else if Odd(n) then
    virtualSize := n
  else
    virtualSize := n + 1;

  // Tables needed for all NxN (n >= 4):
  // Phase 1: UDCentTrans, UDCenterMove, UDCentBrick256Prun, UDCentersSlice10, UDXCrossMove, UDXCrossPrun
  // Phase 2: FBCenterMove, FBFullCenterSlicePrun
  // Phase 3: Ph3RLFBCenterMove, Ph3RLFBXCrossMove, Ph3RLFBXCrossPrun
  // Phase 3 oblique (6x6+): Ph3Brick702RLFBCentPrun
  // Phase 4: Ph4UDCentBrickPrun
  // min2phase: prun_phase1_v4.dat, prun_phase2_v4.dat (in ~/.softcube/tables/)
  tableCount := 15;
  SetLength(allTables, tableCount);
  SetLength(tableDesc, tableCount);
  SetLength(tableSizes, tableCount);
  idx := 0;

  if n >= 4 then
  begin
    AddTable('UDCentTrans', 'Phase 1 sym transform', 23 * 1024 * 1024);
    AddTable('UDCenterMove', 'Phase 1 center move', 152 * 1024 * 1024);
    AddTable('UDCentBrick256Prun', 'Phase 1 pruning', 180 * 1024 * 1024);
    AddTable('UDXCrossMove', 'Phase 1 x-cross move', 102 * 1024 * 1024);
    AddTable('UDXCrossPrun', 'Phase 1 x-cross pruning', 12 * 1024 * 1024);
    AddTable('FBCenterMove', 'Phase 2 center move', 3 * 1024 * 1024);
    AddTable('FBFullCenterSlicePrun', 'Phase 2 pruning', Int64(2560) * 1024 * 1024);
    AddTable('Ph3RLFBCenterMove', 'Phase 3 center move', 517 * 1024);
    AddTable('Ph3RLFBXCrossMove', 'Phase 3 x-cross move', 345 * 1024);
    AddTable('Ph3RLFBXCrossPrun', 'Phase 3 x-cross pruning', 23 * 1024 * 1024);
    AddTable('Ph4UDCentBrickPrun', 'Phase 4 pruning', 23 * 1024 * 1024);
  end;

  // Tables needed for 7x7+ (virtual size >= 7 has oblique orbit pairs)
  if virtualSize >= 7 then
  begin
    AddTable('UDCentersSlice10', 'Phase 1 oblique distance', Int64(10) * 1024 * 1024 * 1024);
    AddTable('Ph3Brick702RLFBCentPrun', 'Phase 3 oblique pruning', Int64(3900) * 1024 * 1024);
  end;

  tableCount := idx; // actual count

  totalNeeded := 0;
  totalExisting := 0;
  missingCount := 0;

  WriteLn(Format('Tables needed for %dx%d cube:', [n, n]));
  WriteLn('');

  for idx := 0 to tableCount - 1 do
  begin
    totalNeeded := totalNeeded + tableSizes[idx];
    if FileExists(TableDir + allTables[idx]) then
    begin
      // Get actual file size
      if FindFirst(TableDir + allTables[idx], faAnyFile, fi) = 0 then
      begin
        totalExisting := totalExisting + fi.Size;
        WriteLn(Format('  [OK]      %-30s  %s', [allTables[idx], tableDesc[idx]]));
        FindClose(fi);
      end;
    end
    else
    begin
      Inc(missingCount);
      WriteLn(Format('  [MISSING] %-30s  %s  (~%.1f GB)',
        [allTables[idx], tableDesc[idx], tableSizes[idx] / (1024.0 * 1024 * 1024)]));
    end;
  end;

  WriteLn('');
  if missingCount = 0 then
    WriteLn('All tables present. Ready to solve.')
  else
  begin
    WriteLn(Format('%d table(s) missing.', [missingCount]));
    WriteLn(Format('Run: ./nxn_solver --generate-tables %d', [n]));
    WriteLn('Note: Large tables (3+ GB) may take hours to generate.');
  end;
end;

procedure GenerateTablesForSize(n: integer);
var
  tmpFc, tmpFcEven: FaceletCube;
  phaseMoves: string;
  phaseLength: integer;
begin
  Verbose := True;
  WriteLnVerbose(Format('Generating all tables needed for %dx%d cubes...', [n, n]));
  WriteLnVerbose(Format('Table directory: %s', [TableDir]));

  // Create a solved cube of the right size
  if not Odd(n) then
  begin
    tmpFcEven := FaceletCube.Create(n);
    try
      tmpFc := FaceletCube.Create(tmpFcEven); // creates virtual (n+1) cube
    finally
      tmpFcEven.Free;
    end;
  end
  else
    tmpFc := FaceletCube.Create(n);

  try
    WriteLnVerbose('=== Phase 1 tables ===');
    phaseMoves := SolvePhase1(tmpFc, phaseLength);
    WriteLnVerbose('Phase 1 tables: OK');

    WriteLnVerbose('=== Phase 2 tables ===');
    phaseMoves := SolvePhase2(tmpFc, phaseLength);
    WriteLnVerbose('Phase 2 tables: OK');

    WriteLnVerbose('=== Phase 3 tables ===');
    phaseMoves := SolvePhase3(tmpFc, phaseLength);
    WriteLnVerbose('Phase 3 tables: OK');

    WriteLnVerbose('=== Phase 4 tables ===');
    phaseMoves := SolvePhase4(tmpFc, phaseLength);
    WriteLnVerbose('Phase 4 tables: OK');

    WriteLnVerbose('=== min2phase (3x3) tables ===');
    InitMin2Phase;
    WriteLnVerbose('min2phase tables: OK');

    WriteLnVerbose('');
    WriteLnVerbose(Format('All tables for %dx%d generated successfully!', [n, n]));
  finally
    tmpFc.Free;
  end;
end;

procedure ParseArgs;
begin
  selfTest := False;
  generateTables := False;
  solverPath := '';
  tableDirArg := '';
  Verbose := False;

  // --gen-scramble <size> <moves> : output scrambled facelet string (for testing)
  if (ParamCount >= 3) and (ParamStr(1) = '--gen-scramble') then
  begin
    WriteLn(GenerateScramble(StrToInt(ParamStr(2)), ParamStr(3)));
    Halt(0);
  end;

  if (ParamCount >= 1) and (ParamStr(1) = '--selftest') then
  begin
    selfTest := True;
    // Parse remaining args
    i := 2;
    while i <= ParamCount do
    begin
      if ParamStr(i) = '--table-dir' then
      begin
        Inc(i);
        if i > ParamCount then PrintUsage;
        tableDirArg := ParamStr(i);
      end
      else if ParamStr(i) = '--verbose' then
        Verbose := True;
      Inc(i);
    end;
    Exit;
  end;

  if (ParamCount >= 2) and (ParamStr(1) = '--check-tables') then
  begin
    cubeSize := StrToIntDef(ParamStr(2), 0);
    if cubeSize < 4 then
    begin
      WriteLn(StdErr, 'Error: --check-tables requires cube size >= 4');
      Halt(1);
    end;
    // Check for --table-dir
    i := 3;
    while i <= ParamCount do
    begin
      if ParamStr(i) = '--table-dir' then
      begin
        Inc(i);
        if i <= ParamCount then tableDirArg := ParamStr(i);
      end;
      Inc(i);
    end;
    InitTableDir(tableDirArg);
    CheckTablesForSize(cubeSize);
    Halt(0);
  end;

  if (ParamCount >= 2) and (ParamStr(1) = '--generate-tables') then
  begin
    generateTables := True;
    cubeSize := StrToIntDef(ParamStr(2), 0);
    if cubeSize < 4 then
    begin
      WriteLn(StdErr, 'Error: --generate-tables requires cube size >= 4');
      Halt(1);
    end;
    // Check for --table-dir
    i := 3;
    while i <= ParamCount do
    begin
      if ParamStr(i) = '--table-dir' then
      begin
        Inc(i);
        if i <= ParamCount then tableDirArg := ParamStr(i);
      end;
      Inc(i);
    end;
    Verbose := True;
    Exit;
  end;

  if ParamCount < 2 then
    PrintUsage;

  cubeSize := StrToIntDef(ParamStr(1), 0);
  if cubeSize < 2 then
  begin
    WriteLn(StdErr, 'Error: cube size must be >= 2');
    Halt(1);
  end;

  faceletStr := ParamStr(2);
  if Length(faceletStr) <> 6 * cubeSize * cubeSize then
  begin
    WriteLn(StdErr, Format('Error: facelet string must be %d chars for a %dx%d cube, got %d',
      [6 * cubeSize * cubeSize, cubeSize, cubeSize, Length(faceletStr)]));
    Halt(1);
  end;

  i := 3;
  while i <= ParamCount do
  begin
    if ParamStr(i) = '--table-dir' then
    begin
      Inc(i);
      if i > ParamCount then
        PrintUsage;
      tableDirArg := ParamStr(i);
    end
    else if ParamStr(i) = '--max-length' then
    begin
      Inc(i);
      if i > ParamCount then
        PrintUsage;
      solveMaxLength := StrToIntDef(ParamStr(i), 23);
    end
    else if ParamStr(i) = '--time-limit' then
    begin
      Inc(i);
      if i > ParamCount then
        PrintUsage;
      solveTimeLimitMs := StrToIntDef(ParamStr(i), 5000);
    end
    else if ParamStr(i) = '--verbose' then
      Verbose := True
    else
    begin
      WriteLn(StdErr, 'Error: unknown option: ', ParamStr(i));
      PrintUsage;
    end;
    Inc(i);
  end;
end;

function Build3x3From2x2(const def2x2: string): string;
const
  FaceLetters: array[0..5] of Char = ('U', 'R', 'F', 'D', 'L', 'B');
var
  f, src, dst: Integer;
  fl: Char;
begin
  SetLength(Result, 54);
  for f := 0 to 5 do
  begin
    src := f * 4 + 1;
    dst := f * 9 + 1;
    fl := FaceLetters[f];
    Result[dst + 0] := def2x2[src + 0];
    Result[dst + 1] := fl;
    Result[dst + 2] := def2x2[src + 1];
    Result[dst + 3] := fl;
    Result[dst + 4] := fl;
    Result[dst + 5] := fl;
    Result[dst + 6] := def2x2[src + 2];
    Result[dst + 7] := fl;
    Result[dst + 8] := def2x2[src + 3];
  end;
end;

function Solve3x3: string;
begin
  Result := CallMin2Phase(faceletStr, solverPath, solveMaxLength, solveTimeLimitMs);
end;

function SolveNxN: string;
var
  totalMoves, phaseMoves: string;
  totalLength, phaseLength: integer;
  phaseStartMs: QWord;
  faceletStr3x3: string;
  fcEven: FaceletCube;
  p6tokens: TStringList;
  p6token: string;
  p6k, p6p, p6numPrefix, p6reps: integer;
  p6faceChar: char;
  p6ax: Axis;
  p6sliceIdx: integer;
  p6solved: boolean;
  p6face, p6row, p6col: integer;
begin
  totalMoves := '';
  totalLength := 0;

  // Create cube from facelet string
  // For even cubes, first parse into actual size, then convert to virtual (size+1)
  if not Odd(cubeSize) then
  begin
    fcEven := FaceletCube.Create(cubeSize);
    try
      if not ParseFaceletString(faceletStr, cubeSize, fcEven.faceCols) then
      begin
        WriteLn(StdErr, 'Error: invalid facelet string');
        Halt(1);
      end;
      // Copy constructor creates virtual (size+1) cube for even sizes
      fc := FaceletCube.Create(fcEven);
      WriteLnVerbose(Format('Even cube %dx%d -> virtual %dx%d',
        [cubeSize, cubeSize, fc.size, fc.size]));
    finally
      fcEven.Free;
    end;
  end
  else
  begin
    fc := FaceletCube.Create(cubeSize);
    if not ParseFaceletString(faceletStr, cubeSize, fc.faceCols) then
    begin
      WriteLn(StdErr, 'Error: invalid facelet string');
      Halt(1);
    end;
  end;

  try

    WriteLnVerbose('STATE_INITIAL: ' + BuildFaceletString(fc.size, fc.faceCols));

    // Phase 1: UD centers to UD faces
    phaseStartMs := GetTickCount64;
    WriteLnVerbose('=== Phase 1: UD centers to UD faces ===');
    phaseMoves := SolvePhase1(fc, phaseLength);
    Inc(totalLength, phaseLength);
    if phaseMoves <> '' then
    begin
      if totalMoves <> '' then
        totalMoves := totalMoves + ' ';
      totalMoves := totalMoves + phaseMoves;
    end;
    WriteLn(StdErr, Format('PHASE: 1 UD centers - %d moves in %s', [phaseLength, FormatMs(GetTickCount64 - phaseStartMs)]));
    Flush(StdErr);
    WriteLnVerbose(Format('Phase 1: %d moves', [phaseLength]));
    WriteLnVerbose('STATE_AFTER_P1: ' + BuildFaceletString(fc.size, fc.faceCols));
    WriteLnVerbose(Format('P1 coord check: plus(%d,%d)=%d, xcross(%d,%d)=%d',
      [1, fc.size div 2, fc.Phase1CenterCoord(1, fc.size div 2),
       1, 1, fc.Phase1CenterCoord(1, 1)]));

    // Phase 2: RLFB centers to RLFB faces (FB partition)
    phaseStartMs := GetTickCount64;
    WriteLnVerbose('=== Phase 2: FB centers to FB faces ===');
    phaseMoves := SolvePhase2(fc, phaseLength);
    Inc(totalLength, phaseLength);
    if phaseMoves <> '' then
    begin
      if totalMoves <> '' then
        totalMoves := totalMoves + ' ';
      totalMoves := totalMoves + phaseMoves;
    end;
    WriteLn(StdErr, Format('PHASE: 2 FB centers - %d moves in %s', [phaseLength, FormatMs(GetTickCount64 - phaseStartMs)]));
    Flush(StdErr);
    WriteLnVerbose(Format('Phase 2: %d moves', [phaseLength]));
    WriteLnVerbose('STATE_AFTER_P2: ' + BuildFaceletString(fc.size, fc.faceCols));
    WriteLnVerbose(Format('P1 coord after P2: plus=%d, xcross=%d',
      [fc.Phase1CenterCoord(1, fc.size div 2), fc.Phase1CenterCoord(1, 1)]));

    // Phase 3: RLFB centers fully solved
    phaseStartMs := GetTickCount64;
    WriteLnVerbose('=== Phase 3: RLFB centers to their faces ===');
    phaseMoves := SolvePhase3(fc, phaseLength);
    Inc(totalLength, phaseLength);
    if phaseMoves <> '' then
    begin
      if totalMoves <> '' then
        totalMoves := totalMoves + ' ';
      totalMoves := totalMoves + phaseMoves;
    end;
    WriteLn(StdErr, Format('PHASE: 3 RLFB centers - %d moves in %s', [phaseLength, FormatMs(GetTickCount64 - phaseStartMs)]));
    Flush(StdErr);
    WriteLnVerbose(Format('Phase 3: %d moves', [phaseLength]));
    WriteLnVerbose('STATE_AFTER_P3: ' + BuildFaceletString(fc.size, fc.faceCols));
    WriteLnVerbose(Format('P1 coord after P3: plus=%d, xcross=%d',
      [fc.Phase1CenterCoord(1, fc.size div 2), fc.Phase1CenterCoord(1, 1)]));

    // Phase 4: UD centers fully solved
    phaseStartMs := GetTickCount64;
    WriteLnVerbose('=== Phase 4: UD centers to their faces ===');
    phaseMoves := SolvePhase4(fc, phaseLength);
    Inc(totalLength, phaseLength);
    if phaseMoves <> '' then
    begin
      if totalMoves <> '' then
        totalMoves := totalMoves + ' ';
      totalMoves := totalMoves + phaseMoves;
    end;
    WriteLn(StdErr, Format('PHASE: 4 UD centers - %d moves in %s', [phaseLength, FormatMs(GetTickCount64 - phaseStartMs)]));
    Flush(StdErr);
    WriteLnVerbose(Format('Phase 4: %d moves', [phaseLength]));
    WriteLnVerbose('STATE_AFTER_P4: ' + BuildFaceletString(fc.size, fc.faceCols));
    WriteLnVerbose(Format('P1 coord after P4: plus=%d, xcross=%d',
      [fc.Phase1CenterCoord(1, fc.size div 2), fc.Phase1CenterCoord(1, 1)]));
    // Dump actual center colors at orbit (1,1)
    phaseMoves := 'Centers at (1,1): ';
    for i := 0 to 23 do
      phaseMoves := phaseMoves + ColorIndexToChar(fc.clusterColorIndex(1, 1, i));
    WriteLnVerbose(phaseMoves);
    WriteLnVerbose(Format('Total center moves: %d', [totalLength]));

    // Phase 5: Edge pairing
    phaseStartMs := GetTickCount64;
    WriteLnVerbose('=== Phase 5: Edge pairing ===');
    phaseMoves := SolvePhase5(fc, phaseLength);
    Inc(totalLength, phaseLength);
    if phaseMoves <> '' then
    begin
      if totalMoves <> '' then
        totalMoves := totalMoves + ' ';
      totalMoves := totalMoves + phaseMoves;
    end;
    WriteLn(StdErr, Format('PHASE: 5 Edge pairing - %d moves in %s', [phaseLength, FormatMs(GetTickCount64 - phaseStartMs)]));
    Flush(StdErr);
    WriteLnVerbose(Format('Phase 5: %d moves', [phaseLength]));
    WriteLnVerbose('STATE_AFTER_P5: ' + BuildFaceletString(fc.size, fc.faceCols));

    // Phase 5 uses zero-net-displacement constraint in ImprovePosition, so no
    // displacement fix is needed between Phase 5 and Phase 6.

    // Phase 6: 3x3 reduction solve
    phaseStartMs := GetTickCount64;
    SearchStatusPrefix := '3x3 phase: ';
    // Set solution prefix so SOLUTION: lines include phases 1-5 moves
    SearchSolutionPrefix := Trim(totalMoves);
    WriteLnVerbose('=== Phase 6: 3x3 reduction solve ===');
    faceletStr3x3 := Extract3x3State(fc);
    WriteLnVerbose('3x3 state: ' + faceletStr3x3);
    phaseMoves := CallMin2Phase(faceletStr3x3, solverPath, solveMaxLength, solveTimeLimitMs);

    // Handle even-cube PLL parity: if 3x3 solve fails, apply parity fix and retry
    if (phaseMoves = '!ERROR') and not Odd(cubeSize) then
    begin
      WriteLnVerbose('Parity detected on even cube - applying PLL parity fix');
      ApplyParityFix(fc);
      // Add parity fix moves to the solution
      Inc(totalLength, 7); // r2 U2 r2 Uw2 r2 u2 = 7 moves
      if totalMoves <> '' then
        totalMoves := totalMoves + ' ';
      totalMoves := totalMoves + '2R2 U2 2R2 U2 2U2 2R2 2U2';
      // Update solution prefix with parity fix moves
      SearchSolutionPrefix := Trim(totalMoves);

      // Re-extract 3x3 state and solve again
      faceletStr3x3 := Extract3x3State(fc);
      WriteLnVerbose('3x3 state after parity fix: ' + faceletStr3x3);
      phaseMoves := CallMin2Phase(faceletStr3x3, solverPath, solveMaxLength, solveTimeLimitMs);
    end;

    if phaseMoves = '!ERROR' then
    begin
      WriteLn(StdErr, 'Error: 3x3 reduction solve failed (invalid state)');
      WriteLn(StdErr, '3x3 state: ' + faceletStr3x3);
      Halt(3);
    end;

    // Apply Phase 6 moves to the virtual 5x5 fc and verify it's solved
    if Trim(phaseMoves) <> '' then
    begin
      p6tokens := TStringList.Create;
      try
        p6tokens.Delimiter := ' ';
        p6tokens.StrictDelimiter := True;
        p6tokens.DelimitedText := Trim(phaseMoves);
        for p6k := 0 to p6tokens.Count - 1 do
        begin
          p6token := Trim(p6tokens[p6k]);
          if p6token = '' then Continue;
          p6p := 1;
          p6numPrefix := 0;
          while (p6p <= Length(p6token)) and (p6token[p6p] in ['0'..'9']) do
          begin
            p6numPrefix := p6numPrefix * 10 + (Ord(p6token[p6p]) - Ord('0'));
            Inc(p6p);
          end;
          if p6p > Length(p6token) then Continue;
          p6faceChar := UpCase(p6token[p6p]);
          Inc(p6p);
          case p6faceChar of
            'U': p6ax := U; 'D': p6ax := D; 'R': p6ax := R;
            'L': p6ax := L; 'F': p6ax := F; 'B': p6ax := B;
          else Continue;
          end;
          if p6numPrefix >= 2 then p6sliceIdx := p6numPrefix - 1
          else p6sliceIdx := 0;
          p6reps := 1;
          while p6p <= Length(p6token) do
          begin
            case p6token[p6p] of '2': p6reps := 2; '''', '`': p6reps := 3; end;
            Inc(p6p);
          end;
          for p6p := 1 to p6reps do
            fc.move(p6ax, p6sliceIdx);
        end;
      finally
        p6tokens.Free;
      end;

      // Verify the virtual NxN is solved (skip padding orbit for even→virtual cubes)
      WriteLnVerbose('STATE_AFTER_P6: ' + BuildFaceletString(fc.size, fc.faceCols));
      p6solved := True;
      for p6face := 0 to 5 do
        for p6row := 0 to fc.size - 1 do
          for p6col := 0 to fc.size - 1 do
          begin
            // Skip padding orbit row/col for virtual cubes from even source
            if not Odd(cubeSize) then
              if (p6row = fc.size div 2) or (p6col = fc.size div 2) then
                Continue;
            if fc.faceCols[p6face, p6row, p6col] <> ColorIndex(p6face) then
            begin
              WriteLn(StdErr, Format('5x5 VERIFY FAIL: face %d [%d,%d] = %d, expected %d',
                [p6face, p6row, p6col, Ord(fc.faceCols[p6face, p6row, p6col]), p6face]));
              p6solved := False;
            end;
          end;
      if p6solved then
        WriteLn(StdErr, 'Virtual ' + IntToStr(fc.size) + 'x' + IntToStr(fc.size) + ' VERIFY: Phase 6 solved correctly')
      else
        WriteLn(StdErr, 'Virtual ' + IntToStr(fc.size) + 'x' + IntToStr(fc.size) + ' VERIFY: Phase 6 FAILED');
    end;

    // Count moves in phase 6
    phaseLength := 0;
    if Trim(phaseMoves) <> '' then
      for i := 1 to Length(phaseMoves) do
        if (phaseMoves[i] = ' ') then
          Inc(phaseLength);
    if Trim(phaseMoves) <> '' then
      Inc(phaseLength); // count last move too
    Inc(totalLength, phaseLength);
    if Trim(phaseMoves) <> '' then
    begin
      if totalMoves <> '' then
        totalMoves := totalMoves + ' ';
      totalMoves := totalMoves + Trim(phaseMoves);
    end;
    WriteLn(StdErr, Format('PHASE: 6 3x3 solve - %d moves in %s', [phaseLength, FormatMs(GetTickCount64 - phaseStartMs)]));
    Flush(StdErr);
    WriteLnVerbose(Format('Phase 6: %d moves', [phaseLength]));

    WriteLnVerbose(Format('Total moves: %d', [totalLength]));
  finally
    fc.Free;
  end;

  Result := totalMoves;
end;

procedure RunSelfTest;
type
  TTestCase = record
    size: integer;
    scramble: string;
    name: string;
  end;
var
  tests: array of TTestCase;
  t: integer;
  passed, failed: integer;
begin
  SetLength(tests, 6);
  tests[0].size := 4; tests[0].scramble := 'R'; tests[0].name := '4x4 R';
  tests[1].size := 4; tests[1].scramble := 'R U'; tests[1].name := '4x4 R U';
  tests[2].size := 4; tests[2].scramble := 'R U F'; tests[2].name := '4x4 R U F';
  tests[3].size := 5; tests[3].scramble := 'R'; tests[3].name := '5x5 R';
  tests[4].size := 5; tests[4].scramble := 'R U F'; tests[4].name := '5x5 R U F';
  tests[5].size := 4; tests[5].scramble := 'R 2R U 2U F'; tests[5].name := '4x4 R 2R U 2U F';

  passed := 0;
  failed := 0;

  for t := 0 to High(tests) do
  begin
    cubeSize := tests[t].size;
    faceletStr := GenerateScramble(cubeSize, tests[t].scramble);

    WriteLn(StdErr, '');
    WriteLn(StdErr, Format('=== Test %d: %s (scramble: %s) ===', [t + 1, tests[t].name, tests[t].scramble]));
    WriteLn(StdErr, 'Facelet: ' + faceletStr);

    startTime := Now;
    try
      allMoves := SolveNxN;
      elapsedMs := Round((Now - startTime) * 24 * 60 * 60 * 1000);

      WriteLn(StdErr, Format('Solution (%d ms): %s', [elapsedMs, allMoves]));

      if allMoves = '' then
      begin
        WriteLn(StdErr, 'PASS (already solved)');
        Inc(passed);
      end
      else if VerifySolution(faceletStr, cubeSize, allMoves) then
      begin
        WriteLn(StdErr, 'PASS');
        Inc(passed);
      end
      else
      begin
        WriteLn(StdErr, 'FAIL (verification failed)');
        Inc(failed);
      end;
    except
      on E: Exception do
      begin
        WriteLn(StdErr, 'FAIL (exception: ' + E.Message + ')');
        Inc(failed);
      end;
    end;
  end;

  WriteLn(StdErr, '');
  WriteLn(StdErr, Format('Results: %d passed, %d failed, %d total', [passed, failed, passed + failed]));
  if failed > 0 then
    Halt(1);
end;

begin
  ParseArgs;
  InitTableDir(tableDirArg);

  if selfTest then
  begin
    RunSelfTest;
    Exit;
  end;

  if generateTables then
  begin
    GenerateTablesForSize(cubeSize);
    Exit;
  end;

  WriteLn(StdErr, Format('NxN Solver v%s (build %d) - solving %dx%d cube',
    [SOLVER_VERSION, SOLVER_BUILD, cubeSize, cubeSize]));
  Flush(StdErr);

  startTime := Now;

  if cubeSize = 2 then
  begin
    faceletStr := Build3x3From2x2(faceletStr);
    cubeSize := 3;
    allMoves := Solve3x3;
  end
  else if cubeSize = 3 then
    allMoves := Solve3x3
  else
    allMoves := SolveNxN;

  elapsedMs := Round((Now - startTime) * 24 * 60 * 60 * 1000);

  // Verify solution by applying moves to a fresh copy of the original cube
  if (cubeSize > 3) and (allMoves <> '') then
  begin
    if VerifySolution(faceletStr, cubeSize, allMoves) then
      WriteLn(StdErr, 'VERIFY: Solution verified OK on ' + IntToStr(cubeSize) + 'x' + IntToStr(cubeSize))
    else
    begin
      WriteLn(StdErr, 'VERIFY: Solution FAILED on ' + IntToStr(cubeSize) + 'x' + IntToStr(cubeSize));
      WriteLn(StdErr, 'Moves: ' + allMoves);
      Halt(4);
    end;
  end;

  // Summary on stderr
  if (cubeSize > 3) and (Trim(allMoves) <> '') then
  begin
    WriteLn(StdErr, Format('=== %dx%d Solution in %s ===',
      [cubeSize, cubeSize, FormatMs(elapsedMs)]));
    Flush(StdErr);
  end;

  // Output format compatible with min2phase:
  // Line 1: timing info
  // Line 2: move sequence
  WriteLn(Format('%d ms', [elapsedMs]));
  WriteLn(allMoves);
end.
