program verify_solution;

{$mode objfpc}{$H+}

{ Verifies an nxn-solver solution by applying it to a FaceletCube
  (the solver's own representation) and checking if the result is solved.

  This isolates whether bugs are in:
  a) The solver itself (solution doesn't work even in FaceletCube)
  b) The notation translation between systems (solution works here but not in GenericCube)
}

uses
  SysUtils, Classes, nxn_cubedefs, nxn_facecube, nxn_util;

procedure ParseMoveToken(const token: string; cubeSize: integer;
  out ax: Axis; out sliceIdx: integer; out reps: integer);
var
  i: integer;
  numPrefix: integer;
  faceChar: char;
begin
  i := 1;
  numPrefix := 0;

  // Parse optional numeric prefix
  while (i <= Length(token)) and (token[i] in ['0'..'9']) do
  begin
    numPrefix := numPrefix * 10 + (Ord(token[i]) - Ord('0'));
    Inc(i);
  end;

  if i > Length(token) then
    raise Exception.CreateFmt('Invalid move token: %s (no face letter)', [token]);

  // Parse face letter
  faceChar := UpCase(token[i]);
  Inc(i);

  case faceChar of
    'U': ax := U;
    'D': ax := D;
    'R': ax := R;
    'L': ax := L;
    'F': ax := F;
    'B': ax := B;
  else
    raise Exception.CreateFmt('Invalid face letter in move: %s', [token]);
  end;

  // Determine slice index
  if numPrefix >= 2 then
    sliceIdx := numPrefix - 1
  else
    sliceIdx := 0;

  // Parse modifier
  reps := 1; // default = CW = 1x move()
  while i <= Length(token) do
  begin
    case token[i] of
      '2': reps := 2;      // 180 degrees
      '''', '`': reps := 3; // CCW = 3x CW
      ' ': ; // skip
    end;
    Inc(i);
  end;
end;

function IsSolved(verifyFC: FaceletCube): boolean;
var
  a: Axis;
  i, j: integer;
begin
  Result := True;
  for a := U to B do
    for i := 0 to verifyFC.size - 1 do
      for j := 0 to verifyFC.size - 1 do
        if verifyFC.faceCols[Ord(a), i, j] <> ColorIndex(Ord(a)) then
          Exit(False);
end;

procedure ShowMismatches(verifyFC: FaceletCube);
var
  a: Axis;
  i, j, count: integer;
  faceNames: array[0..5] of string = ('U', 'D', 'R', 'L', 'F', 'B');
begin
  count := 0;
  for a := U to B do
    for i := 0 to verifyFC.size - 1 do
      for j := 0 to verifyFC.size - 1 do
        if verifyFC.faceCols[Ord(a), i, j] <> ColorIndex(Ord(a)) then
        begin
          Inc(count);
          WriteLn(Format('  %s[%d,%d] = %s (expected %s)',
            [faceNames[Ord(a)], i, j,
             ColorIndexToChar(verifyFC.faceCols[Ord(a), i, j]),
             ColorIndexToChar(ColorIndex(Ord(a)))]));
        end;
  WriteLn(Format('Total mismatches: %d', [count]));
end;

var
  cubeSize: integer;
  faceletStr, solution: string;
  verifyFC, fcEven: FaceletCube;
  tokens: TStringList;
  i, j, sliceIdx, reps: integer;
  ax: Axis;
  token: string;
begin
  if ParamCount < 3 then
  begin
    WriteLn('Usage: verify_solution <size> <facelet_string> <solution_moves>');
    WriteLn('');
    WriteLn('Tests whether applying solution_moves to the facelet state');
    WriteLn('using the nxn-solver''s own FaceletCube produces a solved cube.');
    WriteLn('');
    WriteLn('This verifies the solver''s notation output is correct for its');
    WriteLn('own representation, independent of GenericCube.');
    Halt(1);
  end;

  cubeSize := StrToInt(ParamStr(1));
  faceletStr := ParamStr(2);
  solution := ParamStr(3);

  WriteLn('=== nxn-solver Self-Verification ===');
  WriteLn('Cube size: ', cubeSize);
  WriteLn('Input state (', Length(faceletStr), ' chars): ', faceletStr);
  WriteLn('Solution: ', solution);
  WriteLn;

  // Create FaceletCube from input (same as nxn_solver.lpr does)
  if not Odd(cubeSize) then
  begin
    fcEven := FaceletCube.Create(cubeSize);
    try
      if not ParseFaceletString(faceletStr, cubeSize, fcEven.faceCols) then
      begin
        WriteLn('Error: invalid facelet string');
        Halt(1);
      end;
      // Copy constructor creates virtual (size+1) cube for even sizes
      verifyFC := FaceletCube.Create(fcEven);
      WriteLn(Format('Even cube %dx%d -> virtual %dx%d',
        [cubeSize, cubeSize, verifyFC.size, verifyFC.size]));
    finally
      fcEven.Free;
    end;
  end
  else
  begin
    verifyFC := FaceletCube.Create(cubeSize);
    if not ParseFaceletString(faceletStr, cubeSize, verifyFC.faceCols) then
    begin
      WriteLn('Error: invalid facelet string');
      Halt(1);
    end;
  end;

  // Show initial state in Kociemba order
  WriteLn('Initial FaceletCube state (URFDLB): ', BuildFaceletString(cubeSize, verifyFC.faceCols));
  WriteLn;

  try
    // Parse and apply each move token
    tokens := TStringList.Create;
    try
      tokens.Delimiter := ' ';
      tokens.StrictDelimiter := True;
      tokens.DelimitedText := Trim(solution);

      WriteLn(Format('Applying %d moves...', [tokens.Count]));
      WriteLn;

      for i := 0 to tokens.Count - 1 do
      begin
        token := Trim(tokens[i]);
        if token = '' then
          Continue;

        ParseMoveToken(token, cubeSize, ax, sliceIdx, reps);

        Write(Format('Move %3d: %-6s (Axis=%d Slice=%d Reps=%d) ',
          [i + 1, token, Ord(ax), sliceIdx, reps]));

        for j := 1 to reps do
          verifyFC.move(ax, sliceIdx);

        if IsSolved(verifyFC) then
          WriteLn('-> SOLVED!')
        else
          WriteLn;
      end;

      WriteLn;
      WriteLn('Final FaceletCube state (URFDLB): ', BuildFaceletString(cubeSize, verifyFC.faceCols));
      WriteLn;

      if IsSolved(verifyFC) then
        WriteLn('RESULT: SOLVED! (Solution is correct for FaceletCube)')
      else
      begin
        WriteLn('RESULT: NOT SOLVED (Solution does NOT work even in FaceletCube!)');
        WriteLn;
        WriteLn('Mismatched facelets:');
        ShowMismatches(verifyFC);
      end;
    finally
      tokens.Free;
    end;
  finally
    verifyFC.Free;
  end;
end.
