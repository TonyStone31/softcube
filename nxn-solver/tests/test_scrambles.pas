program test_scrambles;
{$mode objfpc}{$H+}

uses SysUtils, nxn_min2phase_defs, nxn_min2phase_cubicube, nxn_min2phase_coord,
     nxn_min2phase_search, nxn_min2phase;

var
  cc: TCubieCube;
  facelets, result_str: string;

procedure TestScramble(const name, fl: string);
var
  t1, t2: QWord;
begin
  WriteLn('=== ', name, ' ===');
  WriteLn('  Input:  ', fl);
  t1 := GetTickCount64;
  result_str := SolveMin2Phase(fl);
  t2 := GetTickCount64;
  WriteLn('  Result: ', result_str);
  WriteLn('  Time:   ', t2 - t1, ' ms');
  WriteLn('');
  Flush(Output);
end;

procedure TestMoves(const name: string; const moves: array of integer);
var
  i: integer;
begin
  CubieIdentity(cc);
  for i := 0 to High(moves) do
    CubieMove(cc, moves[i]);
  facelets := CubieCubeToFacelets(cc);
  TestScramble(name, facelets);
end;

begin
  WriteLn('Initializing...'); Flush(Output);
  InitMin2Phase;
  WriteLn('Tables ready.'); Flush(Output);
  WriteLn(''); Flush(Output);

  // Simple tests
  TestScramble('Solved', 'UUUUUUUUURRRRRRRRRFFFFFFFFFDDDDDDDDDLLLLLLLLLBBBBBBBBB');

  // Single moves (G1 case - phase 1 already solved)
  TestMoves('U move', [0]);     // U
  TestMoves('U2 move', [0, 0]); // U2
  TestMoves('D move', [3]);     // D
  TestMoves('R2 move', [1, 1]); // R2

  // Simple scrambles
  TestMoves('R U', [1, 0]);
  TestMoves('R U R'' U''', [1, 0, 1, 1, 1, 0, 0, 0]);

  // More complex: superflip (all edges flipped) - should need many moves
  // F R B L U D F R B L U D F R B L U D... actually let me use a known scramble
  TestMoves('F R U', [2, 1, 0]);
  TestMoves('F2 R2 U2 B2 L2 D2', [2, 2, 1, 1, 0, 0, 5, 5, 4, 4, 3, 3]);

  WriteLn('All tests done.');
end.
