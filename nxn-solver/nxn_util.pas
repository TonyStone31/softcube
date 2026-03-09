unit nxn_util;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, nxn_cubedefs;

type
  TFaceColsArray = array of array of array of ColorIndex;

var
  Verbose: boolean = False;
  edgemx: integer; // maximum index for edge orbits (size div 2)
  stopProgram: boolean = False;
  grandTotal: integer = 0;
  TableDir: string = ''; // Directory for NxN solver table files

// Face mapping: URFDLB order (SoftCube/min2phase) <-> Kociemba UDRLF order
// Kociemba internal: U=0, D=1, R=2, L=3, F=4, B=5
// URFDLB string:     U=0, R=1, F=2, D=3, L=4, B=5
const
  URFDLBToKociemba: array[0..5] of integer = (0, 2, 4, 1, 3, 5);
  KociembaToURFDLB: array[0..5] of integer = (0, 3, 1, 4, 2, 5);

procedure TSWriteLn(const msg: string);
procedure TSWrite(const msg: string);
procedure WriteLnVerbose(const msg: string);
procedure WriteVerbose(const msg: string);
procedure WriteProgress(const phase, table: string; done, total: int64);
procedure InitTableDir(const dir: string = '');

function ParseFaceletString(const faceStr: string; cubeSize: integer;
  var faceCols: TFaceColsArray): boolean;

function MoveToNotation(mv: Moves; orbitX, orbitY: integer): string;

function BuildFaceletString(cubeSize: integer;
  var faceCols: TFaceColsArray): string;

function CharToColorIndex(ch: char): ColorIndex;
function ColorIndexToChar(c: ColorIndex): char;
function FormatMs(ms: QWord): string;

implementation

function TimeStamp: string;
begin
  Result := '[' + FormatDateTime('hh:nn:ss', Now) + '] ';
end;

procedure TSWriteLn(const msg: string);
begin
  WriteLn(StdErr, TimeStamp + msg);
  Flush(StdErr);
end;

procedure TSWrite(const msg: string);
begin
  Write(StdErr, TimeStamp + msg);
  Flush(StdErr);
end;

procedure WriteLnVerbose(const msg: string);
begin
  if Verbose then
    TSWriteLn(msg);
end;

procedure WriteVerbose(const msg: string);
begin
  if Verbose then
    Write(StdErr, msg);
end;

procedure WriteProgress(const phase, table: string; done, total: int64);
begin
  if total > 0 then
    TSWriteLn(Format('PROGRESS:%s:%s:%d:%d', [phase, table, done, total]))
  else
    TSWriteLn(Format('PROGRESS:%s:%s:0:0', [phase, table]));
end;

procedure InitTableDir(const dir: string = '');
begin
  if dir <> '' then
    TableDir := IncludeTrailingPathDelimiter(dir)
  else
    TableDir := IncludeTrailingPathDelimiter(
      ExtractFilePath(ParamStr(0)) + 'data');
  if not DirectoryExists(TableDir) then
    ForceDirectories(TableDir);
end;

function CharToColorIndex(ch: char): ColorIndex;
begin
  case ch of
    'U': Result := UCol;
    'D': Result := DCol;
    'R': Result := RCol;
    'L': Result := LCol;
    'F': Result := FCol;
    'B': Result := BCol;
  else
    Result := NoCol;
  end;
end;

function ColorIndexToChar(c: ColorIndex): char;
begin
  case c of
    UCol: Result := 'U';
    DCol: Result := 'D';
    RCol: Result := 'R';
    LCol: Result := 'L';
    FCol: Result := 'F';
    BCol: Result := 'B';
  else
    Result := '-';
  end;
end;

function ParseFaceletString(const faceStr: string; cubeSize: integer;
  var faceCols: TFaceColsArray): boolean;
var
  n2, idx, face, kFace, row, col: integer;
begin
  n2 := cubeSize * cubeSize;
  if Length(faceStr) <> 6 * n2 then
    Exit(False);

  idx := 1;
  for face := 0 to 5 do
  begin
    kFace := URFDLBToKociemba[face];
    for row := 0 to cubeSize - 1 do
      for col := 0 to cubeSize - 1 do
      begin
        faceCols[kFace, row, col] := CharToColorIndex(faceStr[idx]);
        Inc(idx);
      end;
  end;
  Result := True;
end;

function BuildFaceletString(cubeSize: integer;
  var faceCols: TFaceColsArray): string;
var
  n2, face, kFace, row, col: integer;
begin
  n2 := cubeSize * cubeSize;
  SetLength(Result, 6 * n2);
  for face := 0 to 5 do
  begin
    kFace := URFDLBToKociemba[face];
    for row := 0 to cubeSize - 1 do
      for col := 0 to cubeSize - 1 do
        Result[face * n2 + row * cubeSize + col + 1] :=
          ColorIndexToChar(faceCols[kFace, row, col]);
  end;
end;

function FormatMs(ms: QWord): string;
var
  s, m: QWord;
begin
  if ms < 1000 then
    Result := Format('%d ms', [ms])
  else if ms < 60000 then
  begin
    s := ms div 1000;
    Result := Format('%d.%d s', [s, (ms mod 1000) div 100]);
  end
  else
  begin
    m := ms div 60000;
    s := (ms mod 60000) div 1000;
    if m >= 60 then
      Result := Format('%d h %d min %d s', [m div 60, m mod 60, s])
    else
      Result := Format('%d min %d s', [m, s]);
  end;
end;

function MoveToNotation(mv: Moves; orbitX, orbitY: integer): string;
var
  n, axisIdx, power: integer;
  faceName: string;
  sliceNum: integer;
begin
  if (mv = InitMove) or (mv = NoMove) then
    Exit('');

  n := Ord(mv) mod 18;
  axisIdx := n div 3;
  power := (n mod 3) + 1;

  case Axis(axisIdx) of
    U: faceName := 'U';
    D: faceName := 'D';
    R: faceName := 'R';
    L: faceName := 'L';
    F: faceName := 'F';
    B: faceName := 'B';
  end;

  if mv <= fB3 then
    Result := faceName
  else if mv <= xB3 then
  begin
    sliceNum := orbitX + 1;
    Result := IntToStr(sliceNum) + faceName;
  end
  else
  begin
    sliceNum := orbitY + 1;
    Result := IntToStr(sliceNum) + faceName;
  end;

  case power of
    2: Result := Result + '2';
    3: Result := Result + '''';
  end;
end;

end.
