unit UMain;

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
  SysUtils,
  ExtCtrls,
  Menus,
  Spin,
  ComCtrls,
  StdCtrls,
  strutils,
  UConst,
  UDraw,
  process,
  Types,
  URubik;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnBackClock: TButton;
    btnBackCounter: TButton;
    btnScrampleState: TButton;
    btnScrambleTarget: TButton;
    btnCurrentStateReset: TButton;
    btnTargetSolveReset: TButton;
    btnDownClock: TButton;
    btnDownCounter: TButton;
    btnFrontClock: TButton;
    btnFrontCounter: TButton;
    btnLeftClock: TButton;
    btnLeftCounter: TButton;
    btnReset3Dview: TButton;
    btnExecute: TButton;
    btnRightClock: TButton;
    btnRightCounter: TButton;
    btnSearchForSolution: TButton;
    btnUpClock: TButton;
    btnUpCounter: TButton;
    btn2phaseSolve: TButton;
    edtMoveString: TEdit;
    imgFilters: TImage;
    lblSpeedControl: TLabel;
    lblClickExplainer: TLabel;
    lblClickExplainer1: TLabel;
    lblControlDirections: TLabel;
    lblCurrentMove: TLabel;
    lblNoticeTarget: TLabel;
    memMoveSum: TMemo;
    ts2DViews: TPageControl;
    pnlDestination: TPanel;
    pnlSetState: TPanel;
    pntBox3Dview: TPaintBox;
    pnl3Dview: TPanel;
    pnlSolution: TPanel;
    pntBoxCurrentState: TPaintBox;
    pntBoxTargetSolve: TPaintBox;
    spinEdtAnimationSpeed: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    tglKeyBoardControl: TCheckBox;
    procedure btnScrambleTargetClick(Sender: TObject);
    procedure btnScrampleStateClick(Sender: TObject);
    procedure btnTargetSolveResetClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnCurrentStateResetClick(Sender: TObject);
    procedure btnReset3DviewClick(Sender: TObject);
    procedure btnRightClockClick(Sender: TObject);
    procedure btn2phaseSolveClick(Sender: TObject);
    procedure edtMoveStringKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure pntBoxCurrentStateMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure pntBoxCurrentStateMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ts2DViewsChange(Sender: TObject);
    procedure pntBox3DviewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure pntBoxTargetSolveMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure pntBoxTargetSolvePaint(Sender: TObject);
    procedure pntBoxCurrentStateMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure pntBoxCurrentStatePaint(Sender: TObject);
    procedure pntBox3DviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure pntBox3DviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure pntBox3DviewPaint(Sender: TObject);
    procedure btnSearchForSolutionClick(Sender: TObject);
    procedure ManualRotateFace(Face: integer; clockWise: boolean);
    procedure SetInitialCubeView;
    procedure tglKeyBoardControlChange(Sender: TObject);
  private
    procedure ActiveSleep(ms: cardinal);
    procedure ExecuteSolverAndParseOutput(const faceString: string; aMemo: TMemo; MoveString: TEdit);
    procedure ToggleButtonsExcept(Form: TForm; ExceptButton: TButton; Enable: boolean);



  public

  end;

var
  frmMain: TfrmMain;
  tmx: integer = 0;
  tmy: integer = 0;
  IsRunning: boolean = False;
  mouseDrag3D: boolean;
  FaceCodeMover: integer = 0;
  keyBoardControlActive: boolean;

implementation

{$R *.lfm}


procedure TfrmMain.ToggleButtonsExcept(Form: TForm; ExceptButton: TButton; Enable: boolean);
var
  i: integer;
  comp: TComponent;
begin
  for i := 0 to Form.ComponentCount - 1 do
  begin
    comp := Form.Components[i];
    if (comp is TButton) and (comp <> ExceptButton) then
    begin
      TButton(comp).Enabled := Enable;
    end;
  end;
  Application.ProcessMessages;
end;

procedure TfrmMain.ActiveSleep(ms: cardinal);
var
  TargetTime: cardinal;
begin
  TargetTime := GetTickCount64 + ms;
  while GetTickCount64 < TargetTime do
  begin
    Application.ProcessMessages;
    Sleep(5);
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Randomize;
  DoubleBuffered := True;
  CurrentCubeState := C_CUBE_COMPLETE;
  TargetCubeState := C_CUBE_COMPLETE;
  Cube3D := VIEW_OF_3D_CUBE;
  lblCurrentMove.Caption := ' ';
  SetInitialCubeView;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  //if keyBoardControlActive then Key:=0;
end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: char);
begin
  case Key of
    'a', 'A': FaceCodeMover := 4;
    's', 'S': FaceCodeMover := 5;
    'd', 'D': FaceCodeMover := 2;
    'w', 'W': FaceCodeMover := 0;
    'r', 'R': FaceCodeMover := 3;
    'f', 'F': FaceCodeMover := 1;
  end;
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if IsRunning or (not keyBoardControlActive) then Exit;
  if (FaceCodeMover <> 2) and (FaceCodeMover <> 4) then
  begin

    if (Key = VK_LEFT) then
      ManualRotateFace(FaceCodeMover, False);
    if (Key = VK_RIGHT) then
      ManualRotateFace(FaceCodeMover, True);
  end else begin
    if (Key = VK_UP) then
      ManualRotateFace(FaceCodeMover, False);
    if (Key = VK_DOWN) then
      ManualRotateFace(FaceCodeMover, True);
  end;
end;

procedure TfrmMain.pntBoxCurrentStateMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TfrmMain.pntBoxCurrentStateMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin

end;

procedure TfrmMain.ts2DViewsChange(Sender: TObject);
begin
  pntBox3Dview.Refresh;
end;

procedure TfrmMain.pntBoxTargetSolvePaint(Sender: TObject);
begin
  DrawCube(pntBoxTargetSolve, TargetCubeState);
end;

procedure TfrmMain.pntBoxCurrentStateMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  colorIndex: integer;
begin
  if IsRunning then Exit;
  colorIndex := GetCubeyColor(CurrentCubeState, Point(x, y));

  if Button = mbLeft then
  begin
    // Cycles colors 2 to 5 with left mouse button
    if (colorIndex >= 2) and (colorIndex < 5) then
      Inc(colorIndex) // Move to the next color
    else if colorIndex = 5 then
      colorIndex := 2 // Wrap back to color 2
    else
      colorIndex := 2; // Default to 2 if outside range
  end else if Button = mbRight then
  begin
    // Toggles between colors 1 and 6 with right mouse button
    if colorIndex = 6 then
      colorIndex := 1
    else
      colorIndex := 6;
  end;

  PlaceColorClicker(CurrentCubeState, point(x, y), colorIndex);

  DrawCube(pntBoxCurrentState, CurrentCubeState);
  pntBox3Dview.Refresh;
end;

procedure TfrmMain.pntBoxTargetSolveMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  colorIndex: integer;
begin
  if IsRunning then Exit;
  colorIndex := GetCubeyColor(TargetCubeState, Point(x, y));

  if Button = mbLeft then
  begin
    // Cycles colors 2 to 5 with left mouse button
    if (colorIndex >= 2) and (colorIndex < 5) then
      Inc(colorIndex) // Move to the next color
    else if colorIndex = 5 then
      colorIndex := 2 // Wrap back to color 2
    else
      colorIndex := 2; // Default to 2 if outside range
  end else if Button = mbRight then
  begin
    // Toggles between colors 1 and 6 with right mouse button
    if colorIndex = 6 then
      colorIndex := 1
    else
      colorIndex := 6;
  end;

  PlaceColorClicker(TargetCubeState, point(x, y), colorIndex);

  DrawCube(pntBoxTargetSolve, TargetCubeState);
  pntBox3Dview.Refresh;
end;

procedure TfrmMain.pntBoxCurrentStatePaint(Sender: TObject);
begin
  DrawCube(pntBoxCurrentState, CurrentCubeState);
end;

procedure TfrmMain.SetInitialCubeView;
var
  AngleX, AngleY: double;
begin
  axeX[0] := -1;
  axeX[1] := 0;
  axeX[2] := 0; // Reset X axis

  axeY[0] := 0;
  axeY[1] := -1;
  axeY[2] := 0; // Reset Y axis

  axeZ[0] := 0;
  axeZ[1] := 0;
  axeZ[2] := -1; // Reset Z axis

  Cube3D := VIEW_OF_3D_CUBE;
  // Found I need to set one angle at a time.
  AngleY := 45 * Pi / 180; // Rotate -45 degrees around the Y-axis for left/right view
  Rotate3d(cube3d, 0, AngleY, 0);

  AngleX := -34 * Pi / 180; // Rotate 45 degrees around the X-axis to see the top
  Rotate3d(cube3d, AngleX, 0, 0);
  DrawCube3d(pntBox3Dview, CurrentCubeState, Cube3D);
end;

procedure TfrmMain.tglKeyBoardControlChange(Sender: TObject);
begin
  keyBoardControlActive := tglKeyBoardControl.Checked;
end;

procedure TfrmMain.btnReset3DviewClick(Sender: TObject);
begin
  SetInitialCubeView;
end;

procedure TfrmMain.pntBox3DviewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  mouseDrag3D := False;
end;

procedure TfrmMain.pntBox3DviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  mouseDrag3D := True;
  tmx := x;
  tmy := y;
end;

procedure TfrmMain.pntBox3DviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin
  if not mouseDrag3D then exit;
  if IsRunning then exit;
  if (ssLeft in Shift) then Rotate3d(cube3d, (-y + tmy) * pi / 180, (-x + tmx) * pi / 180, 0)
  else if (ssRight in Shift) then Rotate3d(cube3d, 0, 0, (-x + tmx) * pi / 180);

  tmx := x;
  tmy := y;

  if ts2DViews.ActivePageIndex = 0 then
    DrawCube3d(pntBox3Dview, CurrentCubeState, cube3d)
  else
    DrawCube3d(pntBox3Dview, TargetCubeState, cube3d);

end;

procedure TfrmMain.pntBox3DviewPaint(Sender: TObject);
begin
  if ts2DViews.ActivePageIndex = 0 then
    DrawCube3d(pntBox3Dview, CurrentCubeState, cube3d)
  else
    DrawCube3d(pntBox3Dview, TargetCubeState, cube3d);
end;

procedure TfrmMain.btnScrampleStateClick(Sender: TObject);
var
  i: integer;
begin
  CurrentCubeState := C_CUBE_COMPLETE;
  for i := 0 to 50 do rotateface(TUnitRubik(CurrentCubeState), random(6) + 1, random(3) + 1);
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  DrawCube3d(pntBox3Dview, CurrentCubeState, cube3d);
end;

procedure TfrmMain.btnScrambleTargetClick(Sender: TObject);
var
  i: integer;
begin
  TargetCubeState := C_CUBE_COMPLETE;
  for i := 0 to 50 do rotateface(TUnitRubik(TargetCubeState), random(6) + 1, random(3) + 1);
  DrawCube(pntBoxTargetSolve, TargetCubeState);
  DrawCube3d(pntBox3Dview, TargetCubeState, cube3d);

end;

procedure TfrmMain.btnExecuteClick(Sender: TObject);
var
  f, i, ii, j, v: integer;
  s, lbl: string;
  tmp: tcube3d;
begin
  if IsRunning then
  begin
    btnExecute.Caption := 'Execute';
    ToggleButtonsExcept(Self, btnExecute, True);
    IsRunning := False;
    exit;
  end;
  IsRunning := True;
  btnExecute.Caption := 'Stop';
  ToggleButtonsExcept(Self, btnExecute, False);

  s := AnsiUpperCase(edtMoveString.Text);
  LFDstringCorrection(s); //Really think what I did here is hackish because I will need to invert the 3 sides
                          //anywhere and anytime you want to use signmaster notation.  Need to rethink this
                          //however for now it does do the trick.
  i := 1;

  while i <= length(s) do
  begin
    v := spinEdtAnimationSpeed.Value;

    if not IsRunning then exit;

    j := 1;
    lblCurrentMove.Caption := s[i];

    if (i < length(s)) and (s[i + 1] = '''') then
    begin
      j := 3;
      lblCurrentMove.Caption := s[i] + s[i + 1];
    end;
    if (i < length(s)) and (s[i + 1] = '2') then
    begin
      j := 2;
      lblCurrentMove.Caption := s[i] + s[i + 1];
    end;

    Application.ProcessMessages;

    case s[i] of
      'L': f := CUBE_LEFT;
      'R': f := CUBE_RIGHT;
      'B': f := CUBE_BACK;
      'F': f := CUBE_FRONT;
      'U': f := CUBE_TOP;
      'D': f := CUBE_BOTTOM;
      else
        if j > 1 then Inc(i, 2)
        else
          Inc(i);
        continue;
    end;

    // animation 3D
    tmp := cube3d;
    for ii := 0 to 90 do
    begin
      if (v > 7) and (ii mod 4 <> 0) then Continue;
      if not IsRunning then Break;
      if ii mod v = 0 then ActiveSleep(Round(-2 * v) + 20);

      if j = 1 then
      begin
        Rotate3dface(cube3d, f, -ii * pi / 180);
      end else if j = 2 then
      begin
        Rotate3dface(cube3d, f, -ii * pi / 90);
      end else if j = 3 then
      begin
        Rotate3dface(cube3d, f, ii * pi / 180);
      end;
      DrawCube3d(pntBox3Dview, CurrentCubeState, cube3d);
      cube3d := tmp;
    end;

    RotateFace(TUnitRubik(CurrentCubeState), f, j);
    DrawCube3d(pntBox3Dview, CurrentCubeState, cube3d);
    DrawCube(pntBoxCurrentState, CurrentCubeState);

    ActiveSleep(Round(-300 * v) + 3200);

    if j > 1 then Inc(i, 2)
    else
      Inc(i);
    lblCurrentMove.Caption := ' ';
  end;
  IsRunning := False;
  btnExecute.Caption := 'Execute';
  ToggleButtonsExcept(Self, btnExecute, True);
end;

procedure TfrmMain.btnTargetSolveResetClick(Sender: TObject);
begin
  TargetCubeState := C_CUBE_COMPLETE;
  DrawCube(pntBoxTargetSolve, TargetCubeState);
  pntBox3Dview.Refresh;
end;

procedure TfrmMain.btnCurrentStateResetClick(Sender: TObject);
begin
  CurrentCubeState := C_CUBE_COMPLETE;
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  pntBox3Dview.Refresh;
end;

procedure TfrmMain.btnRightClockClick(Sender: TObject);
var
  n: integer;
  tmp: tcube3d;
  i, v: integer;
begin
  n := TButton(Sender).Tag;
  v := spinEdtAnimationSpeed.Value;

  tmp := cube3d;
  for i := 0 to 90 do
  begin
    if (v > 7) and (i mod 4 <> 0) then Continue;
    if i mod v = 0 then ActiveSleep(Round(-2 * v) + 20);

    Rotate3dface(cube3d, n mod 10 + 1, (((n div 10) * 2 - 1) * i) * pi / 180);
    pntBox3Dview.Refresh;
    cube3d := tmp;
  end;

  Rotateface(TUnitRubik(CurrentCubeState), n mod 10 + 1, (n div 10) * 2 + 1);
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  pntBox3Dview.Refresh;
end;

procedure TfrmMain.btn2phaseSolveClick(Sender: TObject);
begin
  ExecuteSolverAndParseOutput(CubeToDefinitionString(CurrentCubeState), memMoveSum, edtMoveString);
end;

procedure TfrmMain.ExecuteSolverAndParseOutput(const faceString: string; aMemo: TMemo; MoveString: TEdit);
var
  Process: TProcess;
  OutputLines: TStringList;
  i, dotCount: integer;
begin
  Screen.Cursor := crHourGlass;
  MoveString.Text := 'External solver running... Please Wait';
  dotCount := 0;

  Process := TProcess.Create(nil);
  OutputLines := TStringList.Create;
  try
    Process.Executable := './linux-2phase';
    Process.Parameters.Add(faceString);
    Process.Options := Process.Options + [poUsePipes];
    Process.Execute;

    while Process.Running do
    begin
      Application.ProcessMessages;
      Inc(dotCount);
      if dotCount > 20 then dotCount := 1;
      MoveString.Text := 'External solver running... Please Wait' + StringOfChar('.', dotCount);
      Sleep(200);
    end;

    OutputLines.LoadFromStream(Process.Output);

    aMemo.Lines.Clear;
    for i := 0 to OutputLines.Count - 1 do
      aMemo.Lines.Add(OutputLines[i]);

    // Assume the solution is on the second line and display it in MoveString
    if OutputLines.Count >= 2 then
      MoveString.Text := OutputLines[1]
    else
      MoveString.Text := 'Error: No solution found.';
  finally
    OutputLines.Free;
    Process.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.edtMoveStringKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Key = VK_RETURN then btnExecuteClick(Sender);
end;

procedure TfrmMain.ManualRotateFace(Face: integer; clockWise: boolean);
var
  tmp: tcube3d;
  i, v, n: integer;
begin
  ToggleButtonsExcept(Self, btnReset3Dview, False);
  if clockWise then n := Face + 10
  else
    n := Face;
  v := spinEdtAnimationSpeed.Value;

  tmp := cube3d;
  for i := 0 to 90 do
  begin
    if (v > 7) and (i mod 4 <> 0) then Continue;
    if i mod v = 0 then ActiveSleep(Round(-2 * v) + 20);

    Rotate3dface(cube3d, n mod 10 + 1, (((n div 10) * 2 - 1) * i) * pi / 180);
    pntBox3Dview.Refresh;
    cube3d := tmp;
  end;

  Rotateface(TUnitRubik(CurrentCubeState), face mod 10 + 1, (n div 10) * 2 + 1);
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  pntBox3Dview.Refresh;
  ToggleButtonsExcept(Self, btnReset3Dview, True);
end;

procedure TfrmMain.btnSearchForSolutionClick(Sender: TObject);
var
  s: string;
  tmp: TFaceRubik;
begin
  memMoveSum.Clear;
  s := '';
  if not VerifyCube(CurrentCubeState, s) then
  begin
    memMoveSum.Lines.add('The cube has been disassembled or tampered with:');
    memMoveSum.Lines.add(s);
    exit;
  end;

  solu := '';
  tmp := CurrentCubeState;

  // Step 1
  placeWhiteEdges(tmp);
  memMoveSum.Lines.Add('---> placeWhiteEdges');
  memMoveSum.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  LFDstringCorrection(solu);
  s := solu;
  solu := '';
  // Step 2
  placeWhiteCorners(tmp);
  memMoveSum.Lines.Add('---> placeWhiteCorners');
  memMoveSum.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  LFDstringCorrection(solu);
  s := s + solu;
  solu := '';
  // Step 3
  placeSecondLayerEdges(tmp);
  memMoveSum.Lines.Add('---> placeSecondLayerEdges');
  memMoveSum.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  LFDstringCorrection(solu);
  s := s + solu;
  solu := '';
  // Step 4
  PlaceYellowEdges(tmp);
  memMoveSum.Lines.Add('---> PlaceYellowEdges');
  memMoveSum.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  LFDstringCorrection(solu);
  s := s + solu;
  solu := '';
  // Step 5
  OrientYellowEdges(tmp);
  memMoveSum.Lines.Add('---> OrientYellowEdges');
  memMoveSum.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  LFDstringCorrection(solu);
  s := s + solu;
  solu := '';
  // Step 6
  PlaceYellowCorners(tmp);
  memMoveSum.Lines.Add('---> PlaceYellowCorners');
  memMoveSum.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  LFDstringCorrection(solu);
  s := s + solu;
  solu := '';
  // Step 7
  OrientYellowCorners(tmp);
  memMoveSum.Lines.Add('---> OrientYellowCorners');
  memMoveSum.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  LFDstringCorrection(solu);
  s := s + solu;
  solu := '';

  memMoveSum.Lines.Add('movements ' + IntToStr(CountMoves(s)));
  FilterMoves(s, CurrentCubeState, imgFilters.canvas);
  pntBoxCurrentState.Refresh;
  memMoveSum.Lines.Add('movements after filter ' + IntToStr(CountMoves(s)) + ')');
  memMoveSum.Lines.Add('');
  memMoveSum.Lines.Add('---> Filter');
  memMoveSum.Lines.Add(s);
  UpperCase(s);
  edtMoveString.Text := s;
end;


end.
