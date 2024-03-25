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
  Buttons,
  Dialogs,
  StdCtrls,
  strutils,
  UConst,
  UDraw,
  process,
  Types,
  URubik;

type
  TRotationDirection = (rdLeft, rdRight, rdUp, rdDown);

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btn2phaseSolve: TSpeedButton;
    btnBackClock: TSpeedButton;
    btn3DViewRotate90Left: TSpeedButton;
    btnBackCounter: TSpeedButton;
    btnControlHelp: TSpeedButton;
    btnCurrentStateReset: TSpeedButton;
    btn3DviewReset: TSpeedButton;
    btnDownClock: TSpeedButton;
    btn3DViewRotateRight: TSpeedButton;
    btnDownCounter: TSpeedButton;
    btnExecute: TSpeedButton;
    btnFrontClock: TSpeedButton;
    btnFrontCounter: TSpeedButton;
    btnLeftClock: TSpeedButton;
    btn3DViewRotate90Up: TSpeedButton;
    btnLeftCounter: TSpeedButton;
    btnRightClock: TSpeedButton;
    btnRightCounter: TSpeedButton;
    btnScrambleTarget: TSpeedButton;
    btnScrampleState: TSpeedButton;
    btnSearchForSolution: TSpeedButton;
    btnTargetSolveReset: TSpeedButton;
    btnUpClock: TSpeedButton;
    btnUpCounter: TSpeedButton;
    edtMoveString: TMemo;
    lblCurrentMove: TLabel;
    lblSingMaster: TLabel;
    lblSpeedControl: TLabel;
    lblNoticeTarget: TLabel;
    memRandScramble: TMemo;
    memSolveSummary: TMemo;
    pnlCubeControls: TPanel;
    pnlFaceControls: TPanel;
    pntBox3Dview: TPaintBox;
    SplitterVerticalMain: TSplitter;
    SplitterHorizontalMain: TSplitter;
    tglKeyBoardControl: TCheckBox;
    ts2DViews: TPageControl;
    pnlDestination: TPanel;
    pnlSetState: TPanel;
    pnl3Dview: TPanel;
    pnlSolution: TPanel;
    pntBoxCurrentState: TPaintBox;
    pntBoxTargetSolve: TPaintBox;
    spinEdtAnimationSpeed: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btn3DViewRotate90LeftClick(Sender: TObject);
    procedure btn3DViewRotate90UpMouseEnter(Sender: TObject);
    procedure btn3DViewRotate90UpMouseLeave(Sender: TObject);
    procedure btnControlHelpClick(Sender: TObject);
    procedure btn3DViewRotateRightClick(Sender: TObject);
    procedure btn3DViewRotate90UpClick(Sender: TObject);
    procedure btnScrambleTargetClick(Sender: TObject);
    procedure btnScrampleStateClick(Sender: TObject);
    procedure btnTargetSolveResetClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnCurrentStateResetClick(Sender: TObject);
    procedure btn3DviewResetClick(Sender: TObject);
    procedure btnMoveClick(Sender: TObject);
    procedure btn2phaseSolveClick(Sender: TObject);
    procedure edtMoveStringKeyPress(Sender: TObject; var Key: char);
    procedure edtMoveStringKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure pntBoxCurrentStateMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: integer;
      MousePos: TPoint; var Handled: boolean);
    procedure pntBoxCurrentStateMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: boolean);
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
    procedure ExecuteSolverAndParseOutput(const faceString: string; aMemo: TMemo; MoveString: TMemo);
    procedure FastRotateFace(Face: integer; clockWise: boolean);
    function GenerateRandomScramble(MoveCount: integer): string;
    procedure RotateCubeLeftRight(Direction: integer);
    procedure RotateCubePeakUnder();
    procedure RotateCubeFlipUp();
    procedure RotateCubeUnPeakUnder();
    procedure ToggleButtonsExcept(Form: TForm; ExceptButton: TSpeedButton; Enable: boolean);
  public
    procedure ExecuteNotation(var s: string; SpeedVal: integer);

  end;

var
  frmMain: TfrmMain;
  tmx: integer = 0;
  tmy: integer = 0;
  IsRunning: boolean = False;
  Cube3DTransActive: boolean;
  mouseDrag3D: boolean;
  FaceCodeMover: integer = 0;
  keyBoardControlActive: boolean;

implementation

{$R *.lfm}

procedure TfrmMain.ToggleButtonsExcept(Form: TForm; ExceptButton: TSpeedButton; Enable: boolean);
var
  i: integer;
  comp: TComponent;
begin
  for i := 0 to Form.ComponentCount - 1 do
  begin
    comp := Form.Components[i];
    if (comp is TSpeedButton) and (comp <> ExceptButton) then
    begin
      TSpeedButton(comp).Enabled := Enable;
    end;
  end;
  Application.ProcessMessages;
end;

procedure TfrmMain.ExecuteNotation(var s: string; SpeedVal: integer);
var
  tmp: tcube3d;
  lblS: string;
  j: integer;
  ii: integer;
  i: integer;
  f: integer;
begin
  LFDstringCorrection(s); //Really think what I did here is hackish because I will need to invert the 3 sides
  //anywhere and anytime you want to use signmaster notation.  Need to rethink this
  //however for now it does do the trick.
  i := 1;

  while i <= length(s) do
  begin
    if not IsRunning then exit;

    j := 1;
    if (i < length(s)) and (s[i + 1] = '''') then
    begin
      j := 3;
      lblS := s[i] + s[i + 1];
      LFDstringCorrection(lblS);
    end else if (i < length(s)) and (s[i + 1] = '2') then
    begin
      j := 2;
      lblS := s[i] + s[i + 1];

    end else begin
      lblS := s[i];
      LFDstringCorrection(lblS);
    end;
    lblCurrentMove.Caption := lblS;
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
      if (SpeedVal = 11) then Continue;
      if (SpeedVal > 7) and (ii mod 3 <> 0) then Continue;
      if not IsRunning then Break;


      if j = 1 then
      begin
        ActiveSleep(Round(-2 * SpeedVal) + 20);
        Rotate3dface(cube3d, f, -ii * pi / 180);
      end else if j = 2 then
      begin
        ActiveSleep(Round(-2 * SpeedVal) + 30);
        Rotate3dface(cube3d, f, -ii * pi / 90);
      end else if j = 3 then
      begin
        ActiveSleep(Round(-2 * SpeedVal) + 20);
        Rotate3dface(cube3d, f, ii * pi / 180);
      end;
      DrawCube3d(pntBox3Dview, CurrentCubeState, cube3d);
      cube3d := tmp;
    end;

    RotateFace(TUnitRubik(CurrentCubeState), f, j);
    DrawCube3d(pntBox3Dview, CurrentCubeState, cube3d);
    DrawCube(pntBoxCurrentState, CurrentCubeState);

    if SpeedVal < 11 then
      ActiveSleep(Round(-300 * SpeedVal) + 3200);

    if j > 1 then Inc(i, 2)
    else
      Inc(i);
    lblCurrentMove.Caption := ' ';
  end;
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
  if keyBoardControlActive then Key := 0;
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
  colorIndex := GetColor2D(CurrentCubeState, Point(x, y));

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

  SetColor2D(CurrentCubeState, point(x, y), colorIndex);

  DrawCube(pntBoxCurrentState, CurrentCubeState);
  pntBox3Dview.Refresh;
end;

procedure TfrmMain.pntBoxCurrentStateMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
var
  colorIndex: integer;
begin
  if IsRunning then Exit; // Ensure the cube isn't in a running state
  colorIndex := GetColor2D(CurrentCubeState, MousePos); // Assuming GetColor2D can work with MousePos directly

  // Adjust the colorIndex based on WheelDelta
  if WheelDelta > 0 then
  begin
    // Scrolled up: Cycle forward through the colors 2 to 5
    colorIndex := ((colorIndex - 2 + 1) mod 4) + 2;
  end else if WheelDelta < 0 then
  begin
    // Scrolled down: Cycle backward through the colors 2 to 5
    if colorIndex = 2 then
      colorIndex := 5
    else if colorIndex > 2 then
      Dec(colorIndex) // Move to the previous color
    else
      colorIndex := 2; // Default to 2 if outside the target range
  end;

  SetColor2D(CurrentCubeState, MousePos, colorIndex);
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  pntBox3Dview.Refresh;
end;



procedure TfrmMain.pntBoxCurrentStateMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: boolean);
begin

end;

procedure TfrmMain.pntBoxTargetSolveMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  colorIndex: integer;
begin
  if IsRunning then Exit;
  colorIndex := GetColor2D(TargetCubeState, Point(x, y));

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

  SetColor2D(TargetCubeState, point(x, y), colorIndex);

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

  AngleY := 45 * Pi / 180;
  Rotate3d(cube3d, 0, AngleY, 0);

  AngleX := -32 * Pi / 180; // Rotate 45 degrees around the X-axis to see the top
  Rotate3d(cube3d, AngleX, 0, 0);
  DrawCube3d(pntBox3Dview, CurrentCubeState, Cube3D);
  Cube3DTransActive := False;
end;

procedure TfrmMain.RotateCubeLeftRight(Direction: integer);
var
  i: integer;
  AngleYIncrement, AngleX: double;
  SleepTime: integer;
begin
  repeat
    Sleep(10);
  until Cube3DTransActive = False;

  Cube3DTransActive := True;

  // Direction = 1 for left, and Direction = -1 for right
  AngleYIncrement := Direction * (90 * Pi / 180) / 60;

  for i := 1 to 60 do
  begin
    // ANGLE IT BACK UP SO IT IS JUST A FLAT FRONT VIEW
    AngleX := 32 * Pi / 180;
    Rotate3d(cube3d, AngleX, 0, 0);

    // NOW ROTATE IT INCREMENTALLY!!!
    Rotate3d(cube3d, 0, AngleYIncrement, 0);

    // NOW ROTATE IT BACK DOWN TO THE ANGLE WE LIKE TO SEE THE TOP!!
    AngleX := -32 * Pi / 180;
    Rotate3d(cube3d, AngleX, 0, 0);

    // WE DON'T DRAW UNTIL ALL AXIS ARE SET!!!
    DrawCube3d(pntBox3Dview, CurrentCubeState, cube3d);
    Sleep(i div 6);
  end;

  // CANNOT BELIEVE HOW MANY HOURS I WASTED HERE!  TRYING TO INCREMENT/DECREMENT the X angle to keep the top clenaly in view

  Cube3DTransActive := False;
end;

procedure TfrmMain.RotateCubeFlipUp();
var
  i: integer;
  AngleXIncrement: double;
  SleepTime: ValReal;
begin
  repeat
    Sleep(10);
  until Cube3DTransActive = False;

  Cube3DTransActive := True;
  AngleXIncrement := (180 * Pi / 180) / 60;

  for i := 1 to 60 do
  begin
    Rotate3d(cube3d, AngleXIncrement, 0, 0);
    DrawCube3d(pntBox3Dview, CurrentCubeState, cube3d);
    Sleep(i div 5);
  end;

  Cube3DTransActive := False;
end;

procedure TfrmMain.RotateCubePeakUnder();
var
  i: integer;
  AngleXIncrement: double;
  SleepTime: ValReal;
begin
  repeat
    Sleep(10);
  until Cube3DTransActive = False;

  Cube3DTransActive := True;
  AngleXIncrement := ((45 + 22.5) * Pi / 180) / 60;

  for i := 1 to 60 do
  begin
    Rotate3d(cube3d, AngleXIncrement, 0, 0);
    DrawCube3d(pntBox3Dview, CurrentCubeState, cube3d);
    Sleep(i div 6);
  end;

  Cube3DTransActive := False;
end;

procedure TfrmMain.RotateCubeUnPeakUnder();
var
  i: integer;
  AngleXIncrement: double;
  SleepTime: ValReal;
begin
  repeat
    Sleep(10);
  until Cube3DTransActive = False;

  Cube3DTransActive := True;
  AngleXIncrement := ((45 + 22.5) * Pi / 180) / 60;

  for i := 1 to 60 do
  begin
    Rotate3d(cube3d, -AngleXIncrement, 0, 0);
    DrawCube3d(pntBox3Dview, CurrentCubeState, cube3d);
    Sleep(i div 6);
  end;

  Cube3DTransActive := False;
end;

procedure TfrmMain.btn3DViewRotate90LeftClick(Sender: TObject);
begin
  RotateCubeLeftRight(1);
end;

procedure TfrmMain.btn3DViewRotate90UpMouseEnter(Sender: TObject);
begin
  // Needs more thought but partially works.  Will leave commented out for now
  RotateCubePeakUnder();
end;

procedure TfrmMain.btn3DViewRotate90UpMouseLeave(Sender: TObject);
begin
  // Needs more thought but partially works.  Will leave commented out for now
  RotateCubeUnPeakUnder();
end;

procedure TfrmMain.btn3DViewRotate90UpClick(Sender: TObject);
begin
  RotateCubeFlipUp;
end;

procedure TfrmMain.btn3DViewRotateRightClick(Sender: TObject);
begin
  RotateCubeLeftRight(-1);
end;


procedure TfrmMain.tglKeyBoardControlChange(Sender: TObject);
begin
  keyBoardControlActive := tglKeyBoardControl.Checked;
end;

procedure TfrmMain.btn3DviewResetClick(Sender: TObject);
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
  NumMoves, i: integer;
  InputStr, scrambledNotation: string;
  UserOK: boolean;
begin
  Randomize;

  InputStr := '15';
  UserOK := InputQuery('Scramble Cube', 'Enter the number of scramble moves:', InputStr);

  if UserOK then
  begin
    if TryStrToInt(InputStr, NumMoves) then
    begin
      if NumMoves > 100 then
        NumMoves := 100; // Capping number of moves
      //RandomRotateFaces(NumMoves);
      scrambledNotation := GenerateRandomScramble(NumMoves);
      memRandScramble.Text := scrambledNotation;
      ToggleButtonsExcept(Self, btn3DviewReset, False);
      IsRunning := True;
      ExecuteNotation(scrambledNotation, 11);
      IsRunning := False;
      ToggleButtonsExcept(Self, btn3DviewReset, True);
    end else
      ShowMessage('Please enter a valid number.');
  end;
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

procedure TfrmMain.btnControlHelpClick(Sender: TObject);
begin

end;

procedure TfrmMain.btnExecuteClick(Sender: TObject);
var
  s: string;
begin
  if IsRunning then
  begin
    btnExecute.Caption := '🚀 Execute';
    ToggleButtonsExcept(Self, btnExecute, True);
    IsRunning := False;
    exit;
  end;
  IsRunning := True;
  btnExecute.Caption := 'Stop';
  ToggleButtonsExcept(Self, btnExecute, False);

  s := AnsiUpperCase(edtMoveString.Text);
  ExecuteNotation(s, spinEdtAnimationSpeed.Value);

  IsRunning := False;
  btnExecute.Caption := '🚀 Execute';
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

procedure TfrmMain.btnMoveClick(Sender: TObject);
var
  n: integer;
  tmp: tcube3d;
  i, v: integer;
begin
  n := TSpeedButton(Sender).Tag;
  v := spinEdtAnimationSpeed.Value;

  tmp := cube3d;
  for i := 0 to 90 do
  begin
    if v = 11 then Continue;
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
  ExecuteSolverAndParseOutput(CubeToDefinitionString(CurrentCubeState), memSolveSummary, edtMoveString);
end;

procedure TfrmMain.edtMoveStringKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  //if Key = VK_RETURN then btnExecuteClick(Sender);
end;


procedure TfrmMain.edtMoveStringKeyPress(Sender: TObject; var Key: char);
begin

  if Key in ['u', 'd', 'l', 'r', 'f', 'b'] then
  begin
    Key := UpCase(Key);
  end else if not (Key in ['U', 'D', 'L', 'R', 'F', 'B', '2', '''', ' ', #13, #8]) then
  begin
    Key := #0;
  end;
end;

procedure TfrmMain.ExecuteSolverAndParseOutput(const faceString: string; aMemo: TMemo; MoveString: TMemo);
var
  Process: TProcess;
  OutputLines: TStringList;
  i, dotCount: integer;
begin
  Screen.Cursor := crHourGlass;
  MoveString.Text := '2-Phase solver running... Please Wait';
  dotCount := 0;

  Process := TProcess.Create(nil);
  OutputLines := TStringList.Create;
  try
    {$IFDEF Linux}
    Process.Executable := './linux-2phase';
    {$ENDIF}
    {$IFDEF Windows}
    Process.Executable := 'win64-2phase.exe';
    {$ENDIF}
    Process.Parameters.Add(faceString);
    Process.Options := Process.Options + [poUsePipes, poNoConsole];
    Process.Execute;

    while Process.Running do
    begin
      Application.ProcessMessages;
      Inc(dotCount);
      if dotCount > 20 then dotCount := 1;
      MoveString.Text := '2-Phase solver running... Please Wait' + StringOfChar('.', dotCount);
      Sleep(200);
    end;

    OutputLines.LoadFromStream(Process.Output);

    aMemo.Lines.Clear;
    for i := 0 to OutputLines.Count - 1 do
      aMemo.Lines.Add(OutputLines[i]);

    // Assume the solution is on the second line and display it in MoveString
    if OutputLines.Count >= 2 then
    begin
      OutputLines[1] := FormatMovesString(OutputLines[1]);
      MoveString.Text := OutputLines[1];
    end else
      MoveString.Text := 'Error: No solution found.';
  finally
    OutputLines.Free;
    Process.Free;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.ManualRotateFace(Face: integer; clockWise: boolean);
var
  tmp: tcube3d;
  i, v, n: integer;
begin

  if clockWise then n := Face + 10
  else
    n := Face;
  v := spinEdtAnimationSpeed.Value;

  tmp := cube3d;
  for i := 0 to 90 do
  begin
    if (v > 7) and (i mod 10 <> 0) then Continue;
    if i mod v = 0 then ActiveSleep(Round(-2 * v) + 20);

    Rotate3dface(cube3d, n mod 10 + 1, (((n div 10) * 2 - 1) * i) * pi / 180);
    pntBox3Dview.Refresh;
    cube3d := tmp;
  end;

  Rotateface(TUnitRubik(CurrentCubeState), face mod 10 + 1, (n div 10) * 2 + 1);
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  pntBox3Dview.Refresh;

end;

procedure TfrmMain.FastRotateFace(Face: integer; clockWise: boolean);
var
  tmp: tcube3d;
  i, v, n: integer;
begin

  if clockWise then n := Face + 10
  else
    n := Face;
  v := spinEdtAnimationSpeed.Value;

  tmp := cube3d;
  for i := 0 to 90 do
  begin
    if i mod 20 <> 0 then Continue;
    Rotate3dface(cube3d, n mod 10 + 1, (((n div 10) * 2 - 1) * i) * pi / 180);
    pntBox3Dview.Refresh;
    cube3d := tmp;
  end;

  Rotateface(TUnitRubik(CurrentCubeState), face mod 10 + 1, (n div 10) * 2 + 1);
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  pntBox3Dview.Refresh;
end;



function TfrmMain.GenerateRandomScramble(MoveCount: integer): string;
const
  Moves: array[0..5] of string = ('U', 'D', 'L', 'R', 'F', 'B');
  Modifiers: array[0..2] of string = ('', '''', '2');
var
  LastMove, MoveIndex, ModifierIndex: integer;
  Scramble: string;
  i: integer;
begin
  Scramble := '';
  LastMove := -1; // Initialize with an impossible move index

  for i := 1 to MoveCount do
  begin
    // Ensure the next move is different from the last move
    repeat
      MoveIndex := Random(Length(Moves));
    until (MoveIndex <> LastMove) and ((i < 2) or (Moves[MoveIndex] <> Moves[LastMove]));

    ModifierIndex := Random(Length(Modifiers));

    if Scramble <> '' then
      Scramble := Scramble + ' ';
    Scramble := Scramble + Moves[MoveIndex] + Modifiers[ModifierIndex];

    LastMove := MoveIndex;
  end;

  Result := Scramble;
end;

procedure TfrmMain.btnSearchForSolutionClick(Sender: TObject);
var
  s: string;
  tmp: TFaceRubik;
begin
  memSolveSummary.Clear;
  s := '';
  if not VerifyCube(CurrentCubeState, s) then
  begin
    memSolveSummary.Lines.add('The cube has been disassembled or tampered with:');
    memSolveSummary.Lines.add(s);
    exit;
  end;

  solu := '';
  tmp := CurrentCubeState;

  // Step 1
  placeWhiteEdges(tmp);
  memSolveSummary.Lines.Add('---> placeWhiteEdges');
  memSolveSummary.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  LFDstringCorrection(solu);
  s := solu;
  solu := '';
  // Step 2
  placeWhiteCorners(tmp);
  memSolveSummary.Lines.Add('---> placeWhiteCorners');
  memSolveSummary.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  LFDstringCorrection(solu);
  s := s + solu;
  solu := '';
  // Step 3
  placeSecondLayerEdges(tmp);
  memSolveSummary.Lines.Add('---> placeSecondLayerEdges');
  memSolveSummary.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  LFDstringCorrection(solu);
  s := s + solu;
  solu := '';
  // Step 4
  PlaceYellowEdges(tmp);
  memSolveSummary.Lines.Add('---> PlaceYellowEdges');
  memSolveSummary.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  LFDstringCorrection(solu);
  s := s + solu;
  solu := '';
  // Step 5
  OrientYellowEdges(tmp);
  memSolveSummary.Lines.Add('---> OrientYellowEdges');
  memSolveSummary.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  LFDstringCorrection(solu);
  s := s + solu;
  solu := '';
  // Step 6
  PlaceYellowCorners(tmp);
  memSolveSummary.Lines.Add('---> PlaceYellowCorners');
  memSolveSummary.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  LFDstringCorrection(solu);
  s := s + solu;
  solu := '';
  // Step 7
  OrientYellowCorners(tmp);
  memSolveSummary.Lines.Add('---> OrientYellowCorners');
  memSolveSummary.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  LFDstringCorrection(solu);
  s := s + solu;
  solu := '';

  memSolveSummary.Lines.Add('movements ' + IntToStr(CountMoves(s)));
  FilterMoves(s, CurrentCubeState);
  pntBoxCurrentState.Refresh;
  memSolveSummary.Lines.Add('movements after filter ' + IntToStr(CountMoves(s)) + ')');
  memSolveSummary.Lines.Add('');
  memSolveSummary.Lines.Add('---> Filter');
  memSolveSummary.Lines.Add(s);
  UpperCase(s);
  s := FormatMovesString(s);
  edtMoveString.Text := s;
end;


end.
