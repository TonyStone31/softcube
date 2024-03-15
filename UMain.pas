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
  StdCtrls, Dialogs,
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
    btnDefault3Dview: TSpeedButton;
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
    memRandScramble: TMemo;
    lblCurrentMove: TLabel;
    lblSingMaster: TLabel;
    lblSingMaster1: TLabel;
    lblSpeedControl: TLabel;
    lblNoticeTarget: TLabel;
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
    procedure btnControlHelpClick(Sender: TObject);
    procedure btn3DViewRotateRightClick(Sender: TObject);
    procedure btn3DViewRotate90UpClick(Sender: TObject);
    procedure btnScrambleTargetClick(Sender: TObject);
    procedure btnScrampleStateClick(Sender: TObject);
    procedure btnTargetSolveResetClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnCurrentStateResetClick(Sender: TObject);
    procedure btnDefault3DviewClick(Sender: TObject);
    procedure btnRightClockClick(Sender: TObject);
    procedure btn2phaseSolveClick(Sender: TObject);
    procedure edtMoveStringKeyPress(Sender: TObject; var Key: char);
    procedure edtMoveStringKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
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
    function GenerateRandomMovesString(MoveCount: Integer): string;
    procedure RandomRotateFaces(Count: integer);
    procedure RotateCubeLeft;
    procedure RotateCubeUp();
    procedure ToggleButtonsExcept(Form: TForm; ExceptButton: TSpeedButton; Enable: boolean);
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

procedure TfrmMain.RotateCubeUp();
const
  ViewPortChange90: double = 90 * Pi / 180;
var
  AngleX, AngleY: double;
begin
  { #todo -oTonyStone : Yeah this is horrible.  Some day i will revisit and make it rotate the 3D view by 90 degrees only in any direction.  and animate it as well.  For now it is giving me a headache :( }
  // Always apply a tilt downwards to view the top
  AngleX := 180 * Pi / 360; // Tilt angle to always show the top

  AngleX := AngleX + ViewPortChange90;


  Rotate3d(Cube3D, AngleX, 0, 0);


  DrawCube3d(pntBox3Dview, CurrentCubeState, Cube3D);
end;

procedure TfrmMain.RotateCubeLeft();
const
  ViewPortChange90: double = 90 * Pi / 180;
var
  AngleX, AngleY: double;
begin
  AngleX := -180 * Pi / 360; // Tilt angle to always show the top
  AngleX := AngleX + ViewPortChange90;
  Rotate3d(Cube3D, AngleX, 0, 0);



  AngleY := 90 * Pi / 360;
  AngleY := AngleY + ViewPortChange90;

  //AngleY := -ViewPortChange90; // Rotate left
  //  rdRight: AngleY := ViewPortChange90; // Rotate right
  //  rdUp: AngleX := AngleX + ViewPortChange90; // Adjust up rotation including initial tilt
  //  rdDown: AngleX := AngleX - ViewPortChange90; // Adjust down rotation including initial tilt
  //end;

  // Apply the rotation
  //if AngleY <> 0 then
  //  Rotate3d(Cube3D, 0, AngleY, 0); // Rotate around Y-axis for left/right
  // Always apply the X rotation for the tilt, potentially adjusted for up/down
  Rotate3d(Cube3D, 0, AngleY, 0);

  // Redraw the cube with the new orientation
  DrawCube3d(pntBox3Dview, CurrentCubeState, Cube3D);
end;


procedure TfrmMain.btn3DViewRotate90LeftClick(Sender: TObject);
begin
  RotateCubeLeft;
end;

procedure TfrmMain.btn3DViewRotate90UpClick(Sender: TObject);
begin
  RotateCubeUp;
end;

procedure TfrmMain.btn3DViewRotateRightClick(Sender: TObject);
begin
  RotateCubeLeft;
end;


procedure TfrmMain.tglKeyBoardControlChange(Sender: TObject);
begin
  keyBoardControlActive := tglKeyBoardControl.Checked;
end;

procedure TfrmMain.btnDefault3DviewClick(Sender: TObject);
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
  InputStr: string;
begin
  Randomize;

  // Prompt the user for the number of scramble moves
  InputStr := InputBox('Scramble Cube', 'Enter the number of scramble moves:', '15');

  // Verify the input is a valid number and not more than 100
  if TryStrToInt(InputStr, NumMoves) then
  begin
    if NumMoves > 100 then
      NumMoves := 100; // Cap the number of moves at 100
    RandomRotateFaces(NumMoves);
  end
  else
    ShowMessage('Please enter a valid number.'); // Display error message if input is not a number
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
  f, i, ii, j, v: integer;
  s, lblS: string;
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
      if (v > 7) and (ii mod 4 <> 0) then Continue;
      if not IsRunning then Break;


      if j = 1 then
      begin
        ActiveSleep(Round(-2 * v) + 20);
        Rotate3dface(cube3d, f, -ii * pi / 180);
      end else if j = 2 then
      begin
        ActiveSleep(Round(-2 * v) + 40);
        Rotate3dface(cube3d, f, -ii * pi / 90);
      end else if j = 3 then
      begin
        ActiveSleep(Round(-2 * v) + 20);
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
  n := TSpeedButton(Sender).Tag;
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
  ExecuteSolverAndParseOutput(CubeToDefinitionString(CurrentCubeState), memSolveSummary, edtMoveString);
end;

procedure TfrmMain.edtMoveStringKeyPress(Sender: TObject; var Key: char);
begin
  // Convert lowercase Singmaster notation characters to uppercase
  if Key in ['u', 'd', 'l', 'r', 'f', 'b'] then
  begin
    Key := UpCase(Key); // Convert to uppercase
  end // Allow uppercase letters, the number 2, apostrophe, space, and Enter.
  else if not (Key in ['U', 'D', 'L', 'R', 'F', 'B', '2', '''', ' ', #13]) then
  begin
    // If the key pressed is not in the allowed list, ignore the key press
    Key := #0; // Discard the key press by setting Key to #0
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
    if (v > 7) and (i mod 10 <> 0) then Continue;
    Rotate3dface(cube3d, n mod 10 + 1, (((n div 10) * 2 - 1) * i) * pi / 180);
    pntBox3Dview.Refresh;
    cube3d := tmp;
  end;

  Rotateface(TUnitRubik(CurrentCubeState), face mod 10 + 1, (n div 10) * 2 + 1);
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  pntBox3Dview.Refresh;

end;

procedure TfrmMain.RandomRotateFaces(Count: integer);
var
  i, RandomFace, LastFace: integer;
  ClockWise, LastClockWise: boolean;
  MoveStr, Moves: string;
  FaceStr: array[0..5] of string = ('U', 'F', 'R', 'B', 'L', 'D');
begin
  ToggleButtonsExcept(Self, btnDefault3Dview, False);
  memRandScramble.Lines.Clear; // Clear previous content
  memRandScramble.Lines.Add('Random scramble executed:');
  Moves := ''; // Initialize the string that will hold all moves

  // Initialize LastFace with a value that won't match any valid face index
  LastFace := -1;
  LastClockWise := False; // Initial value doesn't matter, it will be set in the loop

  for i := 1 to Count do
  begin
    repeat
      RandomFace := Random(6); // Select a random face index
      ClockWise := Random(2) = 0; // Select clockwise or counterclockwise
    until not ((RandomFace = LastFace) and (ClockWise <> LastClockWise)); // Avoid immediate reversal

    // Perform the rotation only if it doesn't cancel the previous move
    FastRotateFace(RandomFace, ClockWise);

    // Convert the move to Singmaster notation
    MoveStr := FaceStr[RandomFace];
    if ClockWise then MoveStr := MoveStr + '''';

    // Append the move to the Moves string separated by a space
    if Moves <> '' then
      Moves := Moves + ' ' + MoveStr
    else
      Moves := MoveStr;

    // Store this move as the last move for comparison in the next iteration
    LastFace := RandomFace;
    LastClockWise := ClockWise;
  end;

  // After collecting all moves, append them to the memo on the second line
  LFDstringCorrection(Moves);
  memRandScramble.Lines.Add(Moves);

  ToggleButtonsExcept(Self, btnDefault3Dview, True);
end;



function TfrmMain.GenerateRandomMovesString(MoveCount: Integer): string;
const
  Faces: array[0..5] of string = ('U', 'D', 'L', 'R', 'F', 'B');
  Modifiers: array[0..2] of string = ('', '2', '''');
var
  i, FaceIndex, ModifierIndex: Integer;
  LastFaceIndex, LastModifierIndex: Integer;
  Move, LastMove: string;
  Moves: TStringList;
begin
  Moves := TStringList.Create;
  try
    Moves.Delimiter := ' '; // Use space as the delimiter
    Moves.StrictDelimiter := True; // Don't treat consecutive delimiters as one

    LastFaceIndex := -1; // Initialize with an impossible value
    LastModifierIndex := -1; // Initialize with an impossible value

    for i := 1 to MoveCount do
    begin
      repeat
        FaceIndex := Random(Length(Faces)); // Select a random face
        ModifierIndex := Random(Length(Modifiers)); // Select a random modifier ('', '2', or ''')
        Move := Faces[FaceIndex] + Modifiers[ModifierIndex];
      until not (
                  (FaceIndex = LastFaceIndex) and // Prevent the same face being selected twice in a row without modifier change
                  ((ModifierIndex = LastModifierIndex) or // Prevent same move or direct reversal (e.g., R R' or R' R)
                  ((ModifierIndex = 0) and (LastModifierIndex = 2)) or
                  ((ModifierIndex = 2) and (LastModifierIndex = 0)))
                );

      Moves.Add(Move); // Append the move to the list

      // Update last move trackers
      LastFaceIndex := FaceIndex;
      LastModifierIndex := ModifierIndex;
      LastMove := Move;
    end;

    Result := Moves.DelimitedText; // Convert the list to a delimited string
  finally
    Moves.Free;
  end;
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
  edtMoveString.Text := s;
end;


end.
