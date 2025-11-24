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
  GraphMath,
  Math,
  Spin,
  ComCtrls,
  Buttons,
  Dialogs,
  StdCtrls,
  BGRAVirtualScreen,
  BGRABitmap,
  BGRABitmapTypes,
  strutils,
  UConst,
  UDraw,
  UWebcamScan,
  process,
  Types,
  URubik;

type
  TRotationDirection = (rdLeft, rdRight, rdUp, rdDown);

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btn2phaseSolve: TSpeedButton;
    btn3DViewRotate90Left: TSpeedButton;
    btn3DviewReset: TSpeedButton;
    btnBackClock: TSpeedButton;
    btnBackCounter: TSpeedButton;
    btnControlHelp: TSpeedButton;
    btn3DViewRotateRight: TSpeedButton;
    btnCurrentStateReset: TSpeedButton;
    btnDownClock: TSpeedButton;
    btnDownCounter: TSpeedButton;
    btn3DViewRotate90Up: TSpeedButton;
    btnExecute: TSpeedButton;
    btnExecuteUntilSolved: TSpeedButton;
    btnStepBackward: TSpeedButton;
    btnStepForward: TSpeedButton;
    pnlPlaybackControls: TPanel;
    btnFrontClock: TSpeedButton;
    btnFrontCounter: TSpeedButton;
    btnLeftClock: TSpeedButton;
    btnLeftCounter: TSpeedButton;
    btnRightClock: TSpeedButton;
    btnRightCounter: TSpeedButton;
    btnScrambleTarget: TSpeedButton;
    btnScrampleState: TSpeedButton;
    btnScanWebcam: TSpeedButton;
    btnSearchForSolution: TSpeedButton;
    btnTargetSolveReset: TSpeedButton;
    btnUpClock: TSpeedButton;
    btnUpCounter: TSpeedButton;
    edtMoveString: TMemo;
    FlowPanelSolveButtons: TFlowPanel;
    lblCubeSize: TLabel;
    lblCubeSizeInfo: TLabel;
    lblCurrentMove: TLabel;
    lblPeakUnder: TLabel;
    lblSingMaster: TLabel;
    lblSpeedControl: TLabel;
    lblNoticeTarget: TLabel;
    memRandScramble: TMemo;
    memSolveSummary: TMemo;
    pnlCubeControls: TPanel;
    pnlFaceControls: TPanel;
    pnlSettings: TPanel;
    pnlSetState: TPanel;
    pntBox3Dview: TBGRAVirtualScreen;
    pntBoxCurrentState: TPaintBox;
    spinEdtCubeSize: TSpinEdit;
    SplitterVerticalMain: TSplitter;
    SplitterHorizontalMain: TSplitter;
    chkKeyBoardControl: TCheckBox;
    tmrMarchingAnts: TTimer;
    ts2DViews: TPageControl;
    pnlDestination: TPanel;
    pnl3Dview: TPanel;
    pnlSolution: TPanel;
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
    procedure btnExecuteUntilSolvedClick(Sender: TObject);
    procedure btnScrambleTargetClick(Sender: TObject);
    procedure btnScrampleStateClick(Sender: TObject);
    procedure btnScanWebcamClick(Sender: TObject);
    procedure btnTargetSolveResetClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnStepBackwardClick(Sender: TObject);
    procedure btnStepForwardClick(Sender: TObject);
    procedure btnCurrentStateResetClick(Sender: TObject);
    procedure btn3DviewResetClick(Sender: TObject);
    procedure btnMoveClick(Sender: TObject);
    procedure btn2phaseSolveClick(Sender: TObject);
    procedure edtMoveStringKeyPress(Sender: TObject; var Key: char);
    procedure edtMoveStringKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure lblPeakUnderMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure lblPeakUnderMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure pntBox3DviewMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure pntBoxCurrentStateMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
    procedure pntBoxCurrentStateMouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
    procedure ts2DViewsChange(Sender: TObject);
    procedure pntBoxTargetSolveMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure pntBoxTargetSolvePaint(Sender: TObject);
    procedure pntBoxCurrentStateMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure pntBoxCurrentStatePaint(Sender: TObject);
    procedure pntBox3DviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure pntBox3DviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure btnSearchForSolutionClick(Sender: TObject);
    procedure ManualRotateFace(Face: integer; clockWise: boolean);
    procedure SetInitialCubeView;
    procedure spinEdtCubeSizeChange(Sender: TObject);
    procedure chkKeyBoardControlChange(Sender: TObject);
    procedure tmrMarchingAntsTimer(Sender: TObject);
  private
    procedure ActiveSleep(ms: cardinal);
    procedure ExecuteSolverAndParseOutput(const faceString: string;
      aMemo: TMemo; MoveString: TMemo);
    procedure FastRotateFace(Face: integer; clockWise: boolean);
    function GenerateRandomScramble(MoveCount: integer): string;
    procedure RotateCubeLeftRight(Direction: integer);
    procedure RotateCubePeakUnder();
    procedure RotateCubeFlipUp();
    procedure RotateCubeUnPeakUnder();
    procedure ToggleButtonsExcept(Form: TForm; ExceptButton: TSpeedButton;
      Enable: boolean);
    procedure ParseNotationMoves(const s: string);
    procedure ExecuteSingleMove(MoveIndex: integer; Animated: boolean);
    procedure ExecuteSingleMoveReverse(MoveIndex: integer; Animated: boolean);
    procedure HighlightCurrentMove;
    procedure UpdatePlaybackButtons;
    procedure EnterExecutionMode;
    procedure ExitExecutionMode;
  public
    procedure ExecuteNotation(var s: string; SpeedVal: integer);

  end;

type
  TPeekState = (psNormal, psPeakingUp, psPeaked, psPeakingDown);
  TExecutionState = (esIdle, esExecuting, esPaused);

  TMoveRecord = record
    Face: integer;      // CUBE_LEFT, CUBE_RIGHT, etc.
    Turns: integer;     // 1, 2, or 3
    StartPos: integer;  // Position in original string
    Length: integer;    // Length in original string (1 or 2 chars)
  end;

var
  frmMain: TfrmMain;
  tmx: integer = 0;
  tmy: integer = 0;
  IsRunning: boolean = False;
  Cube3DTransActive: boolean;
  PeekState: TPeekState = psNormal;
  WantToUnPeak: boolean = False;
  mouseDrag3D: boolean;
  FaceCodeMover: integer = 0;
  keyBoardControlActive: boolean;

  // Playback control variables
  ExecutionState: TExecutionState = esIdle;
  ParsedMoves: array of TMoveRecord;
  CurrentMoveIndex: integer = -1;
  OriginalNotation: string = '';

implementation

{$R *.lfm}

procedure TfrmMain.ToggleButtonsExcept(Form: TForm; ExceptButton: TSpeedButton;
  Enable: boolean);
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
  i: integer;
  f: integer;
  StartTime, ElapsedMs, TargetDuration: QWord;
  Progress, CurrentAngle, TargetAngle: Double;
begin
  LFDstringCorrection(s);
  //Really think what I did here is hackish because I will need to invert the 3 sides
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
    end
    else if (i < length(s)) and (s[i + 1] = '2') then
    begin
      j := 2;
      lblS := s[i] + s[i + 1];

    end
    else
    begin
      lblS := s[i];
      LFDstringCorrection(lblS);
    end;
    lblCurrentMove.Caption := lblS;
    // DON'T call ProcessMessages here - causes slowdown!

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

    // Read current speed from control (allows changing speed during execution)
    SpeedVal := spinEdtAnimationSpeed.Value;

    // Skip animation entirely for speed 11 only
    if (SpeedVal >= 11) then
    begin
      RotateFace(TUnitRubik(CurrentCubeState), f, j);
      // Don't refresh every move - just at the end
      if j > 1 then Inc(i, 2)
      else Inc(i);
      lblCurrentMove.Caption := ' ';
      continue;
    end;

    // Time-based animation (speeds 1-10)
    case SpeedVal of
      1: TargetDuration := 5000;
      2: TargetDuration := 3000;
      3: TargetDuration := 2000;
      4: TargetDuration := 1500;
      5: TargetDuration := 1000;
      6: TargetDuration := 700;
      7: TargetDuration := 500;
      8: TargetDuration := 350;
      9: TargetDuration := 250;
      10: TargetDuration := 150;
    else
      TargetDuration := 1000;
    end;

    // Scale duration - only j=2 takes 2x as long (j=3 is still just 90°!)
    if j = 2 then
      TargetDuration := TargetDuration * 2;

    // Calculate target angle: j=2 is 180°, j=1 and j=3 are 90°
    if j = 2 then
      TargetAngle := 180
    else
      TargetAngle := 90;

    tmp := cube3d;
    StartTime := GetTickCount64;

    while True do
    begin
      ElapsedMs := GetTickCount64 - StartTime;
      if ElapsedMs >= TargetDuration then Break;
      if not IsRunning then Break;

      // Re-read speed to allow mid-animation changes
      SpeedVal := spinEdtAnimationSpeed.Value;

      // Linear progress from 0.0 to 1.0
      Progress := ElapsedMs / TargetDuration;

      // Apply easing for natural movement (slow start → fast → slow end)
      Progress := EaseInOutQuad(Progress);

      CurrentAngle := Progress * TargetAngle;

      // Direction: j=1 and j=2 go negative (one direction), j=3 goes positive (opposite)
      if j = 3 then
        Rotate3dface(cube3d, f, CurrentAngle * pi / 180)
      else
        Rotate3dface(cube3d, f, -CurrentAngle * pi / 180);

      // DrawCube3d handled by OnRedraw event
      pntBox3Dview.DiscardBitmap;
      Application.ProcessMessages;

      cube3d := tmp;
      Sleep(1);  // Prevent CPU burn
    end;

    RotateFace(TUnitRubik(CurrentCubeState), f, j);
    DrawCube(pntBoxCurrentState, CurrentCubeState);
    // DrawCube3d handled by OnRedraw event
    pntBoxCurrentState.Refresh;
    pntBox3Dview.DiscardBitmap;
    Application.ProcessMessages;

    if j > 1 then Inc(i, 2)
    else
      Inc(i);
    lblCurrentMove.Caption := ' ';
  end;

  // Final refresh at the end for speed 8+
  // DrawCube3d handled by OnRedraw event
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  pntBoxCurrentState.Refresh;
  pntBox3Dview.DiscardBitmap;

end;

procedure TfrmMain.ActiveSleep(ms: cardinal);
var
  TargetTime: QWord;
  StartTime: QWord;
  LoopCount: Integer;
begin
  // Quick exit if no sleep needed
  if ms = 0 then Exit;

  StartTime := GetTickCount64;
  TargetTime := GetTickCount64 + ms;
  LoopCount := 0;

  while GetTickCount64 < TargetTime do
  begin
    Application.ProcessMessages;
    Sleep(1);
    Inc(LoopCount);

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

  // Ensure rotation flag starts in correct state
  Cube3DTransActive := False;
  mouseDrag3D := False;


  SetInitialCubeView;

  // Initialize cube size info label
  spinEdtCubeSizeChange(nil);

  // Initialize playback buttons
  UpdatePlaybackButtons;
end;

procedure TfrmMain.spinEdtCubeSizeChange(Sender: TObject);
var
  CubeSize: Integer;
  InfoText: string;
begin
  CubeSize := spinEdtCubeSize.Value;

  // Update the info label with cube size and name
  case CubeSize of
    2: InfoText := '(2x2x2 - Pocket Cube)';
    3: InfoText := '(3x3x3 - Standard)';
    4: InfoText := '(4x4x4 - Rubik''s Revenge)';
    5: InfoText := '(5x5x5 - Professor''s Cube)';
    6: InfoText := '(6x6x6 - V-Cube 6)';
    7: InfoText := '(7x7x7 - V-Cube 7)';
    else
      InfoText := Format('(%dx%dx%d)', [CubeSize, CubeSize, CubeSize]);
  end;

  lblCubeSizeInfo.Caption := InfoText;

  // TODO: When TGenericCube is integrated:
  // - Free current generic cube instance
  // - Create new TGenericCube with selected size
  // - Update drawing system to use new cube
  // - Refresh displays
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  ActualFace: Integer;
  IsClockwise: Boolean;
begin
  // Keyboard control system:
  // Up/Down arrows: cycle through visible faces
  // Left/Right arrows: rotate selected face (direction based on face position)
  // A/D keys: rotate 3D view left/right
  // W key: flip view 180°
  // S key: reset view to initial position

  if not keyBoardControlActive then Exit;
  if IsRunning then Exit;
  if Cube3DTransActive then Exit;  // Don't allow input during view rotation

  case Key of
    VK_UP:
    begin
      // Cycle to previous visible face
      SelectedVisibleFace := (SelectedVisibleFace + 2) mod 3;
      pntBox3Dview.DiscardBitmap;
      Key := 0;
    end;

    VK_DOWN:
    begin
      // Cycle to next visible face
      SelectedVisibleFace := (SelectedVisibleFace + 1) mod 3;
      pntBox3Dview.DiscardBitmap;
      Key := 0;
    end;

    VK_LEFT:
    begin
      // Rotate selected face with "push" logic
      // The direction depends on:
      // 1. Which screen position (left vs right of screen)
      // 2. Which actual cube face is there (determines base direction)
      // 3. Whether cube is flipped (inverts side face directions)
      ActualFace := VisibleFaces[SelectedVisibleFace];

      case SelectedVisibleFace of
        0: begin
          // Top or Bottom face - Left arrow = CCW when top, CW when bottom
          if ActualFace = 1 then
            IsClockwise := False
          else
            IsClockwise := True;
        end;
        1: begin
          // Left side of screen
          // Front(2) and Right(3): Left = CCW
          // Back(4) and Left(5): Left = CW
          if (ActualFace = 2) or (ActualFace = 3) then
            IsClockwise := False
          else
            IsClockwise := True;
          // Invert when cube is flipped
          if ViewFlipped then
            IsClockwise := not IsClockwise;
          // Invert for odd number of A/D rotations
          if Odd(ViewRotationY) then
            IsClockwise := not IsClockwise;
        end;
        2: begin
          // Right side of screen (opposite of left side)
          // Front(2) and Right(3): Left = CW
          // Back(4) and Left(5): Left = CCW
          if (ActualFace = 2) or (ActualFace = 3) then
            IsClockwise := True
          else
            IsClockwise := False;
          // Invert when cube is flipped
          if ViewFlipped then
            IsClockwise := not IsClockwise;
          // Invert for odd number of A/D rotations
          if Odd(ViewRotationY) then
            IsClockwise := not IsClockwise;
        end;
      else
        IsClockwise := False;
      end;

      // Debug: show current state in caption
      Caption := Format('LEFT: Sel=%d Face=%d Flip=%s Rot=%d CW=%s',
        [SelectedVisibleFace, ActualFace, BoolToStr(ViewFlipped, 'Y', 'N'),
         ViewRotationY, BoolToStr(IsClockwise, 'CW', 'CCW')]);

      ManualRotateFace(ActualFace - 1, IsClockwise);
      Key := 0;
    end;

    VK_RIGHT:
    begin
      // Rotate selected face - opposite direction of VK_LEFT
      ActualFace := VisibleFaces[SelectedVisibleFace];

      case SelectedVisibleFace of
        0: begin
          // Top or Bottom face - Right arrow = CW when top, CCW when bottom
          if ActualFace = 1 then
            IsClockwise := True
          else
            IsClockwise := False;
        end;
        1: begin
          // Left side of screen
          // Front(2) and Right(3): Right = CW
          // Back(4) and Left(5): Right = CCW
          if (ActualFace = 2) or (ActualFace = 3) then
            IsClockwise := True
          else
            IsClockwise := False;
          // Invert when cube is flipped
          if ViewFlipped then
            IsClockwise := not IsClockwise;
          // Invert for odd number of A/D rotations
          if Odd(ViewRotationY) then
            IsClockwise := not IsClockwise;
        end;
        2: begin
          // Right side of screen (opposite of left side)
          // Front(2) and Right(3): Right = CCW
          // Back(4) and Left(5): Right = CW
          if (ActualFace = 2) or (ActualFace = 3) then
            IsClockwise := False
          else
            IsClockwise := True;
          // Invert when cube is flipped
          if ViewFlipped then
            IsClockwise := not IsClockwise;
          // Invert for odd number of A/D rotations
          if Odd(ViewRotationY) then
            IsClockwise := not IsClockwise;
        end;
      else
        IsClockwise := True;
      end;

      // Debug: show current state in caption
      Caption := Format('RIGHT: Sel=%d Face=%d Flip=%s Rot=%d CW=%s',
        [SelectedVisibleFace, ActualFace, BoolToStr(ViewFlipped, 'Y', 'N'),
         ViewRotationY, BoolToStr(IsClockwise, 'CW', 'CCW')]);

      ManualRotateFace(ActualFace - 1, IsClockwise);
      Key := 0;
    end;

    Ord('A'), Ord('a'):
    begin
      // Rotate view left (same as left button)
      RotateCubeLeftRight(1);
      Key := 0;
    end;

    Ord('D'), Ord('d'):
    begin
      // Rotate view right (same as right button)
      RotateCubeLeftRight(-1);
      Key := 0;
    end;

    Ord('W'), Ord('w'):
    begin
      // Flip view 180° (same as up button)
      RotateCubeFlipUp;
      Key := 0;
    end;

    Ord('S'), Ord('s'):
    begin
      // Reset view to initial position
      SetInitialCubeView;
      Key := 0;
    end;
  end;
end;

procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  // Key handling moved to FormKeyDown for immediate response
  // This is kept for compatibility but most logic is in KeyDown now
  if not keyBoardControlActive then Exit;

  // Consume handled keys
  case Key of
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN,
    Ord('A'), Ord('a'), Ord('D'), Ord('d'),
    Ord('W'), Ord('w'), Ord('S'), Ord('s'): Key := 0;
  end;
end;

procedure TfrmMain.lblPeakUnderMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button <> mbLeft then exit;

  // Only start peeking if we're in normal state
  if PeekState <> psNormal then Exit;
  if Cube3DTransActive then Exit;

  WantToUnPeak := False;
  RotateCubePeakUnder();

  // After peek completes, check if mouse was released during animation
  if WantToUnPeak then
    RotateCubeUnPeakUnder();
end;

procedure TfrmMain.lblPeakUnderMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
begin
  if Button <> mbLeft then exit;

  // If we're peeking up, set flag to unpeak after animation completes
  if PeekState = psPeakingUp then
  begin
    WantToUnPeak := True;
    Exit;
  end;

  // If we're fully peaked, start unpeeking
  if PeekState = psPeaked then
    RotateCubeUnPeakUnder();
end;

procedure TfrmMain.pntBox3DviewMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
var
  i: integer;
  clickedPolygonIndex: integer;
  closestZ: single;
  selectedFace, selectedCubelet: integer;
  currentColorIndex: integer;
begin
  clickedPolygonIndex := -1;
  closestZ := -MaxInt; // Start with the smallest possible value for comparison.

  // Iterate over the polygons and determine if the mouse point is inside any of them
  for i := 0 to High(PolyOrder) do
  begin
    // Only proceed if the polygon is not a back face and is drawn
    if (PolyOrder[i].order <> -1) and (PolyOrder[i].color <> clBlack) then
    begin
      if PointInPolygon(MousePos, PolyOrder[i].pt) then
      begin
        // If this polygon is closer to the viewer (greater Z), select it
        if PolyOrder[i].z > closestZ then
        begin
          clickedPolygonIndex := PolyOrder[i].order;
          closestZ := PolyOrder[i].z;
        end;
      end;
    end;
  end;

  // If a valid polygon was under the mouse pointer, update the color
  if clickedPolygonIndex <> -1 then
  begin
    // Adjust the calculation of the face and cubelet
    selectedFace := (clickedPolygonIndex div 45) + 1; // 45 polygons per face
    selectedCubelet := (clickedPolygonIndex mod 45) div 5; // Each cubelet has 5 polygons

    // Skip if the clicked cubelet is the middle one (cubelet 4)
    if selectedCubelet = 4 then
      Exit;

    // Get the current color index of the clicked cubelet
    currentColorIndex := CurrentCubeState[selectedFace, selectedCubelet];

    // Adjust the colorIndex based on WheelDelta
    if WheelDelta > 0 then
    begin
      // Scrolled up: Cycle forward through the colors 2 to 5
      currentColorIndex := ((currentColorIndex - 2 + 1) mod 4) + 2;
    end
    else if WheelDelta < 0 then
    begin
      // Scrolled down: Cycle backward through the colors 2 to 5
      if currentColorIndex = 2 then
        currentColorIndex := 5
      else if currentColorIndex > 2 then
        Dec(currentColorIndex) // Move to the previous color
      else
        currentColorIndex := 2; // Default to 2 if outside the target range
    end;

    // Set the new color to the clicked cubelet
    CurrentCubeState[selectedFace, selectedCubelet] := currentColorIndex;

    // Update the form caption to indicate which face and cubelet were modified
    frmMain.Caption := Format('Mouse Wheel on Face: %d, Cubelet: %d',
      [selectedFace, selectedCubelet]);

    // Trigger a redraw to reflect the change
    // DrawCube3d handled by OnRedraw event
    DrawCube(pntBoxCurrentState, CurrentCubeState);
    pntBoxCurrentState.Refresh;
    pntBox3Dview.DiscardBitmap;
  end
  else
  begin
    frmMain.Caption := 'No valid cubelet under mouse pointer';
  end;

  Handled := True;
end;


procedure TfrmMain.ts2DViewsChange(Sender: TObject);
begin
  pntBox3Dview.DiscardBitmap;
end;

procedure TfrmMain.pntBoxTargetSolvePaint(Sender: TObject);
begin
  DrawCube(pntBoxTargetSolve, TargetCubeState);
  // DON'T call Refresh inside Paint handler - causes infinite loop!
end;

procedure TfrmMain.pntBoxCurrentStateMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  colorIndex: integer;
begin
  if IsRunning then Exit;
  colorIndex := GetCubeletColor(CurrentCubeState, Point(x, y));

  if Button = mbLeft then
  begin
    // Cycles colors 2 to 5 with left mouse button
    if (colorIndex >= 2) and (colorIndex < 5) then
      Inc(colorIndex) // Move to the next color
    else if colorIndex = 5 then
      colorIndex := 2 // Wrap back to color 2
    else
      colorIndex := 2; // Default to 2 if outside range
  end
  else if Button = mbRight then
  begin
    // Toggles between colors 1 and 6 with right mouse button
    if colorIndex = 6 then
      colorIndex := 1
    else
      colorIndex := 6;
  end;

  SetCubeletColor(CurrentCubeState, point(x, y), colorIndex);

  DrawCube(pntBoxCurrentState, CurrentCubeState);
  pntBoxCurrentState.Refresh;
  pntBox3Dview.DiscardBitmap;
end;

procedure TfrmMain.pntBox3DviewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  i: integer;
  clickedPolygonIndex: integer;
  clickPoint: TPoint;
  closestZ: single;
  selectedFace, selectedCubelet: integer;
  currentColorIndex: integer;
begin
  clickPoint := Point(X, Y);
  clickedPolygonIndex := -1;
  closestZ := -MaxInt;  // Start with the smallest possible value for comparison.

  // Iterate over the polygons and determine if the click point is inside any of them
  for i := 0 to High(PolyOrder) do
  begin
    // Only proceed if the polygon is not a back face and is drawn
    if (PolyOrder[i].order <> -1) and (PolyOrder[i].color <> clBlack) then
    begin
      if PointInPolygon(clickPoint, PolyOrder[i].pt) then
      begin
        // If this polygon is closer to the viewer (greater Z), select it
        if PolyOrder[i].z > closestZ then
        begin
          clickedPolygonIndex := PolyOrder[i].order;
          closestZ := PolyOrder[i].z;
        end;
      end;
    end;
  end;

  // If a valid polygon was clicked, update the color
  if clickedPolygonIndex <> -1 then
  begin
    // Adjust the calculation of the face and cubelet
    selectedFace := (clickedPolygonIndex div 45) + 1; // 45 polygons per face
    selectedCubelet := (clickedPolygonIndex mod 45) div 5; // Each cubelet has 5 polygons

    // Do not change the color of the middle cubelet
    if selectedCubelet = 4 then
    begin
      frmMain.Caption := Format('Clicked on Face: %d, Cubelet: %d (Center - No Change)',
        [selectedFace, selectedCubelet]);
      Exit;
    end;

    // Get the current color index of the clicked cubelet
    currentColorIndex := CurrentCubeState[selectedFace, selectedCubelet];

    // Cycle colors based on mouse button clicked
    if Button = mbLeft then
    begin
      // Cycles colors 2 to 5 with left mouse button
      if (currentColorIndex >= 2) and (currentColorIndex < 5) then
        Inc(currentColorIndex) // Move to the next color
      else if currentColorIndex = 5 then
        currentColorIndex := 2 // Wrap back to color 2
      else
        currentColorIndex := 2; // Default to 2 if outside range
    end
    else if Button = mbRight then
    begin
      // Toggles between colors 1 and 6 with right mouse button
      if currentColorIndex = 6 then
        currentColorIndex := 1
      else
        currentColorIndex := 6;
    end;

    // Set the new color to the clicked cubelet
    CurrentCubeState[selectedFace, selectedCubelet] := currentColorIndex;

    // Update the form caption to show which face and cubelet was clicked
    frmMain.Caption := Format('Clicked on Face: %d, Cubelet: %d',
      [selectedFace, selectedCubelet]);

    // Trigger a redraw to reflect the change
    // DrawCube3d handled by OnRedraw event
    DrawCube(pntBoxCurrentState, CurrentCubeState);
    pntBoxCurrentState.Refresh;
    pntBox3Dview.DiscardBitmap;
  end
  else
  begin
    frmMain.Caption := 'No valid cubelet clicked';
  end;
end;

procedure TfrmMain.pntBoxCurrentStateMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
var
  colorIndex: integer;
begin
  if IsRunning then Exit; // Ensure the cube isn't in a running state
  colorIndex := GetCubeletColor(CurrentCubeState, MousePos);
  // Assuming GetColor can work with MousePos directly

  // Adjust the colorIndex based on WheelDelta
  if WheelDelta > 0 then
  begin
    // Scrolled up: Cycle forward through the colors 2 to 5
    colorIndex := ((colorIndex - 2 + 1) mod 4) + 2;
  end
  else if WheelDelta < 0 then
  begin
    // Scrolled down: Cycle backward through the colors 2 to 5
    if colorIndex = 2 then
      colorIndex := 5
    else if colorIndex > 2 then
      Dec(colorIndex) // Move to the previous color
    else
      colorIndex := 2; // Default to 2 if outside the target range
  end;

  SetCubeletColor(CurrentCubeState, MousePos, colorIndex);
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  pntBoxCurrentState.Refresh;
  pntBox3Dview.DiscardBitmap;
end;

procedure TfrmMain.pntBoxCurrentStateMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
begin

end;

procedure TfrmMain.pntBoxTargetSolveMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  colorIndex: integer;
begin
  if IsRunning then Exit;
  colorIndex := GetCubeletColor(TargetCubeState, Point(x, y));

  if Button = mbLeft then
  begin
    // Cycles colors 2 to 5 with left mouse button
    if (colorIndex >= 2) and (colorIndex < 5) then
      Inc(colorIndex) // Move to the next color
    else if colorIndex = 5 then
      colorIndex := 2 // Wrap back to color 2
    else
      colorIndex := 2; // Default to 2 if outside range
  end
  else if Button = mbRight then
  begin
    // Toggles between colors 1 and 6 with right mouse button
    if colorIndex = 6 then
      colorIndex := 1
    else
      colorIndex := 6;
  end;

  SetCubeletColor(TargetCubeState, point(x, y), colorIndex);

  DrawCube(pntBoxTargetSolve, TargetCubeState);
  pntBoxTargetSolve.Refresh;
  pntBox3Dview.DiscardBitmap;
end;

procedure TfrmMain.pntBoxCurrentStatePaint(Sender: TObject);
begin
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  // DON'T call Refresh inside Paint handler - causes infinite loop!
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

  // Reset visible faces to initial view: Top, Front, Right
  // Face indices: 1=Top, 2=Front, 3=Right, 4=Back, 5=Left, 6=Bottom
  VisibleFaces[0] := 1;  // Top
  VisibleFaces[1] := 2;  // Front
  VisibleFaces[2] := 3;  // Right
  ViewRotationY := 0;    // Reset rotation counter
  ViewFlipped := False;  // Reset flip state

  // DrawCube3d handled by OnRedraw event
  Cube3DTransActive := False;
  pntBoxCurrentState.Refresh;
  pntBox3Dview.DiscardBitmap;
end;

procedure TfrmMain.RotateCubeLeftRight(Direction: integer);
var
  AngleX, TotalAngleY, CurrentAngleY, DeltaAngleY: double;
  TimeoutCounter: Integer;
  StartTime, ElapsedMs, TargetDuration: QWord;
  Progress, PrevProgress: Double;
  OldFront, OldRight, OldBack, OldLeft: Integer;
begin
  // Wait for any active rotation to finish, with timeout protection
  TimeoutCounter := 0;
  repeat
    Sleep(1);  // Small sleep to prevent tight CPU loop
    Inc(TimeoutCounter);
    if TimeoutCounter > 5000 then  // 5 second timeout (5000ms with 1ms sleep)
    begin
      Cube3DTransActive := False;  // Force reset
      ShowMessage('Warning: Rotation was stuck. Resetting state.');
      Exit;
    end;
  until Cube3DTransActive = False;

  Cube3DTransActive := True;

  // Time-based rotation: 800ms for smooth view rotation
  TargetDuration := 800;
  TotalAngleY := Direction * 90 * Pi / 180;  // Direction: 1=left, -1=right
  StartTime := GetTickCount64;

  // Track previous progress for incremental rotation
  PrevProgress := 0;
  Progress := 0;

  while True do
  begin
    ElapsedMs := GetTickCount64 - StartTime;
    if ElapsedMs >= TargetDuration then
    begin
      // Apply final increment to complete the rotation
      DeltaAngleY := (1.0 - Progress) * TotalAngleY;

      AngleX := 32 * Pi / 180;
      Rotate3d(cube3d, AngleX, 0, 0);
      Rotate3d(cube3d, 0, DeltaAngleY, 0);
      AngleX := -32 * Pi / 180;
      Rotate3d(cube3d, AngleX, 0, 0);

      Break;
    end;

    // Calculate current progress with easing
    PrevProgress := Progress;
    Progress := EaseInOutQuad(ElapsedMs / TargetDuration);

    // Calculate DELTA rotation since last frame (incremental!)
    DeltaAngleY := (Progress - PrevProgress) * TotalAngleY;

    // ANGLE IT BACK UP SO IT IS JUST A FLAT FRONT VIEW
    AngleX := 32 * Pi / 180;
    Rotate3d(cube3d, AngleX, 0, 0);

    // NOW ROTATE IT INCREMENTALLY!!!
    Rotate3d(cube3d, 0, DeltaAngleY, 0);

    // NOW ROTATE IT BACK DOWN TO THE ANGLE WE LIKE TO SEE THE TOP!!
    AngleX := -32 * Pi / 180;
    Rotate3d(cube3d, AngleX, 0, 0);

    // WE DON'T DRAW UNTIL ALL AXIS ARE SET!!!
    pntBox3Dview.DiscardBitmap;
    Application.ProcessMessages;
    Sleep(1);  // Prevent CPU burn
  end;

  // Update VisibleFaces array after rotation completes
  // Face indices: 1=Top, 2=Front, 3=Right, 4=Back, 5=Left, 6=Bottom
  // VisibleFaces[0] = top face (stays same for Y rotation)
  // VisibleFaces[1] = front face (changes)
  // VisibleFaces[2] = right face (changes)
  //
  // Rotating LEFT (Direction=1): Front->Right->Back->Left->Front
  // Rotating RIGHT (Direction=-1): Front->Left->Back->Right->Front
  OldFront := VisibleFaces[1];
  OldRight := VisibleFaces[2];

  // Calculate the opposite faces
  // Front(2)<->Back(4), Right(3)<->Left(5)
  case OldFront of
    2: OldBack := 4;  // Front -> Back
    4: OldBack := 2;  // Back -> Front
    3: OldBack := 5;  // Right -> Left
    5: OldBack := 3;  // Left -> Right
  else
    OldBack := 4;
  end;

  case OldRight of
    2: OldLeft := 4;
    4: OldLeft := 2;
    3: OldLeft := 5;
    5: OldLeft := 3;
  else
    OldLeft := 5;
  end;

  if Direction = 1 then
  begin
    // Rotating LEFT: viewing cube turns left, so we see what was on the right
    // New front = old right, New right = old back
    VisibleFaces[1] := OldRight;
    VisibleFaces[2] := OldBack;
    // Track rotation count (0-3, wrapping)
    ViewRotationY := (ViewRotationY + 1) mod 4;
  end
  else
  begin
    // Rotating RIGHT: viewing cube turns right, so we see what was on the left
    // New front = old left, New right = old front
    VisibleFaces[1] := OldLeft;
    VisibleFaces[2] := OldFront;
    // Track rotation count (0-3, wrapping)
    ViewRotationY := (ViewRotationY + 3) mod 4;  // +3 is same as -1 mod 4
  end;

  Cube3DTransActive := False;
end;

procedure TfrmMain.RotateCubeFlipUp();
var
  TotalAngleX, DeltaAngleX: double;
  TimeoutCounter: Integer;
  StartTime, ElapsedMs, TargetDuration: QWord;
  Progress, PrevProgress: Double;
  OldTop, OldFront, OldBottom, OldBack: Integer;
begin
  // Wait for any active rotation to finish, with timeout protection
  TimeoutCounter := 0;
  repeat
    Sleep(1);  // Small sleep to prevent tight CPU loop
    Inc(TimeoutCounter);
    if TimeoutCounter > 5000 then  // 5 second timeout (5000ms with 1ms sleep)
    begin
      Cube3DTransActive := False;  // Force reset
      ShowMessage('Warning: Rotation was stuck. Resetting state.');
      Exit;
    end;
  until Cube3DTransActive = False;

  Cube3DTransActive := True;

  // Time-based rotation: 800ms for smooth 180° flip
  TargetDuration := 800;
  TotalAngleX := 180 * Pi / 180;
  StartTime := GetTickCount64;

  // Track previous progress for incremental rotation
  PrevProgress := 0;
  Progress := 0;

  while True do
  begin
    ElapsedMs := GetTickCount64 - StartTime;
    if ElapsedMs >= TargetDuration then
    begin
      // Apply final increment to complete the rotation
      DeltaAngleX := (1.0 - Progress) * TotalAngleX;
      Rotate3d(cube3d, DeltaAngleX, 0, 0);
      Break;
    end;

    // Calculate current progress with easing
    PrevProgress := Progress;
    Progress := EaseInOutQuad(ElapsedMs / TargetDuration);

    // Calculate DELTA rotation since last frame (incremental!)
    DeltaAngleX := (Progress - PrevProgress) * TotalAngleX;

    // Apply incremental rotation
    Rotate3d(cube3d, DeltaAngleX, 0, 0);

    pntBox3Dview.DiscardBitmap;
    Application.ProcessMessages;
    Sleep(1);  // Prevent CPU burn
  end;

  // Update VisibleFaces array after 180° flip completes
  // Face indices: 1=Top, 2=Front, 3=Right, 4=Back, 5=Left, 6=Bottom
  // A 180° flip around the horizontal axis (what we see as left-right):
  // - Top(1) <-> Bottom(6)
  // - The two side faces swap positions AND become their opposites
  //   Left side of screen goes to right, right goes to left
  //   AND Front(2)<->Back(4), Right(3)<->Left(5)
  OldTop := VisibleFaces[0];
  OldFront := VisibleFaces[1];  // "left side of screen"
  OldBack := VisibleFaces[2];   // "right side of screen" (reusing variable name)

  // Calculate opposite of top face
  case OldTop of
    1: OldBottom := 6;  // Top -> Bottom
    6: OldBottom := 1;  // Bottom -> Top
  else
    OldBottom := 6;
  end;

  // New top = old bottom
  VisibleFaces[0] := OldBottom;

  // The side faces: after 180° flip, left-of-screen becomes right-of-screen
  // and vice versa, BUT we also see the opposite face
  // So new left-of-screen = opposite of old right-of-screen
  // And new right-of-screen = opposite of old left-of-screen

  // Calculate opposites: Front(2)<->Back(4), Right(3)<->Left(5)
  case OldFront of
    2: VisibleFaces[2] := 4;  // Front was on left, Back now on right
    4: VisibleFaces[2] := 2;  // Back was on left, Front now on right
    3: VisibleFaces[2] := 5;  // Right was on left, Left now on right
    5: VisibleFaces[2] := 3;  // Left was on left, Right now on right
  else
    VisibleFaces[2] := 4;
  end;

  case OldBack of  // This was "right side of screen"
    2: VisibleFaces[1] := 4;  // Front was on right, Back now on left
    4: VisibleFaces[1] := 2;  // Back was on right, Front now on left
    3: VisibleFaces[1] := 5;  // Right was on right, Left now on left
    5: VisibleFaces[1] := 3;  // Left was on right, Right now on left
  else
    VisibleFaces[1] := 2;
  end;

  // Toggle flip state - affects side face rotation directions
  ViewFlipped := not ViewFlipped;

  Cube3DTransActive := False;
end;

procedure TfrmMain.RotateCubePeakUnder();
var
  TotalAngleX, DeltaAngleX: double;
  TimeoutCounter: Integer;
  StartTime, ElapsedMs, TargetDuration: QWord;
  Progress, PrevProgress: Double;
begin
  // Wait for any active rotation to finish, with timeout protection
  TimeoutCounter := 0;
  repeat
    Sleep(1);  // Small sleep to prevent tight CPU loop
    Inc(TimeoutCounter);
    if TimeoutCounter > 5000 then  // 5 second timeout (5000ms with 1ms sleep)
    begin
      Cube3DTransActive := False;  // Force reset
      ShowMessage('Warning: Rotation was stuck. Resetting state.');
      Exit;
    end;
  until Cube3DTransActive = False;

  Cube3DTransActive := True;
  PeekState := psPeakingUp;

  // Time-based rotation: 600ms for smooth peek
  TargetDuration := 600;
  TotalAngleX := (45 + 22.5) * Pi / 180;  // 67.5 degrees
  StartTime := GetTickCount64;

  // Track previous progress for incremental rotation
  PrevProgress := 0;
  Progress := 0;

  while True do
  begin
    ElapsedMs := GetTickCount64 - StartTime;
    if ElapsedMs >= TargetDuration then
    begin
      // Apply final increment to complete the rotation
      DeltaAngleX := (1.0 - Progress) * TotalAngleX;
      Rotate3d(cube3d, DeltaAngleX, 0, 0);
      Break;
    end;

    // Calculate current progress with easing
    PrevProgress := Progress;
    Progress := EaseInOutQuad(ElapsedMs / TargetDuration);

    // Calculate DELTA rotation since last frame (incremental!)
    DeltaAngleX := (Progress - PrevProgress) * TotalAngleX;

    // Apply incremental rotation
    Rotate3d(cube3d, DeltaAngleX, 0, 0);

    pntBox3Dview.DiscardBitmap;
    Application.ProcessMessages;
    Sleep(1);  // Prevent CPU burn
  end;

  PeekState := psPeaked;
  Cube3DTransActive := False;
end;

procedure TfrmMain.RotateCubeUnPeakUnder();
var
  TotalAngleX, DeltaAngleX: double;
  TimeoutCounter: Integer;
  StartTime, ElapsedMs, TargetDuration: QWord;
  Progress, PrevProgress: Double;
begin
  // Wait for any active rotation to finish, with timeout protection
  TimeoutCounter := 0;
  repeat
    Sleep(1);  // Small sleep to prevent tight CPU loop
    Inc(TimeoutCounter);
    if TimeoutCounter > 5000 then  // 5 second timeout (5000ms with 1ms sleep)
    begin
      Cube3DTransActive := False;  // Force reset
      ShowMessage('Warning: Rotation was stuck. Resetting state.');
      Exit;
    end;
  until Cube3DTransActive = False;

  Cube3DTransActive := True;
  PeekState := psPeakingDown;

  // Time-based rotation: 600ms for smooth un-peek
  TargetDuration := 600;
  TotalAngleX := -((45 + 22.5) * Pi / 180);  // -67.5 degrees (opposite direction)
  StartTime := GetTickCount64;

  // Track previous progress for incremental rotation
  PrevProgress := 0;
  Progress := 0;

  while True do
  begin
    ElapsedMs := GetTickCount64 - StartTime;
    if ElapsedMs >= TargetDuration then
    begin
      // Apply final increment to complete the rotation
      DeltaAngleX := (1.0 - Progress) * TotalAngleX;
      Rotate3d(cube3d, DeltaAngleX, 0, 0);
      Break;
    end;

    // Calculate current progress with easing
    PrevProgress := Progress;
    Progress := EaseInOutQuad(ElapsedMs / TargetDuration);

    // Calculate DELTA rotation since last frame (incremental!)
    DeltaAngleX := (Progress - PrevProgress) * TotalAngleX;

    // Apply incremental rotation
    Rotate3d(cube3d, DeltaAngleX, 0, 0);

    pntBox3Dview.DiscardBitmap;
    Application.ProcessMessages;
    Sleep(1);  // Prevent CPU burn
  end;

  PeekState := psNormal;
  Cube3DTransActive := False;
end;

procedure TfrmMain.btn3DViewRotate90LeftClick(Sender: TObject);
begin
  if Cube3DTransActive then Exit;
  RotateCubeLeftRight(1);
end;

procedure TfrmMain.btn3DViewRotate90UpMouseEnter(Sender: TObject);
begin

end;

procedure TfrmMain.btn3DViewRotate90UpMouseLeave(Sender: TObject);
begin

end;

procedure TfrmMain.btn3DViewRotate90UpClick(Sender: TObject);
begin
  if Cube3DTransActive then Exit;
  RotateCubeFlipUp;
end;

procedure TfrmMain.btnExecuteUntilSolvedClick(Sender: TObject);
var
  s: TCaption;
  repeated: integer;
begin
  repeated := 0;
  if IsRunning then
  begin
    btnExecuteUntilSolved.Caption := '🚀 Execute';
    ToggleButtonsExcept(Self, btnExecuteUntilSolved, True);
    IsRunning := False;
    exit;
  end;
  IsRunning := True;
  btnExecuteUntilSolved.Caption := 'Stop';
  ToggleButtonsExcept(Self, btnExecuteUntilSolved, False);

  repeat
    s := AnsiUpperCase(edtMoveString.Text);
    ExecuteNotation(s, spinEdtAnimationSpeed.Value);
    Inc(repeated);
  until CompareCubes(CurrentCubeState, TargetCubeState) or (IsRunning = False);

  if IsRunning then
    memSolveSummary.Lines[0] :=
      'Solved state reached after repeating sequence ' + (IntToStr(repeated)) + ' times.'
  else
    memSolveSummary.Lines.Clear;

  IsRunning := False;
  btnExecuteUntilSolved.Caption := '🚀 Execute Until Solved';
  ToggleButtonsExcept(Self, btnExecuteUntilSolved, True);
end;

procedure TfrmMain.btn3DViewRotateRightClick(Sender: TObject);
begin
  if Cube3DTransActive then Exit;
  RotateCubeLeftRight(-1);
end;

procedure TfrmMain.chkKeyBoardControlChange(Sender: TObject);
begin
  keyBoardControlActive := chkKeyBoardControl.Checked;

  // Enable/disable keyboard face selection mode
  KeyboardFaceSelectMode := chkKeyBoardControl.Checked;

  // Start/stop the marching ants animation timer
  tmrMarchingAnts.Enabled := KeyboardFaceSelectMode;

  // Reset selection to first visible face (Top)
  if KeyboardFaceSelectMode then
    SelectedVisibleFace := 0;

  // Refresh the 3D view to show/hide selection
  pntBox3Dview.DiscardBitmap;
end;

procedure TfrmMain.tmrMarchingAntsTimer(Sender: TObject);
begin
  // Animate the marching ants by incrementing offset
  MarchingAntsOffset := (MarchingAntsOffset + 2) mod 16;

  // Refresh the 3D view to show updated animation
  if KeyboardFaceSelectMode then
    pntBox3Dview.DiscardBitmap;
end;

procedure TfrmMain.btn3DviewResetClick(Sender: TObject);
begin
  SetInitialCubeView;
end;

procedure TfrmMain.pntBox3DviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
begin
  if ts2DViews.ActivePageIndex = 0 then
    DrawCube3d(Bitmap, CurrentCubeState, cube3d)
  else
    DrawCube3d(Bitmap, TargetCubeState, cube3d);
end;

procedure TfrmMain.btnScrampleStateClick(Sender: TObject);
var
  NumMoves, i: integer;
  InputStr, scrambledNotation: string;
  UserOK: boolean;
begin
  // Exit execution mode if user scrambles
  if ExecutionState <> esIdle then
    ExitExecutionMode;

  Randomize;

  InputStr := '15';
  UserOK := InputQuery('Scramble Cube', 'Enter the number of scramble moves:', InputStr);

  if UserOK then
  begin
    if TryStrToInt(InputStr, NumMoves) then
    begin
      //if NumMoves > 100 then
      //  NumMoves := 100; // Capping number of moves
      //RandomRotateFaces(NumMoves);
      scrambledNotation := GenerateRandomScramble(NumMoves);
      memRandScramble.Text := scrambledNotation;
      ToggleButtonsExcept(Self, btn3DviewReset, False);
      IsRunning := True;
      ExecuteNotation(scrambledNotation, 11);
      IsRunning := False;
      ToggleButtonsExcept(Self, btn3DviewReset, True);
      pntBoxCurrentState.Refresh;
      pntBox3Dview.DiscardBitmap;
    end
    else
      ShowMessage('Please enter a valid number.');
  end;
end;

procedure TfrmMain.btnScanWebcamClick(Sender: TObject);
var
  WebcamForm: TfrmWebcamScan;
begin
  WebcamForm := TfrmWebcamScan.Create(Self);
  try
    if WebcamForm.ShowModal = mrOK then
    begin
      // User completed scanning - update current cube state
      CurrentCubeState := WebcamForm.ScannedCubeState;
      pntBoxCurrentState.Refresh;
      pntBox3Dview.DiscardBitmap;
      ShowMessage('Cube scanned successfully!');
    end;
  finally
    WebcamForm.Free;
  end;
end;

procedure TfrmMain.btnScrambleTargetClick(Sender: TObject);
var
  i: integer;
begin
  TargetCubeState := C_CUBE_COMPLETE;
  for i := 0 to 50 do rotateface(TUnitRubik(TargetCubeState), random(6) +
      1, random(3) + 1);
  DrawCube(pntBoxTargetSolve, TargetCubeState);
  // DrawCube3d handled by OnRedraw event

end;

procedure TfrmMain.btnControlHelpClick(Sender: TObject);
var
  Instructions: string;
begin
  Instructions := 'Using Keyboard Controls:' + sLineBreak + sLineBreak +
    '1. Enable Keyboard Control:' + sLineBreak +
    '   - Check the "Keyboard" checkbox located in the user interface.' +
    sLineBreak + '' + sLineBreak + '2. Select Face to Rotate:''' +
    sLineBreak +
    '   - Once the "Keyboard" checkbox is enabled, you can choose which face of the Rubik''s Cube to rotate using the arrow keys.'
    + sLineBreak + '' + sLineBreak + '3. Face Selection:' + sLineBreak +
    '   - Press the corresponding keys to select the face you want to rotate:' +
    sLineBreak + '   - Press ''W'' for the Up face.' + sLineBreak +
    '   - Press ''A'' for the Left face.    ' + sLineBreak +
    '   - Press ''S'' for the Down face.' + sLineBreak +
    '   - Press ''D'' for the Right face.' + sLineBreak +
    '   -Press ''R'' for the Front face.' + sLineBreak +
    '   - Press ''''F'''' for the Back face.' + sLineBreak + '' +
    sLineBreak + '4. Rotating the Face:' + sLineBreak +
    '   - After selecting a face, you can rotate it using the arrow keys:' +
    sLineBreak + '   - Press the left arrow key to rotate the face counterclockwise.' +
    sLineBreak + '   - Press the right arrow key to rotate the face clockwise.' +
    sLineBreak + '' + sLineBreak + '5. Disabling Keyboard Control:''' +
    sLineBreak + '   - To disable keyboard controls, uncheck the "Keyboard" checkbox.' +
    sLineBreak + '' + sLineBreak + 'Note:' + sLineBreak +
    '   - Keyboard controls are only available when the "Keyboard" checkbox is enabled.'
    +
    sLineBreak +
    '   - Ensure that the application is not currently executing any other commands when using keyboard controls.'
    + sLineBreak + '' + sLineBreak +
    'For additional assistance, please refer to the application''''s user manual or contact support.';

  ShowMessage(Instructions);

end;

procedure TfrmMain.btnExecuteClick(Sender: TObject);
var
  s: string;
begin
  // Toggle between Execute and Pause
  case ExecutionState of
    esIdle:
    begin
      // Start new execution
      s := AnsiUpperCase(edtMoveString.Text);
      if Trim(s) = '' then Exit;

      // CRITICAL: Apply LFDstringCorrection hack before parsing
      LFDstringCorrection(s);

      EnterExecutionMode;
      ParseNotationMoves(s);
      CurrentMoveIndex := -1;

      // Execute all moves
      while (CurrentMoveIndex < Length(ParsedMoves) - 1) and (ExecutionState = esExecuting) do
      begin
        Inc(CurrentMoveIndex);
        ExecuteSingleMove(CurrentMoveIndex, True);
        HighlightCurrentMove;
        Application.ProcessMessages;
      end;

      // When complete, switch to paused so user can step backward
      if CurrentMoveIndex >= Length(ParsedMoves) - 1 then
      begin
        ExecutionState := esPaused;
        UpdatePlaybackButtons;
      end;
    end;

    esExecuting:
    begin
      // Pause execution
      ExecutionState := esPaused;
      UpdatePlaybackButtons;
    end;

    esPaused:
    begin
      // If at end, Clear instead of Resume
      if CurrentMoveIndex >= Length(ParsedMoves) - 1 then
      begin
        ExitExecutionMode;
        Exit;
      end;

      // Resume execution
      ExecutionState := esExecuting;
      UpdatePlaybackButtons;

      while (CurrentMoveIndex < Length(ParsedMoves) - 1) and (ExecutionState = esExecuting) do
      begin
        Inc(CurrentMoveIndex);
        ExecuteSingleMove(CurrentMoveIndex, True);
        HighlightCurrentMove;
        Application.ProcessMessages;
      end;

      // When complete, return to paused so user can step backward
      if CurrentMoveIndex >= Length(ParsedMoves) - 1 then
      begin
        ExecutionState := esPaused;
        UpdatePlaybackButtons;
      end;
    end;
  end;
end;

procedure TfrmMain.btnTargetSolveResetClick(Sender: TObject);
begin
  TargetCubeState := C_CUBE_COMPLETE;
  DrawCube(pntBoxTargetSolve, TargetCubeState);
  pntBoxTargetSolve.Refresh;
  pntBox3Dview.DiscardBitmap;
end;

procedure TfrmMain.btnStepBackwardClick(Sender: TObject);
begin
  if ExecutionState <> esPaused then Exit;
  if CurrentMoveIndex < 0 then Exit;

  // Disable step buttons during animation
  btnStepBackward.Enabled := False;
  btnStepForward.Enabled := False;
  Application.ProcessMessages;

  // Execute reverse of current move
  ExecuteSingleMoveReverse(CurrentMoveIndex, True);
  Dec(CurrentMoveIndex);
  HighlightCurrentMove;
  UpdatePlaybackButtons;
end;

procedure TfrmMain.btnStepForwardClick(Sender: TObject);
begin
  if ExecutionState <> esPaused then Exit;
  if CurrentMoveIndex >= Length(ParsedMoves) - 1 then Exit;

  // Disable step buttons during animation
  btnStepBackward.Enabled := False;
  btnStepForward.Enabled := False;
  Application.ProcessMessages;

  Inc(CurrentMoveIndex);
  ExecuteSingleMove(CurrentMoveIndex, True);
  HighlightCurrentMove;
  UpdatePlaybackButtons;
end;

procedure TfrmMain.btnCurrentStateResetClick(Sender: TObject);
begin
  CurrentCubeState := C_CUBE_COMPLETE;
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  pntBoxCurrentState.Refresh;
  pntBox3Dview.DiscardBitmap;
end;

procedure TfrmMain.btnMoveClick(Sender: TObject);
var
  n: integer;
  tmp: tcube3d;
  v: integer;
  StartTime, ElapsedMs, TargetDuration: QWord;
  Progress, CurrentAngle: Double;
begin
  // Exit execution mode if user manually rotates
  if ExecutionState <> esIdle then
    ExitExecutionMode;

  // Prevent re-entry
  if IsRunning then
  begin
    Exit;
  end;
  IsRunning := True;
  try
    n := TSpeedButton(Sender).Tag;
    v := spinEdtAnimationSpeed.Value;

    // For speed 11, skip animation entirely - instant rotation
    if v >= 11 then
    begin
      Rotateface(TUnitRubik(CurrentCubeState), n mod 10 + 1, (n div 10) * 2 + 1);
      pntBoxCurrentState.Refresh;
      pntBox3Dview.DiscardBitmap;
      Exit;
    end;

  // Time-based animation (speeds 1-10)
  case v of
    1: TargetDuration := 5000;
    2: TargetDuration := 3000;
    3: TargetDuration := 2000;
    4: TargetDuration := 1500;
    5: TargetDuration := 1000;
    6: TargetDuration := 700;
    7: TargetDuration := 500;
    8: TargetDuration := 350;
    9: TargetDuration := 250;
    10: TargetDuration := 150;
  else
    TargetDuration := 1000;
  end;

  tmp := cube3d;
  StartTime := GetTickCount64;

  while True do
  begin
    ElapsedMs := GetTickCount64 - StartTime;
    if ElapsedMs >= TargetDuration then Break;

    // Linear progress from 0.0 to 1.0
    Progress := ElapsedMs / TargetDuration;

    // Apply easing for natural movement (slow start → fast → slow end)
    Progress := EaseInOutQuad(Progress);

    CurrentAngle := Progress * 90;

    Rotate3dface(cube3d, n mod 10 + 1, (((n div 10) * 2 - 1) * CurrentAngle) * pi / 180);
    // DrawCube3d handled by OnRedraw event
    pntBox3Dview.DiscardBitmap;
    Application.ProcessMessages;

    cube3d := tmp;
    Sleep(1);  // Prevent CPU burn
  end;

  Rotateface(TUnitRubik(CurrentCubeState), n mod 10 + 1, (n div 10) * 2 + 1);
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  // DrawCube3d handled by OnRedraw event
  pntBoxCurrentState.Refresh;
  pntBox3Dview.DiscardBitmap;
  finally
    IsRunning := False;
  end;
end;

procedure TfrmMain.btn2phaseSolveClick(Sender: TObject);
begin
  ExecuteSolverAndParseOutput(CubeToDefinitionString(CurrentCubeState),
    memSolveSummary, edtMoveString);
end;

procedure TfrmMain.edtMoveStringKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  //if Key = VK_RETURN then btnExecuteClick(Sender);
end;

procedure TfrmMain.edtMoveStringKeyPress(Sender: TObject; var Key: char);
begin

  if Key in ['u', 'd', 'l', 'r', 'f', 'b'] then
  begin
    Key := UpCase(Key);
  end
  else if not (Key in ['U', 'D', 'L', 'R', 'F', 'B', '2', '''', ' ', #13, #8]) then
  begin
    Key := #0;
  end;
end;

procedure TfrmMain.ExecuteSolverAndParseOutput(const faceString: string;
  aMemo: TMemo; MoveString: TMemo);
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
      MoveString.Text := '2-Phase solver running... Please Wait' +
        StringOfChar('.', dotCount);
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
    end
    else
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
  v, n: integer;
  StartTime, LoopStart, LoopEnd, ElapsedMs, TargetDuration: QWord;
  FrameCount: Integer;
  Progress, CurrentAngle: Double;
begin
  // Exit execution mode if user manually rotates
  if ExecutionState <> esIdle then
    ExitExecutionMode;

  // Prevent re-entry
  if IsRunning then Exit;
  IsRunning := True;
  try
    StartTime := GetTickCount64;
    if clockWise then n := Face + 10
    else
      n := Face;
    v := spinEdtAnimationSpeed.Value;

    // For speed 11, skip animation entirely - instant rotation
    if v >= 11 then
    begin
      Rotateface(TUnitRubik(CurrentCubeState), face mod 10 + 1, (n div 10) * 2 + 1);
      pntBoxCurrentState.Refresh;
      pntBox3Dview.DiscardBitmap;
      Caption := Format('Speed:%d | INSTANT MODE | Took:%dms', [v, GetTickCount64 - StartTime]);
      Exit;
    end;

  // Time-based animation (speeds 1-10)
  case v of
    1: TargetDuration := 5000;
    2: TargetDuration := 3000;
    3: TargetDuration := 2000;
    4: TargetDuration := 1500;
    5: TargetDuration := 1000;
    6: TargetDuration := 700;
    7: TargetDuration := 500;
    8: TargetDuration := 350;
    9: TargetDuration := 250;
    10: TargetDuration := 150;
  else
    TargetDuration := 1000;
  end;

  tmp := cube3d;
  FrameCount := 0;
  LoopStart := GetTickCount64;

  while True do
  begin
    ElapsedMs := GetTickCount64 - LoopStart;
    if ElapsedMs >= TargetDuration then Break;

    Inc(FrameCount);

    // Linear progress from 0.0 to 1.0
    Progress := ElapsedMs / TargetDuration;

    // Apply easing for natural movement (slow start → fast → slow end)
    Progress := EaseInOutQuad(Progress);

    CurrentAngle := Progress * 90;

    Rotate3dface(cube3d, n mod 10 + 1, (((n div 10) * 2 - 1) * CurrentAngle) * pi / 180);
    // DrawCube3d handled by OnRedraw event
    pntBox3Dview.DiscardBitmap;
    Application.ProcessMessages;

    cube3d := tmp;
    Sleep(1);  // Prevent CPU burn
  end;

  LoopEnd := GetTickCount64;

  // Apply the actual move to the cube state
  Rotateface(TUnitRubik(CurrentCubeState), face mod 10 + 1, (n div 10) * 2 + 1);
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  // DrawCube3d handled by OnRedraw event
  pntBoxCurrentState.Refresh;
  pntBox3Dview.DiscardBitmap;

  // Diagnostic output
  Caption := Format('Speed:%d | Frames:%d | Loop:%dms | Total:%dms | PerFrame:%dms',
    [v, FrameCount, LoopEnd - LoopStart, GetTickCount64 - StartTime,
     (LoopEnd - LoopStart) div Max(FrameCount, 1)]);
  finally
    IsRunning := False;
  end;
end;

procedure TfrmMain.FastRotateFace(Face: integer; clockWise: boolean);
var
  tmp: tcube3d;
  i, n: integer;
begin
  if clockWise then n := Face + 10
  else
    n := Face;

  // Fast rotation - always quick, no delay
  tmp := cube3d;
  for i := 0 to 90 do
  begin
    if i mod 20 <> 0 then Continue;
    Rotate3dface(cube3d, n mod 10 + 1, (((n div 10) * 2 - 1) * i) * pi / 180);
    pntBox3Dview.DiscardBitmap;
    cube3d := tmp;
  end;

  Rotateface(TUnitRubik(CurrentCubeState), face mod 10 + 1, (n div 10) * 2 + 1);
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  pntBoxCurrentState.Refresh;
  pntBox3Dview.DiscardBitmap;
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

procedure TfrmMain.ParseNotationMoves(const s: string);
var
  i, j: integer;
  face: integer;
  turns: integer;
  moveStr: string;
begin
  SetLength(ParsedMoves, 0);
  OriginalNotation := s;
  i := 1;

  while i <= Length(s) do
  begin
    turns := 1;
    face := -1;
    moveStr := '';

    // Parse the move
    case s[i] of
      'L': face := CUBE_LEFT;
      'R': face := CUBE_RIGHT;
      'B': face := CUBE_BACK;
      'F': face := CUBE_FRONT;
      'U': face := CUBE_TOP;
      'D': face := CUBE_BOTTOM;
      else
      begin
        Inc(i);
        continue;
      end;
    end;

    moveStr := s[i];

    // Check for modifiers
    if (i < Length(s)) and (s[i + 1] = '''') then
    begin
      turns := 3;
      moveStr := moveStr + '''';
      Inc(i);
    end
    else if (i < Length(s)) and (s[i + 1] = '2') then
    begin
      turns := 2;
      moveStr := moveStr + '2';
      Inc(i);
    end;

    // Add move to array
    SetLength(ParsedMoves, Length(ParsedMoves) + 1);
    ParsedMoves[High(ParsedMoves)].Face := face;
    ParsedMoves[High(ParsedMoves)].Turns := turns;
    ParsedMoves[High(ParsedMoves)].StartPos := i - Length(moveStr) + 1;
    ParsedMoves[High(ParsedMoves)].Length := Length(moveStr);

    Inc(i);
  end;
end;

procedure TfrmMain.ExecuteSingleMove(MoveIndex: integer; Animated: boolean);
var
  tmp: tcube3d;
  f, j: integer;
  SpeedVal: integer;
  StartTime, ElapsedMs: QWord;
  Progress, CurrentAngle, TargetAngle: Double;
  TargetDuration: QWord;
begin
  if (MoveIndex < 0) or (MoveIndex >= Length(ParsedMoves)) then Exit;

  f := ParsedMoves[MoveIndex].Face;
  j := ParsedMoves[MoveIndex].Turns;
  SpeedVal := spinEdtAnimationSpeed.Value;

  if not Animated or (SpeedVal >= 11) then
  begin
    // Instant move
    RotateFace(TUnitRubik(CurrentCubeState), f, j);
    DrawCube(pntBoxCurrentState, CurrentCubeState);
    // DrawCube3d handled by OnRedraw event
    pntBoxCurrentState.Refresh;
    pntBox3Dview.DiscardBitmap;
    Exit;
  end;

  // Time-based animation (speeds 1-10)
  // Better speed scale: Speed 1 = 5 seconds (slow teaching), Speed 10 = 150ms (fast)
  case SpeedVal of
    1: TargetDuration := 5000;
    2: TargetDuration := 3000;
    3: TargetDuration := 2000;
    4: TargetDuration := 1500;
    5: TargetDuration := 1000;
    6: TargetDuration := 700;
    7: TargetDuration := 500;
    8: TargetDuration := 350;
    9: TargetDuration := 250;
    10: TargetDuration := 150;
  else
    TargetDuration := 1000;
  end;

  // Calculate target angle based on turns
  // j=1: 90° one direction, j=2: 180° same direction, j=3: 90° opposite direction
  if j = 2 then
    TargetAngle := 180
  else
    TargetAngle := 90;

  // CRITICAL: Scale duration - only j=2 takes 2x as long (j=3 is still just 90°!)
  if j = 2 then
    TargetDuration := TargetDuration * 2;

  tmp := cube3d;
  StartTime := GetTickCount64;

  while True do
  begin
    ElapsedMs := GetTickCount64 - StartTime;
    if ElapsedMs >= TargetDuration then Break;

    // Linear progress from 0.0 to 1.0
    Progress := ElapsedMs / TargetDuration;

    // Apply easing for natural movement (slow start → fast → slow end)
    Progress := EaseInOutQuad(Progress);

    CurrentAngle := Progress * TargetAngle;

    // Apply rotation at current angle
    if j = 1 then
      Rotate3dface(cube3d, f, -CurrentAngle * pi / 180)
    else if j = 2 then
      Rotate3dface(cube3d, f, -CurrentAngle * pi / 180)
    else if j = 3 then
      Rotate3dface(cube3d, f, CurrentAngle * pi / 180);

    // DrawCube3d handled by OnRedraw event
    pntBox3Dview.DiscardBitmap;
    Application.ProcessMessages;

    cube3d := tmp;
    Sleep(1);  // Prevent CPU burn
  end;

  // Apply final rotation to cube state
  RotateFace(TUnitRubik(CurrentCubeState), f, j);
  DrawCube(pntBoxCurrentState, CurrentCubeState);
  // DrawCube3d handled by OnRedraw event
  pntBoxCurrentState.Refresh;
  pntBox3Dview.DiscardBitmap;
end;

procedure TfrmMain.ExecuteSingleMoveReverse(MoveIndex: integer; Animated: boolean);
var
  reverseTurns: integer;
begin
  if (MoveIndex < 0) or (MoveIndex >= Length(ParsedMoves)) then Exit;

  // Reverse the move: 1 -> 3, 2 -> 2, 3 -> 1
  case ParsedMoves[MoveIndex].Turns of
    1: reverseTurns := 3;
    2: reverseTurns := 2;
    3: reverseTurns := 1;
    else reverseTurns := 1;
  end;

  // Temporarily modify the move and execute it
  ParsedMoves[MoveIndex].Turns := reverseTurns;
  ExecuteSingleMove(MoveIndex, Animated);
  // Restore original
  case reverseTurns of
    1: ParsedMoves[MoveIndex].Turns := 3;
    2: ParsedMoves[MoveIndex].Turns := 2;
    3: ParsedMoves[MoveIndex].Turns := 1;
  end;
end;

procedure TfrmMain.HighlightCurrentMove;
var
  i: integer;
  newText, moveText: string;
begin
  // Rebuild text with emoji markers around current move
  newText := '';

  for i := 0 to High(ParsedMoves) do
  begin
    // Get the move text
    moveText := Copy(OriginalNotation, ParsedMoves[i].StartPos, ParsedMoves[i].Length);

    if i = CurrentMoveIndex then
      // Current move: wrap with colorful markers
      newText := newText + '🔹' + moveText + '🔹 '
    else
      // Other moves: normal
      newText := newText + moveText + ' ';
  end;

  edtMoveString.Text := Trim(newText);
end;

procedure TfrmMain.UpdatePlaybackButtons;
begin
  // Update button states based on execution state
  case ExecutionState of
    esIdle:
    begin
      btnExecute.Caption := '▶ Execute';
      btnExecute.Enabled := True;
      btnStepBackward.Enabled := False;
      btnStepForward.Enabled := False;
    end;
    esExecuting:
    begin
      btnExecute.Caption := '⏸ Pause';
      btnExecute.Enabled := True;
      btnStepBackward.Enabled := False;
      btnStepForward.Enabled := False;
    end;
    esPaused:
    begin
      // If at end, show Clear instead of Resume
      if CurrentMoveIndex >= Length(ParsedMoves) - 1 then
        btnExecute.Caption := '✖ Clear'
      else
        btnExecute.Caption := '▶ Resume';

      btnExecute.Enabled := True;
      btnStepBackward.Enabled := CurrentMoveIndex >= 0;
      btnStepForward.Enabled := CurrentMoveIndex < Length(ParsedMoves) - 1;
    end;
  end;
end;

procedure TfrmMain.EnterExecutionMode;
begin
  ExecutionState := esExecuting;
  edtMoveString.ReadOnly := True;
  edtMoveString.Color := clBtnFace;
  UpdatePlaybackButtons;
end;

procedure TfrmMain.ExitExecutionMode;
begin
  if ExecutionState = esIdle then Exit;

  ExecutionState := esIdle;
  CurrentMoveIndex := -1;
  SetLength(ParsedMoves, 0);
  OriginalNotation := '';
  edtMoveString.ReadOnly := False;
  edtMoveString.Color := clWindow;
  edtMoveString.Text := '';  // Clear notation
  UpdatePlaybackButtons;
end;


end.
