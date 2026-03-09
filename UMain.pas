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
  Buttons,
  Dialogs,
  StdCtrls,
  BGRAVirtualScreen,
  BGRABitmap,
  BGRABitmapTypes,
  IniFiles,
  Math,
  UConst,
  UDraw,
  UGenericCube,
  UWebcamScan,
  UTerminalOutput,
  UAbout,
  UHelp,
  process;

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
    btnScanWebcam: TSpeedButton;
    btnScrampleState: TSpeedButton;
    btnStepBackward: TSpeedButton;
    btnStepForward: TSpeedButton;
    MainMenu1: TMainMenu;
    mnuApp: TMenuItem;
    mnuAbout: TMenuItem;
    mnuHelp: TMenuItem;
    mnuAppSep1: TMenuItem;
    mnuQuit: TMenuItem;
    mnuTools: TMenuItem;
    mnuTablesGenerate: TMenuItem;
    mnuTablesInfo: TMenuItem;
    mnuSeparator1: TMenuItem;
    mnuTablesClean: TMenuItem;
    mnuSeparator2: TMenuItem;
    mnuSolveQuality: TMenuItem;
    mnuQualityFast: TMenuItem;
    mnuQualityBalanced: TMenuItem;
    mnuQualityOptimal: TMenuItem;
    mnuCubeView: TMenuItem;
    mnuSeparator3: TMenuItem;
    mnuAnimEnabled: TMenuItem;
    mnuAnimSpeed: TMenuItem;
    mnuSpeed1: TMenuItem;
    mnuSpeed2: TMenuItem;
    mnuSpeed3: TMenuItem;
    mnuSpeed4: TMenuItem;
    mnuSpeed5: TMenuItem;
    mnuSpeed6: TMenuItem;
    mnuSpeed7: TMenuItem;
    mnuSpeed8: TMenuItem;
    mnuSpeed9: TMenuItem;
    mnuSpeed10: TMenuItem;
    mnuSolverTimeout: TMenuItem;
    mnuTimeout5: TMenuItem;
    mnuTimeout10: TMenuItem;
    mnuTimeout15: TMenuItem;
    mnuTimeout20: TMenuItem;
    mnuTimeout25: TMenuItem;
    mnuTimeout30: TMenuItem;
    mnuTimeout35: TMenuItem;
    mnuTimeout40: TMenuItem;
    mnuTimeout45: TMenuItem;
    mnuTimeout60: TMenuItem;
    mnuTimeoutNone: TMenuItem;
    memRandScramble: TMemo;
    mnuShowTerminal: TMenuItem;
    pnlPlaybackControls: TPanel;
    btnFrontClock: TSpeedButton;
    btnFrontCounter: TSpeedButton;
    btnLeftClock: TSpeedButton;
    btnLeftCounter: TSpeedButton;
    btnRightClock: TSpeedButton;
    btnRightCounter: TSpeedButton;
    btnUpClock: TSpeedButton;
    btnUpCounter: TSpeedButton;
    edtMoveString: TMemo;
    FlowPanelSolveButtons: TFlowPanel;
    lblCubeSize: TLabel;
    lblCubeSizeInfo: TLabel;
    lblCurrentMove: TLabel;
    lblPeakUnder: TLabel;
    lblSingMaster: TLabel;
    pnlCubeControls: TPanel;
    pnlFaceControls: TPanel;
    pnlSetState: TPanel;
    pnlSettings: TPanel;
    pntBox3Dview: TBGRAVirtualScreen;
    pntBoxCurrentState: TPaintBox;
    spinEdtCubeSize: TSpinEdit;
    SplitterVerticalMain: TSplitter;
    SplitterHorizontalMain: TSplitter;
    chkKeyBoardControl: TCheckBox;
    tmrMarchingAnts: TTimer;
    pnl3Dview: TPanel;
    pnlSolution: TPanel;
    procedure mnuAnimEnabledClick(Sender: TObject);
    procedure mnuSpeedClick(Sender: TObject);
    procedure btn3DViewRotate90LeftClick(Sender: TObject);
    procedure btn3DViewRotate90UpMouseEnter(Sender: TObject);
    procedure btn3DViewRotate90UpMouseLeave(Sender: TObject);
    procedure btnControlHelpClick(Sender: TObject);
    procedure btn3DViewRotateRightClick(Sender: TObject);
    procedure btn3DViewRotate90UpClick(Sender: TObject);
    procedure btnExecuteUntilSolvedClick(Sender: TObject);
    procedure btnScrampleStateClick(Sender: TObject);
    procedure btnScanWebcamClick(Sender: TObject);
    procedure btnExecuteClick(Sender: TObject);
    procedure btnStepBackwardClick(Sender: TObject);
    procedure btnStepForwardClick(Sender: TObject);
    procedure btnCurrentStateResetClick(Sender: TObject);
    procedure btn3DviewResetClick(Sender: TObject);
    procedure btnMoveClick(Sender: TObject);
    procedure btn2phaseSolveClick(Sender: TObject);
    procedure mnuAboutClick(Sender: TObject);
    procedure mnuHelpClick(Sender: TObject);
    procedure mnuQuitClick(Sender: TObject);
    procedure mnuShowTerminalClick(Sender: TObject);
    procedure mnuTablesGenerateClick(Sender: TObject);
    procedure mnuTablesInfoClick(Sender: TObject);
    procedure mnuTablesCleanClick(Sender: TObject);
    procedure mnuQualityFastClick(Sender: TObject);
    procedure mnuQualityBalancedClick(Sender: TObject);
    procedure mnuQualityOptimalClick(Sender: TObject);
    procedure mnuTimeoutClick(Sender: TObject);
    procedure edtMoveStringKeyPress(Sender: TObject; var Key: char);
    procedure edtMoveStringKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormChangeBounds(Sender: TObject);
    procedure SnapTerminalOutput;
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
    procedure pntBoxCurrentStateMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure pntBoxCurrentStatePaint(Sender: TObject);
    procedure pntBox3DviewMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure pntBox3DviewRedraw(Sender: TObject; Bitmap: TBGRABitmap);
    procedure ManualRotateFace(Face: integer; clockWise: boolean);
    procedure SetInitialCubeView;
    procedure spinEdtCubeSizeChange(Sender: TObject);
    procedure chkKeyBoardControlChange(Sender: TObject);
    procedure tmrMarchingAntsTimer(Sender: TObject);
  private
    FAnimationSpeed: integer;
    FAnimationEnabled: boolean;
    FSolverProcess: TProcess;
    FSolverCancelled: boolean;
    FSolveMaxLength: integer;
    FSolveTimeLimitMs: integer;
    FLastScrambleMoves: integer;
    FSolverTimeoutMs: QWord;
    procedure ActiveSleep(ms: cardinal);
    procedure ExecuteSolverAndParseOutput(const faceString: string;
      MoveString: TMemo);
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
    procedure ExecuteNotationNxN(const s: string);
    procedure ManualRotateFaceSlice(genFace: TFace; genDir: TMoveDirection;
      SliceStart, SliceEnd: Integer);
    procedure QueueOrExecuteMove(Face: integer; clockWise: boolean);
    procedure QueueOrExecuteSliceMove(genFace: TFace; genDir: TMoveDirection;
      SliceStart, SliceEnd: Integer);
    procedure ProcessMoveQueue;
  public
    procedure ExecuteNotation(var s: string; SpeedVal: integer);

  end;

type
  TPeekState = (psNormal, psPeakingUp, psPeaked, psPeakingDown);
  TExecutionState = (esIdle, esExecuting, esPaused);

  TMoveRecord = record
    Face: integer;      // Legacy 0-based face index (for display only)
    Turns: integer;     // 1=CW, 2=180, 3=CCW
    StartPos: integer;  // Position in original string
    Length: integer;     // Length in original string
    // NxN slice fields
    GenFace: TFace;
    Direction: TMoveDirection;
    SliceStart: Integer;
    SliceEnd: Integer;
    MoveStr: string;    // Original move token for display
  end;

var
  frmMain: TfrmMain;
  IsRunning: boolean = False;
  AnimatingFace: boolean = False;  // Re-entry guard for ManualRotateFace animation
  FBatchExecuting: boolean = False; // Suppresses per-move redraws during batch execution
  FPrevMainLeft: integer = 0;       // Previous main form position for delta tracking
  FPrevMainTop: integer = 0;
  Cube3DTransActive: boolean;
  PeekState: TPeekState = psNormal;
  WantToUnPeak: boolean = False;
  mouseDrag3D: boolean;
  FaceCodeMover: integer = 0;
  keyBoardControlActive: boolean;

  // Generic cube instance (always active for all sizes)
  GenericCube: TGenericCube = nil;
  ActiveCubeSize: Integer = 3;  // Tracks current cube size

  // Playback control variables
  ExecutionState: TExecutionState = esIdle;
  ParsedMoves: array of TMoveRecord;
  CurrentMoveIndex: integer = -1;
  OriginalNotation: string = '';

implementation

{$R *.lfm}

// Rotate NxN 3D view geometry
procedure RotateView3D(rx, ry, rz: single);
begin
  if NxNCubeSize > 0 then
    Rotate3dN(Cube3DN, rx, ry, rz);
end;

// Helper: Add a move to the queue (returns false if queue is full)
function QueueMove(Face: Integer; Clockwise: Boolean): Boolean;
begin
  Result := False;
  if MoveQueueCount >= MAX_MOVE_QUEUE then Exit;
  MoveQueue[MoveQueueCount].Face := Face;
  MoveQueue[MoveQueueCount].Clockwise := Clockwise;
  MoveQueue[MoveQueueCount].UseSlice := False;
  Inc(MoveQueueCount);
  Result := True;
end;

// Helper: Add a slice move to the queue (returns false if queue is full)
function QueueSliceMove(genFace: TFace; genDir: TMoveDirection;
  SliceStart, SliceEnd: Integer): Boolean;
begin
  Result := False;
  if MoveQueueCount >= MAX_MOVE_QUEUE then Exit;
  MoveQueue[MoveQueueCount].UseSlice := True;
  MoveQueue[MoveQueueCount].GenFace := genFace;
  MoveQueue[MoveQueueCount].GenDir := genDir;
  MoveQueue[MoveQueueCount].SliceStart := SliceStart;
  MoveQueue[MoveQueueCount].SliceEnd := SliceEnd;
  Inc(MoveQueueCount);
  Result := True;
end;

// Helper: Get next move from queue (returns false if empty)
function DequeueMove(out Item: TMoveQueueItem): Boolean;
var
  i: Integer;
begin
  Result := False;
  if MoveQueueCount = 0 then Exit;
  Item := MoveQueue[0];
  for i := 0 to MoveQueueCount - 2 do
    MoveQueue[i] := MoveQueue[i + 1];
  Dec(MoveQueueCount);
  Result := True;
end;

// Helper: Calculate rotation direction based on current view state
procedure GetRotationParams(IsLeftArrow: Boolean; out ActualFace: Integer; out IsClockwise: Boolean);
begin
  ActualFace := VisibleFaces[SelectedVisibleFace];

  case SelectedVisibleFace of
    0: begin
      if ActualFace = 1 then
        IsClockwise := not IsLeftArrow
      else
        IsClockwise := IsLeftArrow;
    end;
    1: begin
      if (ActualFace = 2) or (ActualFace = 3) then
        IsClockwise := not IsLeftArrow
      else
        IsClockwise := IsLeftArrow;
      if ViewFlipped then
        IsClockwise := not IsClockwise;
      if Odd(ViewRotationY) then
        IsClockwise := not IsClockwise;
    end;
    2: begin
      if (ActualFace = 2) or (ActualFace = 3) then
        IsClockwise := IsLeftArrow
      else
        IsClockwise := not IsLeftArrow;
      if ViewFlipped then
        IsClockwise := not IsClockwise;
      if Odd(ViewRotationY) then
        IsClockwise := not IsClockwise;
    end;
  else
    IsClockwise := not IsLeftArrow;
  end;
end;

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
begin
  // Unified: delegate to ExecuteNotationNxN which uses ManualRotateFace
  ExecuteNotationNxN(s);
end;

procedure TfrmMain.ExecuteNotationNxN(const s: string);
var
  Moves: TArray<TCubeMove>;
  Move: TCubeMove;
  i, redrawInterval: Integer;
  SliceStart, SliceEnd: Integer;
begin
  Moves := TCubeMoveParser.ParseMoveSequence(s, ActiveCubeSize);

  redrawInterval := Max(1, Length(Moves) div 10);
  try
    for i := 0 to Length(Moves) - 1 do
    begin
      Move := Moves[i];
      SliceStart := Move.SliceDepth;
      if Move.IsWide then
        SliceEnd := SliceStart + Move.SliceWidth - 1
      else
        SliceEnd := SliceStart;
      FBatchExecuting := not FAnimationEnabled;
      case Move.Direction of
        dirCW:
          ManualRotateFaceSlice(Move.Face, dirCW, SliceStart, SliceEnd);
        dir180:
        begin
          ManualRotateFaceSlice(Move.Face, dirCW, SliceStart, SliceEnd);
          ManualRotateFaceSlice(Move.Face, dirCW, SliceStart, SliceEnd);
        end;
        dirCCW:
          ManualRotateFaceSlice(Move.Face, dirCCW, SliceStart, SliceEnd);
      end;
      if FBatchExecuting then
      begin
        if (i mod redrawInterval = 0) or (i = Length(Moves) - 1) then
        begin
          pntBoxCurrentState.Refresh;
          pntBox3Dview.DiscardBitmap;
          Application.ProcessMessages;
        end;
      end
      else
        lblCurrentMove.Caption := TCubeMoveParser.MoveToString(Move, ActiveCubeSize);
    end;
  finally
    FBatchExecuting := False;
    pntBoxCurrentState.Refresh;
    pntBox3Dview.DiscardBitmap;
  end;

  lblCurrentMove.Caption := ' ';
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

function GetSettingsFile: string;
begin
  Result := ExtractFilePath(ParamStr(0)) + 'softcube.ini';
end;

procedure TfrmMain.FormCreate(Sender: TObject);
var
  ini: TIniFile;
  timeoutMin, qualityIdx: integer;
  timeoutItem: TMenuItem;
  i: integer;
  result_str: string;
begin
  Randomize;
  DoubleBuffered := True;
  lblCurrentMove.Caption := ' ';

  // Ensure rotation flag starts in correct state
  Cube3DTransActive := False;
  mouseDrag3D := False;

  // Default animation settings
  FAnimationSpeed := 7;
  FAnimationEnabled := True;

  // Initialize main form position tracking for terminal output follow
  FPrevMainLeft := Self.Left;
  FPrevMainTop := Self.Top;

  // Default solve settings
  FSolveMaxLength := 23;
  FSolveTimeLimitMs := 5000;
  FSolverTimeoutMs := 600000;
  FLastScrambleMoves := 15;

  // Load saved settings
  if FileExists(GetSettingsFile) then
  begin
    ini := TIniFile.Create(GetSettingsFile);
    try
      spinEdtCubeSize.Value := ini.ReadInteger('General', 'CubeSize', 3);
      FAnimationSpeed := ini.ReadInteger('General', 'AnimationSpeed', 7);
      FAnimationEnabled := ini.ReadBool('General', 'AnimationEnabled', True);

      qualityIdx := ini.ReadInteger('Solver', 'Quality', 0);
      case qualityIdx of
        1: mnuQualityBalancedClick(nil);
        2: mnuQualityOptimalClick(nil);
      else
        mnuQualityFastClick(nil);
      end;

      FLastScrambleMoves := ini.ReadInteger('Solver', 'ScrambleMoves', 15);
      timeoutMin := ini.ReadInteger('Solver', 'TimeoutMinutes', 10);
      FSolverTimeoutMs := QWord(timeoutMin) * 60 * 1000;
      // Check the matching menu item
      for i := 0 to mnuSolverTimeout.Count - 1 do
      begin
        timeoutItem := mnuSolverTimeout.Items[i];
        if timeoutItem.Tag = timeoutMin then
        begin
          timeoutItem.Checked := True;
          Break;
        end;
      end;
    finally
      ini.Free;
    end;
  end;

  // Sync animation menu checkmarks with loaded values
  mnuAnimEnabled.Checked := FAnimationEnabled;
  for i := 0 to mnuAnimSpeed.Count - 1 do
    mnuAnimSpeed.Items[i].Checked := (mnuAnimSpeed.Items[i].Tag = FAnimationSpeed);

  // spinEdtCubeSizeChange initializes GenericCube, NxN 3D, and calls SetInitialCubeView
  spinEdtCubeSizeChange(nil);

  // Restore saved cube state (after GenericCube is created by spinEdtCubeSizeChange)
  if FileExists(GetSettingsFile) and (GenericCube <> nil) then
  begin
    ini := TIniFile.Create(GetSettingsFile);
    try
      result_str := ini.ReadString('CubeState', 'Facelets', '');
      if (result_str <> '') and (Length(result_str) = 6 * GenericCube.CubeSize * GenericCube.CubeSize) then
      begin
        try
          GenericCube.FromDefinitionString(result_str);
          pntBoxCurrentState.Invalidate;
          pntBox3Dview.RedrawBitmap;
        except
          // If state is corrupt, just keep the solved cube
        end;
      end;
    finally
      ini.Free;
    end;
  end;

  // Initialize playback buttons
  UpdatePlaybackButtons;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  ini: TIniFile;
  qualityIdx, timeoutMin, i: integer;
begin
  ini := TIniFile.Create(GetSettingsFile);
  try
    ini.WriteInteger('General', 'CubeSize', spinEdtCubeSize.Value);
    ini.WriteInteger('General', 'AnimationSpeed', FAnimationSpeed);
    ini.WriteBool('General', 'AnimationEnabled', FAnimationEnabled);

    if mnuQualityOptimal.Checked then qualityIdx := 2
    else if mnuQualityBalanced.Checked then qualityIdx := 1
    else qualityIdx := 0;
    ini.WriteInteger('Solver', 'Quality', qualityIdx);
    ini.WriteInteger('Solver', 'ScrambleMoves', FLastScrambleMoves);

    timeoutMin := 10;
    for i := 0 to mnuSolverTimeout.Count - 1 do
      if mnuSolverTimeout.Items[i].Checked then
      begin
        timeoutMin := mnuSolverTimeout.Items[i].Tag;
        Break;
      end;
    ini.WriteInteger('Solver', 'TimeoutMinutes', timeoutMin);

    // Save cube state
    if GenericCube <> nil then
      ini.WriteString('CubeState', 'Facelets', GenericCube.ToDefinitionString);
  finally
    ini.Free;
  end;
end;

procedure TfrmMain.SnapTerminalOutput;
begin
  // Only reposition if window is not already open (don't disrupt user-placed window)
  if frmTerminalOutput.Visible then Exit;
  // Align to bottom-left corner of main form (overlapping, not below it)
  frmTerminalOutput.Left := Self.Left;
  frmTerminalOutput.Top := Self.Top + Self.Height - frmTerminalOutput.Height;
  FPrevMainLeft := Self.Left;
  FPrevMainTop := Self.Top;
end;

procedure TfrmMain.FormChangeBounds(Sender: TObject);
var
  deltaX, deltaY: integer;
  mainRect, termRect, expandedMain: TRect;
begin
  deltaX := Self.Left - FPrevMainLeft;
  deltaY := Self.Top - FPrevMainTop;
  FPrevMainLeft := Self.Left;
  FPrevMainTop := Self.Top;

  if not frmTerminalOutput.Visible then Exit;
  if (deltaX = 0) and (deltaY = 0) then Exit;  // resize only, not a move

  // Only follow if terminal is within 300px of main form (not dragged far away)
  mainRect := Rect(Self.Left, Self.Top, Self.Left + Self.Width, Self.Top + Self.Height);
  termRect := Rect(frmTerminalOutput.Left, frmTerminalOutput.Top,
                   frmTerminalOutput.Left + frmTerminalOutput.Width,
                   frmTerminalOutput.Top + frmTerminalOutput.Height);
  expandedMain := Rect(mainRect.Left - 300, mainRect.Top - 300,
                       mainRect.Right + 300, mainRect.Bottom + 300);

  if (termRect.Left < expandedMain.Right) and (termRect.Right > expandedMain.Left) and
     (termRect.Top < expandedMain.Bottom) and (termRect.Bottom > expandedMain.Top) then
  begin
    frmTerminalOutput.Left := frmTerminalOutput.Left + deltaX;
    frmTerminalOutput.Top := frmTerminalOutput.Top + deltaY;
  end;
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

  // Create/recreate generic cube for the selected size
  ActiveCubeSize := CubeSize;
  if GenericCube <> nil then
    FreeAndNil(GenericCube);
  GenericCube := TGenericCube.Create(CubeSize);

  InitNxN3D(CubeSize);

  // Reset slice selection
  SelectedSliceDepth := 0;
  SelectedVisibleFace := 0;

  SetInitialCubeView;

  // Refresh displays
  pntBoxCurrentState.Invalidate;
  pntBox3Dview.Invalidate;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  ActualFace: Integer;
  IsClockwise: Boolean;
  IsArrowKey: Boolean;
begin
  if not keyBoardControlActive then Exit;

  // Check if this is an arrow key
  IsArrowKey := (Key = VK_LEFT) or (Key = VK_RIGHT) or (Key = VK_UP) or (Key = VK_DOWN);

  // Key repeat filtering for all arrow keys
  if IsArrowKey then
  begin
    if (Key = LastArrowKeyDown) and ArrowKeyIsHeld then
      Exit;  // Ignore auto-repeat
    LastArrowKeyDown := Key;
    ArrowKeyIsHeld := True;
  end;

  if Cube3DTransActive or IsRunning or AnimatingFace then
  begin
    if (Key <> VK_LEFT) and (Key <> VK_RIGHT) and
       (Key <> VK_UP) and (Key <> VK_DOWN) then Exit;
  end;

  case Key of
    VK_UP:
    begin
      // Cycle through face+depth selections
      if ActiveCubeSize <= 3 then
      begin
        SelectedSliceDepth := 0;
        SelectedVisibleFace := (SelectedVisibleFace + 2) mod 3;
      end
      else
      begin
        if SelectedSliceDepth > 0 then
          Dec(SelectedSliceDepth)
        else
        begin
          SelectedVisibleFace := (SelectedVisibleFace + 2) mod 3;
          SelectedSliceDepth := (ActiveCubeSize - 1) div 2;
        end;
      end;
      pntBox3Dview.DiscardBitmap;
      Key := 0;
    end;

    VK_DOWN:
    begin
      if ActiveCubeSize <= 3 then
      begin
        SelectedSliceDepth := 0;
        SelectedVisibleFace := (SelectedVisibleFace + 1) mod 3;
      end
      else
      begin
        if SelectedSliceDepth < (ActiveCubeSize - 1) div 2 then
          Inc(SelectedSliceDepth)
        else
        begin
          SelectedSliceDepth := 0;
          SelectedVisibleFace := (SelectedVisibleFace + 1) mod 3;
        end;
      end;
      pntBox3Dview.DiscardBitmap;
      Key := 0;
    end;

    VK_LEFT:
    begin
      GetRotationParams(True, ActualFace, IsClockwise);
      if SelectedSliceDepth = 0 then
        QueueOrExecuteMove(ActualFace - 1, IsClockwise)
      else
      begin
        if IsClockwise then
          QueueOrExecuteSliceMove(
            TFace(CLegacyToGenericOrd[ActualFace]), dirCW,
            SelectedSliceDepth, SelectedSliceDepth)
        else
          QueueOrExecuteSliceMove(
            TFace(CLegacyToGenericOrd[ActualFace]), dirCCW,
            SelectedSliceDepth, SelectedSliceDepth);
      end;
      Key := 0;
    end;

    VK_RIGHT:
    begin
      GetRotationParams(False, ActualFace, IsClockwise);
      if SelectedSliceDepth = 0 then
        QueueOrExecuteMove(ActualFace - 1, IsClockwise)
      else
      begin
        if IsClockwise then
          QueueOrExecuteSliceMove(
            TFace(CLegacyToGenericOrd[ActualFace]), dirCW,
            SelectedSliceDepth, SelectedSliceDepth)
        else
          QueueOrExecuteSliceMove(
            TFace(CLegacyToGenericOrd[ActualFace]), dirCCW,
            SelectedSliceDepth, SelectedSliceDepth);
      end;
      Key := 0;
    end;

    Ord('A'), Ord('a'):
    begin
      RotateCubeLeftRight(1);
      Key := 0;
    end;

    Ord('D'), Ord('d'):
    begin
      RotateCubeLeftRight(-1);
      Key := 0;
    end;

    Ord('W'), Ord('w'):
    begin
      RotateCubeFlipUp;
      Key := 0;
    end;

    Ord('S'), Ord('s'):
    begin
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

  // Reset arrow key repeat filter on key release
  if (Key = VK_LEFT) or (Key = VK_RIGHT) or (Key = VK_UP) or (Key = VK_DOWN) then
  begin
    ArrowKeyIsHeld := False;
    LastArrowKeyDown := 0;
  end;

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
  hitFace: TFace;
  hitRow, hitCol: Integer;
  curColor: TCubeColor;
  nowTick: QWord;
begin
  if GenericCube = nil then Exit;
  if NxNCubeSize <= 0 then Exit;

  // Throttle fast mouse wheels
  nowTick := GetTickCount64;
  if (nowTick - LastWheelColorTick) < 50 then begin Handled := True; Exit; end;
  LastWheelColorTick := nowTick;

  if not HitTestCube3dN(MousePos, NxNCubeSize, hitFace, hitRow, hitCol) then Exit;

  curColor := GenericCube.Facelets[hitFace, hitRow, hitCol];

  if WheelDelta > 0 then
    curColor := CycleColorLeft(curColor)
  else if WheelDelta < 0 then
    curColor := CycleColorRight(curColor);

  GenericCube.Facelets[hitFace, hitRow, hitCol] := curColor;

  pntBoxCurrentState.Refresh;
  pntBox3Dview.DiscardBitmap;
  Handled := True;
end;


procedure TfrmMain.pntBoxCurrentStateMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  hitFace: TFace;
  hitRow, hitCol: Integer;
  curColor: TCubeColor;
begin
  if IsRunning then Exit;
  if GenericCube = nil then Exit;

  if not HitTestCubeN(pntBoxCurrentState, GenericCube.CubeSize, Point(X, Y),
                      hitFace, hitRow, hitCol) then Exit;

  curColor := GenericCube.Facelets[hitFace, hitRow, hitCol];

  if Button = mbLeft then
    curColor := CycleColorLeft(curColor)
  else if Button = mbRight then
    curColor := CycleColorRight(curColor);

  GenericCube.Facelets[hitFace, hitRow, hitCol] := curColor;

  pntBoxCurrentState.Refresh;
  pntBox3Dview.DiscardBitmap;
end;

procedure TfrmMain.pntBox3DviewMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  clickPoint: TPoint;
  hitFace: TFace;
  hitRow, hitCol: Integer;
  curColor: TCubeColor;
begin
  if GenericCube = nil then Exit;
  if NxNCubeSize <= 0 then Exit;
  clickPoint := Point(X, Y);

  if not HitTestCube3dN(clickPoint, NxNCubeSize, hitFace, hitRow, hitCol) then Exit;

  curColor := GenericCube.Facelets[hitFace, hitRow, hitCol];

  if Button = mbLeft then
    curColor := CycleColorLeft(curColor)
  else if Button = mbRight then
    curColor := CycleColorRight(curColor);

  GenericCube.Facelets[hitFace, hitRow, hitCol] := curColor;

  pntBoxCurrentState.Refresh;
  pntBox3Dview.DiscardBitmap;
end;

procedure TfrmMain.pntBoxCurrentStateMouseWheel(Sender: TObject;
  Shift: TShiftState; WheelDelta: integer; MousePos: TPoint; var Handled: boolean);
var
  hitFace: TFace;
  hitRow, hitCol: Integer;
  curColor: TCubeColor;
  nowTick: QWord;
begin
  if IsRunning then Exit;
  if GenericCube = nil then Exit;

  // Throttle fast mouse wheels - minimum 50ms between color changes
  nowTick := GetTickCount64;
  if (nowTick - LastWheelColorTick) < 50 then begin Handled := True; Exit; end;
  LastWheelColorTick := nowTick;

  if not HitTestCubeN(pntBoxCurrentState, GenericCube.CubeSize, MousePos,
                      hitFace, hitRow, hitCol) then Exit;

  curColor := GenericCube.Facelets[hitFace, hitRow, hitCol];

  if WheelDelta > 0 then
    curColor := CycleColorLeft(curColor)
  else if WheelDelta < 0 then
    curColor := CycleColorRight(curColor);

  GenericCube.Facelets[hitFace, hitRow, hitCol] := curColor;

  pntBoxCurrentState.Refresh;
  pntBox3Dview.DiscardBitmap;
  Handled := True;
end;

procedure TfrmMain.pntBoxCurrentStateMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: boolean);
begin

end;

procedure TfrmMain.pntBoxCurrentStatePaint(Sender: TObject);
begin
  if GenericCube <> nil then
    DrawCubeN(pntBoxCurrentState, GenericCube);
  // DON'T call Refresh inside Paint handler - causes infinite loop!
end;

procedure TfrmMain.SetInitialCubeView;
var
  AngleX, AngleY: double;
begin
  // Regenerate NxN geometry
  if NxNCubeSize > 0 then
    Cube3DN := GenerateNxNCube3D(NxNCubeSize);

  AngleY := 45 * Pi / 180;
  AngleX := -32 * Pi / 180;

  // Apply initial view rotation
  RotateView3D(0, AngleY, 0);
  RotateView3D(AngleX, 0, 0);

  // Reset visible faces to initial view: Top, Front, Right
  // Face indices: 1=Top, 2=Front, 3=Right, 4=Back, 5=Left, 6=Bottom
  VisibleFaces[0] := 1;  // Top
  VisibleFaces[1] := 2;  // Front
  VisibleFaces[2] := 3;  // Right
  ViewRotationY := 0;    // Reset rotation counter
  ViewFlipped := False;  // Reset flip state

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
      RotateView3D(AngleX, 0, 0);
      RotateView3D(0, DeltaAngleY, 0);
      AngleX := -32 * Pi / 180;
      RotateView3D(AngleX, 0, 0);

      Break;
    end;

    // Calculate current progress with easing
    PrevProgress := Progress;
    Progress := EaseInOutQuad(ElapsedMs / TargetDuration);

    // Calculate DELTA rotation since last frame (incremental!)
    DeltaAngleY := (Progress - PrevProgress) * TotalAngleY;

    // ANGLE IT BACK UP SO IT IS JUST A FLAT FRONT VIEW
    AngleX := 32 * Pi / 180;
    RotateView3D(AngleX, 0, 0);

    // NOW ROTATE IT INCREMENTALLY!!!
    RotateView3D(0, DeltaAngleY, 0);

    // NOW ROTATE IT BACK DOWN TO THE ANGLE WE LIKE TO SEE THE TOP!!
    AngleX := -32 * Pi / 180;
    RotateView3D(AngleX, 0, 0);

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
      RotateView3D(DeltaAngleX, 0, 0);
      Break;
    end;

    // Calculate current progress with easing
    PrevProgress := Progress;
    Progress := EaseInOutQuad(ElapsedMs / TargetDuration);

    // Calculate DELTA rotation since last frame (incremental!)
    DeltaAngleX := (Progress - PrevProgress) * TotalAngleX;

    // Apply incremental rotation
    RotateView3D(DeltaAngleX, 0, 0);

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
      RotateView3D(DeltaAngleX, 0, 0);
      Break;
    end;

    // Calculate current progress with easing
    PrevProgress := Progress;
    Progress := EaseInOutQuad(ElapsedMs / TargetDuration);

    // Calculate DELTA rotation since last frame (incremental!)
    DeltaAngleX := (Progress - PrevProgress) * TotalAngleX;

    // Apply incremental rotation
    RotateView3D(DeltaAngleX, 0, 0);

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
      RotateView3D(DeltaAngleX, 0, 0);
      Break;
    end;

    // Calculate current progress with easing
    PrevProgress := Progress;
    Progress := EaseInOutQuad(ElapsedMs / TargetDuration);

    // Calculate DELTA rotation since last frame (incremental!)
    DeltaAngleX := (Progress - PrevProgress) * TotalAngleX;

    // Apply incremental rotation
    RotateView3D(DeltaAngleX, 0, 0);

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
    s := Trim(edtMoveString.Text);
    ExecuteNotation(s, FAnimationSpeed);
    Inc(repeated);
  until ((GenericCube <> nil) and GenericCube.IsSolved) or (IsRunning = False);

  if IsRunning then
  begin
    frmTerminalOutput.Show;
    frmTerminalOutput.memTerminal.Lines[0] :=
      'Solved state reached after repeating sequence ' + (IntToStr(repeated)) + ' times.';
  end
  else
    frmTerminalOutput.memTerminal.Lines.Clear;

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
  if (GenericCube <> nil) and (NxNCubeSize > 0) then
    DrawCube3dN(Bitmap, GenericCube, Cube3DN);
end;

procedure TfrmMain.btnScrampleStateClick(Sender: TObject);
var
  NumMoves: integer;
  InputStr, scrambledNotation: string;
  UserOK: boolean;
begin
  // Exit execution mode if user scrambles
  if ExecutionState <> esIdle then
    ExitExecutionMode;

  Randomize;

  InputStr := IntToStr(FLastScrambleMoves);
  UserOK := InputQuery('Scramble Cube', 'Enter the number of scramble moves:', InputStr);

  if UserOK then
  begin
    if TryStrToInt(InputStr, NumMoves) and (NumMoves > 0) then
    begin
      FLastScrambleMoves := NumMoves;
      scrambledNotation := GenerateRandomScramble(NumMoves);
      memRandScramble.Text := scrambledNotation;

      // Animate scramble using ManualRotateFace (always uses GenericCube)
      ToggleButtonsExcept(Self, btn3DviewReset, False);
      ExecuteNotationNxN(scrambledNotation);
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
      // User completed scanning - update cube state
      GenericCubeFromRubik(GenericCube, WebcamForm.ScannedCubeState);
      pntBoxCurrentState.Refresh;
      pntBox3Dview.DiscardBitmap;
      ShowMessage('Cube scanned successfully!');
    end;
  finally
    WebcamForm.Free;
  end;
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
  redrawInterval: integer;
begin
  // Toggle between Execute and Pause
  case ExecutionState of
    esIdle:
    begin
      // Start new execution
      s := Trim(edtMoveString.Text);
      if s = '' then Exit;

      EnterExecutionMode;
      ParseNotationMoves(s);
      CurrentMoveIndex := -1;

      // Execute all moves - dynamically checks FAnimationEnabled each iteration
      redrawInterval := Max(1, Length(ParsedMoves) div 10);
      try
        while (CurrentMoveIndex < Length(ParsedMoves) - 1) and (ExecutionState = esExecuting) do
        begin
          Inc(CurrentMoveIndex);
          FBatchExecuting := not FAnimationEnabled;
          ExecuteSingleMove(CurrentMoveIndex, True);
          if FBatchExecuting then
          begin
            if (CurrentMoveIndex mod redrawInterval = 0) or (CurrentMoveIndex = Length(ParsedMoves) - 1) then
            begin
              pntBoxCurrentState.Refresh;
              pntBox3Dview.DiscardBitmap;
              HighlightCurrentMove;
              Application.ProcessMessages;
            end;
          end
          else
          begin
            HighlightCurrentMove;
            Application.ProcessMessages;
          end;
        end;
      finally
        FBatchExecuting := False;
        pntBoxCurrentState.Refresh;
        pntBox3Dview.DiscardBitmap;
        HighlightCurrentMove;
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

      redrawInterval := Max(1, Length(ParsedMoves) div 10);
      try
        while (CurrentMoveIndex < Length(ParsedMoves) - 1) and (ExecutionState = esExecuting) do
        begin
          Inc(CurrentMoveIndex);
          FBatchExecuting := not FAnimationEnabled;
          ExecuteSingleMove(CurrentMoveIndex, True);
          if FBatchExecuting then
          begin
            if (CurrentMoveIndex mod redrawInterval = 0) or (CurrentMoveIndex = Length(ParsedMoves) - 1) then
            begin
              pntBoxCurrentState.Refresh;
              pntBox3Dview.DiscardBitmap;
              HighlightCurrentMove;
              Application.ProcessMessages;
            end;
          end
          else
          begin
            HighlightCurrentMove;
            Application.ProcessMessages;
          end;
        end;
      finally
        FBatchExecuting := False;
        pntBoxCurrentState.Refresh;
        pntBox3Dview.DiscardBitmap;
        HighlightCurrentMove;
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
  GenericCube.Reset;

  pntBoxCurrentState.Refresh;
  pntBox3Dview.DiscardBitmap;
end;

procedure TfrmMain.btnMoveClick(Sender: TObject);
var
  Face: integer;
  clockWise: boolean;
  btn: TSpeedButton;
begin
  // Exit execution mode if user manually rotates
  if ExecutionState <> esIdle then
    ExitExecutionMode;

  btn := TSpeedButton(Sender);

  // Explicit button-to-face mapping
  // Face: 0=U, 1=F, 2=R, 3=B, 4=L, 5=D
  if      btn = btnUpClock      then begin Face := 0; clockWise := True;  end
  else if btn = btnUpCounter    then begin Face := 0; clockWise := False; end
  else if btn = btnFrontClock   then begin Face := 1; clockWise := True;  end
  else if btn = btnFrontCounter then begin Face := 1; clockWise := False; end
  else if btn = btnRightClock   then begin Face := 2; clockWise := True;  end
  else if btn = btnRightCounter then begin Face := 2; clockWise := False; end
  else if btn = btnBackClock    then begin Face := 3; clockWise := True;  end
  else if btn = btnBackCounter  then begin Face := 3; clockWise := False; end
  else if btn = btnLeftClock    then begin Face := 4; clockWise := True;  end
  else if btn = btnLeftCounter  then begin Face := 4; clockWise := False; end
  else if btn = btnDownClock    then begin Face := 5; clockWise := True;  end
  else if btn = btnDownCounter  then begin Face := 5; clockWise := False; end
  else Exit;

  QueueOrExecuteMove(Face, clockWise);
end;

procedure TfrmMain.btn2phaseSolveClick(Sender: TObject);
begin
  // If solver is running, cancel it
  if Assigned(FSolverProcess) and FSolverProcess.Running then
  begin
    FSolverCancelled := True;
    FSolverProcess.Terminate(1);
    Exit;
  end;

  SnapTerminalOutput;
  frmTerminalOutput.Show;
  ExecuteSolverAndParseOutput(GenericCube.ToDefinitionString, edtMoveString);
end;

function CountMovesInString(const s: string): integer;
var
  tokens: TStringList;
  k: integer;
  tok: string;
begin
  Result := 0;
  if Trim(s) = '' then Exit;
  tokens := TStringList.Create;
  try
    tokens.Delimiter := ' ';
    tokens.StrictDelimiter := True;
    tokens.DelimitedText := Trim(s);
    for k := 0 to tokens.Count - 1 do
    begin
      tok := Trim(tokens[k]);
      if tok <> '' then Inc(Result);
    end;
  finally
    tokens.Free;
  end;
end;

function FormatElapsed(ms: QWord): string;
begin
  if ms < 1000 then
    Result := Format('%d ms', [ms])
  else if ms < 60000 then
    Result := Format('%.1f s', [ms / 1000.0])
  else
    Result := Format('%d min %d s', [ms div 60000, (ms mod 60000) div 1000]);
end;

function GetNxNTableDir: string;
begin
  Result := IncludeTrailingPathDelimiter(
    ExtractFilePath(ParamStr(0)) + 'nxn-solver' + PathDelim + 'data');
end;

function CheckNxNTables(cubeSize: integer; out missingInfo: string): boolean;
var
  virtualSize: integer;
  tables: array of string;
  tableSizeMB: array of integer;
  idx, missing: integer;
  totalMissingMB: integer;

  procedure AddTable(const name: string; sizeMB: integer);
  begin
    SetLength(tables, idx + 1);
    SetLength(tableSizeMB, idx + 1);
    tables[idx] := name;
    tableSizeMB[idx] := sizeMB;
    Inc(idx);
  end;

begin
  idx := 0;
  if cubeSize <= 3 then
  begin
    Result := True;
    missingInfo := '';
    Exit;
  end;

  if Odd(cubeSize) then virtualSize := cubeSize
  else virtualSize := cubeSize + 1;

  AddTable('UDCentTrans', 23);
  AddTable('UDCenterMove', 152);
  AddTable('UDCentBrick256Prun', 180);
  AddTable('UDXCrossMove', 102);
  AddTable('UDXCrossPrun', 12);
  AddTable('FBCenterMove', 3);
  AddTable('FBFullCenterSlicePrun', 2560);
  AddTable('Ph3RLFBCenterMove', 1);
  AddTable('Ph3RLFBXCrossMove', 1);
  AddTable('Ph3RLFBXCrossPrun', 23);
  AddTable('Ph4UDCentBrickPrun', 23);

  if virtualSize >= 7 then
  begin
    AddTable('UDCentersSlice10', 10240);
    AddTable('Ph3Brick702RLFBCentPrun', 3900);
  end;

  missing := 0;
  totalMissingMB := 0;
  missingInfo := '';

  for idx := 0 to High(tables) do
  begin
    if not FileExists(GetNxNTableDir + tables[idx]) then
    begin
      Inc(missing);
      totalMissingMB := totalMissingMB + tableSizeMB[idx];
      if missingInfo <> '' then missingInfo := missingInfo + ', ';
      missingInfo := missingInfo + tables[idx];
    end;
  end;

  if missing > 0 then
    missingInfo := Format('%d table(s) missing (~%.1f GB on disk): %s',
      [missing, totalMissingMB / 1024.0, missingInfo]);

  Result := (missing = 0);
end;

procedure TfrmMain.mnuAboutClick(Sender: TObject);
begin
  with TfrmAbout.Create(Self) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

procedure TfrmMain.mnuHelpClick(Sender: TObject);
begin
  if frmHelp = nil then
    Application.CreateForm(TfrmHelp, frmHelp);
  frmHelp.Show;
  frmHelp.BringToFront;
end;

procedure TfrmMain.mnuQuitClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmMain.mnuAnimEnabledClick(Sender: TObject);
begin
  FAnimationEnabled := not FAnimationEnabled;
  mnuAnimEnabled.Checked := FAnimationEnabled;
end;

procedure TfrmMain.mnuSpeedClick(Sender: TObject);
begin
  FAnimationSpeed := (Sender as TMenuItem).Tag;
end;

procedure TfrmMain.mnuShowTerminalClick(Sender: TObject);
begin
  SnapTerminalOutput;
  frmTerminalOutput.Show;
  frmTerminalOutput.BringToFront;
end;

procedure TfrmMain.mnuTablesInfoClick(Sender: TObject);
var
  tableDir, solverPath, appDir: string;
  sr: TSearchRec;
  totalSize: int64;
  fileCount: integer;
  sizeStr: string;
begin
  tableDir := GetNxNTableDir;
  appDir := ExtractFilePath(ParamStr(0));

  SnapTerminalOutput;
  frmTerminalOutput.Show;
  frmTerminalOutput.memTerminal.Lines.Clear;
  frmTerminalOutput.memTerminal.Lines.Add('=== Solver Status ===');
  frmTerminalOutput.memTerminal.Lines.Add('');

  // Solver binaries
  frmTerminalOutput.memTerminal.Lines.Add('--- Solver Binaries ---');
  frmTerminalOutput.memTerminal.Lines.Add('Application directory: ' + appDir);
  frmTerminalOutput.memTerminal.Lines.Add('');

  // NxN solver (4x4+)
  {$IFDEF UNIX}
  solverPath := appDir + 'nxn-solver' + PathDelim + 'nxn_solver';
  {$ELSE}
  solverPath := appDir + 'nxn-solver' + PathDelim + 'nxn_solver.exe';
  {$ENDIF}
  if FileExists(solverPath) then
    frmTerminalOutput.memTerminal.Lines.Add('  NxN solver (4x4+):  ' + solverPath + '  [OK]')
  else
    frmTerminalOutput.memTerminal.Lines.Add('  NxN solver (4x4+):  ' + solverPath + '  [MISSING]');

  // 3x3 solver is built-in
  frmTerminalOutput.memTerminal.Lines.Add('  3x3 solver:         Built-in (pure Pascal min2phase)');
  frmTerminalOutput.memTerminal.Lines.Add('');

  // Table files
  frmTerminalOutput.memTerminal.Lines.Add('--- Cached Table Files ---');
  frmTerminalOutput.memTerminal.Lines.Add('Data directory: ' + tableDir);
  frmTerminalOutput.memTerminal.Lines.Add('');

  totalSize := 0;
  fileCount := 0;

  if DirectoryExists(tableDir) then
  begin
    if FindFirst(tableDir + '*', faAnyFile, sr) = 0 then
    begin
      repeat
        if (sr.Attr and faDirectory) = 0 then
        begin
          if sr.Size >= 1024 * 1024 * 1024 then
            sizeStr := Format('%.1f GB', [sr.Size / (1024.0 * 1024.0 * 1024.0)])
          else if sr.Size >= 1024 * 1024 then
            sizeStr := Format('%.0f MB', [sr.Size / (1024.0 * 1024.0)])
          else
            sizeStr := Format('%.0f KB', [sr.Size / 1024.0]);
          frmTerminalOutput.memTerminal.Lines.Add(Format('  %-30s  %s', [sr.Name, sizeStr]));
          totalSize := totalSize + sr.Size;
          Inc(fileCount);
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;
  end;

  if fileCount = 0 then
    frmTerminalOutput.memTerminal.Lines.Add('  (no cached tables)')
  else
  begin
    frmTerminalOutput.memTerminal.Lines.Add('');
    if totalSize >= 1024 * 1024 * 1024 then
      sizeStr := Format('%.1f GB', [totalSize / (1024.0 * 1024.0 * 1024.0)])
    else
      sizeStr := Format('%.0f MB', [totalSize / (1024.0 * 1024.0)]);
    frmTerminalOutput.memTerminal.Lines.Add(Format('Total: %d file(s), %s', [fileCount, sizeStr]));
  end;

  // Check what's needed for current cube size
  if ActiveCubeSize >= 4 then
  begin
    frmTerminalOutput.memTerminal.Lines.Add('');
    if CheckNxNTables(ActiveCubeSize, sizeStr) then
      frmTerminalOutput.memTerminal.Lines.Add(Format('Tables for %dx%d: READY', [ActiveCubeSize, ActiveCubeSize]))
    else
      frmTerminalOutput.memTerminal.Lines.Add(Format('Tables for %dx%d: %s', [ActiveCubeSize, ActiveCubeSize, sizeStr]));
  end;
end;

procedure TfrmMain.mnuTablesGenerateClick(Sender: TObject);
var
  missingInfo, result_str, progressLine, progressParts: string;
  progressPhase, progressTable, progressDoneStr, progressTotalStr: string;
  progressDone, progressTotal, progressPct: int64;
  StartTick, elapsed: QWord;
  i: integer;
begin
  SnapTerminalOutput;
  frmTerminalOutput.Show;
  if ActiveCubeSize <= 3 then
  begin
    frmTerminalOutput.memTerminal.Lines.Clear;
    frmTerminalOutput.memTerminal.Lines.Add('3x3 tables are generated automatically on first solve.');
    Exit;
  end;

  if CheckNxNTables(ActiveCubeSize, missingInfo) then
  begin
    frmTerminalOutput.memTerminal.Lines.Clear;
    frmTerminalOutput.memTerminal.Lines.Add(Format('All tables for %dx%d are already present.', [ActiveCubeSize, ActiveCubeSize]));
    Exit;
  end;

  if MessageDlg('Generate Tables?',
    Format('Generate solver tables for %dx%d?' + LineEnding + LineEnding +
      '%s' + LineEnding + LineEnding +
      'This may take a long time for large cubes.',
      [ActiveCubeSize, ActiveCubeSize, missingInfo]),
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  // Launch table generation process
  progressLine := '';
  Screen.Cursor := crHourGlass;
  FSolverCancelled := False;
  frmTerminalOutput.memTerminal.Lines.Clear;
  frmTerminalOutput.memTerminal.Lines.Add(Format('=== Generating %dx%d tables ===', [ActiveCubeSize, ActiveCubeSize]));
  frmTerminalOutput.memTerminal.Lines.Add(Format('Table directory: %s', [GetNxNTableDir]));
  frmTerminalOutput.memTerminal.Lines.Add('');
  edtMoveString.Text := Format('Generating %dx%d tables...', [ActiveCubeSize, ActiveCubeSize]);
  Application.ProcessMessages;

  FSolverProcess := TProcess.Create(nil);
  btn2phaseSolve.Caption := 'Cancel';
  try
    {$IFDEF Linux}
    FSolverProcess.Executable := ExtractFilePath(ParamStr(0)) + 'nxn-solver' + PathDelim + 'nxn_solver';
    {$ENDIF}
    {$IFDEF Windows}
    FSolverProcess.Executable := ExtractFilePath(ParamStr(0)) + 'nxn-solver' + PathDelim + 'nxn_solver.exe';
    {$ENDIF}
    FSolverProcess.Parameters.Add('--generate-tables');
    FSolverProcess.Parameters.Add(IntToStr(ActiveCubeSize));
    FSolverProcess.Parameters.Add('--table-dir');
    FSolverProcess.Parameters.Add(GetNxNTableDir);
    FSolverProcess.Options := FSolverProcess.Options + [poUsePipes, poStderrToOutput, poNoConsole];
    StartTick := GetTickCount64;
    FSolverProcess.Execute;

    while FSolverProcess.Running do
    begin
      Application.ProcessMessages;
      if FSolverCancelled then
      begin
        FSolverProcess.Terminate(1);
        edtMoveString.Text := 'Table generation cancelled.';
        frmTerminalOutput.memTerminal.Lines.Add('');
        frmTerminalOutput.memTerminal.Lines.Add('*** Cancelled ***');
        Exit;
      end;
      elapsed := GetTickCount64 - StartTick;

      while FSolverProcess.Output.NumBytesAvailable > 0 do
      begin
        SetLength(result_str, FSolverProcess.Output.NumBytesAvailable);
        FSolverProcess.Output.Read(result_str[1], Length(result_str));
        for i := 1 to Length(result_str) do
        begin
          if result_str[i] = #10 then
          begin
            if Pos('PROGRESS:', progressLine) = 1 then
            begin
              progressParts := progressLine;
              Delete(progressParts, 1, 9);
              progressPhase := Copy(progressParts, 1, Pos(':', progressParts) - 1);
              Delete(progressParts, 1, Pos(':', progressParts));
              progressTable := Copy(progressParts, 1, Pos(':', progressParts) - 1);
              Delete(progressParts, 1, Pos(':', progressParts));
              progressDoneStr := Copy(progressParts, 1, Pos(':', progressParts) - 1);
              Delete(progressParts, 1, Pos(':', progressParts));
              progressTotalStr := progressParts;
              progressDone := StrToInt64Def(progressDoneStr, 0);
              progressTotal := StrToInt64Def(progressTotalStr, 1);
              if progressTotal > 0 then
                progressPct := (progressDone * 100) div progressTotal
              else
                progressPct := 0;
              if (frmTerminalOutput.memTerminal.Lines.Count > 0) and
                 (Pos('[', frmTerminalOutput.memTerminal.Lines[frmTerminalOutput.memTerminal.Lines.Count - 1]) = 1) then
                frmTerminalOutput.memTerminal.Lines[frmTerminalOutput.memTerminal.Lines.Count - 1] :=
                  Format('[%3d%%] %s: %s', [progressPct, progressPhase, progressTable])
              else
                frmTerminalOutput.memTerminal.Lines.Add(
                  Format('[%3d%%] %s: %s', [progressPct, progressPhase, progressTable]));
              edtMoveString.Text := Format('Generating: %s - %s (%d%%) [%s]',
                [progressPhase, progressTable, progressPct, FormatElapsed(elapsed)]);
            end
            else if progressLine <> '' then
              frmTerminalOutput.memTerminal.Lines.Add(progressLine);
            progressLine := '';
            frmTerminalOutput.memTerminal.SelStart := Length(frmTerminalOutput.memTerminal.Text);
          end
          else if result_str[i] <> #13 then
            progressLine := progressLine + result_str[i];
        end;
        Application.ProcessMessages;
      end;

      edtMoveString.Text := Format('Generating %dx%d tables... (%s)',
        [ActiveCubeSize, ActiveCubeSize, FormatElapsed(elapsed)]);
      Sleep(50);
    end;

    // Read remaining output
    while FSolverProcess.Output.NumBytesAvailable > 0 do
    begin
      SetLength(result_str, FSolverProcess.Output.NumBytesAvailable);
      FSolverProcess.Output.Read(result_str[1], Length(result_str));
      for i := 1 to Length(result_str) do
      begin
        if result_str[i] = #10 then
        begin
          if (progressLine <> '') and (Pos('PROGRESS:', progressLine) <> 1) then
            frmTerminalOutput.memTerminal.Lines.Add(progressLine);
          progressLine := '';
        end
        else if result_str[i] <> #13 then
          progressLine := progressLine + result_str[i];
      end;
    end;
    if progressLine <> '' then
      frmTerminalOutput.memTerminal.Lines.Add(progressLine);

    elapsed := GetTickCount64 - StartTick;
    frmTerminalOutput.memTerminal.Lines.Add('');
    if FSolverProcess.ExitCode = 0 then
    begin
      frmTerminalOutput.memTerminal.Lines.Add(Format('Done! Tables generated in %s.', [FormatElapsed(elapsed)]));
      edtMoveString.Text := 'Tables ready.';
    end
    else
    begin
      frmTerminalOutput.memTerminal.Lines.Add(Format('Table generation failed (exit code %d).', [FSolverProcess.ExitCode]));
      edtMoveString.Text := 'Table generation failed.';
    end;
    frmTerminalOutput.memTerminal.SelStart := Length(frmTerminalOutput.memTerminal.Text);
  finally
    FreeAndNil(FSolverProcess);
    btn2phaseSolve.Caption := '🧩 Solve';
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmMain.mnuTablesCleanClick(Sender: TObject);
var
  tableDir: string;
  sr: TSearchRec;
  totalSize: int64;
  fileCount: integer;
  sizeStr: string;
begin
  tableDir := GetNxNTableDir;

  totalSize := 0;
  fileCount := 0;

  if DirectoryExists(tableDir) then
    if FindFirst(tableDir + '*', faAnyFile, sr) = 0 then
    begin
      repeat
        if (sr.Attr and faDirectory) = 0 then
        begin
          totalSize := totalSize + sr.Size;
          Inc(fileCount);
        end;
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;

  if fileCount = 0 then
  begin
    MessageDlg('No Tables', 'No cached solver tables found.', mtInformation, [mbOK], 0);
    Exit;
  end;

  if totalSize >= 1024 * 1024 * 1024 then
    sizeStr := Format('%.1f GB', [totalSize / (1024.0 * 1024.0 * 1024.0)])
  else
    sizeStr := Format('%.0f MB', [totalSize / (1024.0 * 1024.0)]);

  if MessageDlg('Delete Solver Tables?',
    Format('%d cached table file(s) using %s.' + LineEnding + LineEnding +
      'Delete all cached tables? They will be regenerated when needed.',
      [fileCount, sizeStr]),
    mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  if DirectoryExists(tableDir) then
    if FindFirst(tableDir + '*', faAnyFile, sr) = 0 then
    begin
      repeat
        if (sr.Attr and faDirectory) = 0 then
          DeleteFile(tableDir + sr.Name);
      until FindNext(sr) <> 0;
      FindClose(sr);
    end;

  frmTerminalOutput.Show;
  frmTerminalOutput.memTerminal.Lines.Clear;
  frmTerminalOutput.memTerminal.Lines.Add('All cached solver tables deleted.');
  frmTerminalOutput.memTerminal.Lines.Add('Tables will be regenerated when needed.');
end;

procedure TfrmMain.mnuQualityFastClick(Sender: TObject);
begin
  FSolveMaxLength := 23;
  FSolveTimeLimitMs := 5000;
  mnuQualityFast.Checked := True;
end;

procedure TfrmMain.mnuQualityBalancedClick(Sender: TObject);
begin
  FSolveMaxLength := 20;
  FSolveTimeLimitMs := 15000;
  mnuQualityBalanced.Checked := True;
end;

procedure TfrmMain.mnuQualityOptimalClick(Sender: TObject);
begin
  FSolveMaxLength := 18;
  FSolveTimeLimitMs := 60000;
  mnuQualityOptimal.Checked := True;
end;

procedure TfrmMain.mnuTimeoutClick(Sender: TObject);
begin
  if TMenuItem(Sender).Tag = 0 then
    FSolverTimeoutMs := 0  // no limit
  else
    FSolverTimeoutMs := QWord(TMenuItem(Sender).Tag) * 60 * 1000;
  TMenuItem(Sender).Checked := True;
end;

procedure TfrmMain.edtMoveStringKeyUp(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  //if Key = VK_RETURN then btnExecuteClick(Sender);
end;

procedure TfrmMain.edtMoveStringKeyPress(Sender: TObject; var Key: char);
begin
  // Accept: face letters (upper/lower), w, M/E/S, digits, modifiers, control chars
  if Key in ['U', 'D', 'L', 'R', 'F', 'B',           // uppercase face letters
             'u', 'd', 'l', 'r', 'f', 'b',           // lowercase = wide shorthand
             'M', 'm', 'E', 'e', 'S', 's',           // M/E/S slice moves
             'w', 'W',                                 // wide suffix
             '0'..'9',                                 // numeric prefix + '2' modifier
             '''', ' ', #13, #8] then                  // apostrophe, space, enter, backspace
    // Allow the character
  else
    Key := #0;
end;

procedure TfrmMain.ExecuteSolverAndParseOutput(const faceString: string;
  MoveString: TMemo);
var
  OutputLines: TStringList;
  i, dotCount, moveCount, pipePos: integer;
  actualFaceString, statusMsg, result_str, lastSolverMoves: string;
  progressLine, progressParts, progressPhase, progressTable: string;
  progressDoneStr, progressTotalStr: string;
  progressDone, progressTotal: int64;
  progressPct: int64;
  StartTick, TimeoutMs, elapsed: QWord;
begin
  progressLine := '';
  Screen.Cursor := crHourGlass;
  FSolverCancelled := False;

  // Set status message and prepare face string based on cube size
  case ActiveCubeSize of
    2: begin
         statusMsg := 'Solving 2x2... Please Wait';
         actualFaceString := faceString;
       end;
    3: begin
         statusMsg := '2-Phase solver running... Please Wait';
         actualFaceString := faceString;
       end;
  else
    begin
      statusMsg := Format('Solving %dx%d... Please Wait', [ActiveCubeSize, ActiveCubeSize]);
      actualFaceString := faceString;
    end;
  end;

  frmTerminalOutput.ClearOutput;
  MoveString.Text := statusMsg;
  dotCount := 0;

  begin
    // Check if tables exist first (only needed for 4x4+)
    if (ActiveCubeSize >= 4) and not CheckNxNTables(ActiveCubeSize, result_str) then
    begin
      frmTerminalOutput.ClearOutput;
      frmTerminalOutput.AddLine(Format('%dx%d solver tables not ready', [ActiveCubeSize, ActiveCubeSize]));
      frmTerminalOutput.AddLine(result_str);
      frmTerminalOutput.AddLine('');
      frmTerminalOutput.AddLine('Tables must be generated before solving.');
      frmTerminalOutput.AddLine('This is a one-time process per cube size.');
      frmTerminalOutput.AddLine('');
      if ActiveCubeSize >= 6 then
        frmTerminalOutput.AddLine('WARNING: 6x6+ tables require ~4 GB disk space and several hours to generate.')
      else
        frmTerminalOutput.AddLine('4x4/5x5 tables take ~5-10 minutes to generate.');

      if MessageDlg('Generate Tables?',
        Format('Solver tables for %dx%d are not ready.' + LineEnding + LineEnding +
          '%s' + LineEnding + LineEnding +
          'Generate tables now? This may take a long time for large cubes.',
          [ActiveCubeSize, ActiveCubeSize, result_str]),
        mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        // Launch table generation
        frmTerminalOutput.ClearOutput;
        frmTerminalOutput.AddLine(Format('=== Generating %dx%d tables ===', [ActiveCubeSize, ActiveCubeSize]));
        frmTerminalOutput.AddLine(Format('Table directory: %s', [GetNxNTableDir]));
        frmTerminalOutput.AddLine('');
        MoveString.Text := Format('Generating %dx%d tables...', [ActiveCubeSize, ActiveCubeSize]);
        Application.ProcessMessages;

        FSolverProcess := TProcess.Create(nil);
        btn2phaseSolve.Caption := 'Cancel';
        try
          {$IFDEF Linux}
          FSolverProcess.Executable := ExtractFilePath(ParamStr(0)) + 'nxn-solver' + PathDelim + 'nxn_solver';
          {$ENDIF}
          {$IFDEF Windows}
          FSolverProcess.Executable := ExtractFilePath(ParamStr(0)) + 'nxn-solver' + PathDelim + 'nxn_solver.exe';
          {$ENDIF}
          FSolverProcess.Parameters.Add('--generate-tables');
          FSolverProcess.Parameters.Add(IntToStr(ActiveCubeSize));
          FSolverProcess.Parameters.Add('--table-dir');
          FSolverProcess.Parameters.Add(GetNxNTableDir);
          FSolverProcess.Options := FSolverProcess.Options + [poUsePipes, poStderrToOutput, poNoConsole];
          StartTick := GetTickCount64;
          FSolverProcess.Execute;

          while FSolverProcess.Running do
          begin
            Application.ProcessMessages;
            if FSolverCancelled then
            begin
              FSolverProcess.Terminate(1);
              MoveString.Text := 'Table generation cancelled.';
              frmTerminalOutput.AddLine('');
              frmTerminalOutput.AddLine('*** Cancelled ***');
              Exit;
            end;
            elapsed := GetTickCount64 - StartTick;

            // Read available output and display as console log
            while FSolverProcess.Output.NumBytesAvailable > 0 do
            begin
              SetLength(result_str, FSolverProcess.Output.NumBytesAvailable);
              FSolverProcess.Output.Read(result_str[1], Length(result_str));
              for i := 1 to Length(result_str) do
              begin
                if result_str[i] = #10 then
                begin
                  if Pos('PROGRESS:', progressLine) = 1 then
                  begin
                    // Parse PROGRESS:Phase:Table:done:total
                    progressParts := progressLine;
                    Delete(progressParts, 1, 9);
                    progressPhase := Copy(progressParts, 1, Pos(':', progressParts) - 1);
                    Delete(progressParts, 1, Pos(':', progressParts));
                    progressTable := Copy(progressParts, 1, Pos(':', progressParts) - 1);
                    Delete(progressParts, 1, Pos(':', progressParts));
                    progressDoneStr := Copy(progressParts, 1, Pos(':', progressParts) - 1);
                    Delete(progressParts, 1, Pos(':', progressParts));
                    progressTotalStr := progressParts;
                    progressDone := StrToInt64Def(progressDoneStr, 0);
                    progressTotal := StrToInt64Def(progressTotalStr, 1);
                    if progressTotal > 0 then
                      progressPct := (progressDone * 100) div progressTotal
                    else
                      progressPct := 0;
                    // Update last line in memo with progress bar
                    frmTerminalOutput.UpdateLastLine(
                      Format('[%3d%%] %s: %s', [progressPct, progressPhase, progressTable]));
                    MoveString.Text := Format('Generating: %s - %s (%d%%) [%s]',
                      [progressPhase, progressTable, progressPct, FormatElapsed(elapsed)]);
                  end
                  else if progressLine <> '' then
                    frmTerminalOutput.AddLine(progressLine);
                  progressLine := '';
                  // Auto-scroll memo to bottom
                  frmTerminalOutput.ScrollToBottom;
                end
                else if result_str[i] <> #13 then
                  progressLine := progressLine + result_str[i];
              end;
              Application.ProcessMessages;
            end;

            MoveString.Text := Format('Generating %dx%d tables... (%s)',
              [ActiveCubeSize, ActiveCubeSize, FormatElapsed(elapsed)]);
            Sleep(50);
          end;

          // Read any remaining output after process ends
          while FSolverProcess.Output.NumBytesAvailable > 0 do
          begin
            SetLength(result_str, FSolverProcess.Output.NumBytesAvailable);
            FSolverProcess.Output.Read(result_str[1], Length(result_str));
            for i := 1 to Length(result_str) do
            begin
              if result_str[i] = #10 then
              begin
                if (progressLine <> '') and (Pos('PROGRESS:', progressLine) <> 1) then
                  frmTerminalOutput.AddLine(progressLine);
                progressLine := '';
              end
              else if result_str[i] <> #13 then
                progressLine := progressLine + result_str[i];
            end;
          end;
          if progressLine <> '' then
            frmTerminalOutput.AddLine(progressLine);

          elapsed := GetTickCount64 - StartTick;
          frmTerminalOutput.AddLine('');
          if FSolverProcess.ExitCode = 0 then
          begin
            frmTerminalOutput.AddLine(Format('Done! Tables generated in %s.', [FormatElapsed(elapsed)]));
            frmTerminalOutput.AddLine('Click Solve again to solve the cube.');
            MoveString.Text := 'Tables ready - click Solve again.';
          end
          else
          begin
            frmTerminalOutput.AddLine(Format('Table generation failed (exit code %d).', [FSolverProcess.ExitCode]));
            MoveString.Text := 'Table generation failed.';
          end;
          frmTerminalOutput.ScrollToBottom;
        finally
          FreeAndNil(FSolverProcess);
          btn2phaseSolve.Caption := '🧩 Solve';
          Screen.Cursor := crDefault;
        end;
        Exit;
      end
      else
      begin
        MoveString.Text := 'Tables not generated.';
        Screen.Cursor := crDefault;
        Exit;
      end;
    end;

    // Solver (spawns external process, uses built-in min2phase for 3x3 phase)
    FSolverProcess := TProcess.Create(nil);
    OutputLines := TStringList.Create;
    btn2phaseSolve.Caption := 'Cancel';
    try
      {$IFDEF Linux}
      FSolverProcess.Executable := ExtractFilePath(ParamStr(0)) + 'nxn-solver' + PathDelim + 'nxn_solver';
      {$ENDIF}
      {$IFDEF Windows}
      FSolverProcess.Executable := ExtractFilePath(ParamStr(0)) + 'nxn-solver' + PathDelim + 'nxn_solver.exe';
      {$ENDIF}
      FSolverProcess.Parameters.Add(IntToStr(ActiveCubeSize));
      FSolverProcess.Parameters.Add(actualFaceString);
      FSolverProcess.Parameters.Add('--table-dir');
      FSolverProcess.Parameters.Add(GetNxNTableDir);
      FSolverProcess.Parameters.Add('--max-length');
      FSolverProcess.Parameters.Add(IntToStr(FSolveMaxLength));
      FSolverProcess.Parameters.Add('--time-limit');
      FSolverProcess.Parameters.Add(IntToStr(FSolveTimeLimitMs));

      FSolverProcess.Options := FSolverProcess.Options + [poUsePipes, poNoConsole];

      StartTick := GetTickCount64;
      FSolverProcess.Execute;

      TimeoutMs := FSolverTimeoutMs;
      dotCount := 0;

      progressLine := '';
      lastSolverMoves := '';
      while FSolverProcess.Running do
      begin
        Application.ProcessMessages;

        if FSolverCancelled then
        begin
          if lastSolverMoves <> '' then
          begin
            moveCount := CountMovesInString(lastSolverMoves);
            elapsed := GetTickCount64 - StartTick;
            frmTerminalOutput.AddLine('');
            frmTerminalOutput.AddLine(Format('=== Cancelled - best solution: %d moves in %s ===',
              [moveCount, FormatElapsed(elapsed)]));
            frmTerminalOutput.AddLine(lastSolverMoves);
            MoveString.Text := lastSolverMoves;
          end
          else
          begin
            MoveString.Text := 'Solver cancelled (no solution found yet).';
            frmTerminalOutput.AddLine('Solver was cancelled (no solution found yet).');
          end;
          Exit;
        end;

        // Read stderr for progress (SOLUTION/SEARCH/PROGRESS lines)
        while FSolverProcess.Stderr.NumBytesAvailable > 0 do
        begin
          SetLength(result_str, FSolverProcess.Stderr.NumBytesAvailable);
          FSolverProcess.Stderr.Read(result_str[1], Length(result_str));
          for i := 1 to Length(result_str) do
          begin
            if result_str[i] = #10 then
            begin
              if Pos('SOLUTION:', progressLine) = 1 then
              begin
                pipePos := Pos('|', progressLine);
                if pipePos > 0 then
                begin
                  lastSolverMoves := Copy(progressLine, pipePos + 1, MaxInt);
                  frmTerminalOutput.AddLine(Copy(progressLine, 1, pipePos - 1));
                  MoveString.Text := lastSolverMoves;
                end
                else
                begin
                  frmTerminalOutput.AddLine(progressLine);
                  MoveString.Text := Copy(progressLine, 11, MaxInt);
                end;
              end
              else if Pos('SEARCH:', progressLine) = 1 then
                frmTerminalOutput.AddLine(progressLine)
              else if Pos('PHASE:', progressLine) = 1 then
              begin
                frmTerminalOutput.AddLine(progressLine);
                MoveString.Text := Copy(progressLine, 8, MaxInt);
              end
              else if Pos('PROGRESS:', progressLine) = 1 then
              begin
                // Parse PROGRESS:Phase:Table:done:total
                progressParts := progressLine;
                Delete(progressParts, 1, 9);
                progressPhase := Copy(progressParts, 1, Pos(':', progressParts) - 1);
                Delete(progressParts, 1, Pos(':', progressParts));
                progressTable := Copy(progressParts, 1, Pos(':', progressParts) - 1);
                elapsed := GetTickCount64 - StartTick;
                MoveString.Text := Format('%s: %s (%s)',
                  [progressPhase, progressTable, FormatElapsed(elapsed)]);
              end
              else if progressLine <> '' then
              begin
                // Show any other solver output (version, progress updates, etc.)
                frmTerminalOutput.AddLine(progressLine);
              end;
              progressLine := '';
              frmTerminalOutput.ScrollToBottom;
            end
            else if result_str[i] <> #13 then
              progressLine := progressLine + result_str[i];
          end;
        end;

        Inc(dotCount);
        if dotCount > 20 then dotCount := 1;
        elapsed := GetTickCount64 - StartTick;
        frmTerminalOutput.Caption := statusMsg + StringOfChar('.', dotCount) +
          Format(' (%s)', [FormatElapsed(elapsed)]);
        if lastSolverMoves = '' then
          MoveString.Text := statusMsg + StringOfChar('.', dotCount) +
            Format(' (%s)', [FormatElapsed(elapsed)]);
        Sleep(100);

        if (TimeoutMs > 0) and (elapsed > TimeoutMs) then
        begin
          FSolverProcess.Terminate(1);
          MoveString.Text := Format('Solver timed out after %s.', [FormatElapsed(elapsed)]);
          frmTerminalOutput.ClearOutput;
          frmTerminalOutput.AddLine(Format('Solver timed out after %s.', [FormatElapsed(elapsed)]));
          Exit;
        end;
      end;

      elapsed := GetTickCount64 - StartTick;

      // Handle cancel (process may have died before the in-loop check ran)
      if FSolverCancelled then
      begin
        if lastSolverMoves <> '' then
        begin
          moveCount := CountMovesInString(lastSolverMoves);
          frmTerminalOutput.AddLine('');
          frmTerminalOutput.AddLine(Format('=== Cancelled - best solution: %d moves in %s ===',
            [moveCount, FormatElapsed(elapsed)]));
          frmTerminalOutput.AddLine(lastSolverMoves);
          MoveString.Text := lastSolverMoves;
        end
        else
        begin
          MoveString.Text := 'Solver cancelled (no solution found yet).';
          frmTerminalOutput.AddLine('Solver was cancelled (no solution found yet).');
        end;
        Exit;
      end;

      // Drain remaining stderr (SOLUTION/SEARCH lines)
      while FSolverProcess.Stderr.NumBytesAvailable > 0 do
      begin
        SetLength(result_str, FSolverProcess.Stderr.NumBytesAvailable);
        FSolverProcess.Stderr.Read(result_str[1], Length(result_str));
        for i := 1 to Length(result_str) do
        begin
          if result_str[i] = #10 then
          begin
            if (Pos('SOLUTION:', progressLine) = 1) or
               (Pos('SEARCH:', progressLine) = 1) or
               (Pos('PHASE:', progressLine) = 1) then
              frmTerminalOutput.AddLine(progressLine);
            progressLine := '';
          end
          else if result_str[i] <> #13 then
            progressLine := progressLine + result_str[i];
        end;
      end;

      OutputLines.LoadFromStream(FSolverProcess.Output);

      // Check exit code
      if FSolverProcess.ExitCode <> 0 then
      begin
        // Read stderr for error details
        result_str := '';
        if FSolverProcess.Stderr <> nil then
        begin
          OutputLines.Clear;
          OutputLines.LoadFromStream(FSolverProcess.Stderr);
          for i := 0 to OutputLines.Count - 1 do
            if Trim(OutputLines[i]) <> '' then
              frmTerminalOutput.AddLine(OutputLines[i]);
        end;
        frmTerminalOutput.AddLine(Format('%dx%d solver failed (exit code %d)',
          [ActiveCubeSize, ActiveCubeSize, FSolverProcess.ExitCode]));
        if ActiveCubeSize >= 6 then
          frmTerminalOutput.AddLine('Hint: 6x6+ requires pre-generated tables. Run: ./nxn-solver/nxn_solver --generate-tables ' +
            IntToStr(ActiveCubeSize));
        MoveString.Text := 'Solver error - see summary.';
      end
      // Output format: line 1 = timing, line 2 = moves
      else if OutputLines.Count >= 2 then
      begin
        result_str := Trim(OutputLines[1]);
        moveCount := CountMovesInString(result_str);

        frmTerminalOutput.AddLine('');
        frmTerminalOutput.AddLine(Format('=== %dx%d Solution: %d moves in %s ===',
          [ActiveCubeSize, ActiveCubeSize, moveCount, FormatElapsed(elapsed)]));
        frmTerminalOutput.AddLine(result_str);
        MoveString.Text := result_str;
      end
      else
      begin
        frmTerminalOutput.AddLine('Error: No solution found.');
        if OutputLines.Count >= 1 then
          frmTerminalOutput.AddLine(OutputLines[0]);
        MoveString.Text := 'Error: No solution found.';
      end;
    finally
      OutputLines.Free;
      FreeAndNil(FSolverProcess);
      btn2phaseSolve.Caption := '🧩 Solve';
      frmTerminalOutput.Caption := 'Terminal Output';
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfrmMain.ManualRotateFace(Face: integer; clockWise: boolean);
const
  // Map 0-based face index to TFace enum
  // 0=U, 1=F, 2=R, 3=B, 4=L, 5=D
  FaceToGeneric: array[0..5] of TFace = (faceU, faceF, faceR, faceB, faceL, faceD);
var
  genDir: TMoveDirection;
begin
  if clockWise then genDir := dirCW else genDir := dirCCW;
  ManualRotateFaceSlice(FaceToGeneric[Face mod 6], genDir, 0, 0);
end;

procedure TfrmMain.ManualRotateFaceSlice(genFace: TFace; genDir: TMoveDirection;
  SliceStart, SliceEnd: Integer);
const
  // Animation angle sign for CW rotation, indexed by TFace ordinal.
  //   +1 = positive angle looks CW from outside (F, D, L)
  //   -1 = positive angle looks CCW from outside (U, R, B)
  CWAnimSign: array[0..5] of Integer = (-1, -1, 1, 1, 1, -1);
var
  tmpN: TDynCube3D;
  v: integer;
  StartTime, ElapsedMs, TargetDuration: QWord;
  Progress, CurrentAngle, RotAngle: Double;
  clockWise: Boolean;
  N: Integer;
begin
  // Bounds validation
  N := ActiveCubeSize;
  if SliceStart < 0 then SliceStart := 0;
  if SliceEnd < SliceStart then SliceEnd := SliceStart;
  if SliceStart >= N then Exit;
  if SliceEnd >= N then SliceEnd := N - 1;

  clockWise := (genDir = dirCW);

  // Prevent re-entry during animation
  if AnimatingFace then Exit;
  AnimatingFace := True;
  try
    v := FAnimationSpeed;

    // Instant rotation when animation is disabled
    if not FAnimationEnabled then
    begin
      GenericCube.RotateFace(genFace, genDir, SliceStart, SliceEnd);
      if not FBatchExecuting then
      begin
        pntBoxCurrentState.Refresh;
        pntBox3Dview.DiscardBitmap;
      end;
      AnimatingFace := False;
      ProcessMoveQueue;
      Exit;
    end;

    // Time-based animation
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

    // Save 3D state for animation reset each frame
    tmpN := Copy(Cube3DN);
    StartTime := GetTickCount64;

    while True do
    begin
      ElapsedMs := GetTickCount64 - StartTime;
      if ElapsedMs >= TargetDuration then Break;

      Progress := EaseInOutQuad(ElapsedMs / TargetDuration);
      CurrentAngle := Progress * 90;
      RotAngle := CWAnimSign[Ord(genFace)] * CurrentAngle;
      if not clockWise then RotAngle := -RotAngle;

      Rotate3dSliceN(Cube3DN, NxNCubeSize, Ord(genFace),
        SliceStart, SliceEnd, RotAngle * Pi / 180);
      pntBox3Dview.DiscardBitmap;
      Application.ProcessMessages;

      // Restore geometry for next frame
      Cube3DN := Copy(tmpN);
      Sleep(1);
    end;

    // Apply the actual move to the generic cube state
    GenericCube.RotateFace(genFace, genDir, SliceStart, SliceEnd);
  
    pntBoxCurrentState.Refresh;
    pntBox3Dview.DiscardBitmap;
  finally
    AnimatingFace := False;
  end;

  ProcessMoveQueue;
end;

procedure TfrmMain.ProcessMoveQueue;
var
  Item: TMoveQueueItem;
begin
  if DequeueMove(Item) then
  begin
    Application.ProcessMessages;
    if Item.UseSlice then
      ManualRotateFaceSlice(Item.GenFace, Item.GenDir, Item.SliceStart, Item.SliceEnd)
    else
      ManualRotateFace(Item.Face, Item.Clockwise);
  end;
end;

procedure TfrmMain.QueueOrExecuteMove(Face: integer; clockWise: boolean);
begin
  if AnimatingFace then
    QueueMove(Face, clockWise)
  else
    ManualRotateFace(Face, clockWise);
end;

procedure TfrmMain.QueueOrExecuteSliceMove(genFace: TFace; genDir: TMoveDirection;
  SliceStart, SliceEnd: Integer);
begin
  if AnimatingFace then
    QueueSliceMove(genFace, genDir, SliceStart, SliceEnd)
  else
    ManualRotateFaceSlice(genFace, genDir, SliceStart, SliceEnd);
end;


function TfrmMain.GenerateRandomScramble(MoveCount: integer): string;
const
  FaceLetters: array[0..5] of string = ('U', 'D', 'L', 'R', 'F', 'B');
  Modifiers: array[0..2] of string = ('', '''', '2');
var
  LastFace, FaceIdx, ModIdx, MoveType, Depth, MaxDepth, N: integer;
  Scramble, MoveStr: string;
  i: integer;
  Move: TCubeMove;
begin
  Scramble := '';
  LastFace := -1;
  N := ActiveCubeSize;

  for i := 1 to MoveCount do
  begin
    // Pick a face different from last
    repeat
      FaceIdx := Random(6);
    until FaceIdx <> LastFace;

    ModIdx := Random(3);  // CW, CCW, 180

    if N <= 3 then
    begin
      // 3x3 and below: outer moves only
      MoveStr := FaceLetters[FaceIdx] + Modifiers[ModIdx];
    end
    else
    begin
      // 4x4+: mix of outer, wide, and inner slice moves
      MaxDepth := (N div 2) - 1;
      MoveType := Random(10);

      Move.Face := TFace(FaceIdx);
      case ModIdx of
        0: Move.Direction := dirCW;
        1: Move.Direction := dirCCW;
        2: Move.Direction := dir180;
      end;

      if MoveType < 5 then
      begin
        // 50%: outer move
        Move.SliceDepth := 0;
        Move.SliceWidth := 1;
        Move.IsWide := False;
      end
      else if MoveType < 8 then
      begin
        // 30%: wide move (2-layer or deeper)
        Depth := Random(MaxDepth) + 2;
        if Depth > MaxDepth + 1 then Depth := 2;
        Move.SliceDepth := 0;
        Move.SliceWidth := Depth;
        Move.IsWide := True;
      end
      else
      begin
        // 20%: single inner slice
        Depth := Random(MaxDepth) + 1;
        Move.SliceDepth := Depth;
        Move.SliceWidth := 1;
        Move.IsWide := False;
      end;

      MoveStr := TCubeMoveParser.MoveToString(Move, N);
    end;

    if Scramble <> '' then
      Scramble := Scramble + ' ';
    Scramble := Scramble + MoveStr;

    LastFace := FaceIdx;
  end;

  Result := Scramble;
end;


procedure TfrmMain.ParseNotationMoves(const s: string);
var
  Tokens: TStringList;
  i, Count: Integer;
  Token: string;
  Move: TCubeMove;
  SliceStart, SliceEnd: Integer;
begin
  SetLength(ParsedMoves, 0);
  OriginalNotation := s;

  // Use TCubeMoveParser for full NxN notation support
  Tokens := TStringList.Create;
  try
    Tokens.Delimiter := ' ';
    Tokens.StrictDelimiter := True;
    Tokens.DelimitedText := Trim(s);

    Count := 0;
    for i := 0 to Tokens.Count - 1 do
    begin
      Token := Trim(Tokens[i]);
      if Token = '' then Continue;

      try
        Move := TCubeMoveParser.ParseMove(Token, ActiveCubeSize);
      except
        Continue;  // Skip invalid tokens
      end;

      // Calculate slice range
      SliceStart := Move.SliceDepth;
      if Move.IsWide then
        SliceEnd := SliceStart + Move.SliceWidth - 1
      else
        SliceEnd := SliceStart;

      // Convert direction to turns
      SetLength(ParsedMoves, Count + 1);
      case Move.Direction of
        dirCW:  ParsedMoves[Count].Turns := 1;
        dir180: ParsedMoves[Count].Turns := 2;
        dirCCW: ParsedMoves[Count].Turns := 3;
      end;

      ParsedMoves[Count].Face := Ord(Move.Face);
      ParsedMoves[Count].GenFace := Move.Face;
      ParsedMoves[Count].Direction := Move.Direction;
      ParsedMoves[Count].SliceStart := SliceStart;
      ParsedMoves[Count].SliceEnd := SliceEnd;
      ParsedMoves[Count].MoveStr := TCubeMoveParser.MoveToString(Move, ActiveCubeSize);
      ParsedMoves[Count].StartPos := 0;
      ParsedMoves[Count].Length := System.Length(Token);

      Inc(Count);
    end;
  finally
    Tokens.Free;
  end;
end;

procedure TfrmMain.ExecuteSingleMove(MoveIndex: integer; Animated: boolean);
var
  j: integer;
  oldAnimEnabled: boolean;
  gf: TFace;
  ss, se: Integer;
begin
  if (MoveIndex < 0) or (MoveIndex >= Length(ParsedMoves)) then Exit;

  j := ParsedMoves[MoveIndex].Turns;  // 1=CW, 2=180, 3=CCW
  gf := ParsedMoves[MoveIndex].GenFace;
  ss := ParsedMoves[MoveIndex].SliceStart;
  se := ParsedMoves[MoveIndex].SliceEnd;

  if not Animated then
  begin
    // Set speed to instant temporarily
    oldAnimEnabled := FAnimationEnabled;
    FAnimationEnabled := False;
    try
      case j of
        1: ManualRotateFaceSlice(gf, dirCW, ss, se);
        2: begin
             ManualRotateFaceSlice(gf, dirCW, ss, se);
             ManualRotateFaceSlice(gf, dirCW, ss, se);
           end;
        3: ManualRotateFaceSlice(gf, dirCCW, ss, se);
      end;
    finally
      FAnimationEnabled := oldAnimEnabled;
    end;
    Exit;
  end;

  // Animated
  case j of
    1: ManualRotateFaceSlice(gf, dirCW, ss, se);
    2: begin
         ManualRotateFaceSlice(gf, dirCW, ss, se);
         ManualRotateFaceSlice(gf, dirCW, ss, se);
       end;
    3: ManualRotateFaceSlice(gf, dirCCW, ss, se);
  end;
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
    moveText := ParsedMoves[i].MoveStr;

    if i = CurrentMoveIndex then
      newText := newText + '🔹' + moveText + '🔹 '
    else
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
