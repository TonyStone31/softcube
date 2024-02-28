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
  StdCtrls, ComCtrls,
  strutils,
  UConst,
  UDraw,
  URubik;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    btnBackClock: TButton;
    btnBackCounter: TButton;
    btnCurrentStateRandomize: TButton;
    btnCurrentStateReset: TButton;
    btnDownClock: TButton;
    btnDownCounter: TButton;
    btnFrontClock: TButton;
    btnFrontCounter: TButton;
    btnLeftClock: TButton;
    btnLeftCounter: TButton;
    btnReset3Dview: TButton;
    btnPerformMoves: TButton;
    btnRightClock: TButton;
    btnRightCounter: TButton;
    btnSearchForSolution: TButton;
    btnUpClock: TButton;
    btnUpCounter: TButton;
    edtMoveString: TEdit;
    imgFilters: TImage;
    Label1: TLabel;
    lblClickExplainer: TLabel;
    lblClickExplainer1: TLabel;
    lblControlDirections: TLabel;
    lblFaceCode: TLabel;
    lblNoticeTarget: TLabel;
    memMoveSum: TMemo;
    pcOneDviews: TPageControl;
    pnlDestination: TPanel;
    pnlSetState: TPanel;
    pntBox3Dview: TPaintBox;
    pnl3Dview: TPanel;
    pnlSolution: TPanel;
    pntBoxCurrentState: TPaintBox;
    pntBoxDestination: TPaintBox;
    spinEdtAnimationSpeed: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure btnCurrentStateRandomizeClick(Sender: TObject);
    procedure btnPerformMovesClick(Sender: TObject);
    procedure btnCurrentStateResetClick(Sender: TObject);
    procedure btnReset3DviewClick(Sender: TObject);
    procedure btnRightClockClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure lblClickExplainerClick(Sender: TObject);
    procedure pntBox3DviewMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure pntBoxDestinationPaint(Sender: TObject);
    procedure pntBoxCurrentStateMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure pntBoxCurrentStateMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure pntBoxCurrentStatePaint(Sender: TObject);
    procedure pntBox3DviewMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: integer);
    procedure pntBox3DviewMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
    procedure pntBox3DviewPaint(Sender: TObject);
    procedure btnSearchForSolutionClick(Sender: TObject);
    procedure ManualRotateFace(Face: integer; clockWise: boolean);
    procedure SetInitialCubeView;
    procedure Timer1Timer(Sender: TObject);
  private


    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

var
  frmMain: TfrmMain;

  tmx: integer = 0;
  tmy: integer = 0;
  IsRunning: boolean = False;
  mouseDrag3D: boolean;
  FaceCodeMover: integer = 0;
  //InitialCube3D: TCube3D;

implementation

{$R *.lfm}




procedure TfrmMain.FormCreate(Sender: TObject);
var
  i: integer;
begin
  Randomize;
  DoubleBuffered := True;
  original := C_CUBE_COMPLETE;
  for i := 0 to 50 do rotateface(TUnitRubik(original), random(6) + 1, random(2) * 2 + 1);
  destination := C_CUBE_COMPLETE;
  Cube3D := VIEW_OF_3D_CUBE;

  SetInitialCubeView;

end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin

end;

procedure TfrmMain.FormKeyPress(Sender: TObject; var Key: char);
begin
  // Check if the key is between '1' and '6'
  //if (Key >= '1') and (Key <= '6') then
  //begin
  //  // Subtract '0' to convert char to its numerical value and then subtract 1
  //  // to make it zero-based as it seems your FaceCodeMover starts from 0 for key '1'
  //  FaceCodeMover := Ord(Key) - Ord('0') - 1;
  //  lblFaceCode.Caption := IntToStr(FaceCodeMover);
  //end;
  case Key of
    'a', 'A': FaceCodeMover := 4;
    's', 'S': FaceCodeMover := 5;
    'd', 'D': FaceCodeMover := 2;
    'w', 'W': FaceCodeMover := 0;
    'r', 'R': FaceCodeMover := 3;
    'f', 'F': FaceCodeMover := 1;
  end;
  lblFaceCode.Caption := IntToStr(FaceCodeMover);
end;


procedure TfrmMain.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin

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

procedure TfrmMain.lblClickExplainerClick(Sender: TObject);
begin

end;

procedure TfrmMain.pntBoxDestinationPaint(Sender: TObject);
begin
  DrawCube(pntBoxDestination, destination);
end;

procedure TfrmMain.pntBoxCurrentStateMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  colorIndex: integer;
begin
  if IsRunning then Exit;
  colorIndex := GetCubeyColor(original, Point(x, y));
  // Assume GetColor returns the current color index at the clicked position
 // WriteLn(colorIndex);
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

  PlaceColorClicker(original, point(x, y), colorIndex); // Set the new color for the clicked cube part

  // Redraw the cube and refresh the 3D view
  DrawCube(pntBoxCurrentState, original);
  pntBox3Dview.Refresh;
end;

procedure TfrmMain.pntBoxCurrentStateMouseMove(Sender: TObject; Shift: TShiftState; X, Y: integer);
begin

end;

procedure TfrmMain.pntBoxCurrentStatePaint(Sender: TObject);
begin
  DrawCube(pntBoxCurrentState, original);
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
  DrawCube3d(pntBox3Dview, original, Cube3D);

end;

procedure TfrmMain.Timer1Timer(Sender: TObject);
begin
  //WriteLn('x: ' + IntToStr(tmx));
  //WriteLn('y: ' + IntToStr(tmy));
  //
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


  DrawCube3d(pntBox3Dview, original, cube3d);

end;

procedure TfrmMain.pntBox3DviewPaint(Sender: TObject);
begin
  //if radiobutton1.Checked then
  DrawCube3d(pntBox3Dview, original, cube3d);
  //else
  //DrawCube3d(pntBox3Dview, destination, cube3d);
end;

procedure TfrmMain.btnCurrentStateRandomizeClick(Sender: TObject);
var
  i: integer;
begin
  original := C_CUBE_COMPLETE;
  for i := 0 to 50 do rotateface(TUnitRubik(original), random(6) + 1, random(3) + 1);
  DrawCube(pntBoxCurrentState, original);
  DrawCube3d(pntBox3Dview, original, cube3d);
end;

procedure TfrmMain.btnPerformMovesClick(Sender: TObject);
var
  f, i, ii, j, v: integer;
  s: string;
  tmp: tcube3d;
  sleepDuration: int64;
begin
  if IsRunning then
  begin
    btnPerformMoves.Caption := 'Execute';
    IsRunning := False;
    exit;
  end;
  IsRunning := True;
  btnPerformMoves.Caption := 'Stop';

  s := AnsiUpperCase(edtMoveString.Text);
  i := 1;
  //v := spinEdtAnimationSpeed.Value;
  //sleepDuration := Round(26 - 2.5 * v);


  while i <= length(s) do
  begin
    v := spinEdtAnimationSpeed.Value;
    sleepDuration := Round(-2 * v + 20);
    application.ProcessMessages;
    if not IsRunning then exit;
    j := 1;
    if (i < length(s)) and (s[i + 1] = '''') then j := 3;
    if (i < length(s)) and (s[i + 1] = '2') then j := 2;
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
      if not IsRunning then break;
      if v <= 10 then application.ProcessMessages;
      case j of
        1: Rotate3dface(cube3d, f, -ii * pi / 180);
        2: Rotate3dface(cube3d, f, -ii * pi / 90);
        3: Rotate3dface(cube3d, f, ii * pi / 180);
      end;

      DrawCube3d(pntBox3Dview, original, cube3d);

      Sleep(sleepDuration);

      cube3d := tmp;
    end;

    RotateFace(TUnitRubik(original), f, j);
    DrawCube3d(pntBox3Dview, original, cube3d);
    DrawCube(pntBoxCurrentState, original);
    Sleep(Round(-50 * v + 600));

    if j > 1 then Inc(i, 2)
    else
      Inc(i);
  end;
  IsRunning := False;
  btnPerformMoves.Caption := 'Execute';

end;

procedure TfrmMain.btnCurrentStateResetClick(Sender: TObject);
begin
  original := C_CUBE_COMPLETE;
  DrawCube(pntBoxCurrentState, original);
  pntBox3Dview.Refresh;
end;

procedure TfrmMain.btnRightClockClick(Sender: TObject);
var
  n: integer;
  tmp: tcube3d;
  i, v: integer;
begin
  n := TButton(Sender).Tag;
  //RadioButton1.Checked := True;
  v := spinEdtAnimationSpeed.Value;

  // animation 3D

  tmp := cube3d;
  for i := 0 to 90 do
  begin
    Rotate3dface(cube3d, n mod 10 + 1, (((n div 10) * 2 - 1) * i) * pi / 180);
    pntBox3Dview.Refresh;
    sleep(50 - v * 5);
    cube3d := tmp;
  end;


  Rotateface(TUnitRubik(original), n mod 10 + 1, (n div 10) * 2 + 1);
  DrawCube(pntBoxCurrentState, original);
  pntBox3Dview.Refresh;

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

  // animation 3D

  tmp := cube3d;
  for i := 0 to 90 do
  begin
    Rotate3dface(cube3d, n mod 10 + 1, (((n div 10) * 2 - 1) * i) * pi / 180);
    pntBox3Dview.Refresh;
    sleep(50 - v * 5);
    cube3d := tmp;
  end;


  Rotateface(TUnitRubik(original), face mod 10 + 1, (n div 10) * 2 + 1);
  DrawCube(pntBoxCurrentState, original);
  pntBox3Dview.Refresh;

end;

procedure TfrmMain.btnSearchForSolutionClick(Sender: TObject);
var
  s: string;
  tmp: TFaceRubik;
begin
  memMoveSum.Clear;
  s := '';
  if not VerifyCube(original, s) then
  begin
    memMoveSum.Lines.add('The cube has been disassembled or tampered with:');
    memMoveSum.Lines.add(s);
    exit;
  end;

  solu := '';
  tmp := original;

  // Step 1
  placeWhiteEdges(tmp);
  memMoveSum.Lines.Add('---> placeWhiteEdges');
  memMoveSum.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  s := solu;
  solu := '';
  // Step 2
  placeWhiteCorners(tmp);
  memMoveSum.Lines.Add('---> placeWhiteCorners');
  memMoveSum.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  s := s + solu;
  solu := '';
  // Step 3
  placeSecondLayerEdges(tmp);
  memMoveSum.Lines.Add('---> placeSecondLayerEdges');
  memMoveSum.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  s := s + solu;
  solu := '';
  // Step 4
  PlaceYellowEdges(tmp);
  memMoveSum.Lines.Add('---> PlaceYellowEdges');
  memMoveSum.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  s := s + solu;
  solu := '';
  // Step 5
  OrientYellowEdges(tmp);
  memMoveSum.Lines.Add('---> OrientYellowEdges');
  memMoveSum.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  s := s + solu;
  solu := '';
  // Step 6
  PlaceYellowCorners(tmp);
  memMoveSum.Lines.Add('---> PlaceYellowCorners');
  memMoveSum.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  s := s + solu;
  solu := '';
  // Step 7
  OrientYellowCorners(tmp);
  memMoveSum.Lines.Add('---> OrientYellowCorners');
  memMoveSum.Lines.Add(AnsiReplaceText(solu, '/', sLineBreak));
  s := s + solu;
  solu := '';

  memMoveSum.Lines.Add('movements ' + IntToStr(CountMoves(s)));
  filtre(s, original, imgFilters.canvas);
  pntBoxCurrentState.Refresh;
  memMoveSum.Lines.Add('movements after filter ' + IntToStr(CountMoves(s)) + ')');
  memMoveSum.Lines.Add('');
  memMoveSum.Lines.Add('---> Filter');
  memMoveSum.Lines.Add(s);
  edtMoveString.Text := s;
end;


end.
