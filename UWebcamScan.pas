unit UWebcamScan;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Buttons, Menus, UConst, BGRABitmap, BGRABitmapTypes, Process, DateUtils, IniFiles;

type
  { Color sample for training }
  TColorSample = record
    R, G, B: Integer;
  end;

  TColorSampleArray = array of TColorSample;

  { TWebcamReaderThread }

  TWebcamReaderThread = class(TThread)
  private
    FOwner: TObject;
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TObject);
  end;

  { TfrmWebcamScan }

  TfrmWebcamScan = class(TForm)
    btnCaptureFace: TSpeedButton;
    btnPreviousFace: TSpeedButton;
    btnTrainingMode: TSpeedButton;
    btnResetTraining: TSpeedButton;
    btnReset: TSpeedButton;
    btnDone: TSpeedButton;
    btnCancel: TSpeedButton;
    lblInstructions: TLabel;
    lblCurrentFace: TLabel;
    pnlWebcamViewer: TPanel;
    pntWebcam: TPaintBox;
    pmColorTraining: TPopupMenu;
    pnlColorIndicators: TPanel;
    lblTopColor: TLabel;
    lblLeftColor: TLabel;
    lblRightColor: TLabel;
    lblBottomColor: TLabel;
    pnlTopIndicator: TPanel;
    pnlLeftIndicator: TPanel;
    pnlRightIndicator: TPanel;
    pnlBottomIndicator: TPanel;
    pnlCubeLayout: TPanel;
    pntCubeLayout: TPaintBox;
    pnlControls: TPanel;
    tmrWebcam: TTimer;
    procedure btnCaptureFaceClick(Sender: TObject);
    procedure btnPreviousFaceClick(Sender: TObject);
    procedure btnTrainingModeClick(Sender: TObject);
    procedure btnResetTrainingClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure btnDoneClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure ColorMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure pntWebcamPaint(Sender: TObject);
    procedure pntWebcamMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure pntCubeLayoutPaint(Sender: TObject);
    procedure pntCubeLayoutMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: integer);
    procedure tmrWebcamTimer(Sender: TObject);
  private
    FCurrentFaceIndex: Integer;  // Index into FScanOrder (0-5)
    FScannedCube: TRubik;
    FFacesScanned: array[0..5] of Boolean;
    FScanOrder: array[0..5] of Integer;  // Order: F, R, B, L, U, D = [2, 3, 4, 5, 1, 6]
    FWebcamBitmap: TBGRABitmap;  // For webcam display
    FFrameBuffer: TBGRABitmap;  // For drawing with grid overlay
    FFFmpegProcess: TProcess;
    FReaderThread: TWebcamReaderThread;  // Background thread for continuous reading
    FFrameSize: Integer;  // Size of one frame in bytes
    FFrameAccumulator: Pointer;  // Buffer to accumulate frame data
    FBytesAccumulated: Integer;  // Bytes accumulated so far
    FTrainingMode: Boolean;  // True if in manual training mode
    FLearnedSamples: array[1..6] of TColorSampleArray;  // Multiple RGB samples for each color
    FColorsTrained: array[1..6] of Boolean;  // Which colors have been learned
    FTrainingSampleR, FTrainingSampleG, FTrainingSampleB: Integer;  // Temp storage for popup menu
    FTrainingSampleRow, FTrainingSampleCol: Integer;  // Cell position for training
    FDetectedColors: array[0..2, 0..2] of Byte;  // Real-time detected colors for current face
    FManualColors: array[0..2, 0..2] of Byte;  // Manually corrected colors (0 = not corrected)
    FCellRGB: array[0..2, 0..2, 0..2] of Integer;  // Sampled RGB for each cell
    FLastDetectionTime: QWord;  // Last time we ran color detection (for time-based limiting)
    procedure SetCurrentFace(AFaceIndex: Integer);
    procedure UpdateColorIndicators;
    procedure DrawWebcamGrid;
    procedure DrawCubeLayout;
    procedure StartWebcam;
    procedure StopWebcam;
    procedure ReadFrame;
    procedure CaptureFaceColors;
    procedure LearnColorFromSample(AColorNum: Integer; R, G, B: Integer);
    function DetectColorFromLearned(R, G, B: Integer): Byte;
    procedure SaveColorTraining;
    procedure LoadColorTraining;
    procedure CreateColorMenu;
    procedure UpdateColorMenuWithDetection(R, G, B: Integer);
    procedure ResetColorTraining;
    procedure DetectCellColors;
    function GetColorLetter(AColorNum: Byte): string;
    function GetFaceName(AFaceIndex: Integer): string;
    function GetFaceColor(AFaceIndex: Integer): TColor;
    procedure RefreshWebcamDisplay;  // Thread-safe method to refresh display
  public
    ScannedCubeState: TRubik;
    property CurrentFaceIndex: Integer read FCurrentFaceIndex;
  end;

var
  frmWebcamScan: TfrmWebcamScan;

implementation

uses UDraw, Math;

{$R *.lfm}

{ Color detection helper functions }

procedure RGBtoHSV(R, G, B: Byte; out H, S, V: Single);
var
  MinVal, MaxVal, Delta: Single;
  RNorm, GNorm, BNorm: Single;
begin
  RNorm := R / 255.0;
  GNorm := G / 255.0;
  BNorm := B / 255.0;

  MinVal := Min(Min(RNorm, GNorm), BNorm);
  MaxVal := Max(Max(RNorm, GNorm), BNorm);
  Delta := MaxVal - MinVal;

  // Value
  V := MaxVal;

  // Saturation
  if MaxVal > 0 then
    S := Delta / MaxVal
  else
    S := 0;

  // Hue
  if Delta = 0 then
    H := 0
  else if MaxVal = RNorm then
  begin
    H := (GNorm - BNorm) / Delta;
    if GNorm < BNorm then
      H := H + 6;
    H := 60 * H;
  end
  else if MaxVal = GNorm then
    H := 60 * ((BNorm - RNorm) / Delta + 2)
  else
    H := 60 * ((RNorm - GNorm) / Delta + 4);
end;

function DetectCubeColor(R, G, B: Byte): Byte;
var
  H, S, V: Single;
begin
  RGBtoHSV(R, G, B, H, S, V);

  // Default to white if very low saturation
  if S < 0.15 then
  begin
    if V > 0.6 then
      Result := 1  // White
    else
      Result := 0;  // Gray/black (shouldn't happen)
    Exit;
  end;

  // Detect color by hue ranges
  // Red: 0-15, 345-360
  if ((H >= 0) and (H <= 15)) or (H >= 345) then
    Result := 3  // Red
  // Orange: 15-35
  else if (H >= 15) and (H <= 35) then
    Result := 5  // Orange
  // Yellow: 35-75
  else if (H >= 35) and (H <= 75) then
    Result := 6  // Yellow
  // Green: 75-165
  else if (H >= 75) and (H <= 165) then
    Result := 2  // Green
  // Blue: 165-260
  else if (H >= 165) and (H <= 260) then
    Result := 4  // Blue
  else
    Result := 1;  // Default to white
end;

{ TfrmWebcamScan }

procedure TfrmWebcamScan.LearnColorFromSample(AColorNum: Integer; R, G, B: Integer);
var
  NewSample: TColorSample;
  Idx, SampleCount: Integer;
  IsDuplicate: Boolean;
  const
    MaxSamplesPerColor = 20;  // Limit to prevent unbounded growth
    DuplicateThreshold = 10;  // RGB distance threshold to consider duplicate
begin
  // Check if this sample is too similar to an existing one (avoid duplicates)
  IsDuplicate := False;
  SampleCount := Length(FLearnedSamples[AColorNum]);

  for Idx := 0 to SampleCount - 1 do
  begin
    if (Abs(FLearnedSamples[AColorNum][Idx].R - R) <= DuplicateThreshold) and
       (Abs(FLearnedSamples[AColorNum][Idx].G - G) <= DuplicateThreshold) and
       (Abs(FLearnedSamples[AColorNum][Idx].B - B) <= DuplicateThreshold) then
    begin
      IsDuplicate := True;
      Break;
    end;
  end;

  // Only add if not a duplicate
  if not IsDuplicate then
  begin
    // If we've hit the limit, remove the oldest sample (FIFO)
    if SampleCount >= MaxSamplesPerColor then
    begin
      // Shift all samples down
      for Idx := 0 to SampleCount - 2 do
        FLearnedSamples[AColorNum][Idx] := FLearnedSamples[AColorNum][Idx + 1];
      // Reuse the last slot
      FLearnedSamples[AColorNum][SampleCount - 1].R := R;
      FLearnedSamples[AColorNum][SampleCount - 1].G := G;
      FLearnedSamples[AColorNum][SampleCount - 1].B := B;
    end
    else
    begin
      // Add new sample
      NewSample.R := R;
      NewSample.G := G;
      NewSample.B := B;
      SetLength(FLearnedSamples[AColorNum], SampleCount + 1);
      FLearnedSamples[AColorNum][SampleCount] := NewSample;
    end;

    FColorsTrained[AColorNum] := True;
    SaveColorTraining;  // Auto-save after learning
  end;
end;

procedure TfrmWebcamScan.SaveColorTraining;
var
  Ini: TIniFile;
  ConfigPath: string;
  i, j, SampleCount: Integer;
  SectionName: string;
begin
  // Save to application directory instead of user config directory
  ConfigPath := ExtractFilePath(ParamStr(0)) + 'colortraining.ini';

  Ini := TIniFile.Create(ConfigPath);
  try
    // Clear all existing sections first
    for i := 1 to 6 do
    begin
      SectionName := 'Color' + IntToStr(i);
      Ini.EraseSection(SectionName);
    end;

    // Write all color samples
    for i := 1 to 6 do
    begin
      if FColorsTrained[i] then
      begin
        SectionName := 'Color' + IntToStr(i);
        SampleCount := Length(FLearnedSamples[i]);
        Ini.WriteInteger(SectionName, 'SampleCount', SampleCount);
        Ini.WriteBool(SectionName, 'Trained', True);

        // Write each sample
        for j := 0 to SampleCount - 1 do
        begin
          Ini.WriteInteger(SectionName, 'R' + IntToStr(j), FLearnedSamples[i][j].R);
          Ini.WriteInteger(SectionName, 'G' + IntToStr(j), FLearnedSamples[i][j].G);
          Ini.WriteInteger(SectionName, 'B' + IntToStr(j), FLearnedSamples[i][j].B);
        end;
      end;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TfrmWebcamScan.LoadColorTraining;
var
  Ini: TIniFile;
  ConfigPath: string;
  i, j, SampleCount: Integer;
  SectionName: string;
  Sample: TColorSample;
begin
  // Load from application directory instead of user config directory
  ConfigPath := ExtractFilePath(ParamStr(0)) + 'colortraining.ini';

  if not FileExists(ConfigPath) then Exit;

  Ini := TIniFile.Create(ConfigPath);
  try
    for i := 1 to 6 do
    begin
      SectionName := 'Color' + IntToStr(i);
      if Ini.ReadBool(SectionName, 'Trained', False) then
      begin
        SampleCount := Ini.ReadInteger(SectionName, 'SampleCount', 0);

        // For backward compatibility, check if old format exists
        if SampleCount = 0 then
        begin
          // Try old format with single R, G, B values
          if Ini.ValueExists(SectionName, 'R') then
          begin
            Sample.R := Ini.ReadInteger(SectionName, 'R', 0);
            Sample.G := Ini.ReadInteger(SectionName, 'G', 0);
            Sample.B := Ini.ReadInteger(SectionName, 'B', 0);
            SetLength(FLearnedSamples[i], 1);
            FLearnedSamples[i][0] := Sample;
            FColorsTrained[i] := True;
          end;
        end
        else
        begin
          // Load all samples in new format
          SetLength(FLearnedSamples[i], SampleCount);
          for j := 0 to SampleCount - 1 do
          begin
            FLearnedSamples[i][j].R := Ini.ReadInteger(SectionName, 'R' + IntToStr(j), 0);
            FLearnedSamples[i][j].G := Ini.ReadInteger(SectionName, 'G' + IntToStr(j), 0);
            FLearnedSamples[i][j].B := Ini.ReadInteger(SectionName, 'B' + IntToStr(j), 0);
          end;
          FColorsTrained[i] := True;
        end;
      end;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TfrmWebcamScan.CreateColorMenu;
var
  MenuItem: TMenuItem;
begin
  pmColorTraining := TPopupMenu.Create(Self);

  MenuItem := TMenuItem.Create(pmColorTraining);
  MenuItem.Caption := 'White';
  MenuItem.Tag := 1;
  MenuItem.OnClick := @ColorMenuClick;
  pmColorTraining.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(pmColorTraining);
  MenuItem.Caption := 'Green';
  MenuItem.Tag := 2;
  MenuItem.OnClick := @ColorMenuClick;
  pmColorTraining.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(pmColorTraining);
  MenuItem.Caption := 'Red';
  MenuItem.Tag := 3;
  MenuItem.OnClick := @ColorMenuClick;
  pmColorTraining.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(pmColorTraining);
  MenuItem.Caption := 'Blue';
  MenuItem.Tag := 4;
  MenuItem.OnClick := @ColorMenuClick;
  pmColorTraining.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(pmColorTraining);
  MenuItem.Caption := 'Orange';
  MenuItem.Tag := 5;
  MenuItem.OnClick := @ColorMenuClick;
  pmColorTraining.Items.Add(MenuItem);

  MenuItem := TMenuItem.Create(pmColorTraining);
  MenuItem.Caption := 'Yellow';
  MenuItem.Tag := 6;
  MenuItem.OnClick := @ColorMenuClick;
  pmColorTraining.Items.Add(MenuItem);
end;

procedure TfrmWebcamScan.UpdateColorMenuWithDetection(R, G, B: Integer);
var
  i: Integer;
  DetectedColor: Byte;
  ColorNames: array[1..6] of string = ('White', 'Green', 'Red', 'Blue', 'Orange', 'Yellow');
begin
  // Detect which color this sample matches
  DetectedColor := DetectColorFromLearned(R, G, B);

  // Update all menu items to show detection
  for i := 0 to pmColorTraining.Items.Count - 1 do
  begin
    if pmColorTraining.Items[i].Tag = DetectedColor then
      pmColorTraining.Items[i].Caption := ColorNames[pmColorTraining.Items[i].Tag] + ' ✓ (detected)'
    else
      pmColorTraining.Items[i].Caption := ColorNames[pmColorTraining.Items[i].Tag];
  end;
end;

procedure TfrmWebcamScan.ResetColorTraining;
var
  i: Integer;
  ConfigPath: string;
begin
  // Clear all training data
  for i := 1 to 6 do
  begin
    FColorsTrained[i] := False;
    SetLength(FLearnedSamples[i], 0);  // Clear the dynamic array
  end;

  // Delete the config file from application directory
  ConfigPath := ExtractFilePath(ParamStr(0)) + 'colortraining.ini';
  if FileExists(ConfigPath) then
    DeleteFile(ConfigPath);

  lblInstructions.Caption := 'Training data has been reset. Click on cells to train new colors.';
end;

function TfrmWebcamScan.GetColorLetter(AColorNum: Byte): string;
begin
  case AColorNum of
    1: Result := 'W';  // White
    2: Result := 'G';  // Green
    3: Result := 'R';  // Red
    4: Result := 'B';  // Blue
    5: Result := 'O';  // Orange
    6: Result := 'Y';  // Yellow
  else
    Result := '?';
  end;
end;

procedure TfrmWebcamScan.DetectCellColors;
var
  Row, Col: Integer;
  CenterX, CenterY, GridSize, CellSize: Integer;
  SampleX, SampleY, SampleRadius: Integer;
  dx, dy, SampleCount: Integer;
  RSum, GSum, BSum: Integer;
  Pixel: TBGRAPixel;
begin
  if FWebcamBitmap.Width = 0 then Exit;

  // Calculate grid parameters
  CenterX := FWebcamBitmap.Width div 2;
  CenterY := FWebcamBitmap.Height div 2;
  GridSize := 300;
  CellSize := GridSize div 3;
  SampleRadius := 5;  // Reduced from 15 to 5 for much faster sampling

  // Sample each cell
  for Row := 0 to 2 do
  begin
    for Col := 0 to 2 do
    begin
      // Skip if manually corrected
      if FManualColors[Row, Col] <> 0 then
      begin
        FDetectedColors[Row, Col] := FManualColors[Row, Col];
        Continue;
      end;

      // Sample the center of this cell
      SampleX := (CenterX - GridSize div 2) + (Col * CellSize) + (CellSize div 2);
      SampleY := (CenterY - GridSize div 2) + (Row * CellSize) + (CellSize div 2);

      RSum := 0; GSum := 0; BSum := 0; SampleCount := 0;

      // Sample a circle
      for dy := -SampleRadius to SampleRadius do
      begin
        for dx := -SampleRadius to SampleRadius do
        begin
          // Only sample within circle
          if (dx * dx + dy * dy) <= (SampleRadius * SampleRadius) then
          begin
            if (SampleX + dx >= 0) and (SampleX + dx < FWebcamBitmap.Width) and
               (SampleY + dy >= 0) and (SampleY + dy < FWebcamBitmap.Height) then
            begin
              Pixel := FWebcamBitmap.GetPixel(SampleX + dx, SampleY + dy);
              RSum += Pixel.red;
              GSum += Pixel.green;
              BSum += Pixel.blue;
              Inc(SampleCount);
            end;
          end;
        end;
      end;

      if SampleCount > 0 then
      begin
        RSum := RSum div SampleCount;
        GSum := GSum div SampleCount;
        BSum := BSum div SampleCount;

        // Store RGB for this cell
        FCellRGB[Row, Col, 0] := RSum;
        FCellRGB[Row, Col, 1] := GSum;
        FCellRGB[Row, Col, 2] := BSum;

        // Detect color
        FDetectedColors[Row, Col] := DetectColorFromLearned(RSum, GSum, BSum);
      end;
    end;
  end;
end;

function TfrmWebcamScan.DetectColorFromLearned(R, G, B: Integer): Byte;
var
  i, j, BestColor, SampleCount: Integer;
  MinDistance, Distance: Double;
  dR, dG, dB: Integer;
  TrainedCount: Integer;
begin
  // Count how many colors we've trained
  TrainedCount := 0;
  for i := 1 to 6 do
    if FColorsTrained[i] then
      Inc(TrainedCount);

  // If we don't have enough trained colors, fall back to HSV detection
  if TrainedCount < 3 then
  begin
    Result := DetectCubeColor(R, G, B);
    Exit;
  end;

  // Find closest matching learned color using Euclidean distance
  // Check against ALL samples for each color and use the minimum distance
  MinDistance := 999999;
  BestColor := 1;

  for i := 1 to 6 do
  begin
    if FColorsTrained[i] then
    begin
      SampleCount := Length(FLearnedSamples[i]);

      // Check distance to each sample for this color
      for j := 0 to SampleCount - 1 do
      begin
        dR := R - FLearnedSamples[i][j].R;
        dG := G - FLearnedSamples[i][j].G;
        dB := B - FLearnedSamples[i][j].B;
        Distance := Sqrt(dR * dR + dG * dG + dB * dB);

        if Distance < MinDistance then
        begin
          MinDistance := Distance;
          BestColor := i;
        end;
      end;
    end;
  end;

  Result := BestColor;
end;

procedure TfrmWebcamScan.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  // Define scanning order: F(2), R(3), B(4), L(5), U(1), D(6)
  FScanOrder[0] := 2;  // Green (Front)
  FScanOrder[1] := 3;  // Red (Right)
  FScanOrder[2] := 4;  // Blue (Back)
  FScanOrder[3] := 5;  // Orange (Left)
  FScanOrder[4] := 1;  // White (Up)
  FScanOrder[5] := 6;  // Yellow (Down)

  FCurrentFaceIndex := 0;  // Start with first face in order
  FScannedCube := C_CUBE_COMPLETE;  // Initialize to solved state
  FTrainingMode := False;

  // Initialize color training arrays
  for i := 1 to 6 do
  begin
    FColorsTrained[i] := False;
    SetLength(FLearnedSamples[i], 0);  // Initialize empty dynamic array for each color
  end;

  // Create bitmaps for video display
  FWebcamBitmap := TBGRABitmap.Create(640, 480, BGRABlack);
  FFrameBuffer := TBGRABitmap.Create(640, 480, BGRABlack);
  FFrameSize := 640 * 480 * 3;  // Width * Height * 3 bytes per pixel (BGR24)

  // Allocate frame accumulator buffer
  GetMem(FFrameAccumulator, FFrameSize);
  FBytesAccumulated := 0;

  // Initialize time-based color detection
  FLastDetectionTime := GetTickCount64;

  // Initialize all faces as not scanned
  FFacesScanned[0] := False;
  FFacesScanned[1] := False;
  FFacesScanned[2] := False;
  FFacesScanned[3] := False;
  FFacesScanned[4] := False;
  FFacesScanned[5] := False;

  SetCurrentFace(0);
  btnPreviousFace.Enabled := False;
  btnDone.Enabled := False;

  // Create popup menu for color training
  CreateColorMenu;

  // Load previously saved color training
  LoadColorTraining;

  // Setup FFmpeg process (will be started in FormShow)
  FFFmpegProcess := nil;
  tmrWebcam.Enabled := False;
  tmrWebcam.Interval := 33;  // About 30 FPS
end;

procedure TfrmWebcamScan.FormShow(Sender: TObject);
begin
  StartWebcam;
end;

procedure TfrmWebcamScan.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  StopWebcam;
end;

procedure TfrmWebcamScan.FormDestroy(Sender: TObject);
begin
  StopWebcam;
  FWebcamBitmap.Free;
  FFrameBuffer.Free;
  FreeMem(FFrameAccumulator);
end;

procedure TfrmWebcamScan.StartWebcam;
var
  DevIndex: Integer;
  DevicePath: string;
  Probe, Test: TProcess;
  OutputLines: TStringList;
  PreferredFmt: string;
  Line: string;
  StderrStream: TMemoryStream;
  FoundDevice: Boolean;
  total, bytesRead: Integer;
  chunk: array[0..8191] of byte;
  startTime: TDateTime;
begin
  StopWebcam;
  FoundDevice := False;
  PreferredFmt := '';

  {$IFDEF LINUX}
  for DevIndex := 0 to 9 do
  begin
    DevicePath := Format('/dev/video%d', [DevIndex]);
    if not FileExists(DevicePath) then Continue;

    // --- probe formats for this device ---
    Probe := TProcess.Create(nil);
    OutputLines := TStringList.Create;
    StderrStream := TMemoryStream.Create;
    try
      Probe.Executable := 'ffmpeg';
      Probe.Options := [poUsePipes, poStderrToOutput, poNoConsole];
      Probe.Parameters.AddStrings([
        '-hide_banner', '-f', 'v4l2', '-list_formats', 'all', '-i', DevicePath
      ]);

      try
        Probe.Execute;
        Sleep(100);
        if Probe.Output.NumBytesAvailable > 0 then
        begin
          StderrStream.CopyFrom(Probe.Output, Probe.Output.NumBytesAvailable);
          StderrStream.Position := 0;
          OutputLines.LoadFromStream(StderrStream);
        end;
        Probe.Terminate(0);
      except
        on E: Exception do Continue;
      end;

      for Line in OutputLines do
      begin
        if (Pos('yuyv', LowerCase(Line)) > 0) then
          PreferredFmt := 'yuyv422'
        else if (Pos('mjpeg', LowerCase(Line)) > 0) and (PreferredFmt = '') then
          PreferredFmt := 'mjpeg'
        else if (Pos('rgb', LowerCase(Line)) > 0) and (PreferredFmt = '') then
          PreferredFmt := 'rgb24';
      end;

      Probe.Free;
      OutputLines.Free;
      StderrStream.Free;

      if PreferredFmt = '' then PreferredFmt := 'yuyv422';

      // --- quick test to see if we get actual frames ---
      Test := TProcess.Create(nil);
      Test.Executable := 'ffmpeg';
      Test.Options := [poUsePipes, poStderrToOutput, poNoConsole];
      Test.Parameters.AddStrings([
        '-hide_banner',
        '-loglevel', 'error',
        '-f', 'v4l2',
        '-input_format', PreferredFmt,
        '-video_size', '320x240',
        '-i', DevicePath,
        '-frames:v', '1',
        '-pix_fmt', 'bgr24',
        '-vcodec', 'rawvideo',
        '-f', 'rawvideo', '-'
      ]);
      try
        Test.Execute;

        // Wait longer, up to 2 seconds, reading chunks
        total := 0;
        startTime := Now;
        repeat
          Sleep(100);
          if Test.Output.NumBytesAvailable > 0 then
          begin
            bytesRead := Test.Output.Read(chunk, SizeOf(chunk));
            Inc(total, bytesRead);
          end;
        until (MilliSecondsBetween(Now, startTime) > 2000) or (total > 1000);

        if total > 1000 then
        begin
          FoundDevice := True;
          Break;
        end;
      finally
        Test.Terminate(0);
        Test.Free;
      end;
    except
      on E: Exception do;
    end;

    if FoundDevice then Break;
  end;

  if not FoundDevice then
  begin
    ShowMessage(Format('No working webcam detected on /dev/video0-9.%sLast tried: %s with format %s',
      [LineEnding, DevicePath, PreferredFmt]));
    Exit;
  end;
  {$ELSE}
  DevicePath := 'video=USB Video Device';
  PreferredFmt := 'mjpeg';
  {$ENDIF}

  // --- Start the real feed ---
  try
    FFFmpegProcess := TProcess.Create(nil);
    FFFmpegProcess.Executable := 'ffmpeg';
    FFFmpegProcess.Options := [poUsePipes, poNoConsole];

    {$IFDEF LINUX}
    FFFmpegProcess.Parameters.AddStrings([
      '-hide_banner', '-loglevel', 'error',
      '-fflags', 'nobuffer',
      '-flags', 'low_delay',
      '-thread_queue_size', '1',
      '-use_wallclock_as_timestamps', '1',
      '-f', 'v4l2',
      '-input_format', PreferredFmt,
      '-framerate', '30',
      '-video_size', '640x480',
      '-i', DevicePath,
      '-vf', 'fps=15,format=bgr24',
      '-vcodec', 'rawvideo',
      '-f', 'rawvideo',
      '-flush_packets', '1',
      '-'
    ]);
    {$ENDIF}

    {$IFDEF WINDOWS}
    FFFmpegProcess.Parameters.AddStrings([
      '-hide_banner', '-loglevel', 'error',
      '-fflags', 'nobuffer',
      '-flags', 'low_delay',
      '-f', 'dshow',
      '-framerate', '30',
      '-video_size', '640x480',
      '-i', 'video=USB Video Device',
      '-pix_fmt', 'rgb24',
      '-vcodec', 'rawvideo',
      '-f', 'rawvideo', '-'
    ]);
    {$ENDIF}

    {$IFDEF DARWIN}
    FFFmpegProcess.Parameters.AddStrings([
      '-hide_banner', '-loglevel', 'error',
      '-fflags', 'nobuffer',
      '-flags', 'low_delay',
      '-f', 'avfoundation',
      '-framerate', '30',
      '-video_size', '640x480',
      '-i', '0',
      '-pix_fmt', 'rgb24',
      '-vcodec', 'rawvideo',
      '-f', 'rawvideo', '-'
    ]);
    {$ENDIF}

    FFFmpegProcess.Execute;
    FBytesAccumulated := 0;  // Reset accumulator

    // Start reader thread (instead of timer)
    FReaderThread := TWebcamReaderThread.Create(Self);

    //ShowMessage(Format('Webcam started: %s (%s)%sFFmpeg PID: %d',
    //  [DevicePath, PreferredFmt, LineEnding, FFFmpegProcess.ProcessID]));
  except
    on E: Exception do
    begin
      ShowMessage('Unable to start webcam: ' + E.Message);
      FreeAndNil(FFFmpegProcess);
    end;
  end;
end;

procedure TfrmWebcamScan.StopWebcam;
begin
  // Clear thread's owner reference first to prevent access after destruction
  if Assigned(FReaderThread) then
  begin
    FReaderThread.Terminate;
    TWebcamReaderThread(FReaderThread).FOwner := nil;  // Prevent further access
    FReaderThread := nil;  // let it free itself after loop exits
  end;

  if Assigned(FFFmpegProcess) then
  begin
    try
      if FFFmpegProcess.Running then
        FFFmpegProcess.Terminate(0);
    except end;
    FreeAndNil(FFFmpegProcess);
  end;
end;

procedure TfrmWebcamScan.RefreshWebcamDisplay;
begin
  // This method is called from main thread via TThread.Queue
  // Check component validity before accessing
  if Assigned(pntWebcam) and not (csDestroying in ComponentState) then
    pntWebcam.Invalidate;
end;

procedure TfrmWebcamScan.ReadFrame;
var
  Available, BytesToDrop, BytesRead, y, x: Integer;
  Src: PByte;
  Dst: PBGRAPixel;
  Tmp: array[0..65535] of Byte;
begin
  // Safety checks to prevent accessing freed or invalid objects
  if (FFFmpegProcess = nil) or (not FFFmpegProcess.Running) then Exit;
  if (FWebcamBitmap = nil) or (FFrameAccumulator = nil) then Exit;
  if (FFrameSize <= 0) then Exit;

  // Drop any backlog (keep only the last frame's worth)
  Available := FFFmpegProcess.Output.NumBytesAvailable;
  if (Available + FBytesAccumulated) > (FFrameSize * 2) then
  begin
    BytesToDrop := (Available + FBytesAccumulated) - FFrameSize;
    FBytesAccumulated := 0;
    while BytesToDrop > 0 do
    begin
      BytesRead := FFFmpegProcess.Output.Read(Tmp, Min(BytesToDrop, SizeOf(Tmp)));
      if BytesRead <= 0 then Break;
      Dec(BytesToDrop, BytesRead);
    end;
  end;

  // Read until we have one full frame
  while (FBytesAccumulated < FFrameSize) and (FFFmpegProcess.Output.NumBytesAvailable > 0) do
  begin
    BytesRead := FFFmpegProcess.Output.Read(PByte(FFrameAccumulator)[FBytesAccumulated],
                                            Min(FFrameSize - FBytesAccumulated, 65536));
    if BytesRead <= 0 then Break;
    Inc(FBytesAccumulated, BytesRead);
  end;

  if FBytesAccumulated < FFrameSize then Exit;

  // Convert newest frame to BGRA
  for y := 0 to FWebcamBitmap.Height - 1 do
  begin
    Dst := FWebcamBitmap.ScanLine[y];
    Src := @PByte(FFrameAccumulator)[y * FWebcamBitmap.Width * 3];
    for x := 0 to FWebcamBitmap.Width - 1 do
    begin
      Dst^.blue  := Src^;
      Inc(Src);
      Dst^.green := Src^;
      Inc(Src);
      Dst^.red   := Src^;
      Inc(Src);
      Dst^.alpha := 255;
      Inc(Dst);
    end;
  end;

  FBytesAccumulated := 0;
  // Note: Invalidate will be called by thread using Queue
end;

procedure TfrmWebcamScan.tmrWebcamTimer(Sender: TObject);
begin
  ReadFrame; // once
end;

procedure TfrmWebcamScan.SetCurrentFace(AFaceIndex: Integer);
var
  i, j: Integer;
begin
  FCurrentFaceIndex := AFaceIndex;
  lblCurrentFace.Caption := 'Scanning Face: ' + GetFaceName(AFaceIndex);

  // Clear manual corrections for the new face
  for i := 0 to 2 do
    for j := 0 to 2 do
    begin
      FManualColors[i, j] := 0;
      FDetectedColors[i, j] := 0;
    end;

  UpdateColorIndicators;
  pntWebcam.Invalidate;
end;

function TfrmWebcamScan.GetFaceName(AFaceIndex: Integer): string;
begin
  case AFaceIndex of
    0: Result := 'Step 1/6: GREEN (Front)';
    1: Result := 'Step 2/6: RED (Right)';
    2: Result := 'Step 3/6: BLUE (Back)';
    3: Result := 'Step 4/6: ORANGE (Left)';
    4: Result := 'Step 5/6: WHITE (Top)';
    5: Result := 'Step 6/6: YELLOW (Bottom)';
  else
    Result := 'Unknown';
  end;
end;

function TfrmWebcamScan.GetFaceColor(AFaceIndex: Integer): TColor;
begin
  // AFaceIndex is now the actual face number - 1 (0-5)
  // 0=White, 1=Green, 2=Red, 3=Blue, 4=Orange, 5=Yellow
  case AFaceIndex of
    0: Result := clWhite;   // Face 1 (U)
    1: Result := clGreen;   // Face 2 (F)
    2: Result := clRed;     // Face 3 (R)
    3: Result := clBlue;    // Face 4 (B)
    4: Result := $0080FF;   // Face 5 (L) Orange
    5: Result := clYellow;  // Face 6 (D)
  else
    Result := clGray;
  end;
end;

procedure TfrmWebcamScan.UpdateColorIndicators;
var
  TopFace, LeftFace, RightFace, BottomFace: Integer;
  CurrentFace: Integer;
begin
  // Get the actual face number from scan order
  CurrentFace := FScanOrder[FCurrentFaceIndex];

  // Based on current face, determine what adjacent faces should be visible
  // From camera's perspective looking at the cube
  case CurrentFace of
    1: begin  // U (Top) - looking down at cube
      TopFace := 4;    // Back
      LeftFace := 3;   // Right (camera's left)
      RightFace := 5;  // Left (camera's right)
      BottomFace := 2; // Front
    end;
    2: begin  // F (Front/Green) - looking at front
      TopFace := 1;    // Top (White)
      LeftFace := 3;   // Right (Red) - camera's left
      RightFace := 5;  // Left (Orange) - camera's right
      BottomFace := 6; // Bottom (Yellow)
    end;
    3: begin  // R (Right/Red) - looking at right side
      TopFace := 1;    // Top
      LeftFace := 4;   // Back - camera's left
      RightFace := 2;  // Front - camera's right
      BottomFace := 6; // Bottom
    end;
    4: begin  // B (Back/Blue) - looking at back
      TopFace := 1;    // Top
      LeftFace := 5;   // Left (Orange) - camera's left
      RightFace := 3;  // Right (Red) - camera's right
      BottomFace := 6; // Bottom
    end;
    5: begin  // L (Left/Orange) - looking at left side
      TopFace := 1;    // Top
      LeftFace := 2;   // Front - camera's left
      RightFace := 4;  // Back - camera's right
      BottomFace := 6; // Bottom
    end;
    6: begin  // D (Bottom) - looking up at cube
      TopFace := 2;    // Front
      LeftFace := 3;   // Right (camera's left)
      RightFace := 5;  // Left (camera's right)
      BottomFace := 4; // Back
    end;
  else
    TopFace := 0; LeftFace := 0; RightFace := 0; BottomFace := 0;
  end;

  // Update indicator colors using actual face numbers
  pnlTopIndicator.Color := GetFaceColor(TopFace - 1);
  pnlLeftIndicator.Color := GetFaceColor(LeftFace - 1);
  pnlRightIndicator.Color := GetFaceColor(RightFace - 1);
  pnlBottomIndicator.Color := GetFaceColor(BottomFace - 1);

  // Update labels with face names
  case TopFace of
    1: lblTopColor.Caption := 'White';
    2: lblTopColor.Caption := 'Green';
    3: lblTopColor.Caption := 'Red';
    4: lblTopColor.Caption := 'Blue';
    5: lblTopColor.Caption := 'Orange';
    6: lblTopColor.Caption := 'Yellow';
  end;
  case LeftFace of
    1: lblLeftColor.Caption := 'White';
    2: lblLeftColor.Caption := 'Green';
    3: lblLeftColor.Caption := 'Red';
    4: lblLeftColor.Caption := 'Blue';
    5: lblLeftColor.Caption := 'Orange';
    6: lblLeftColor.Caption := 'Yellow';
  end;
  case RightFace of
    1: lblRightColor.Caption := 'White';
    2: lblRightColor.Caption := 'Green';
    3: lblRightColor.Caption := 'Red';
    4: lblRightColor.Caption := 'Blue';
    5: lblRightColor.Caption := 'Orange';
    6: lblRightColor.Caption := 'Yellow';
  end;
  case BottomFace of
    1: lblBottomColor.Caption := 'White';
    2: lblBottomColor.Caption := 'Green';
    3: lblBottomColor.Caption := 'Red';
    4: lblBottomColor.Caption := 'Blue';
    5: lblBottomColor.Caption := 'Orange';
    6: lblBottomColor.Caption := 'Yellow';
  end;
end;

procedure TfrmWebcamScan.DrawWebcamGrid;
var
  i, Col: Integer;
  CenterX, CenterY, GridSize, CellSize: Integer;
  ExpectedColor: TColor;
  BGRA_Color: TBGRAPixel;
  CenterCellX, CenterCellY: Integer;
begin
  // Copy webcam bitmap to BGRA buffer for overlay drawing
  FFrameBuffer.Assign(FWebcamBitmap);

  // Draw 3x3 grid overlay on top of webcam feed
  CenterX := FFrameBuffer.Width div 2;
  CenterY := FFrameBuffer.Height div 2;
  GridSize := 300;  // Total grid size
  CellSize := GridSize div 3;

  // Get expected center color for this face
  ExpectedColor := GetFaceColor(FScanOrder[FCurrentFaceIndex] - 1);
  BGRA_Color := ColorToBGRA(ExpectedColor, 100);  // Semi-transparent

  // Fill center cell with expected color
  CenterCellX := CenterX - CellSize div 2;
  CenterCellY := CenterY - CellSize div 2;
  FFrameBuffer.FillRect(CenterCellX + 5, CenterCellY + 5,
    CenterCellX + CellSize - 5, CenterCellY + CellSize - 5,
    BGRA_Color, dmDrawWithTransparency);

  // Draw grid cells
  for i := 0 to 3 do
  begin
    // Vertical lines
    FFrameBuffer.DrawLineAntialias(
      CenterX - GridSize div 2 + i * CellSize,
      CenterY - GridSize div 2,
      CenterX - GridSize div 2 + i * CellSize,
      CenterY + GridSize div 2,
      BGRA(255, 255, 0, 200), 3);

    // Horizontal lines
    FFrameBuffer.DrawLineAntialias(
      CenterX - GridSize div 2,
      CenterY - GridSize div 2 + i * CellSize,
      CenterX + GridSize div 2,
      CenterY - GridSize div 2 + i * CellSize,
      BGRA(255, 255, 0, 200), 3);
  end;

  // Detect colors only 1-2 times per second using time-based limiting
  if (GetTickCount64 - FLastDetectionTime) >= 500 then  // 500ms = 2 times per second
  begin
    DetectCellColors;
    FLastDetectionTime := GetTickCount64;
  end;

  // Draw color labels in each cell
  FFrameBuffer.FontHeight := 48;
  FFrameBuffer.FontName := 'Sans';
  FFrameBuffer.FontQuality := fqFineAntialiasing;

  for i := 0 to 2 do
  begin
    for Col := 0 to 2 do
    begin
      // Calculate cell center position
      CenterCellX := CenterX - GridSize div 2 + (Col * (GridSize div 3)) + ((GridSize div 3) div 2);
      CenterCellY := CenterY - GridSize div 2 + (i * (GridSize div 3)) + ((GridSize div 3) div 2);

      // Draw color letter with shadow for readability
      // Shadow
      FFrameBuffer.TextOut(CenterCellX - 14, CenterCellY - 14,
        GetColorLetter(FDetectedColors[i, Col]),
        BGRA(0, 0, 0, 220));
      // Text - highlight manually corrected cells in yellow
      if FManualColors[i, Col] <> 0 then
        FFrameBuffer.TextOut(CenterCellX - 15, CenterCellY - 15,
          GetColorLetter(FDetectedColors[i, Col]),
          BGRA(255, 255, 0, 255))
      else
        FFrameBuffer.TextOut(CenterCellX - 15, CenterCellY - 15,
          GetColorLetter(FDetectedColors[i, Col]),
          BGRA(255, 255, 255, 255));
    end;
  end;

  // Draw instructions text
  FFrameBuffer.FontHeight := 18;
  FFrameBuffer.TextOut(CenterX - 139, 21,
    'Right-click cells to correct. Then capture.',
    BGRA(0, 0, 0, 200));
  FFrameBuffer.TextOut(CenterX - 140, 20,
    'Right-click cells to correct. Then capture.',
    BGRA(255, 255, 0, 255));
end;

procedure TfrmWebcamScan.pntWebcamPaint(Sender: TObject);
begin
  if FWebcamBitmap.Width > 0 then
  begin
    DrawWebcamGrid;
    FFrameBuffer.Draw(pntWebcam.Canvas, 0, 0, True);
  end;
end;

procedure TfrmWebcamScan.pntWebcamMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  GridRow, GridCol: Integer;
  CenterX, CenterY, GridSize, CellSize: Integer;
begin
  // Only process right-clicks for color correction
  if Button <> mbRight then Exit;

  // Calculate grid parameters (same as DrawWebcamGrid)
  CenterX := pntWebcam.Width div 2;
  CenterY := pntWebcam.Height div 2;
  GridSize := 300;
  CellSize := GridSize div 3;

  // Determine which grid cell was clicked
  GridCol := (X - (CenterX - GridSize div 2)) div CellSize;
  GridRow := (Y - (CenterY - GridSize div 2)) div CellSize;

  // Check if click is within grid bounds
  if (GridRow >= 0) and (GridRow < 3) and (GridCol >= 0) and (GridCol < 3) then
  begin
    // Store the cell position for the correction
    FTrainingSampleRow := GridRow;
    FTrainingSampleCol := GridCol;

    // Get the current RGB values for this cell
    FTrainingSampleR := FCellRGB[GridRow, GridCol, 0];
    FTrainingSampleG := FCellRGB[GridRow, GridCol, 1];
    FTrainingSampleB := FCellRGB[GridRow, GridCol, 2];

    // Update menu items to show currently detected color
    UpdateColorMenuWithDetection(FTrainingSampleR, FTrainingSampleG, FTrainingSampleB);

    // Show popup menu for color selection
    pmColorTraining.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  end;
end;

procedure TfrmWebcamScan.ColorMenuClick(Sender: TObject);
var
  ColorNum: Integer;
  ColorNames: array[1..6] of string = ('White', 'Green', 'Red', 'Blue', 'Orange', 'Yellow');
begin
  if Sender is TMenuItem then
  begin
    ColorNum := TMenuItem(Sender).Tag;

    // Mark this cell as manually corrected
    FManualColors[FTrainingSampleRow, FTrainingSampleCol] := ColorNum;
    FDetectedColors[FTrainingSampleRow, FTrainingSampleCol] := ColorNum;

    // Learn from this correction
    LearnColorFromSample(ColorNum, FTrainingSampleR, FTrainingSampleG, FTrainingSampleB);

    // Show feedback
    lblInstructions.Caption := Format('Corrected to %s. Learning from RGB(%d, %d, %d).',
      [ColorNames[ColorNum], FTrainingSampleR, FTrainingSampleG, FTrainingSampleB]);
  end;
end;

procedure TfrmWebcamScan.DrawCubeLayout;
begin
  // Use existing DrawCube function to show the scanned state
  DrawCube(pntCubeLayout, FScannedCube);
end;

procedure TfrmWebcamScan.pntCubeLayoutPaint(Sender: TObject);
begin
  DrawCubeLayout;
end;

procedure TfrmWebcamScan.pntCubeLayoutMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: integer);
var
  GridX, GridY: Integer;
  FaceNum, Row, Col, CellIndex: Integer;
  SpaceForWidth, SpaceForHeight, CubeySize: Integer;
  CurrentColor: Byte;

  // Face grid positions (same as in UDraw.pas)
  function IsCellInFace(AGridX, AGridY, AFace: Integer; out ARow, ACol: Integer): Boolean;
  const
    FaceGrids: array[1..6, 0..1] of TPoint = (
      ((X: 3; Y: 0), (X: 5; Y: 2)),  // Face 1 (White/U): cols 3-5, rows 0-2
      ((X: 3; Y: 3), (X: 5; Y: 5)),  // Face 2 (Green/F): cols 3-5, rows 3-5
      ((X: 6; Y: 3), (X: 8; Y: 5)),  // Face 3 (Red/R): cols 6-8, rows 3-5
      ((X: 9; Y: 3), (X: 11; Y: 5)), // Face 4 (Blue/B): cols 9-11, rows 3-5
      ((X: 0; Y: 3), (X: 2; Y: 5)),  // Face 5 (Orange/L): cols 0-2, rows 3-5
      ((X: 3; Y: 6), (X: 5; Y: 8))   // Face 6 (Yellow/D): cols 3-5, rows 6-8
    );
  begin
    Result := False;
    if (AGridX >= FaceGrids[AFace, 0].X) and (AGridX <= FaceGrids[AFace, 1].X) and
       (AGridY >= FaceGrids[AFace, 0].Y) and (AGridY <= FaceGrids[AFace, 1].Y) then
    begin
      ACol := AGridX - FaceGrids[AFace, 0].X;
      ARow := AGridY - FaceGrids[AFace, 0].Y;
      Result := True;
    end;
  end;

begin
  // Calculate grid position from mouse click
  SpaceForWidth := pntCubeLayout.Width div 12;
  SpaceForHeight := pntCubeLayout.Height div 9;
  CubeySize := Min(SpaceForWidth, SpaceForHeight);

  GridX := X div CubeySize;
  GridY := Y div CubeySize;

  // Find which face was clicked
  FaceNum := 0;
  for FaceNum := 1 to 6 do
  begin
    if IsCellInFace(GridX, GridY, FaceNum, Row, Col) then
    begin
      // Calculate cell index (0-8)
      CellIndex := Row * 3 + Col;

      // Get current color and cycle to next
      CurrentColor := FScannedCube[FaceNum, CellIndex];

      // Cycle through colors: 1(White) -> 2(Green) -> 3(Red) -> 4(Blue) -> 5(Orange) -> 6(Yellow) -> 1...
      Inc(CurrentColor);
      if CurrentColor > 6 then
        CurrentColor := 1;

      FScannedCube[FaceNum, CellIndex] := CurrentColor;

      // Redraw
      pntCubeLayout.Invalidate;
      Break;
    end;
  end;
end;

procedure TfrmWebcamScan.CaptureFaceColors;
var
  Row, Col, CellIndex: Integer;
  FaceNum: Integer;
begin
  // Get the actual face number from scan order
  FaceNum := FScanOrder[FCurrentFaceIndex];

  // Use the already-detected colors (from real-time detection or manual corrections)
  for Row := 0 to 2 do
  begin
    for Col := 0 to 2 do
    begin
      // Skip center piece - it may have a logo and shouldn't be used for training
      if not ((Row = 1) and (Col = 1)) then
      begin
        LearnColorFromSample(FDetectedColors[Row, Col],
          FCellRGB[Row, Col, 0],
          FCellRGB[Row, Col, 1],
          FCellRGB[Row, Col, 2]);
      end;

      // Store in the cube state (including center piece)
      CellIndex := Row * 3 + Col;  // 0-8
      FScannedCube[FaceNum, CellIndex] := FDetectedColors[Row, Col];
    end;
  end;

  lblInstructions.Caption := 'Face captured! Learning from 8 edge cells (center skipped).';
end;

procedure TfrmWebcamScan.btnCaptureFaceClick(Sender: TObject);
begin
  // Capture colors from the current webcam frame
  CaptureFaceColors;

  // Mark as scanned
  FFacesScanned[FCurrentFaceIndex] := True;

  // Refresh cube layout to show captured colors
  pntCubeLayout.Invalidate;

  // Auto-advance to next face or enable Done if finished
  if FCurrentFaceIndex < 5 then
  begin
    SetCurrentFace(FCurrentFaceIndex + 1);
    btnPreviousFace.Enabled := True;  // Can now go back
  end
  else
  begin
    // All faces captured
    btnDone.Enabled := True;
  end;
end;

procedure TfrmWebcamScan.btnPreviousFaceClick(Sender: TObject);
begin
  // Move to previous face to rescan
  if FCurrentFaceIndex > 0 then
  begin
    SetCurrentFace(FCurrentFaceIndex - 1);
    if FCurrentFaceIndex = 0 then
      btnPreviousFace.Enabled := False;
  end;
end;

procedure TfrmWebcamScan.btnTrainingModeClick(Sender: TObject);
begin
  FTrainingMode := not FTrainingMode;
  if FTrainingMode then
  begin
    btnTrainingMode.Caption := '✓ Training Mode ON';
    btnCaptureFace.Enabled := False;
    btnPreviousFace.Enabled := False;
    btnDone.Visible := False;
    btnResetTraining.Visible := True;
    btnResetTraining.Enabled := True;
    lblInstructions.Caption := 'CLICK on any grid cell to train the color detector.' + LineEnding +
      'Select the actual color from the menu. This improves accuracy for your cube and lighting.';
    lblCurrentFace.Caption := 'Training Mode - Click cells to identify colors';
  end
  else
  begin
    btnTrainingMode.Caption := '🎓 Training Mode';
    btnCaptureFace.Enabled := True;
    if FCurrentFaceIndex > 0 then
      btnPreviousFace.Enabled := True;
    btnDone.Visible := True;
    btnResetTraining.Visible := False;
    lblInstructions.Caption := 'Position the cube face to match the center color.' + LineEnding +
      'Make sure the cube fills the grid, then click Capture Face.';
    SetCurrentFace(FCurrentFaceIndex);  // Refresh face label
  end;
end;

procedure TfrmWebcamScan.btnResetTrainingClick(Sender: TObject);
begin
  if MessageDlg('Reset Training Data', 'Are you sure you want to reset all color training data?',
    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    ResetColorTraining;
  end;
end;

procedure TfrmWebcamScan.btnResetClick(Sender: TObject);
var
  i: Integer;
begin
  FScannedCube := C_CUBE_COMPLETE;
  for i := 0 to 5 do
    FFacesScanned[i] := False;
  SetCurrentFace(0);
  btnPreviousFace.Enabled := False;
  btnDone.Enabled := False;
  pntCubeLayout.Invalidate;
end;

procedure TfrmWebcamScan.btnDoneClick(Sender: TObject);
begin
  // Copy scanned cube state to public property
  ScannedCubeState := FScannedCube;

  // Stop webcam before closing to ensure clean shutdown
  StopWebcam;

  // Close the form
  ModalResult := mrOK;
end;

procedure TfrmWebcamScan.btnCancelClick(Sender: TObject);
begin
  // Stop webcam before closing to ensure clean shutdown
  StopWebcam;

  // Close the form
  ModalResult := mrCancel;
end;

{ TWebcamReaderThread }

constructor TWebcamReaderThread.Create(AOwner: TObject);
begin
  inherited Create(False);
  FOwner := AOwner;
  FreeOnTerminate := True;  // Auto-free when thread exits
end;

procedure TWebcamReaderThread.Execute;
var
  frm: TfrmWebcamScan;
  DeadCounter: Integer;
begin
  DeadCounter := 0;
  while not Terminated do
  begin
    frm := TfrmWebcamScan(FOwner);
    if (frm = nil) or (frm.FFFmpegProcess = nil) or (not frm.FFFmpegProcess.Running) then
    begin
      Inc(DeadCounter);
      if DeadCounter > 10 then Break;  // give it a few checks, then exit
      Sleep(10);
      Continue;
    end;

    try
      frm.ReadFrame;
    except
      Break;
    end;

    if Terminated then Break;

    // Re-check owner in case it was cleared during ReadFrame
    frm := TfrmWebcamScan(FOwner);
    if Assigned(frm) and (not (csDestroying in frm.ComponentState)) then
      TThread.Queue(nil, @frm.RefreshWebcamDisplay);

    Sleep(1);
  end;
end;

end.
