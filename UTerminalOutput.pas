unit UTerminalOutput;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Menus;

type

  { TfrmTerminalOutput }

  TfrmTerminalOutput = class(TForm)
    memTerminal: TMemo;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FRawLines: TStringList;
    FShowTimestamps: boolean;
    FPopup: TPopupMenu;
    FMnuTimestamps: TMenuItem;
    procedure ToggleTimestamps(Sender: TObject);
    procedure RenderLines;
    function StripTimestamp(const line: string): string;
  public
    procedure AddLine(const line: string);
    procedure UpdateLastLine(const line: string);
    procedure ClearOutput;
    procedure ScrollToBottom;
    property ShowTimestamps: boolean read FShowTimestamps;
  end;

var
  frmTerminalOutput: TfrmTerminalOutput;

implementation

{$R *.lfm}

procedure TfrmTerminalOutput.FormCreate(Sender: TObject);
begin
  FRawLines := TStringList.Create;
  FShowTimestamps := True;

  FPopup := TPopupMenu.Create(Self);
  FMnuTimestamps := TMenuItem.Create(FPopup);
  FMnuTimestamps.Caption := 'Show Timestamps';
  FMnuTimestamps.Checked := True;
  FMnuTimestamps.OnClick := ToggleTimestamps;
  FPopup.Items.Add(FMnuTimestamps);
  memTerminal.PopupMenu := FPopup;
end;

procedure TfrmTerminalOutput.FormDestroy(Sender: TObject);
begin
  FRawLines.Free;
end;

procedure TfrmTerminalOutput.FormShow(Sender: TObject);
begin
  if Application.MainForm <> nil then
  begin
    Left := Application.MainForm.Left;
    Top := Application.MainForm.Top + Application.MainForm.Height - Height;
  end;
end;

function TfrmTerminalOutput.StripTimestamp(const line: string): string;
begin
  // Timestamps are in format [HH:MM:SS] at the start of the line
  if (Length(line) >= 11) and (line[1] = '[') and (line[10] = ']') and (line[11] = ' ') then
    Result := Copy(line, 12, MaxInt)
  else
    Result := line;
end;

procedure TfrmTerminalOutput.RenderLines;
var
  i: integer;
begin
  memTerminal.Lines.BeginUpdate;
  try
    memTerminal.Lines.Clear;
    for i := 0 to FRawLines.Count - 1 do
    begin
      if FShowTimestamps then
        memTerminal.Lines.Add(FRawLines[i])
      else
        memTerminal.Lines.Add(StripTimestamp(FRawLines[i]));
    end;
  finally
    memTerminal.Lines.EndUpdate;
  end;
  ScrollToBottom;
end;

procedure TfrmTerminalOutput.ToggleTimestamps(Sender: TObject);
begin
  FShowTimestamps := not FShowTimestamps;
  FMnuTimestamps.Checked := FShowTimestamps;
  RenderLines;
end;

procedure TfrmTerminalOutput.AddLine(const line: string);
begin
  FRawLines.Add(line);
  if FShowTimestamps then
    memTerminal.Lines.Add(line)
  else
    memTerminal.Lines.Add(StripTimestamp(line));
  ScrollToBottom;
end;

procedure TfrmTerminalOutput.UpdateLastLine(const line: string);
var
  displayLine: string;
begin
  if FRawLines.Count > 0 then
    FRawLines[FRawLines.Count - 1] := line
  else
    FRawLines.Add(line);

  if FShowTimestamps then
    displayLine := line
  else
    displayLine := StripTimestamp(line);

  if memTerminal.Lines.Count > 0 then
    memTerminal.Lines[memTerminal.Lines.Count - 1] := displayLine
  else
    memTerminal.Lines.Add(displayLine);
  ScrollToBottom;
end;

procedure TfrmTerminalOutput.ClearOutput;
begin
  FRawLines.Clear;
  memTerminal.Lines.Clear;
end;

procedure TfrmTerminalOutput.ScrollToBottom;
begin
  memTerminal.SelStart := Length(memTerminal.Text);
end;

end.
