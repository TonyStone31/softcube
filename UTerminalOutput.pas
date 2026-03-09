unit UTerminalOutput;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Forms, StdCtrls;

type

  { TfrmTerminalOutput }

  TfrmTerminalOutput = class(TForm)
    memTerminal: TMemo;
    procedure FormShow(Sender: TObject);
  end;

var
  frmTerminalOutput: TfrmTerminalOutput;

implementation

{$R *.lfm}

procedure TfrmTerminalOutput.FormShow(Sender: TObject);
begin
  if Application.MainForm <> nil then
  begin
    Left := Application.MainForm.Left;
    Top := Application.MainForm.Top + Application.MainForm.Height - Height;
  end;
end;

end.
