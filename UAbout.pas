unit UAbout;

{$MODE Delphi}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, ExtCtrls, Buttons;

type

  { TfrmAbout }

  TfrmAbout = class(TForm)
    btnClose: TSpeedButton;
    lblBuilt: TLabel;
    lblEngine: TLabel;
    lblOriginal: TLabel;
    lblSeparator: TLabel;
    lblSubtitle: TLabel;
    lblTitle: TLabel;
    lblVersion: TLabel;
    lblAuthor: TLabel;
    pnlBody: TPanel;
    pnlFooter: TPanel;
    pnlHeader: TPanel;
    procedure btnCloseClick(Sender: TObject);
  end;

var
  frmAbout: TfrmAbout;

implementation

{$R *.lfm}

procedure TfrmAbout.btnCloseClick(Sender: TObject);
begin
  Close;
end;

end.
