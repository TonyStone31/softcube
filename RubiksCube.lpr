program RubiksCube;

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

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Forms,
  Interfaces,
  UMain in 'UMain.pas' {Form1},
  UConst in 'UConst.pas',
  UDraw in 'UDraw.pas',
  UGenericCube in 'UGenericCube.pas',
  UWebcamScan in 'UWebcamScan.pas' {frmWebcamScan},
  UTerminalOutput in 'UTerminalOutput.pas' {frmTerminalOutput},
  UAbout in 'UAbout.pas' {frmAbout},
  UHelp in 'UHelp.pas' {frmHelp};

  //{$R *.res}

begin
  Application.Title:='SoftCube';
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmTerminalOutput, frmTerminalOutput);
  Application.Run;
end.
