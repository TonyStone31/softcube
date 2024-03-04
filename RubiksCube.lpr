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
  Forms,
  Interfaces,
  UMain in 'UMain.pas' {Form1},
  UConst in 'UConst.pas',
  UDraw in 'UDraw.pas',
  URubik in 'URubik.pas';

  //{$R *.res}

begin
  Application.Title:='SoftCube';
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
