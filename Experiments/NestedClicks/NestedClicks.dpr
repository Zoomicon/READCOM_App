program NestedClicks;

uses
  System.StartUpCopy,
  FMX.Forms,
  uForm in 'uForm.pas' {Form2},
  uClickableFrame in 'uClickableFrame.pas' {ClickableFrame: TFrame};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
