program DropFiles;

uses
  System.StartUpCopy,
  FMX.Forms,
  MainForm in 'MainForm.pas' {Form2},
  uDropFrame in 'uDropFrame.pas' {DropFrame: TFrame},
  uInheritedFrame in 'uInheritedFrame.pas' {InheritedFrame: TFrame};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
