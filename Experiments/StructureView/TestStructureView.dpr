program TestStructureView;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uHidableFrame in 'uHidableFrame.pas' {HidableFrame: TFrame},
  uStructureView in 'uStructureView.pas' {StructureView: TFrame};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
