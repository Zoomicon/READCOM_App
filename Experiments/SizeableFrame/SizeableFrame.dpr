program SizeableFrame;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uContentFrame in 'uContentFrame.pas' {ContentFrame: TFrame},
  uContainerFrame in 'uContainerFrame.pas' {ContainerFrame: TFrame};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
