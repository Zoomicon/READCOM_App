program ZoomableContainer;

uses
  System.StartUpCopy,
  FMX.Forms,
  Zoomicon.Zooming.FMX.Utils in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.FMX.Utils.pas',
  Zoomicon.Zooming.Models in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.Models.pas',
  uMainForm in 'uMainForm.pas' {MainForm},
  Zoomicon.Zooming.FMX.ZoomFrame in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.FMX.ZoomFrame.pas' {ZoomFrame: TFrame},
  Zoomicon.Zooming.FMX in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.FMX.pas',
  uZoomFrameForm in 'uZoomFrameForm.pas' {ZoomFrameForm},
  uZoomedLayoutForm in 'uZoomedLayoutForm.pas' {ZoomedLayoutForm};

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  TZoomFrameForm.Create(MainForm).Visible := true;
  //TZoomedLayoutForm.Create(MainForm).Visible := true; //TODO: NOT WORKING CORRECTLY

  Application.Run;
end.
