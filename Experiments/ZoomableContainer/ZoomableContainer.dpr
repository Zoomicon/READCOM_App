program ZoomableContainer;

uses
  System.StartUpCopy,
  FMX.Forms,
  Zoomicon.FMX.Utils in '..\..\Zoomicon.Zooming\Zoomicon.FMX.Utils.pas',
  Zoomicon.Zooming.Models in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.Models.pas',
  uMainForm in 'uMainForm.pas' {MainForm},
  Zoomicon.Zooming.ZoomFrame in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.ZoomFrame.pas' {ZoomFrame: TFrame},
  Zoomicon.Zooming.Classes in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.Classes.pas',
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
