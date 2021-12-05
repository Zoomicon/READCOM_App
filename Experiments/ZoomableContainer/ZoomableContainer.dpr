program ZoomableContainer;

uses
  System.StartUpCopy,
  FMX.Forms,
  Zoomicon.Zooming.Models in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.Models.pas',
  Zoomicon.Zooming.FMX.ZoomFrame in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.FMX.ZoomFrame.pas' {ZoomFrame: TFrame},
  Zoomicon.Zooming.FMX in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.FMX.pas',
  uMainForm in 'uMainForm.pas' {MainForm},
  uZoomFrameForm in 'uZoomFrameForm.pas' {ZoomFrameForm},
  uZoomedLayoutForm in 'uZoomedLayoutForm.pas' {ZoomedLayoutForm},
  Zoomicon.Helpers.FMX.Layouts.ScaledLayoutHelpers in '..\..\Zooming.Helpers\Zoomicon.Helpers.FMX.Layouts\Zoomicon.Helpers.FMX.Layouts.ScaledLayoutHelpers.pas',
  Zoomicon.Helpers.FMX.Layouts.ScrollBoxHelpers in '..\..\Zooming.Helpers\Zoomicon.Helpers.FMX.Layouts\Zoomicon.Helpers.FMX.Layouts.ScrollBoxHelpers.pas',
  Zoomicon.Helpers.RTL.ClassListHelpers in '..\..\Zooming.Helpers\Zoomicon.Helpers.RTL\Zoomicon.Helpers.RTL.ClassListHelpers.pas',
  Zoomicon.Introspection.FMX.StructureView in '..\..\Zoomicon.Introspection\Zoomicon.Introspection.FMX.StructureView.pas' {StructureView: TFrame};

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
