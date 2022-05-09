program ZoomableContainer;

uses
  System.SysUtils,
  System.StartUpCopy,
  FMX.Forms,
  Zoomicon.Zooming.Models in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.Models.pas',
  Zoomicon.Zooming.FMX.ZoomFrame in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.FMX.ZoomFrame.pas' {ZoomFrame: TFrame},
  Zoomicon.Zooming.FMX in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.FMX.pas',
  uMainForm in 'uMainForm.pas' {MainForm},
  uZoomFrameForm in 'uZoomFrameForm.pas' {ZoomFrameForm},
  uZoomedLayoutForm in 'uZoomedLayoutForm.pas' {ZoomedLayoutForm},
  Zoomicon.Introspection.FMX.StructureView in '..\..\Zoomicon.Introspection\Zoomicon.Introspection.FMX.StructureView.pas' {StructureView: TFrame},
  Zoomicon.Helpers.RTL.ClassListHelpers in '..\..\Zoomicon.Helpers.RTL\Zoomicon.Helpers.RTL\Zoomicon.Helpers.RTL.ClassListHelpers.pas',
  Zoomicon.Helpers.FMX.Layouts.ScaledLayoutHelpers in '..\..\Zoomicon.Helpers.FMX\Zoomicon.Helpers.FMX.Layouts\Zoomicon.Helpers.FMX.Layouts.ScaledLayoutHelpers.pas',
  Zoomicon.Helpers.FMX.Layouts.ScrollBoxHelpers in '..\..\Zoomicon.Helpers.FMX\Zoomicon.Helpers.FMX.Layouts\Zoomicon.Helpers.FMX.Layouts.ScrollBoxHelpers.pas',
  Zoomicon.Helpers.FMX.ImgList.ImageListHelpers in '..\..\Zoomicon.Helpers.FMX\Zoomicon.Helpers.FMX.ImgList\Zoomicon.Helpers.FMX.ImgList.ImageListHelpers.pas',
  Zoomicon.Helpers.FMX.TreeView.TreeViewHelpers in '..\..\Zoomicon.Helpers.FMX\Zoomicon.Helpers.FMX.TreeView\Zoomicon.Helpers.FMX.TreeView.TreeViewHelpers.pas',
  Zoomicon.Helpers.FMX.Controls.ControlHelpers in '..\..\Zoomicon.Helpers.FMX\Zoomicon.Helpers.FMX.Controls\Zoomicon.Helpers.FMX.Controls.ControlHelpers.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);

  {$IF DEFINED(MSWINDOWS)} //Trying to have more than 1 form throws Segmentation Fault exception on Android, probably on iOS too. Not sure about MacOS-X or Linux, probably it works for those
  ZoomFrameForm := TZoomFrameForm.Create(MainForm);
  ZoomFrameForm.Visible := true;

  //ZoomedLayoutForm := TZoomedLayoutForm.Create(MainForm).Visible := true; //TODO: NOT WORKING CORRECTLY
  //ZoomedLayoutForm.Visible := true;
  {$ENDIF}

  Application.Run;

  {$IF DEFINED(MSWINDOWS)}
  FreeAndNil(ZoomFrameForm);
  FreeAndNil(ZoomedLayoutForm);
  {$ENDIF}

end.
