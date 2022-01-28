program StructureViewer;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uHidableFrame in 'uHidableFrame.pas' {HidableFrame: TFrame},
  Zoomicon.Introspection.FMX.StructureView in '..\..\Zoomicon.Introspection\Zoomicon.Introspection.FMX.StructureView.pas' {StructureView: TFrame},
  Zoomicon.Helpers.FMX.ImgList.ImageListHelpers in '..\..\Zooming.Helpers\Zoomicon.Helpers.FMX.ImgList\Zoomicon.Helpers.FMX.ImgList.ImageListHelpers.pas',
  Zoomicon.Helpers.FMX.Controls.ControlHelpers in '..\..\Zooming.Helpers\Zoomicon.Helpers.FMX.Controls\Zoomicon.Helpers.FMX.Controls.ControlHelpers.pas',
  Zoomicon.Helpers.RTL.ClassListHelpers in '..\..\Zooming.Helpers\Zoomicon.Helpers.RTL\Zoomicon.Helpers.RTL.ClassListHelpers.pas',
  Zoomicon.Helpers.FMX.TreeView.TreeViewHelpers in '..\..\Zooming.Helpers\Zoomicon.Helpers.FMX.TreeView\Zoomicon.Helpers.FMX.TreeView.TreeViewHelpers.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
