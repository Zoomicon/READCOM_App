program ZoomableContainer;



uses
  System.StartUpCopy,
  FMX.Forms,
  Zoomicon.FMX.Utils in '..\..\Zoomicon.Zooming\Zoomicon.FMX.Utils.pas',
  Zoomicon.Zooming.Models in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.Models.pas',
  uMainForm in 'uMainForm.pas' {MainForm},
  Zoomicon.Zooming.ZoomFrame in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.ZoomFrame.pas' {ZoomFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
