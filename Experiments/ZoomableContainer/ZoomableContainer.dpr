program ZoomableContainer;



uses
  System.StartUpCopy,
  FMX.Forms,
  Zoomicon.FMX.Utils in '..\..\Zoomicon.Zooming\Zoomicon.FMX.Utils.pas',
  Zoomicon.Zooming.Models in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.Models.pas',
  uMainForm in 'uMainForm.pas' {MainForm},
  Zoomicon.Zooming.ZoomFrame in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.ZoomFrame.pas' {ZoomFrame: TFrame},
  Zoomicon.Zooming.Classes in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.Classes.pas',
  uZoomFrameForm in 'uZoomFrameForm.pas' {Form1},
  uZoomedLayoutForm in 'uZoomedLayoutForm.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  TForm1.Create(MainForm).Visible := true;
  TForm3.Create(MainForm).Visible := true;

  Application.Run;
end.
