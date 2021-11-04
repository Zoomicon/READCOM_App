program ZoomableContainer;



uses
  System.StartUpCopy,
  FMX.Forms,
  Zoomicon.FMX.Utils in '..\..\Zoomicon.Zooming\Zoomicon.FMX.Utils.pas',
  Zoomicon.Zooming.Models in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.Models.pas',
  uMainForm in 'uMainForm.pas' {MainForm},
  Zoomicon.Zooming.ZoomFrame in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.ZoomFrame.pas' {ZoomFrame: TFrame},
  Zoomicon.Zooming.Classes in '..\..\Zoomicon.Zooming\Zoomicon.Zooming.Classes.pas',
  Unit1 in 'Unit1.pas' {Form1},
  Unit3 in 'Unit3.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);

  TForm1.Create(MainForm).Visible := true;
  TForm3.Create(MainForm).Visible := true;

  Application.Run;
end.
