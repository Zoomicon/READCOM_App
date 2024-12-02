program Zoomicon.Media.FMX.Demo;

uses
  System.StartUpCopy,
  FMX.Forms,
  MediaDisplayDemo in 'Views\MediaDisplayDemo.pas' {MediaDisplayDemo1},
  Zoomicon.Media.FMX.MediaDisplay in '..\Zoomicon.Media.FMX.MediaDisplay.pas',
  Zoomicon.Media.FMX.Models in '..\Zoomicon.Media.FMX.Models.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMediaDisplayDemo1, MediaDisplayDemo1);
  Application.Run;
end.
