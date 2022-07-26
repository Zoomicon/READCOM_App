program FileChooser;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  Zoomicon.Generics.Collections in '..\..\Zoomicon.Generics\Collections\Zoomicon.Generics.Collections.pas',
  Zoomicon.Media.FMX.FileChooser in '..\..\Zoomicon.Media\Zoomicon.Media.FMX.FileChooser.pas' {FileChooser: TFrame},
  Zoomicon.Media.FMX.DataBinding in '..\..\Zoomicon.Media\Zoomicon.Media.FMX.DataBinding.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
