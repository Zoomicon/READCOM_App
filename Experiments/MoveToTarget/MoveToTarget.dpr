program MoveToTarget;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {Form2},
  Zoomicon.Puzzler.Classes in '..\..\Zoomicon.Puzzler\Zoomicon.Puzzler.Classes.pas',
  Zoomicon.Puzzler.Models in '..\..\Zoomicon.Puzzler\Zoomicon.Puzzler.Models.pas',
  Zoomicon.Generics.Collections in '..\..\Zoomicon.Generics\Collections\Zoomicon.Generics.Collections.pas',
  Zoomicon.Helpers.FMX.Controls.ControlHelpers in '..\..\Zooming.Helpers\Zoomicon.Helpers.FMX.Controls\Zoomicon.Helpers.FMX.Controls.ControlHelpers.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}

  Application.Initialize;
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.
