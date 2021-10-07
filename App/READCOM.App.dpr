program READCOM.App;

uses
  System.StartUpCopy,
  FMX.Forms,
  iPub.Rtl.Messaging in '..\3rdPartyLib\ipub-messaging-main\iPub.Rtl.Messaging.pas',
  READCOM.Views.Main in 'READCOM.Views.Main.pas' {MainForm},
  READCOM.Views.StoryFrame in 'READCOM.Views.StoryFrame.pas' {StoryFrame: TFrame},
  uZoomableFrame in 'uZoomableFrame.pas' {ZoomableFrame: TFrame},
  READCOM.Views.Menu.HUD in 'READCOM.Views.Menu.HUD.pas' {StoryHUD: TFrame},
  READCOM.Views.StoryItem in 'READCOM.Views.StoryItem.pas' {StoryItem: TFrame},
  READCOM.Views.StoryItem.SVG in 'READCOM.Views.StoryItem.SVG.pas' {StoryItemSVG: TFrame},
  Zoomicon.Manipulator in '..\Zoomicon.Manipulator\Zoomicon.Manipulator.pas' {Manipulator: TFrame},
  READCOM.Messages.Classes in 'READCOM.Messages\READCOM.Messages.Classes.pas',
  READCOM.Messages.Models in 'READCOM.Messages\READCOM.Messages.Models.pas',
  READCOM.App.Models in 'READCOM.App.Models.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True; //TODO: remove in production?
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
