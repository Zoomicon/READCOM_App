program READCOM.App;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uStory in 'uStory.pas' {Story: TFrame},
  uStoryPoint in 'uStoryPoint.pas' {StoryPoint: TFrame},
  uZoomableFrame in 'uZoomableFrame.pas' {ZoomableFrame: TFrame},
  iPub.Rtl.Messaging in '..\3rdPartyLib\ipub-messaging-main\iPub.Rtl.Messaging.pas',
  uStoryHUD in 'uStoryHUD.pas' {StoryHUD: TFrame},
  READCOM.App.Messages.Classes in 'READCOM.App.Messages\READCOM.App.Messages.Classes.pas',
  READCOM.App.Messages.Models in 'READCOM.App.Messages\READCOM.App.Messages.Models.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
