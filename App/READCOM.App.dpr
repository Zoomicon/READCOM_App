program READCOM.App;

uses
  System.StartUpCopy,
  FMX.Forms,
  _uMainForm in '_uMainForm.pas' {SomeForm},
  uStory in 'uStory.pas' {Story: TFrame},
  uStoryPoint in 'uStoryPoint.pas' {StoryPoint: TFrame},
  uZoomableFrame in 'uZoomableFrame.pas' {ZoomableFrame: TFrame},
  iPub.Rtl.Messaging in '..\3rdPartyLib\ipub-messaging-main\iPub.Rtl.Messaging.pas',
  uStoryHUD in 'uStoryHUD.pas' {StoryHUD: TFrame},
  READCOM.App.Messages.Classes in 'READCOM.App.Messages\READCOM.App.Messages.Classes.pas',
  READCOM.App.Messages.Models in 'READCOM.App.Messages\READCOM.App.Messages.Models.pas',
  Unit1 in 'Unit1.pas' {Form1},
  uStoryItem in 'uStoryItem.pas' {StoryItem: TFrame},
  uStoryItemSVG in 'uStoryItemSVG.pas' {StoryItemSVG: TFrame},
  Zoomicon.Manipulator in '..\Zoomicon.Manipulator\Zoomicon.Manipulator.pas' {Manipulator: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
