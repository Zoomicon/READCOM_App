program READCOM_App;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMainForm in 'uMainForm.pas' {MainForm},
  uStory in 'uStory.pas' {Story: TFrame},
  uStoryPoint in 'uStoryPoint.pas' {StoryPoint: TFrame},
  uZoomableFrame in 'uZoomableFrame.pas' {ZoomableFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
