program READCOM.App;

uses
  System.StartUpCopy,
  FMX.Forms,
  iPub.Rtl.Messaging in '..\3rdPartyLib\ipub-messaging-main\iPub.Rtl.Messaging.pas',
  uZoomableFrame in 'uZoomableFrame.pas' {ZoomableFrame: TFrame},
  Zoomicon.Collections in '..\Zoomicon.Collections\Zoomicon.Collections.pas',
  Zoomicon.Manipulator in '..\Zoomicon.Manipulator\Zoomicon.Manipulator.pas' {Manipulator: TFrame},
  READCOM.App.Models in 'READCOM.App.Models.pas',
  READCOM.Messages.Classes in 'Messages\READCOM.Messages.Classes.pas',
  READCOM.Messages.Models in 'Messages\READCOM.Messages.Models.pas',
  READCOM.Views.StoryItem in 'Views\READCOM.Views.StoryItem.pas' {StoryItem: TFrame},
  READCOM.Views.ImageStoryItem in 'Views\READCOM.Views.ImageStoryItem.pas' {ImageStoryItem: TFrame},
  READCOM.Views.BitmapImageStoryItem in 'Views\READCOM.Views.BitmapImageStoryItem.pas' {BitmapImageStoryItem: TFrame},
  READCOM.Views.VectorImageStoryItem in 'Views\READCOM.Views.VectorImageStoryItem.pas' {BitmapImageStoryItem: TFrame},
  READCOM.Views.AudioStoryItem in 'Views\READCOM.Views.AudioStoryItem.pas' {AudioStoryItem: TFrame},
  READCOM.Views.PanelStoryItem in 'Views\READCOM.Views.PanelStoryItem.pas' {PanelStoryItem: TFrame},
  READCOM.Views.Menu.HUD in 'Views\READCOM.Views.Menu.HUD.pas' {StoryHUD: TFrame},
  READCOM.Views.Main in 'Views\READCOM.Views.Main.pas' {MainForm},
  ObjectDebuggerFMXForm in '..\3rdPartyLib\object-debugger-for-firemonkey\DemoDesktop\ObjectDebuggerFMXForm.pas' {ObjectDebuggerFMXForm},
  ObjectDebuggerFMXFrame in '..\3rdPartyLib\object-debugger-for-firemonkey\ObjectDebuggerFMXFrame.pas' {FMXObjectDebuggerFrame: TFrame},
  FormMessage in '..\3rdPartyLib\object-debugger-for-firemonkey\FormMessage.pas' {MessageForm},
  Zoomicon.Media.Classes in '..\Zoomicon.Media\Zoomicon.Media.Classes.pas',
  Zoomicon.Media.Models in '..\Zoomicon.Media\Zoomicon.Media.Models.pas';

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True; //TODO: remove in production?
  Randomize; //initializes the built-in random number generator with a random value (obtained from the system clock)
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  //ObjectDebuggerFMXForm1.Show;
  Application.Run;
end.
