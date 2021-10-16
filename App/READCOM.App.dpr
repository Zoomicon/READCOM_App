program READCOM.App;

uses
  System.StartUpCopy,
  FMX.Forms,
  iPub.Rtl.Messaging in '..\3rdPartyLib\ipub-messaging-main\iPub.Rtl.Messaging.pas',
  uZoomableFrame in 'uZoomableFrame.pas' {ZoomableFrame: TFrame},
  Zoomicon.Manipulator in '..\Zoomicon.Manipulator\Zoomicon.Manipulator.pas' {Manipulator: TFrame},
  READCOM.App.Models in 'READCOM.App.Models.pas',
  Zoomicon.Collections in 'Zoomicon.Collections.pas',
  FluentQuery.Chars in '..\3rdPartyLib\FluentQuery\FluentQuery.Chars.pas',
  FluentQuery.Components.MethodFactories in '..\3rdPartyLib\FluentQuery\FluentQuery.Components.MethodFactories.pas',
  FluentQuery.Components in '..\3rdPartyLib\FluentQuery\FluentQuery.Components.pas',
  FluentQuery.Core.EnumerationStrategies in '..\3rdPartyLib\FluentQuery\FluentQuery.Core.EnumerationStrategies.pas',
  FluentQuery.Core.Enumerators in '..\3rdPartyLib\FluentQuery\FluentQuery.Core.Enumerators.pas',
  FluentQuery.Core.MethodFactories in '..\3rdPartyLib\FluentQuery\FluentQuery.Core.MethodFactories.pas',
  FluentQuery.Core.Reduce in '..\3rdPartyLib\FluentQuery\FluentQuery.Core.Reduce.pas',
  FluentQuery.Core.Types in '..\3rdPartyLib\FluentQuery\FluentQuery.Core.Types.pas',
  FluentQuery.DB in '..\3rdPartyLib\FluentQuery\FluentQuery.DB.pas',
  FluentQuery.Files in '..\3rdPartyLib\FluentQuery\FluentQuery.Files.pas',
  FluentQuery.GenericObjects.MethodFactories in '..\3rdPartyLib\FluentQuery\FluentQuery.GenericObjects.MethodFactories.pas',
  FluentQuery.GenericObjects in '..\3rdPartyLib\FluentQuery\FluentQuery.GenericObjects.pas',
  FluentQuery.Generics in '..\3rdPartyLib\FluentQuery\FluentQuery.Generics.pas',
  FluentQuery.Integers.MethodFactories in '..\3rdPartyLib\FluentQuery\FluentQuery.Integers.MethodFactories.pas',
  FluentQuery.Integers in '..\3rdPartyLib\FluentQuery\FluentQuery.Integers.pas',
  FluentQuery.JSON.MethodFactories in '..\3rdPartyLib\FluentQuery\FluentQuery.JSON.MethodFactories.pas',
  FluentQuery.JSON in '..\3rdPartyLib\FluentQuery\FluentQuery.JSON.pas',
  FluentQuery.Pointers in '..\3rdPartyLib\FluentQuery\FluentQuery.Pointers.pas',
  FluentQuery.Strings.MethodFactories in '..\3rdPartyLib\FluentQuery\FluentQuery.Strings.MethodFactories.pas',
  FluentQuery.Strings in '..\3rdPartyLib\FluentQuery\FluentQuery.Strings.pas',
  READCOM.Messages.Classes in 'Messages\READCOM.Messages.Classes.pas',
  READCOM.Messages.Models in 'Messages\READCOM.Messages.Models.pas',
  READCOM.Views.StoryItem in 'Views\READCOM.Views.StoryItem.pas' {StoryItem: TFrame},
  READCOM.Views.ImageStoryItem in 'Views\READCOM.Views.ImageStoryItem.pas' {ImageStoryItem: TFrame},
  READCOM.Views.BitmapImageStoryItem in 'Views\READCOM.Views.BitmapImageStoryItem.pas' {BitmapImageStoryItem: TFrame},
  READCOM.Views.VectorImageStoryItem in 'Views\READCOM.Views.VectorImageStoryItem.pas' {BitmapImageStoryItem: TFrame},
  READCOM.Views.PanelStoryItem in 'Views\READCOM.Views.PanelStoryItem.pas' {PanelStoryItem: TFrame},
  READCOM.Views.Menu.HUD in 'Views\READCOM.Views.Menu.HUD.pas' {StoryHUD: TFrame},
  READCOM.Views.Main in 'Views\READCOM.Views.Main.pas' {MainForm},
  READCOM.Views.AudioStoryItem in 'Views\READCOM.Views.AudioStoryItem.pas' {AudioStoryItem: TFrame};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := True; //TODO: remove in production?
  Randomize; //initializes the built-in random number generator with a random value (obtained from the system clock)
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
