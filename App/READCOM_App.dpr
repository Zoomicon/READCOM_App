//Description: READ-COM App
//Source: https://github.com/zoomicon/READCOM_App
//Author: George Birbilis (http://zoomicon.com)

(*
Dependencies:
+ GetIt Packages (GetIt Package Manager is included in Delphi IDE):
  - TFrameStand
  - SVGIconImageList
  - CodeSite Express
  - Boss Experts (optional)
+ Boss Packages (need Boss Package Manager - configuration is in boss.json):
  - Zoomicon.Generics
  - Zoomicon.Helpers.RTL
  - Zoomicon.Helpers.FMX
  - Zoomicon.Introspection.FMX
  - Zoomicon.ZUI.FMX
  - Zoomicon.Manipulation.FMX
+ Design Packages (need to select all [or one by one], right-click and Install at Projects tool window):
  - Zoomicon.Text.FMX
  - Zoomicon.Media.FMX
*)

program READCOM_App;

{$R *.dres}

{$region 'Used units' ---------------------------------------------------------} //TODO: D12.2 doesn't seem to fold/expand regions in .dpr files
uses
  System.StartUpCopy,
  {-$IFDEF DEBUG} //don't use this IFDEF, else Delphi's CodeInsight shows non-existent errors (though building ok) in READCOM.Views.StoryItems.StoryItem
  FormMessage in '..\3rdPartyLib\object-debugger-for-firemonkey\FormMessage.pas' {MessageForm},
  ObjectDebuggerFMXFrame in '..\3rdPartyLib\object-debugger-for-firemonkey\ObjectDebuggerFMXFrame.pas' {FMXObjectDebuggerFrame: TFrame},
  ObjectDebuggerFMXForm in '..\3rdPartyLib\object-debugger-for-firemonkey\DemoDesktop\ObjectDebuggerFMXForm.pas' {ObjectDebuggerFMXForm},
{-$ENDIF}
  Zoomicon.Media.FMX.MediaPlayerEx in '..\Zoomicon.Media\Zoomicon.Media.FMX.MediaPlayerEx.pas',
  Zoomicon.Media.FMX.Models in '..\Zoomicon.Media\Zoomicon.Media.FMX.Models.pas',
  Zoomicon.Media.FMX.MediaDisplay in '..\Zoomicon.Media\Zoomicon.Media.FMX.MediaDisplay.pas',
  Zoomicon.Media.FMX.ClickableGlyph in '..\Zoomicon.Media\Zoomicon.Media.FMX.ClickableGlyph.pas',
  //
  Zoomicon.Text in '..\Zoomicon.Text\Zoomicon.Text.pas',
  //
  READCOM.App.Globals in 'READCOM.App.Globals.pas' {Globals: TDataModule},
  READCOM.App.Models in 'READCOM.App.Models.pas',
  READCOM.App.URLs in 'READCOM.App.URLs.pas',
  //
  READCOM.Views.Options.StoryItemOptions in 'Views\Options\READCOM.Views.Options.StoryItemOptions.pas' {StoryItemOptions: TFrame},
  READCOM.Views.Options.ImageStoryItemOptions in 'Views\Options\READCOM.Views.Options.ImageStoryItemOptions.pas' {ImageStoryItemOptions: TFrame},
  READCOM.Views.Options.TextStoryItemOptions in 'Views\Options\READCOM.Views.Options.TextStoryItemOptions.pas' {TextStoryItemOptions: TFrame},
  //
  READCOM.Views.StoryItems.StoryItem in 'Views\StoryItems\READCOM.Views.StoryItems.StoryItem.pas' {StoryItem: TFrame},
  READCOM.Views.StoryItems.ImageStoryItem in 'Views\StoryItems\READCOM.Views.StoryItems.ImageStoryItem.pas' {ImageStoryItem: TFrame},
  READCOM.Views.StoryItems.AudioStoryItem in 'Views\StoryItems\READCOM.Views.StoryItems.AudioStoryItem.pas' {AudioStoryItem: TFrame},
  READCOM.Views.StoryItems.TextStoryItem in 'Views\StoryItems\READCOM.Views.StoryItems.TextStoryItem.pas' {TextStoryItem: TFrame},
  READCOM.Views.StoryItems.StoryItemFactory in 'Views\StoryItems\READCOM.Views.StoryItems.StoryItemFactory.pas',
  //
  READCOM.Views.Modal in 'Views\READCOM.Views.Modal.pas' {ModalFrame: TFrame},
  READCOM.Views.Dialogs.About in 'Views\Dialogs\READCOM.Views.Dialogs.About.pas' {AboutFrame: TFrame},
  READCOM.Views.Dialogs.AllText in 'Views\Dialogs\READCOM.Views.Dialogs.AllText.pas' {AllTextFrame: TFrame},
  READCOM.Views.Prompts.Wait in 'Views\Prompts\READCOM.Views.Prompts.Wait.pas' {WaitFrame: TFrame},
  READCOM.Views.Prompts.Rotate in 'Views\Prompts\READCOM.Views.Prompts.Rotate.pas' {RotateFrame: TFrame},
  READCOM.Views.Prompts.Lock in 'Views\Prompts\READCOM.Views.Prompts.Lock.pas' {LockFrame: TFrame},
  //
  READCOM.Views.Menu.HUD in 'Views\READCOM.Views.Menu.HUD.pas' {StoryHUD: TFrame},
  READCOM.Views.Main in 'Views\READCOM.Views.Main.pas' {MainForm},
  //
  READCOM.App.Debugging in 'READCOM.App.Debugging.pas',
  READCOM.App.Messages in 'READCOM.App.Messages.pas',
  READCOM.App.Main in 'READCOM.App.Main.pas';
{$endregion}

{$R *.res}

begin
  Main;
end.

