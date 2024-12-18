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
  - Zoomicon.Manipulation.FMX
  - Zoomicon.Media.FMX
  - Zoomicon.ZUI.FMX
  - READCOM.Core
  Notes:
  - at Build/Compiler/Search path/All configurations - All platforms, using relative search paths to respective Boss cache folders (see App/modules folder)
  - will need to manually install the BOSS packages (see modules subfolder that it creates) if BOSS doesn't install them as design packages
*)

program READCOM_App;

  {$R *.dres}

  {$region 'Used units' ---------------------------------------------------------} //TODO: D12.2 doesn't seem to fold/expand regions in .dpr files
  uses
  System.StartUpCopy,
  FormMessage in '..\3rdPartyLib\object-debugger-for-firemonkey\FormMessage.pas' {MessageForm},
  ObjectDebuggerFMXFrame in '..\3rdPartyLib\object-debugger-for-firemonkey\ObjectDebuggerFMXFrame.pas' {FMXObjectDebuggerFrame: TFrame},
  ObjectDebuggerFMXForm in '..\3rdPartyLib\object-debugger-for-firemonkey\DemoDesktop\ObjectDebuggerFMXForm.pas' {ObjectDebuggerFMXForm},
  Zoomicon.Manipulation.FMX.CustomManipulator in 'modules\zoomicon.manipulation.fmx.delphi\Source\Zoomicon.Manipulation.FMX.CustomManipulator.pas' {CustomManipulator: TFrame},
  READCOM.App.Globals in 'READCOM.App.Globals.pas' {Globals: TDataModule},
  READCOM.App.Themes in 'READCOM.App.Themes.pas' {Themes: TDataModule},
  READCOM.Views.Options.StoryItemOptions in 'Views\Options\READCOM.Views.Options.StoryItemOptions.pas' {StoryItemOptions: TFrame},
  READCOM.Views.Options.ImageStoryItemOptions in 'Views\Options\READCOM.Views.Options.ImageStoryItemOptions.pas' {ImageStoryItemOptions: TFrame},
  READCOM.Views.Options.TextStoryItemOptions in 'Views\Options\READCOM.Views.Options.TextStoryItemOptions.pas' {TextStoryItemOptions: TFrame},
  READCOM.Views.StoryItems.StoryItem in 'Views\StoryItems\READCOM.Views.StoryItems.StoryItem.pas' {StoryItem: TFrame},
  READCOM.Views.StoryItems.ImageStoryItem in 'Views\StoryItems\READCOM.Views.StoryItems.ImageStoryItem.pas' {ImageStoryItem: TFrame},
  READCOM.Views.StoryItems.AudioStoryItem in 'Views\StoryItems\READCOM.Views.StoryItems.AudioStoryItem.pas' {AudioStoryItem: TFrame},
  READCOM.Views.StoryItems.TextStoryItem in 'Views\StoryItems\READCOM.Views.StoryItems.TextStoryItem.pas' {TextStoryItem: TFrame},
  READCOM.Views.StoryItems.StoryItemFactory in 'Views\StoryItems\READCOM.Views.StoryItems.StoryItemFactory.pas',
  READCOM.Views.Modal in 'Views\READCOM.Views.Modal.pas' {ModalFrame: TFrame},
  READCOM.Views.Dialogs.About in 'Views\Dialogs\READCOM.Views.Dialogs.About.pas' {AboutFrame: TFrame},
  READCOM.Views.Dialogs.AllText in 'Views\Dialogs\READCOM.Views.Dialogs.AllText.pas' {AllTextFrame: TFrame},
  READCOM.Views.Prompts.Wait in 'Views\Prompts\READCOM.Views.Prompts.Wait.pas' {WaitFrame: TFrame},
  READCOM.Views.Prompts.Rotate in 'Views\Prompts\READCOM.Views.Prompts.Rotate.pas' {RotateFrame: TFrame},
  READCOM.Views.Prompts.Lock in 'Views\Prompts\READCOM.Views.Prompts.Lock.pas' {LockFrame: TFrame},
  READCOM.Views.Menu.HUD in 'Views\READCOM.Views.Menu.HUD.pas' {StoryHUD: TFrame},
  READCOM.Views.Main in 'Views\READCOM.Views.Main.pas' {MainForm},
  READCOM.App.Debugging in 'READCOM.App.Debugging.pas',
  READCOM.App.Messages in 'READCOM.App.Messages.pas',
  READCOM.App.Main in 'READCOM.App.Main.pas';
{$endregion}

  {$R *.res}

begin
  Main;
end.

