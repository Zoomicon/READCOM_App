//Description: READ-COM App
//Source: https://github.com/zoomicon/READCOM_App
//Author: George Birbilis (http://zoomicon.com)

(*
Dependencies:
+ GetIt Packages (install via GetIt Package Manager, included in Delphi IDE):
  - TFrameStand
  - SVGIconImageList
  - CodeSite Express
  - Boss Experts (optional)
+ Boss Packages (install via Boss Package Manager [use Boss command-line tool and optionally Boss Experts for IDE] - configuration is in boss.json):
  - Zoomicon.Generics
  - Zoomicon.Helpers.RTL
  - Zoomicon.Helpers.FMX
  - Zoomicon.Introspection.FMX (also wraps Object-Debugger-for-Firemonkey)
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
  Zoomicon.Manipulation.FMX.CustomManipulator in 'modules\zoomicon.manipulation.fmx.delphi\Source\Zoomicon.Manipulation.FMX.CustomManipulator.pas' {CustomManipulator: TFrame},
  Zoomicon.Media.FMX.ModalFrame in 'modules\zoomicon.media.fmx.delphi\Source\Zoomicon.Media.FMX.ModalFrame.pas' {ModalFrame: TFrame},
  READCOM.Views.Dialogs.About in 'Views\Dialogs\READCOM.Views.Dialogs.About.pas' {AboutFrame: TFrame},
  READCOM.Views.HUD in 'Views\READCOM.Views.HUD.pas' {StoryHUD: TFrame},
  READCOM.Views.Main in 'Views\READCOM.Views.Main.pas' {MainForm},
  READCOM.App.Messages in 'READCOM.App.Messages.pas',
  READCOM.App.Main in 'READCOM.App.Main.pas',
  READCOM.Resources.Icons in 'modules\readcom.core.delphi\Source\Resources\READCOM.Resources.Icons.pas' {Icons: TDataModule},
  READCOM.Resources.Themes in 'modules\readcom.core.delphi\Source\Resources\READCOM.Resources.Themes.pas' {Themes: TDataModule},
  READCOM.Views.StoryItems.StoryItem in 'Views\StoryItems\READCOM.Views.StoryItems.StoryItem.pas' {StoryItem: TFrame},
  READCOM.Views.StoryItems.ImageStoryItem in 'Views\StoryItems\READCOM.Views.StoryItems.ImageStoryItem.pas' {ImageStoryItem: TFrame},
  READCOM.Views.StoryItems.TextStoryItem in 'Views\StoryItems\READCOM.Views.StoryItems.TextStoryItem.pas' {TextStoryItem: TFrame},
  READCOM.Views.StoryItems.AudioStoryItem in 'Views\StoryItems\READCOM.Views.StoryItems.AudioStoryItem.pas' {AudioStoryItem: TFrame},
  READCOM.Views.Options.StoryItemOptions in 'Views\Options\READCOM.Views.Options.StoryItemOptions.pas' {StoryItemOptions: TFrame},
  READCOM.Views.Options.TextStoryItemOptions in 'Views\Options\READCOM.Views.Options.TextStoryItemOptions.pas' {TextStoryItemOptions: TFrame},
  READCOM.Views.Options.ImageStoryItemOptions in 'Views\Options\READCOM.Views.Options.ImageStoryItemOptions.pas' {ImageStoryItemOptions: TFrame};
  {$endregion}

  {$R *.res}

begin
  Main;
end.

