//Description: READ-COM App
//Source: https://github.com/Zoomicon/READCOM_App
//Author: George Birbilis (https://zoomicon.com)

//Dependencies: see <DEPENDENCIES.md>
program READCOM_App;

  {$R *.dres} //for Windows resources added via Resources and Images (includes 'Default.readcom' startup story document)

  {$region 'Used units' ---------------------------------------------------------} //Note: D12.3 can't fold/expand regions in .dpr files
  uses
  System.StartUpCopy,
  Zoomicon.Media.FMX.ModalFrame in 'modules\zoomicon.media.fmx.delphi\Source\Zoomicon.Media.FMX.ModalFrame.pas' {ModalFrame: TFrame},
  READCOM.Views.Dialogs.About in 'Views\Dialogs\READCOM.Views.Dialogs.About.pas' {AboutFrame: TFrame},
  READCOM.Views.HUD in 'Views\READCOM.Views.HUD.pas' {StoryHUD: TFrame},
  READCOM.Views.Main in 'Views\READCOM.Views.Main.pas' {MainForm},
  READCOM.App.Messages in 'READCOM.App.Messages.pas',
  READCOM.App.Main in 'READCOM.App.Main.pas',
  READCOM.Resources.Icons in 'modules\readcom.core.delphi\Source\Resources\READCOM.Resources.Icons.pas' {Icons: TDataModule},
  READCOM.Resources.Themes in 'modules\readcom.core.delphi\Source\Resources\READCOM.Resources.Themes.pas' {Themes: TDataModule};
  {$endregion}

  {$R *.res} //for Windows App metadata defined via Project Options (App Icon, Versioning Info)

begin
  Main;
end.

