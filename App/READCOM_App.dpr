//Description: READ-COM App
//Source: https://github.com/Zoomicon/READCOM_App
//Author: George Birbilis (https://zoomicon.com)

//Dependencies: see <DEPENDENCIES.md>

program READCOM_App;

  {$R *.dres} //for Windows resources added via Resources and Images (includes 'Default.readcom' startup story document)

  {$region 'Used units' ---------------------------------------------------------} //Note: D12.3 can't fold/expand regions in .dpr files
  uses
    System.StartUpCopy,
    READCOM.App.Main,
    // 
    Zoomicon.Media.FMX.ModalFrame in 'modules\zoomicon.media.fmx.delphi\Source\Zoomicon.Media.FMX.ModalFrame.pas' {ModalFrame: TFrame}, //needed to correctly open About frame in IDE designer
    READCOM.Resources.Icons in 'modules\readcom.core.delphi\Source\Resources\READCOM.Resources.Icons.pas' {Icons: TDataModule}, //needed to correctly open About frame in IDE designer
    //
    READCOM.Views.Dialogs.About in 'Views\Dialogs\READCOM.Views.Dialogs.About.pas' {AboutFrame: TFrame},
    READCOM.App.MessageOverrides in 'READCOM.App.MessageOverrides.pas', //can override app title and other strings here
    READCOM.App.EventOverrides in 'READCOM.App.EventOverrides.pas'; //can implement custom event handlers here    
  {$endregion}

  {$R *.res} //for Windows App metadata defined via Project Options (App Icon, Versioning Info)

begin
  Main(TAboutFrame, EventHandlers.StoryFormReady, EventHandlers.StoryLoaded);
end.

