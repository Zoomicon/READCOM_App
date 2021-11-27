program READCOM.App;

uses
  SysUtils,
  CodeSiteLogging,
  System.StartUpCopy,
  FMX.Forms,
  iPub.Rtl.Messaging in '..\3rdPartyLib\ipub-messaging-main\iPub.Rtl.Messaging.pas',
  Zoomicon.Manipulator in '..\Zoomicon.Manipulator\Zoomicon.Manipulator.pas' {Manipulator: TFrame},
  READCOM.App.Models in 'READCOM.App.Models.pas',
  READCOM.Messages.Classes in 'Messages\READCOM.Messages.Classes.pas',
  READCOM.Messages.Models in 'Messages\READCOM.Messages.Models.pas',
  READCOM.Views.Options.StoryItemOptions in 'Views\Options\READCOM.Views.Options.StoryItemOptions.pas' {StoryItemOptions: TFrame},
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
  Zoomicon.Media.Models in '..\Zoomicon.Media\Zoomicon.Media.Models.pas',
  Zoomicon.Generics.Functors in '..\Zoomicon.Generics\Functors\Zoomicon.Generics.Functors.pas',
  Zoomicon.Generics.Collections in '..\Zoomicon.Generics\Collections\Zoomicon.Generics.Collections.pas',
  FMX.ApplicationHelper in 'FMX.ApplicationHelper.pas',
  READCOM.App.Globals in 'READCOM.App.Globals.pas' {DataModule1: TDataModule},
  Zoomicon.FMX.Utils in '..\Zoomicon.Zooming\Zoomicon.FMX.Utils.pas',
  Zoomicon.Zooming.Models in '..\Zoomicon.Zooming\Zoomicon.Zooming.Models.pas',
  Zoomicon.Zooming.ZoomFrame in '..\Zoomicon.Zooming\Zoomicon.Zooming.ZoomFrame.pas' {ZoomFrame: TFrame},
  Zoomicon.Puzzler.Classes in '..\Zoomicon.Puzzler\Zoomicon.Puzzler.Classes.pas',
  Zoomicon.Puzzler.Models in '..\Zoomicon.Puzzler\Zoomicon.Puzzler.Models.pas',
  READCOM.Views.About in 'Views\READCOM.Views.About.pas' {AboutFrame: TFrame},
  u_UrlOpen in 'u_UrlOpen.pas',
  Zoomicon.Selector in '..\Zoomicon.Manipulator\Zoomicon.Selector.pas',
  READCOM.Views.TextStoryItem in 'Views\READCOM.Views.TextStoryItem.pas' {TextStoryItem: TFrame},
  Zoomicon.Text in '..\Zoomicon.Text\Zoomicon.Text.pas',
  READCOM.Views.Options.BitmapImageStoryItemOptions in 'Views\Options\READCOM.Views.Options.BitmapImageStoryItemOptions.pas' {StoryItemOptions1: TFrame};

{$R *.res}

{$IFDEF DEBUG}

  procedure EnableCodeSite;
  begin
    CodeSite.Enabled := CodeSite.Installed;
    if CodeSite.Enabled then
    begin
      if CodeSite.Enabled then
      begin
        var Destination := TCodeSiteDestination.Create(Application);
        with Destination do
          begin
          with LogFile do
            begin
            Active := True;
            FileName := ChangeFileExt(ExtractFileName(Application.ExeName), '.csl');
            FilePath := '$(MyDocs)\My CodeSite Files\Logs\';
            end;
          Viewer.Active := True; // also show Live Viewer
          end;
        CodeSite.Destination := Destination;
        CodeSite.Clear
      end;
    end;
  end;

{$ENDIF}

begin
  {$IFDEF DEBUG}
  EnableCodeSite;
  ReportMemoryLeaksOnShutdown := True;
  {$ELSE}
  CodeSite.Enabled := False;
  {$ENDIF}

  Randomize; //initializes the built-in random number generator with a random value (obtained from the system clock)
  //ApplicationHandleException := //...
  //ApplicationShowException := //...
  Application.Initialize;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TMainForm, MainForm);
  //ObjectDebuggerFMXForm1.Show;
  Application.Run;
end.
