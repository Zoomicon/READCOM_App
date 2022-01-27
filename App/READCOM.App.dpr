program READCOM.App;

uses
  SysUtils,
  System.StartUpCopy,
  FMX.Forms,
  CodeSiteLogging,
  u_UrlOpen in 'u_UrlOpen.pas',
  FormMessage in '..\3rdPartyLib\object-debugger-for-firemonkey\FormMessage.pas' {MessageForm},
  ObjectDebuggerFMXFrame in '..\3rdPartyLib\object-debugger-for-firemonkey\ObjectDebuggerFMXFrame.pas' {FMXObjectDebuggerFrame: TFrame},
  ObjectDebuggerFMXForm in '..\3rdPartyLib\object-debugger-for-firemonkey\DemoDesktop\ObjectDebuggerFMXForm.pas' {ObjectDebuggerFMXForm},
  Zoomicon.Media.FMX in '..\Zoomicon.Media\Zoomicon.Media.FMX.pas',
  Zoomicon.Media.Models in '..\Zoomicon.Media\Zoomicon.Media.Models.pas',
  Zoomicon.Generics.Functors in '..\Zoomicon.Generics\Functors\Zoomicon.Generics.Functors.pas',
  Zoomicon.Generics.Collections in '..\Zoomicon.Generics\Collections\Zoomicon.Generics.Collections.pas',
  Zoomicon.Puzzler.Classes in '..\Zoomicon.Puzzler\Zoomicon.Puzzler.Classes.pas',
  Zoomicon.Puzzler.Models in '..\Zoomicon.Puzzler\Zoomicon.Puzzler.Models.pas',
  Zoomicon.Text in '..\Zoomicon.Text\Zoomicon.Text.pas',
  Zoomicon.Introspection.FMX.StructureView in '..\Zoomicon.Introspection\Zoomicon.Introspection.FMX.StructureView.pas' {StructureView: TFrame},
  Zoomicon.Manipulation.FMX.CustomManipulator in '..\Zoomicon.Manipulation\Zoomicon.Manipulation.FMX.CustomManipulator.pas' {CustomManipulator: TFrame},
  Zoomicon.Manipulation.FMX.Manipulator in '..\Zoomicon.Manipulation\Zoomicon.Manipulation.FMX.Manipulator.pas' {Manipulator: TFrame},
  Zoomicon.Manipulation.FMX.Selector in '..\Zoomicon.Manipulation\Zoomicon.Manipulation.FMX.Selector.pas',
  Zoomicon.Zooming.FMX in '..\Zoomicon.Zooming\Zoomicon.Zooming.FMX.pas',
  Zoomicon.Zooming.FMX.ZoomFrame in '..\Zoomicon.Zooming\Zoomicon.Zooming.FMX.ZoomFrame.pas' {ZoomFrame: TFrame},
  Zoomicon.Zooming.Models in '..\Zoomicon.Zooming\Zoomicon.Zooming.Models.pas',
  Zoomicon.Helpers.FMX.Controls.ControlHelpers in '..\Zooming.Helpers\Zoomicon.Helpers.FMX.Controls\Zoomicon.Helpers.FMX.Controls.ControlHelpers.pas',
  Zoomicon.Helpers.FMX.Forms.ApplicationHelper in '..\Zooming.Helpers\Zoomicon.Helpers.FMX.Forms\Zoomicon.Helpers.FMX.Forms.ApplicationHelper.pas',
  Zoomicon.Helpers.FMX.Layouts.ScaledLayoutHelpers in '..\Zooming.Helpers\Zoomicon.Helpers.FMX.Layouts\Zoomicon.Helpers.FMX.Layouts.ScaledLayoutHelpers.pas',
  Zoomicon.Helpers.FMX.Layouts.ScrollBoxHelpers in '..\Zooming.Helpers\Zoomicon.Helpers.FMX.Layouts\Zoomicon.Helpers.FMX.Layouts.ScrollBoxHelpers.pas',
  Zoomicon.Helpers.RTL.ClassListHelpers in '..\Zooming.Helpers\Zoomicon.Helpers.RTL\Zoomicon.Helpers.RTL.ClassListHelpers.pas',
  Zoomicon.Helpers.RTL.ComponentHelpers in '..\Zooming.Helpers\Zoomicon.Helpers.RTL\Zoomicon.Helpers.RTL.ComponentHelpers.pas',
  Zoomicon.Helpers.RTL.StreamHelpers in '..\Zooming.Helpers\Zoomicon.Helpers.RTL\Zoomicon.Helpers.RTL.StreamHelpers.pas',
  Zoomicon.Generics.Factories in '..\Zoomicon.Generics\Factories\Zoomicon.Generics.Factories.pas',
  Zoomicon.Generics.Registries in '..\Zoomicon.Generics\Collections\Zoomicon.Generics.Registries.pas',
  Zoomicon.Helpers.FMX.ImgList.ImageListHelpers in '..\Zooming.Helpers\Zoomicon.Helpers.FMX.ImgList\Zoomicon.Helpers.FMX.ImgList.ImageListHelpers.pas',
  Zoomicon.Helpers.FMX.TreeView.TreeViewHelpers in '..\Zooming.Helpers\Zoomicon.Helpers.FMX.TreeView\Zoomicon.Helpers.FMX.TreeView.TreeViewHelpers.pas',
  READCOM.App.Globals in 'READCOM.App.Globals.pas' {Globals: TDataModule},
  READCOM.App.Models in 'READCOM.App.Models.pas',
  READCOM.Views.Options.StoryItemOptions in 'Views\Options\READCOM.Views.Options.StoryItemOptions.pas' {StoryItemOptions: TFrame},
  READCOM.Views.Options.BitmapImageStoryItemOptions in 'Views\Options\READCOM.Views.Options.BitmapImageStoryItemOptions.pas' {BitmapImageStoryItemOptions: TFrame},
  READCOM.Views.StoryItem in 'Views\READCOM.Views.StoryItem.pas' {StoryItem: TFrame},
  READCOM.Views.ImageStoryItem in 'Views\READCOM.Views.ImageStoryItem.pas' {ImageStoryItem: TFrame},
  READCOM.Views.BitmapImageStoryItem in 'Views\READCOM.Views.BitmapImageStoryItem.pas' {BitmapImageStoryItem: TFrame},
  READCOM.Views.VectorImageStoryItem in 'Views\READCOM.Views.VectorImageStoryItem.pas' {VectorImageStoryItem: TFrame},
  READCOM.Views.AudioStoryItem in 'Views\READCOM.Views.AudioStoryItem.pas' {AudioStoryItem: TFrame},
  READCOM.Views.TextStoryItem in 'Views\READCOM.Views.TextStoryItem.pas' {TextStoryItem: TFrame},
  READCOM.Views.PanelStoryItem in 'Views\READCOM.Views.PanelStoryItem.pas' {PanelStoryItem: TFrame},
  READCOM.Views.StoryItemFactory in 'Views\READCOM.Views.StoryItemFactory.pas',
  READCOM.Views.Menu.HUD in 'Views\READCOM.Views.Menu.HUD.pas' {StoryHUD: TFrame},
  READCOM.Views.Main in 'Views\READCOM.Views.Main.pas' {MainForm},
  READCOM.Views.About in 'Views\READCOM.Views.About.pas' {AboutFrame: TFrame};

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

  Application.CreateForm(TGlobals, Globals);
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TMessageForm, MessageForm);
  {$IFDEF DEBUG}
  //Application.CreateForm(TMessageForm, MessageForm);
  //Application.CreateForm(TObjectDebuggerFMXForm, ObjectDebuggerFMXForm1);
  //ObjectDebuggerFMXForm1.Show;
  {$ENDIF}

  try
    Application.Run;
  finally
    {$IFDEF DEBUG}
    //FreeAndNil(ObjectDebuggerFMXForm1); //the object debugger anyway seems to be leaking objects (if different objects are selected)
    {$ENDIF}
  end;
end.
