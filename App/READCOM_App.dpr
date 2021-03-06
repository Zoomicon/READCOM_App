//Description: READ-COM App
//Source: https://github.com/zoomicon/READCOM_App
//Author: George Birbilis (http://zoomicon.com)

program READCOM_App;

{$R *.dres}

uses
  System.StartUpCopy,
  {$IFDEF DEBUG}
  FormMessage in '..\3rdPartyLib\object-debugger-for-firemonkey\FormMessage.pas' {MessageForm},
  ObjectDebuggerFMXFrame in '..\3rdPartyLib\object-debugger-for-firemonkey\ObjectDebuggerFMXFrame.pas' {FMXObjectDebuggerFrame: TFrame},
  ObjectDebuggerFMXForm in '..\3rdPartyLib\object-debugger-for-firemonkey\DemoDesktop\ObjectDebuggerFMXForm.pas' {ObjectDebuggerFMXForm},
  {$ENDIF }
  Zoomicon.Generics.Factories in '..\Zoomicon.Generics\Factories\Zoomicon.Generics.Factories.pas',
  Zoomicon.Generics.Registries in '..\Zoomicon.Generics\Collections\Zoomicon.Generics.Registries.pas',
  Zoomicon.Generics.Functors in '..\Zoomicon.Generics\Functors\Zoomicon.Generics.Functors.pas',
  Zoomicon.Generics.Collections in '..\Zoomicon.Generics\Collections\Zoomicon.Generics.Collections.pas',
  Zoomicon.Helpers.RTL.ClassListHelpers in '..\Zoomicon.Helpers.RTL\Zoomicon.Helpers.RTL\Zoomicon.Helpers.RTL.ClassListHelpers.pas',
  Zoomicon.Helpers.RTL.ComponentHelpers in '..\Zoomicon.Helpers.RTL\Zoomicon.Helpers.RTL\Zoomicon.Helpers.RTL.ComponentHelpers.pas',
  Zoomicon.Helpers.RTL.StreamHelpers in '..\Zoomicon.Helpers.RTL\Zoomicon.Helpers.RTL\Zoomicon.Helpers.RTL.StreamHelpers.pas',
  Zoomicon.Helpers.FMX.Controls.ControlHelper in '..\Zoomicon.Helpers.FMX\Zoomicon.Helpers.FMX.Controls\Zoomicon.Helpers.FMX.Controls.ControlHelper.pas',
  Zoomicon.Helpers.FMX.Forms.ApplicationHelper in '..\Zoomicon.Helpers.FMX\Zoomicon.Helpers.FMX.Forms\Zoomicon.Helpers.FMX.Forms.ApplicationHelper.pas',
  Zoomicon.Helpers.FMX.Layouts.ScaledLayoutHelpers in '..\Zoomicon.Helpers.FMX\Zoomicon.Helpers.FMX.Layouts\Zoomicon.Helpers.FMX.Layouts.ScaledLayoutHelpers.pas',
  Zoomicon.Helpers.FMX.Layouts.ScrollBoxHelpers in '..\Zoomicon.Helpers.FMX\Zoomicon.Helpers.FMX.Layouts\Zoomicon.Helpers.FMX.Layouts.ScrollBoxHelpers.pas',
  Zoomicon.Helpers.FMX.ImgList.ImageListHelpers in '..\Zoomicon.Helpers.FMX\Zoomicon.Helpers.FMX.ImgList\Zoomicon.Helpers.FMX.ImgList.ImageListHelpers.pas',
  Zoomicon.Helpers.FMX.TreeView.TreeViewHelpers in '..\Zoomicon.Helpers.FMX\Zoomicon.Helpers.FMX.TreeView\Zoomicon.Helpers.FMX.TreeView.TreeViewHelpers.pas',
  Zoomicon.Zooming.FMX in '..\Zoomicon.Zooming\Zoomicon.Zooming.FMX.pas',
  Zoomicon.Zooming.FMX.ZoomFrame in '..\Zoomicon.Zooming\Zoomicon.Zooming.FMX.ZoomFrame.pas' {ZoomFrame: TFrame},
  Zoomicon.Zooming.Models in '..\Zoomicon.Zooming\Zoomicon.Zooming.Models.pas',
  Zoomicon.Introspection.FMX.StructureView in '..\Zoomicon.Introspection\Zoomicon.Introspection.FMX.StructureView.pas' {StructureView: TFrame},
  Zoomicon.Manipulation.FMX.CustomManipulator in '..\Zoomicon.Manipulation\Zoomicon.Manipulation.FMX.CustomManipulator.pas' {CustomManipulator: TFrame},
  Zoomicon.Manipulation.FMX.Manipulator in '..\Zoomicon.Manipulation\Zoomicon.Manipulation.FMX.Manipulator.pas' {Manipulator: TFrame},
  Zoomicon.Manipulation.FMX.Selector in '..\Zoomicon.Manipulation\Zoomicon.Manipulation.FMX.Selector.pas',
  Zoomicon.Cache.Classes in '..\Zoomicon.Cache\Zoomicon.Cache.Classes.pas',
  Zoomicon.Cache.Models in '..\Zoomicon.Cache\Zoomicon.Cache.Models.pas',
  Zoomicon.Downloader.Classes in '..\Zoomicon.Downloader\Zoomicon.Downloader.Classes.pas',
  Zoomicon.Downloader.Models in '..\Zoomicon.Downloader\Zoomicon.Downloader.Models.pas',
  Zoomicon.Media.FMX.MediaPlayerEx in '..\Zoomicon.Media\Zoomicon.Media.FMX.MediaPlayerEx.pas',
  Zoomicon.Media.FMX.Models in '..\Zoomicon.Media\Zoomicon.Media.FMX.Models.pas',
  Zoomicon.Puzzler.Classes in '..\Zoomicon.Puzzler\Zoomicon.Puzzler.Classes.pas',
  Zoomicon.Puzzler.Models in '..\Zoomicon.Puzzler\Zoomicon.Puzzler.Models.pas',
  Zoomicon.Text in '..\Zoomicon.Text\Zoomicon.Text.pas',
  READCOM.App.Globals in 'READCOM.App.Globals.pas' {Globals: TDataModule},
  READCOM.App.Models in 'READCOM.App.Models.pas',
  READCOM.App.URLs in 'READCOM.App.URLs.pas',
  READCOM.Views.Options.StoryItemOptions in 'Views\Options\READCOM.Views.Options.StoryItemOptions.pas' {StoryItemOptions: TFrame},
  READCOM.Views.Options.ImageStoryItemOptions in 'Views\Options\READCOM.Views.Options.ImageStoryItemOptions.pas' {ImageStoryItemOptions: TFrame},
  READCOM.Views.Options.TextStoryItemOptions in 'Views\Options\READCOM.Views.Options.TextStoryItemOptions.pas' {TextStoryItemOptions: TFrame},
  READCOM.Views.StoryItem in 'Views\READCOM.Views.StoryItem.pas' {StoryItem: TFrame},
  READCOM.Views.ImageStoryItem in 'Views\READCOM.Views.ImageStoryItem.pas' {ImageStoryItem: TFrame},
  READCOM.Views.AudioStoryItem in 'Views\READCOM.Views.AudioStoryItem.pas' {AudioStoryItem: TFrame},
  READCOM.Views.TextStoryItem in 'Views\READCOM.Views.TextStoryItem.pas' {TextStoryItem: TFrame},
  READCOM.Views.StoryItemFactory in 'Views\READCOM.Views.StoryItemFactory.pas',
  READCOM.Views.Menu.HUD in 'Views\READCOM.Views.Menu.HUD.pas' {StoryHUD: TFrame},
  READCOM.Views.Main in 'Views\READCOM.Views.Main.pas' {MainForm},
  READCOM.Views.About in 'Views\READCOM.Views.About.pas' {AboutFrame: TFrame},
  READCOM.Views.Modal in 'Views\READCOM.Views.Modal.pas' {ModalFrame: TFrame},
  READCOM.Views.Prompts.Wait in 'Views\Prompts\READCOM.Views.Prompts.Wait.pas' {WaitFrame: TFrame},
  READCOM.Views.Prompts.Rotate in 'Views\Prompts\READCOM.Views.Prompts.Rotate.pas' {RotateFrame: TFrame},
  READCOM.Views.Prompts.Lock in 'Views\Prompts\READCOM.Views.Prompts.Lock.pas' {LockFrame: TFrame},
  READCOM.App.Main in 'READCOM.App.Main.pas',
  READCOM.App.Debugging in 'READCOM.App.Debugging.pas',
  READCOM.App.Messages in 'READCOM.App.Messages.pas',
  FMX.Image32SVG in '..\3rdPartyLib\SVGIconImageList\Source\FMX.Image32SVG.pas',
  Img32.SVG.Core in '..\3rdPartyLib\SVGIconImageList\Image32\source\Img32.SVG.Core.pas',
  Zoomicon.Helpers.FMX.Forms.FormHelper in '..\Zoomicon.Helpers.FMX\Zoomicon.Helpers.FMX.Forms\Zoomicon.Helpers.FMX.Forms.FormHelper.pas',
  Zoomicon.Media.FMX.MediaDisplay in '..\Zoomicon.Media\Zoomicon.Media.FMX.MediaDisplay.pas';

{$R *.res}

begin
  Main;
end.

