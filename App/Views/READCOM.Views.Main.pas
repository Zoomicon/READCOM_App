unit READCOM.Views.Main;

//TODO: fix issue where if Popup with Options is shown at least once, then save state puts options data in the stored file, failing to load them (since that class isn't declared [doesn't need to after all] with streaming system). MAYBE JUST DESTROY THE POPUP WHEN CLOSED and make sure all are closed before saving

interface

uses
  iPub.Rtl.Messaging, //for SubscribeAttribute, TipMessagingThread
  Zoomicon.Generics.Collections, //for TObjectListEx
  READCOM.Messages.Models, //for IMessageMenu
  READCOM.App.Models, //for IStoryItem
  READCOM.Views.StoryItem, //for TStoryItem
  READCOM.Views.Menu.HUD,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Objects, FMX.Controls, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Types, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, READCOM.Views.PanelStoryItem,
  Zoomicon.Manipulator, READCOM.Views.AudioStoryItem;

type
  TMainForm = class(TForm)
    ScrollingStoryHost: TScrollBox;
    StoryHUD: TStoryHUD;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormSaveState(Sender: TObject);
    procedure ScrollingStoryHostResized(Sender: TObject);
    procedure StoryHUDBtnMenuClick(Sender: TObject);

  protected
    function LoadSavedState: Boolean;
    procedure StoryResized(Sender: TObject);
    procedure CheckCenterStory;
    {Story}
    function GetStory: IStoryItem;
    procedure SetStory(const Value: IStoryItem);
    {StoryView}
    function GetStoryView: TStoryItem;
    procedure SetStoryView(const Value: TStoryItem);

  public
    //--- Events
    [Subscribe(TipMessagingThread.Main)]
    procedure OnMenu(const AMessage: IMessageMenu);

    [Subscribe(TipMessagingThread.Main)]
    procedure OnEditModeChange(const AMessage: IMessageEditModeChange);

  published
    property Story: IStoryItem read GetStory write SetStory stored false;
    property StoryView: TStoryItem read GetStoryView write SetStoryView stored false;
  end;

var
  MainForm: TMainForm;

implementation
  uses CodeSiteLogging,
       ObjectDebuggerFMXForm,
       FormMessage;

{$R *.fmx}

function TMainForm.LoadSavedState: Boolean;
begin
  result := false;
  With SaveState do
  begin
    //StoragePath := ... //TODO: default is transient, change to make permanent
    if Stream.Size > 0 then
    begin
      var TheStory := TPanelStoryItem.Create(Self);
      try
        TheStory.Load(Stream); //default file format is EXT_READCOM
        {}CodeSite.Send(TheStory.SaveToString);
        StoryView := TheStory; //only set StoryView
        result := true;
      except
        on E: Exception do
          begin
          Stream.Clear; //clear stream if causes loading error //TODO: instead of Clear which doesn't seem to work, try saving instead a new instance of TPanelStoryItem
          CodeSite.SendException(E);
          ShowException(E, @TMainForm.FormCreate);
          FreeAndNil(TheStory); //Free partially loaded - corrupted StoryItem
          end;
      end;
    end;
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CodeSite.EnterMethod('FormCreate');
  GMessaging.Subscribe(Self);

  if (not LoadSavedState) then
    StoryView := TPanelStoryItem.Create(Self);

  CodeSite.ExitMethod('FormCreate');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  GMessaging.Unsubscribe(Self);
end;

procedure TMainForm.FormSaveState(Sender: TObject);
begin
  CodeSite.EnterMethod('SaveState');
  //StoragePath := ... //TODO: default is transient, change to make permanent
  SaveState.Stream.Clear;

  var TheStory := StoryView;
  if Assigned(TheStory) then
    with SaveState do
      try
        TheStory.Save(Stream); //default file format is EXT_READCOM
        {}CodeSite.Send(TheStory.SaveToString);
      except
        On E: Exception do
          begin
          Stream.Clear; //clear stream in case it got corrupted
          CodeSite.SendException(E);
          ShowException(E, @TMainForm.FormCreate);
          end;
    end;
  CodeSite.ExitMethod('SaveState');
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
{
  Application.CreateForm(TMessageForm, MessageForm);
  Application.CreateForm(TObjectDebuggerFMXForm, ObjectDebuggerFMXForm1);
  ObjectDebuggerFMXForm1.Show;
}
end;

procedure TMainForm.OnMenu(const AMessage: IMessageMenu);
begin
  ShowMessage('Menu');
end;

procedure TMainForm.OnEditModeChange(const AMessage: IMessageEditModeChange);
begin
  var view := StoryView;
  if Assigned(view) then
    view.EditMode := AMessage.Value;
end;

{$region 'Story'}

function TMainForm.GetStory: IStoryItem;
begin
  result := TObjectListEx<TFmxObject>.GetFirstInterface<IStoryItem>(Children);
end;

procedure TMainForm.SetStory(const Value: IStoryItem);
begin
  StoryView := Value.GetView as TStoryItem;
end;

{$endregion}

{$region 'StoryView'}

function TMainForm.GetStoryView: TStoryItem;
begin
  result := TObjectListEx<TControl>.GetFirstClass<TStoryItem>(ScrollingStoryHost.Content.Controls)
end;

procedure TMainForm.SetStoryView(const Value: TStoryItem);
begin
  //Remove old story
  var TheStory := StoryView;
  if Assigned(TheStory) then
    begin
    TheStory.Parent := nil;
    FreeAndNil(TheStory); //destroy the old Story
    end;

  //Add new story
  Value.Align := TAlignLayout.Fit;
  //CheckCenterStory;
  Value.Size.Size := TSizeF.Create(ScrollingStoryHost.ClientWidth, ScrollingStoryHost.ClientHeight);
  //Value.Align := TAlignLayout.Scale;
  Value.Parent := ScrollingStoryHost.Content;
  Value.OnResized := StoryResized; //do after setting Align back to None
end;

{$endregion}

procedure TMainForm.CheckCenterStory;
begin
  var view := StoryView;
  if Assigned(view) then
    with view do
    begin
    var centerHorz := (Width < ScrollingStoryHost.ClientWidth);
    var centerVert := (Height < ScrollingStoryHost.ClientHeight);
    if (centerHorz or centerVert) then Align := TAlignLayout.Center;
    //After it has changed size now set again to no alignment
    Align := TAlignLayout.None;
    end;
end;

procedure TMainForm.ScrollingStoryHostResized(Sender: TObject);
begin
//  CheckCenterStory;
end;

procedure TMainForm.StoryHUDBtnMenuClick(Sender: TObject);
begin
  StoryHUD.BtnMenuClick(Sender);

end;

procedure TMainForm.StoryResized(Sender: TObject);
begin
//  CheckCenterStory;
end;

end.
