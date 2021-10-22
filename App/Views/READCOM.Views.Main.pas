unit READCOM.Views.Main;

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
  FMX.Types, uZoomableFrame, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, READCOM.Views.PanelStoryItem,
  Zoomicon.Manipulator, READCOM.Views.AudioStoryItem;

type
  TMainForm = class(TForm)
    ScrollBox: TScrollBox;
    StoryHUD1: TStoryHUD;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormSaveState(Sender: TObject);

  protected
    function GetStory: IStoryItem;
    function GetStoryView: TStoryItem;
    procedure SetStoryView(const Value: TStoryItem);

  public
    //--- Events
    [Subscribe(TipMessagingThread.Main)]
    procedure OnMenu(const AMessage: IMessageMenu);

  published
    property Story: IStoryItem read GetStory stored false;
    property StoryView: TStoryItem read GetStoryView write SetStoryView;
  end;

var
  MainForm: TMainForm;

implementation
  uses CodeSiteLogging,
       ObjectDebuggerFMXForm,
       FormMessage;

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CodeSite.EnterMethod('FormCreate');
  GMessaging.Subscribe(Self);
  var TheStory := TPanelStoryItem.Create(Self);
  //SaveState.StoragePath := ... //TODO: default is transient, change to make permanent
  With SaveState do
    if Stream.Size > 0 then
    begin
      //TheStory.Size.Size := TSizeF.Create(640, 480);
      try
        TheStory.LoadReadCom(Stream);
        {}CodeSite.Send(TheStory.SaveToString);
      except
        on E: Exception do
          begin
          SaveState.Stream.Clear; //clear stream if causes loading error
          CodeSite.SendException(E);
          ShowException(E, @TMainForm.FormCreate);
          end;
      end;
    end;
  StoryView := TheStory;
  CodeSite.ExitMethod('FormCreate');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  GMessaging.Unsubscribe(Self);
end;

procedure TMainForm.FormSaveState(Sender: TObject);
begin
  CodeSite.EnterMethod('SaveState');
  SaveState.Stream.Clear;

  var TheStory := StoryView;
  if Assigned(TheStory) then
    try
      TheStory.SaveReadCom(SaveState.Stream);
      {}CodeSite.Send(TheStory.SaveToString);
    except
      On E: Exception do
        begin
        //SaveState.Stream.Clear; //clear stream in case it got corrupted
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

{$region 'Story'}

function TMainForm.GetStory: IStoryItem;
begin
  result := TObjectListEx<TFmxObject>.GetFirstInterface<IStoryItem>(Children);
end;

{$endregion}

{$region 'StoryVieww'}

function TMainForm.GetStoryView: TStoryItem;
begin
  if Assigned(ScrollBox.Content) then
    result := TObjectListEx<TFmxObject>.GetFirstClass<TStoryItem>(ScrollBox.Content.Children)
  else
    result := nil;
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
  Value.Parent := ScrollBox;
end;

end.
