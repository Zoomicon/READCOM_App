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
  uses ObjectDebuggerFMXForm,
       FormMessage;

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);
begin
  GMessaging.Subscribe(Self);
  //SaveState.StoragePath := ... //TODO: default is transient, change to make permanent
  With SaveState do
    if Stream.Size > 0 then
    begin
      var NewStory := TStoryItem.Create(Self);
      //NewStory.Size.Size := TSizeF.Create(640, 480);
      try
        Stream.ReadComponent(NewStory);
      except
        on E: Exception do
          begin
          SaveState.Stream.Clear; //clear stream if causes loading error
          //TODO: should do some logging here (e.g. could be catching Invalid Stream Format)
          ShowException(E, @TMainForm.FormCreate);
          end;
      end;
      StoryView := NewStory;
    end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  GMessaging.Unsubscribe(Self);
end;

procedure TMainForm.FormSaveState(Sender: TObject);
begin
  SaveState.Stream.Clear;

  var TheStory := StoryView;
  if Assigned(TheStory) then
    SaveState.Stream.WriteComponent(TheStory);
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
  result := TObjectListEx<TFmxObject>.GetFirstClass<TStoryItem>(ScrollBox.Content.Children);
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
  Value.Parent := ScrollBox.Content;
end;

end.
