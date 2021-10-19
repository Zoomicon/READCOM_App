unit READCOM.Views.Main;

interface

uses
  iPub.Rtl.Messaging, //for SubscribeAttribute, TipMessagingThread
  READCOM.Messages.Models, //for IMessageMenu
  READCOM.Views.Menu.HUD,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Objects, FMX.Controls, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Types, uZoomableFrame, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, READCOM.Views.StoryItem, READCOM.Views.PanelStoryItem,
  Zoomicon.Manipulator, READCOM.Views.AudioStoryItem;

type
  TMainForm = class(TForm)
    ScrollBox: TScrollBox;
    StoryHUD1: TStoryHUD;
    StoryPanel1: TPanelStoryItem;
    AudioStoryItem1: TAudioStoryItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);

  public
    //--- Events
    [Subscribe(TipMessagingThread.Main)]
    procedure OnMenu(const AMessage: IMessageMenu);
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
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  GMessaging.Unsubscribe(Self);
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

end.
