unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, uStoryHUD,
  FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls,
  iPub.Rtl.Messaging, //for SubscribeAttribute, TipMessagingThread
  uStoryItem, Unit2,
  READCOM.App.Messages.Models, FMX.Objects, Zoomicon.Manipulator, FMX.Gestures;

type
  TForm1 = class(TForm)
    GestureManager1: TGestureManager;
    Manipulator1: TManipulator;
    StoryItem1: TStoryItem;
    procedure FormCreate(Sender: TObject);
    procedure StoryHUD1BtnMenuClick(Sender: TObject);
    procedure StoryHUD1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
  private
    { Private declarations }
  public

    [Subscribe(TipMessagingThread.Main)]
    procedure OnMenu(const AMessage: IMessageMenu);

    [Subscribe(TipMessagingThread.Main)]
    procedure OnAdd(const AMessage: IMessageAdd);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  GMessaging.Subscribe(Self);
  GestureManager1.RegisterControl(Manipulator1);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  GMessaging.Unsubscribe(Self);
end;

procedure TForm1.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
  ShowMessage('Gesture at form');
end;

procedure TForm1.OnAdd(const AMessage: IMessageAdd);
begin
     ShowMessage('Add');
end;

procedure TForm1.OnMenu(const AMessage: IMessageMenu);
begin
    ShowMessage('Menu1');
end;

procedure TForm1.StoryHUD1BtnMenuClick(Sender: TObject);
begin
  //StoryHUD1.BtnMenuClick(Sender);

end;

procedure TForm1.StoryHUD1Click(Sender: TObject);
begin
  showmessage('button');
end;

end.
