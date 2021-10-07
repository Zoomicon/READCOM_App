unit READCOM.Views.Main;

interface

uses
  iPub.Rtl.Messaging, //for SubscribeAttribute, TipMessagingThread
  READCOM.Messages.Models, //for IMessageMenu
  READCOM.Views.Menu.HUD,
  READCOM.Views.Story,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Objects, FMX.Controls, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Types, uZoomableFrame, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, READCOM.Views.StoryItem, READCOM.Views.StoryFrame;

type
  TMainForm = class(TForm)
    ScrollBox: TScrollBox;
    StoryHUD1: TStoryHUD;
    StoryFrame1: TStoryFrame;

  public
    [Subscribe(TipMessagingThread.Main)]
    procedure OnMenu(const AMessage: IMessageMenu);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

constructor TMainForm.Create(AOwner: TComponent);
begin
  inherited;
  GMessaging.Subscribe(Self);
end;

destructor TMainForm.Destroy;
begin
  GMessaging.Unsubscribe(Self);
  inherited;
end;

procedure TMainForm.OnMenu(const AMessage: IMessageMenu);
begin
  ShowMessage('Menu');
end;

end.
