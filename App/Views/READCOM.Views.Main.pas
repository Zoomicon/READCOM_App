unit READCOM.Views.Main;

interface

uses
  iPub.Rtl.Messaging, //for SubscribeAttribute, TipMessagingThread
  READCOM.Messages.Models, //for IMessageMenu
  READCOM.Views.Menu.HUD,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Objects, FMX.Controls, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Types, uZoomableFrame, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, READCOM.Views.StoryItem, READCOM.Views.PanelStoryItem;

type
  TMainForm = class(TForm)
    ScrollBox: TScrollBox;
    StoryHUD1: TStoryHUD;
    StoryPanel1: TPanelStoryItem;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    //--- Events
    [Subscribe(TipMessagingThread.Main)]
    procedure OnMenu(const AMessage: IMessageMenu);
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
