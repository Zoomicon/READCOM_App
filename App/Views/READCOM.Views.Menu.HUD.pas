unit READCOM.Views.Menu.HUD;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TStoryHUD = class(TFrame)
    BtnAdd: TSpeedButton;
    BtnPrevious: TSpeedButton;
    BtnEdit: TSpeedButton;
    BtnMenu: TSpeedButton;
    BtnNext: TSpeedButton;
    procedure BtnPreviousClick(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure BtnEditClick(Sender: TObject);
    procedure BtnMenuClick(Sender: TObject);
    procedure BtnNextClick(Sender: TObject);
  protected
    FEditMode: Boolean;
    procedure SetEditMode(const Value: Boolean); virtual;
  public
    property EditMode: Boolean read FEditMode write SetEditMode;
  end;

implementation
  uses
    READCOM.Messages.Models, //for IMessageMenu, IMessageAdd
    READCOM.Messages.Classes, //for TMessageMenu, TMessageAdd
    iPub.Rtl.Messaging; //for GMessaging

{$R *.fmx}

procedure TStoryHUD.BtnMenuClick(Sender: TObject);
begin
  GMessaging.Post(TMessageMenu.Create As IMessageMenu);
end;

procedure TStoryHUD.BtnEditClick(Sender: TObject);
begin
  EditMode := not EditMode; //toggle
end;

procedure TStoryHUD.BtnPreviousClick(Sender: TObject);
begin
  GMessaging.Post(TMessageNavigation.Create As IMessageNavigationPrevious);
end;

procedure TStoryHUD.BtnNextClick(Sender: TObject);
begin
  GMessaging.Post(TMessageNavigation.Create As IMessageNavigationNext);
end;

///////////

procedure TStoryHUD.SetEditMode(const Value: Boolean);
begin
  FEditMode := Value;
  GMessaging.Post(TMessageEditModeChange.Create(Value) As IMessageEditModeChange);
end;

procedure TStoryHUD.BtnAddClick(Sender: TObject);
begin
  GMessaging.Post(TMessageAdd.Create As IMessageAdd);
end;

end.
