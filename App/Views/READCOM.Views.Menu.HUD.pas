unit READCOM.Views.Menu.HUD;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TStoryHUD = class(TFrame)
    BtnAdd: TSpeedButton;
    BtnMenu: TSpeedButton;
    btnEdit: TSpeedButton;
    procedure BtnMenuClick(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
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

procedure TStoryHUD.btnEditClick(Sender: TObject);
begin
  EditMode := not EditMode; //toggle
end;

procedure TStoryHUD.BtnMenuClick(Sender: TObject);
begin
  GMessaging.Post(TMessageMenu.Create As IMessageMenu);
end;

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
