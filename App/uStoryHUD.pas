unit uStoryHUD;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TStoryHUD = class(TFrame)
    BtnAdd: TSpeedButton;
    BtnMenu: TSpeedButton;
    procedure BtnMenuClick(Sender: TObject);
    procedure BtnAddClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation
  uses
    READCOM.App.Messages.Models, //for IMessageMenu
    READCOM.App.Messages.Classes, //for TMessageMenu
    iPub.Rtl.Messaging; //for GMessaging

{$R *.fmx}

procedure TStoryHUD.BtnMenuClick(Sender: TObject);
begin
  GMessaging.Post(TMessageMenu.Create As IMessageMenu);
end;

procedure TStoryHUD.BtnAddClick(Sender: TObject);
begin
  GMessaging.Post(TMessageAdd.Create As IMessageMenu);
end;

end.
