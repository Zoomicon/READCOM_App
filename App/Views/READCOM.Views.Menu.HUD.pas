unit READCOM.Views.Menu.HUD;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, System.ImageList, FMX.ImgList,
  FMX.SVGIconImageList, FMX.Layouts, System.Actions, FMX.ActnList;

type
  TStoryHUD = class(TFrame)
    BtnAdd: TSpeedButton;
    BtnPrevious: TSpeedButton;
    BtnEdit: TSpeedButton;
    BtnMenu: TSpeedButton;
    BtnNext: TSpeedButton;
    SVGIconImageList: TSVGIconImageList;
    layoutEdit: TLayout;
    layoutNavigation: TLayout;
    ActionList1: TActionList;
    actionPrevious: TAction;
    actionNext: TAction;
    actionAdd: TAction;
    actionEdit: TAction;
    actionAbout: TAction;
    actionMenu: TAction;
    layoutAll: TLayout;
    procedure actionEditExecute(Sender: TObject);
    procedure actionPreviousExecute(Sender: TObject);
    procedure actionNextExecute(Sender: TObject);
    procedure actionAboutExecute(Sender: TObject);
    procedure actionAddExecute(Sender: TObject);
    procedure actionMenuExecute(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  protected
    {EditMode}
    function GetEditMode: Boolean;
    procedure SetEditMode(const Value: Boolean); virtual;
  public
    property EditMode: Boolean read GetEditMode write SetEditMode;
  end;

implementation
  uses
    READCOM.Messages.Models, //for IMessageMenu, IMessageAdd
    READCOM.Messages.Classes, //for TMessageMenu, TMessageAdd
    READCOM.Views.About, //for TAbout
    iPub.Rtl.Messaging; //for GMessaging

{$R *.fmx}

{$REGION 'Properties'}

{$region 'EditMode'}

function TStoryHUD.GetEditMode: Boolean;
begin
  result := actionEdit.Checked;
end;

procedure TStoryHUD.SetEditMode(const Value: Boolean);
begin
  actionEdit.Checked := Value; //see if it causes firing of event on change
end;

{$endregion}

{$ENDREGION}

{$REGION 'Actions'}

procedure TStoryHUD.actionMenuExecute(Sender: TObject);
begin
  layoutAll.Visible := actionMenu.Checked;
end;

{$region 'Navigation actions'}

procedure TStoryHUD.actionPreviousExecute(Sender: TObject);
begin
  GMessaging.Post(TMessageNavigation.Create As IMessageNavigationPrevious);
end;

procedure TStoryHUD.Button1Click(Sender: TObject);
begin
 showMessage('test');
end;

procedure TStoryHUD.actionNextExecute(Sender: TObject);
begin
  GMessaging.Post(TMessageNavigation.Create As IMessageNavigationNext);
end;

{$endregion}

{$region 'Edit actions'}

procedure TStoryHUD.actionAddExecute(Sender: TObject);
begin
  GMessaging.Post(TMessageAdd.Create As IMessageAdd);
end;

procedure TStoryHUD.actionEditExecute(Sender: TObject);
begin
  var inEditMode := EditMode;
  GMessaging.Post(TMessageEditModeChange.Create(inEditMode) As IMessageEditModeChange);
  layoutEdit.Visible := inEditMode;
end;

{$endregion}

{$region 'Help actions'}

procedure TStoryHUD.actionAboutExecute(Sender: TObject);
begin
  var popup := TPopup.Create(Self);
  with popup do
  begin
    Placement := TPlacement.Center;
    PlacementTarget := Self;
    AddObject(TAboutFrame.Create(Self)); //TODO: see that this doesn't get stored and how to release on popup close
  end;
  popup.Visible := true;
end;

{$endregion}

{$ENDREGION}

end.
