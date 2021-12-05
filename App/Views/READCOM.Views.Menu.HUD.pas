unit READCOM.Views.Menu.HUD;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, System.ImageList, FMX.ImgList,
  FMX.SVGIconImageList, FMX.Layouts, System.Actions, FMX.ActnList,
  FMX.MultiView, SubjectStand, FrameStand;

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
    layoutButtons: TLayout;
    MultiView: TMultiView;
    BtnStructure: TSpeedButton;
    actionStructure: TAction;
    DrawerFrameStand: TFrameStand;
    layoutContent: TLayout;
    procedure actionEditExecute(Sender: TObject);
    procedure actionAboutExecute(Sender: TObject);
    procedure actionMenuExecute(Sender: TObject);
    procedure actionStructureExecute(Sender: TObject);

  protected
    {EditMode}
    function GetEditMode: Boolean;
    procedure SetEditMode(const Value: Boolean); virtual;

  public
    property EditMode: Boolean read GetEditMode write SetEditMode;
  end;

implementation
  uses
    READCOM.Views.About; //for TAbout

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

{$region 'View actions'}

procedure TStoryHUD.actionMenuExecute(Sender: TObject);
begin
  layoutButtons.Visible := actionMenu.Checked;
end;

procedure TStoryHUD.actionStructureExecute(Sender: TObject);
begin
  MultiView.ShowMaster
end;

{$endregion}

{$region 'Edit actions'}

procedure TStoryHUD.actionEditExecute(Sender: TObject);
begin
  var inEditMode := EditMode;
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
