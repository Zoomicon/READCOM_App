//Description: READ-COM HUD (Heads-Up-Display) View
//Author: George Birbilis (http://zoomicon.com)

{-$DEFINE NOSTYLE}

unit READCOM.Views.Menu.HUD;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, System.ImageList, FMX.ImgList,
  FMX.Layouts, System.Actions, FMX.ActnList,
  FMX.MultiView, SubjectStand, FrameStand,
  READCOM.Views.About, //for TAboutFrame
  READCOM.App.Globals, //for Globals.SVGIconImageList
  FMX.Colors;

type

  TEditModeChangedEvent = procedure (Sender: TObject; const Value: Boolean) of object;
  TStructureVisibleChangedEvent = procedure (Sender: TObject; const Value: Boolean) of object;
  TTargetsVisibleChangedEvent = procedure (Sender: TObject; const Value: Boolean) of object;
  TUseStoryTimerChangedEvent = procedure (Sender: TObject; const Value: Boolean) of object;

  TStoryHUD = class(TFrame)
    btnPrevious: TSpeedButton;
    btnToggleEditMode: TSpeedButton;
    BtnMenu: TSpeedButton;
    btnNext: TSpeedButton;
    layoutButtonsNavigation: TLayout;
    ActionList: TActionList;
    actionPrevious: TAction;
    actionNext: TAction;
    actionAddTextStoryItem: TAction;
    actionAbout: TAction;
    actionMenu: TAction;
    layoutButtons: TLayout;
    MultiView: TMultiView;
    btnToggleStructureVisible: TSpeedButton;
    MultiViewFrameStand: TFrameStand;
    layoutContent: TLayout;
    btnToggleTargetsVisible: TSpeedButton;
    layoutButtonsMain: TFlowLayout;
    layoutButtonsEdit: TFlowLayout;
    actionLoad: TAction;
    actionSave: TAction;
    btnLoad: TSpeedButton;
    btnSave: TSpeedButton;
    actionNew: TAction;
    btnNew: TSpeedButton;
    actionHome: TAction;
    btnHome: TSpeedButton;
    btnAbout: TSpeedButton;
    actionCopy: TAction;
    actionPaste: TAction;
    actionDelete: TAction;
    btnToggleUseStoryTimer: TSpeedButton;
    actionAddImageStoryItem: TAction;
    btnAddTextStoryItem: TSpeedButton;
    btnCopy: TSpeedButton;
    btnDelete: TSpeedButton;
    btnFlipHorizontally: TSpeedButton;
    btnFlipVertically: TSpeedButton;
    btnPaste: TSpeedButton;
    btnAddImageStoryItem: TSpeedButton;
    btnCut: TSpeedButton;
    actionCut: TAction;
    btnAdd: TSpeedButton;
    actionAdd: TAction;
    comboForeColor: TComboColorBox;
    comboBackColor: TComboColorBox;
    btnNextTheme: TSpeedButton;
    actionNextTheme: TAction;
    actionOptions: TAction;
    btnOptions: TSpeedButton;
    procedure actionAboutExecute(Sender: TObject);
    procedure actionMenuExecute(Sender: TObject);
    procedure btnToggleStructureVisibleClick(Sender: TObject);
    procedure btnToggleEditModeClick(Sender: TObject);
    procedure btnToggleTargetsVisibleClick(Sender: TObject);
    procedure btnToggleUseStoryTimerClick(Sender: TObject);

  protected
    FAboutFrame: TAboutFrame;
    FMultiViewOpenedWidth: Single;

    FEditMode: Boolean;
    FStructureVisible: Boolean;
    FTargetsVisible: Boolean;
    FUseStoryTimer: Boolean;

    FEditModeChanged: TEditModeChangedEvent;
    FStructureVisibleChanged: TStructureVisibleChangedEvent;
    FTargetsVisibleChanged: TTargetsVisibleChangedEvent;
    FUseStoryTimerChanged: TUseStoryTimerChangedEvent;

    {EditMode}
    procedure SetEditMode(const Value: Boolean); virtual;
    {StructureVisible}
    procedure SetStructureVisible(const Value: Boolean);
    {TargetsVisible}
    procedure SetTargetsVisible(const Value: Boolean);
    {UseStoryTimer}
    procedure SetUseStoryTimer(const Value: Boolean);

  public
    constructor Create(AOwner: TComponent); override;

  published
    property EditMode: Boolean read FEditMode write SetEditMode default false;
    property StructureVisible: Boolean read FStructureVisible write SetStructureVisible default false;
    property TargetsVisible: Boolean read FTargetsVisible write SetTargetsVisible default false;
    property UseStoryTimer: Boolean read FUseStoryTimer write SetUseStoryTimer default false;

    property OnEditModeChanged: TEditModeChangedEvent read FEditModeChanged write FEditModeChanged;
    property OnStructureVisibleChanged: TStructureVisibleChangedEvent read FStructureVisibleChanged write FStructureVisibleChanged;
    property OnTargetsVisibleChanged: TTargetsVisibleChangedEvent read FTargetsVisibleChanged write FTargetsVisibleChanged;
    property OnUseStoryTimerChanged: TUseStoryTimerChangedEvent read FUseStoryTimerChanged write FUseStoryTimerChanged;
  end;

implementation
  uses
    READCOM.App.URLs, READCOM.Views.Main; //for url_Open_In_Browser

{$R *.fmx}

constructor TStoryHUD.Create(AOwner: TComponent);
begin
  inherited;

  {$IF DEFINED(ANDROID) OR DEFINED(IOS)}
  btnLoad.Visible := false; //TODO: implement some simple Load file dialog for mobile devices (flat list of documents). Should have some button to delete files too
  btnSave.Visible := false; //TODO: implement a dialog to ask for a filename (and ask if want to replace if exists)
  //btnToggleEditMode.Visible := false; //TODO: enable edit again after implementing a Save (and ideally adding Share support too) dialog. Plus need to make the toolbars somehow fit in small screens
  btnAdd.Visible := false; //TODO: implement some simple Load file dialog for mobile devices (flat list of documents). Should have some button to delete files too
  {$ENDIF}

  {$IFDEF NOSTYLE}
  btnNextTheme.Visible := false; //TODO: if themes that support all platforms are used, enable again
  {$ENDIF}

  FEditMode := false;
  FMultiViewOpenedWidth := MultiView.Width;
  FTargetsVisible := false;

  StructureVisible := false; //calling the "setter" to hide the side panel (which is open in design mode to define its width)
end;

{$REGION 'Properties'}

{$region 'EditMode'}

procedure TStoryHUD.SetEditMode(const Value: Boolean);
begin
  FEditMode := Value;
  btnToggleEditMode.IsPressed := Value; //don't use "Pressed", need to use "IsPressed"

  layoutButtonsEdit.Visible := Value;

  if Assigned(FEditModeChanged) then
    FEditModeChanged(Self, Value);
end;

{$endregion}

{$region 'StrucureViewVisible'}

procedure TStoryHUD.SetStructureVisible(const Value: Boolean);
begin
  FStructureVisible := Value;
  btnToggleStructureVisible.IsPressed := Value; //don't use "Pressed", need to use "IsPressed"

  if Value then
    MultiView.Width := FMultiViewOpenedWidth
  else
    MultiView.Width := 0;

  if Assigned(FStructureVisibleChanged) then
    FStructureVisibleChanged(Self, Value);
end;

{$endregion}

{$region 'TargetsVisible'}

procedure TStoryHUD.SetTargetsVisible(const Value: Boolean);
begin
  FTargetsVisible := Value;
  btnToggleTargetsVisible.IsPressed := Value; //don't use "Pressed", need to use "IsPressed"

  if Assigned(FTargetsVisibleChanged) then
    FTargetsVisibleChanged(Self, Value);
end;

{$endregion}

{$region 'UseStoryTimer'}

procedure TStoryHUD.SetUseStoryTimer(const Value: Boolean);
begin
  FUseStoryTimer := Value;
  btnToggleUseStoryTimer.IsPressed := Value; //don't use "Pressed", need to use "IsPressed"

  if Assigned(FUseStoryTimerChanged) then
    FUseStoryTimerChanged(Self, Value);
end;

{$endregion}

{$ENDREGION}

{$REGION 'Actions'}

procedure TStoryHUD.actionMenuExecute(Sender: TObject);
begin
  layoutButtons.Visible := actionMenu.Checked;
end;

{$region 'Edit actions'}

procedure TStoryHUD.btnToggleEditModeClick(Sender: TObject);
begin
  EditMode := not EditMode; //don't use "btnToggleEditMode.Pressed", returns inconsistent values
end;

{$endregion}

{$region 'View actions'}

procedure TStoryHUD.btnToggleStructureVisibleClick(Sender: TObject);
begin
  StructureVisible := not StructureVisible; //don't use "btnToggleStructureVisible.Pressed", returns inconsistent values
end;

procedure TStoryHUD.btnToggleTargetsVisibleClick(Sender: TObject);
begin
  TargetsVisible := not TargetsVisible; //don't use "btnToggleTargetsVisible.Pressed", returns inconsistent values
end;

procedure TStoryHUD.btnToggleUseStoryTimerClick(Sender: TObject);
begin
  UseStoryTimer := not UseStoryTimer; //don't use "btnToggleUseStoryTimer.Pressed", returns inconsistent values
end;

{$endregion}

{$region 'Help actions'}

procedure TStoryHUD.actionAboutExecute(Sender: TObject);
begin
  TAboutFrame.ShowModal(MainForm); //has [X] button to close itself
end;

{$endregion}

{$ENDREGION}

end.