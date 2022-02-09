unit READCOM.Views.Options.StoryItemOptions;

interface

uses
  READCOM.App.Globals, //for SVGIconImageList, SVGIconImageList1
  READCOM.App.Models, //for IStoryItemOptions
  System.SysUtils, System.Types,
  System.UITypes, //for TOpenOption
  System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts,
  FMX.Controls.Presentation, System.ImageList,
  FMX.ImgList, FMX.SVGIconImageList, System.Actions, FMX.ActnList, FMX.Edit;

type
  TStoryItemOptions = class(TFrame, IStoryItemOptions)
    AddDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    ActionList: TActionList;
    actionLoad: TAction;
    actionSave: TAction;
    panelUrlAction: TPanel;
    glyphUrlAction: TGlyph;
    editUrlAction: TEdit;
    actionChangeUrlAction: TAction;
    actionAdd: TAction;
    layoutButtons: TFlowLayout;
    btnToggleAnchored: TSpeedButton;
    btnAdd: TSpeedButton;
    btnLoad: TSpeedButton;
    btnSave: TSpeedButton;
    OpenDialog: TOpenDialog;
    btnToggleStoryPoint: TSpeedButton;
    btnToggleHome: TSpeedButton;
    procedure actionToggleAnchoredExecute(Sender: TObject);
    procedure actionLoadExecute(Sender: TObject);
    procedure actionSaveExecute(Sender: TObject);
    procedure actionChangeUrlActionExecute(Sender: TObject);
    procedure Glyph1Tap(Sender: TObject; const Point: TPointF);
    procedure editUrlActionChangeTracking(Sender: TObject);
    procedure editUrlActionMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure actionAddExecute(Sender: TObject);
    procedure actionToggleStoryPointExecute(Sender: TObject);
    procedure actionToggleHomeExecute(Sender: TObject);

  protected
    FStoryItem: IStoryItem;
    FPopup: TPopup;
    procedure CheckCreatePopup;

    {StoryItem}
    function GetStoryItem: IStoryItem;
    procedure SetStoryItem(const Value: IStoryItem);

    {View}
    function GetView: TControl;

  public
    destructor Destroy; override;

    procedure ShowPopup; //TODO: use PopupVisible boolean property instead
    procedure HidePopup;

    function ActAdd: Boolean;
    function ActLoad: Boolean;
    function ActSave: Boolean;

    property Popup: TPopup read FPopup write FPopup stored false;

  published
    property View: TControl read GetView stored false;
    property StoryItem: IStoryItem read GetStoryItem write SetStoryItem stored false;
  end;

implementation
  uses
    FMX.DialogService.Async, //for TDialogServiceAsync
    FMX.Objects, //for TImageWrapMode
    FMX.Styles.Objects; //for TStyleObject

{$R *.fmx}

{TStoryItemOptions}

destructor TStoryItemOptions.Destroy;
begin
  if Assigned(FPopup) then
    FPopup.RemoveObject(Self); //must do before FreeAndNil(FPopup) else it fails when we call "inherited" below (the popup seems to kill us though it wasn't our owner, we were just its child/content)

  FreeAndNil(FPopup);

  inherited; //do last
end;

procedure TStoryItemOptions.editUrlActionChangeTracking(Sender: TObject);
begin
  StoryItem.SetUrlAction(editUrlAction.Text);
end;

procedure TStoryItemOptions.editUrlActionMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  actionChangeUrlAction.Execute;
end;

{$region 'StoryItem'}

function TStoryItemOptions.GetStoryItem: IStoryItem;
begin
  result := FStoryItem;
end;

procedure TStoryItemOptions.SetStoryItem(const Value: IStoryItem);
begin
  FStoryItem := Value;

  with FStoryItem do
  begin
    editUrlAction.Text := GetUrlAction;
    btnToggleHome.IsPressed := Home;
    btnToggleStoryPoint.IsPressed := StoryPoint;
    btnToggleAnchored.IsPressed := Anchored;
  end;
end;

{$endregion}

{$region 'View'}

function TStoryItemOptions.GetView: TControl;
begin
  result := Self;
end;

procedure TStoryItemOptions.Glyph1Tap(Sender: TObject;
  const Point: TPointF);
begin
  ShowMessage('test');
end;

{$endregion}

{$endregion}

{$region 'Actions'}

function TStoryItemOptions.ActAdd: Boolean;
begin
  with AddDialog do
  begin
    DefaultExt := EXT_READCOM;
    Filter := StoryItem.GetAddFilesFilter;
    //Options := Options + [TOpenOption.ofAllowMultiSelect]; //Multi-selection (set in designer)
    result := Execute; //TODO: see if supported on Android (https://stackoverflow.com/questions/69138504/why-does-fmx-topendialog-not-work-in-android)
    if result then
      StoryItem.Add(Files.ToStringArray);
  end;
end;

function TStoryItemOptions.ActLoad: Boolean;
begin
  with OpenDialog do
  begin
    DefaultExt := EXT_READCOM;
    Filter := StoryItem.GetLoadFilesFilter;
    //Options := Options - [TOpenOption.ofAllowMultiSelect]; //Single-selection (set in designer)
    result := Execute; //TODO: see if supported on Android (https://stackoverflow.com/questions/69138504/why-does-fmx-topendialog-not-work-in-android)
    if result then
      StoryItem.Load(Filename); //TODO: seems to cause error (on MouseUp at Form) due to MouseCapture (probably from the popup) not having been released for some (child?) item that gets freed. Should try to get the Root (the form) and do SetCapture(nil) on it or similar, or try to get Capture to us here and release immediately (OR MAYBE THERE IS SOME OTHER ERROR AND WE SHOULD TRY TO REPLACE THE WHOLE ITEM VIA ITS PARENT INSTEAD OF LOADING CONTENT IN IT REMOVING ITS CHILDREN FIRST - THAT WAY WE'LL BE ABLE TO REPLACE AN ITEM WITH ANY OTHER ITEM)
  end;
end;

function TStoryItemOptions.ActSave: Boolean;
begin
  with SaveDialog do
  begin
    DefaultExt := EXT_READCOM;
    Filter := StoryItem.GetSaveFilesFilter;
    result := Execute; //TODO: see if supported on Android (https://stackoverflow.com/questions/69138504/why-does-fmx-topendialog-not-work-in-android)
    if result then
      StoryItem.Save(Filename);
  end;
end;

///

procedure TStoryItemOptions.actionToggleHomeExecute(Sender: TObject);
begin
  StoryItem.SetHome(btnToggleHome.IsPressed);
  ShowPopup; //show popup again to make the toggle evident
end;

procedure TStoryItemOptions.actionToggleStoryPointExecute(Sender: TObject);
begin
  StoryItem.SetStoryPoint(btnToggleStoryPoint.IsPressed);
  ShowPopup; //show popup again to make the toggle evident
end;

procedure TStoryItemOptions.actionToggleAnchoredExecute(Sender: TObject);
begin
  StoryItem.SetAnchored(btnToggleAnchored.IsPressed);
  ShowPopup; //show popup again to make the toggle evident
end;

procedure TStoryItemOptions.actionAddExecute(Sender: TObject);
begin
  actAdd;
end;

procedure TStoryItemOptions.actionLoadExecute(Sender: TObject);
begin
  actLoad;
end;

procedure TStoryItemOptions.actionSaveExecute(Sender: TObject);
begin
  actSave;
end;

procedure TStoryItemOptions.actionChangeUrlActionExecute(Sender: TObject);
begin
  TDialogServiceAsync.InputQuery('URL', ['URL'], [StoryItem.GetUrlAction], procedure(const AResult: TModalResult; const AValues: array of string)
    begin
    if (AResult = mrOk) then
      begin
      editUrlAction.Text := AValues[0]; //this will call "OnChangeTracking" handler
      //ShowPopup; //doesn't work (popup gets hidden after OK). Probably it is executed at other thread (and ignore), haven't tried telling it to do from the UI thread, or try else with a timeout to do it a moment later
      end;
    end
  );
  ShowPopup;
end;

{$endregion}

{$region 'Popup'}

procedure TStoryItemOptions.CheckCreatePopup;
begin
  if not Assigned(FPopup) then
  begin
    var popup := TPopup.Create(nil); //don't set StoryItem.View as owner, seems to always store it (irrespective of "Stored := false") //can't set Self as owner either, makes a circular reference
    var options := Self;
    with popup do
    begin
      Width := options.Width;
      Height := options.Height;
      options.Align := TAlignLayout.Client;
      AddObject(options);
      Placement:=TPlacement.MouseCenter;
      //DragWithParent := true; //don't use, will move with cursor (at Delphi 11)
      //PlacementTarget := (component as TControl);
      //PlacementRectangle:= TBounds.Create(RectF(0, 0, Width, Height));
    end;
    FPopup := popup;
  end;
end;

procedure TStoryItemOptions.ShowPopup;
begin
  CheckCreatePopup;

  if Assigned(FPopup) then
    begin
    FPopup.IsOpen := true;
    StoryItem := StoryItem; //cause re-init of toggle buttons //Note: Have to do it after opening the popup else SpeedButtons that had StaysPressed=true and Pressed=true don't appear pressed till one of them is clicked
    end;
end;

procedure TStoryItemOptions.HidePopup;
begin
  if Assigned(FPopup) then
  begin
    FPopup.IsOpen := false;
    //FreeAndNil(FPopup); //TODO: maybe should do to save resources
  end;
end;

{$endregion}

end.
