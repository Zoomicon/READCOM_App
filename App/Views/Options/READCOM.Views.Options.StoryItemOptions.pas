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
    actionDelete: TAction;
    actionLoad: TAction;
    actionSave: TAction;
    actionAnchor: TAction;
    panelUrlAction: TPanel;
    glyphUrlAction: TGlyph;
    editUrlAction: TEdit;
    actionChangeUrlAction: TAction;
    actionAdd: TAction;
    layoutButtons: TFlowLayout;
    btnAnchor: TSpeedButton;
    btnDelete: TSpeedButton;
    btnAdd: TSpeedButton;
    btnLoad: TSpeedButton;
    btnSave: TSpeedButton;
    OpenDialog: TOpenDialog;
    procedure actionAnchorExecute(Sender: TObject);
    procedure actionDeleteExecute(Sender: TObject);
    procedure actionLoadExecute(Sender: TObject);
    procedure actionSaveExecute(Sender: TObject);
    procedure actionChangeUrlActionExecute(Sender: TObject);
    procedure Glyph1Tap(Sender: TObject; const Point: TPointF);
    procedure editUrlActionChangeTracking(Sender: TObject);
    procedure editUrlActionMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
    procedure actionAddExecute(Sender: TObject);

  protected
    FStoryItem: IStoryItem;
    FPopup: TPopup;
    procedure CheckCreatePopup;

    {StoryItem}
    function GetStoryItem: IStoryItem;
    procedure SetStoryItem(const Value: IStoryItem);

    {View}
    function GetView: TControl;

    {DeleteVisible}
    function IsDeleteVisible: Boolean;
    procedure SetDeleteVisible(const Value: Boolean);

  public
    destructor Destroy; override;

    procedure ShowPopup; //TODO: use PopupVisible boolean property instead
    procedure HidePopup;

    procedure ActAdd;
    procedure ActLoad;
    procedure ActSave;

    property Popup: TPopup read FPopup write FPopup stored false;

  published
    property View: TControl read GetView stored false;
    property StoryItem: IStoryItem read GetStoryItem write SetStoryItem stored false;
    property DeleteVisible: Boolean read IsDeleteVisible write SetDeleteVisible stored false;
  end;

implementation
  uses
    FMX.DialogService.Async, //for TDialogServiceAsync
    FMX.Objects, //for TImageWrapMode
    FMX.Styles.Objects; //for TStyleObject

{$R *.fmx}

{ TStoryItemOptions }

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
    actionAnchor.Checked := IsAnchored;
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

{$region 'DeleteVisible'}

function TStoryItemOptions.IsDeleteVisible: Boolean;
begin
  result := btnDelete.Visible;
end;

procedure TStoryItemOptions.SetDeleteVisible(const Value: Boolean);
begin
  btnDelete.Visible := Value;
end;

{$endregion}

{$region 'Actions'}

procedure TStoryItemOptions.ActAdd;
begin
  actionAddExecute(actionAdd);
end;

procedure TStoryItemOptions.ActLoad;
begin
  actionLoadExecute(actionLoad);
end;

procedure TStoryItemOptions.ActSave;
begin
  actionSaveExecute(actionSave);
end;

procedure TStoryItemOptions.actionAnchorExecute(Sender: TObject);
begin
  actionAnchor.Checked := not actionAnchor.Checked;
  StoryItem.SetAnchored(actionAnchor.Checked);
  ShowPopup;
end;

procedure TStoryItemOptions.actionDeleteExecute(Sender: TObject);
begin
  FreeAndNil(GetStoryItem As TComponent);
end;

procedure TStoryItemOptions.actionAddExecute(Sender: TObject);
begin
  with AddDialog do
  begin
    DefaultExt := EXT_READCOM;
    Filter := StoryItem.GetAddFilesFilter;
    //Options := Options + [TOpenOption.ofAllowMultiSelect]; //Multi-selection
    if Execute then //TODO: see if supported on Android (https://stackoverflow.com/questions/69138504/why-does-fmx-topendialog-not-work-in-android)
      StoryItem.Add(Files.ToStringArray);
  end;
end;

procedure TStoryItemOptions.actionLoadExecute(Sender: TObject);
begin
  with OpenDialog do
  begin
    DefaultExt := EXT_READCOM;
    Filter := StoryItem.GetLoadFilesFilter;
    //Options := Options - [TOpenOption.ofAllowMultiSelect]; //Single-selection
    if Execute then //TODO: see if supported on Android (https://stackoverflow.com/questions/69138504/why-does-fmx-topendialog-not-work-in-android)
      StoryItem.Load(Filename); //TODO: seems to cause error (on MouseUp at Form) due to MouseCapture (probably from the popup) not having been released for some (child?) item that gets freed. Should try to get the Root (the form) and do SetCapture(nil) on it or similar, or try to get Capture to us here and release immediately (OR MAYBE THERE IS SOME OTHER ERROR AND WE SHOULD TRY TO REPLACE THE WHOLE ITEM VIA ITS PARENT INSTEAD OF LOADING CONTENT IN IT REMOVING ITS CHILDREN FIRST - THAT WAY WE'LL BE ABLE TO REPLACE AN ITEM WITH ANY OTHER ITEM)
  end;
end;

procedure TStoryItemOptions.actionSaveExecute(Sender: TObject);
begin
  with SaveDialog do
  begin
    DefaultExt := EXT_READCOM;
    Filter := StoryItem.GetSaveFilesFilter;
    if Execute then //TODO: see if supported on Android (https://stackoverflow.com/questions/69138504/why-does-fmx-topendialog-not-work-in-android)
      StoryItem.Save(Filename);
  end;
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
    FPopup.IsOpen := true;
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
