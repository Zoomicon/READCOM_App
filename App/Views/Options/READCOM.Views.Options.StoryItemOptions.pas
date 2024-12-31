unit READCOM.Views.Options.StoryItemOptions;

interface

uses
  READCOM.Resources.Icons, //for Icons.SVGIconImageList
  READCOM.Models.Stories, //for IStoryItemOptions
  System.SysUtils, System.Types,
  System.UITypes, //for TOpenOption
  System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts,
  FMX.Controls.Presentation, System.ImageList,
  FMX.ImgList, FMX.SVGIconImageList, System.Actions, FMX.ActnList, FMX.Edit,
  FMX.Objects;

resourcestring
  STR_URL = 'URL';
  STR_TAGS = 'Tags';

type
  TStoryItemOptions = class(TFrame, IStoryItemOptions)
    LayoutStoryItemButtons: TFlowLayout;
    ActionList: TActionList;
    actionLoad: TAction;
    actionSave: TAction;
    actionAdd: TAction;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    AddDialog: TOpenDialog;
    btnToggleHome: TSpeedButton;
    btnToggleStoryPoint: TSpeedButton;
    btnToggleAnchored: TSpeedButton;
    btnToggleActionURL: TSpeedButton;
    btnLoad: TSpeedButton;
    btnSave: TSpeedButton;
    btnToggleTags: TSpeedButton;
    Background: TRectangle;
    btnToggleSnapping: TSpeedButton;
    Layout: TFlowLayout;
    LayoutStoryItemBreak: TFlowLayoutBreak;
    procedure actionToggleHomeExecute(Sender: TObject);
    procedure actionToggleStoryPointExecute(Sender: TObject);
    procedure actionToggleSnappingExecute(Sender: TObject);
    procedure actionToggleAnchoredExecute(Sender: TObject);
    procedure actionChangeUrlActionExecute(Sender: TObject);
    procedure actionChangeTagsExecute(Sender: TObject);
    procedure actionLoadExecute(Sender: TObject);
    procedure actionSaveExecute(Sender: TObject);

  protected
    FStoryItem: IStoryItem;
    FPopup: TPopup;
    procedure CheckCreatePopup;

    {StoryItem}
    function GetStoryItem: IStoryItem;
    procedure SetStoryItem(const Value: IStoryItem); virtual;

    {View}
    function GetView: TControl;

  public
    destructor Destroy; override;

    procedure ShowPopup; //TODO: use PopupVisible boolean property instead
    procedure HidePopup;

    function ActAdd: Boolean;
    function ActLoad_GetFilename: String;
    function ActLoad: Boolean;
    function ActSave: Boolean;
    procedure ActChangeUrl;
    procedure ActChangeTags;

    property Popup: TPopup read FPopup write FPopup stored false;

  published
    property View: TControl read GetView stored false;
    property StoryItem: IStoryItem read GetStoryItem write SetStoryItem stored false;
  end;

implementation
  uses
    FMX.DialogService.Async, //for TDialogServiceAsync
    READCOM.Models; //for EXT_READCOM

{$R *.fmx}

{TStoryItemOptions}

{$REGION 'LIFETIME MANAGEMENT'}

destructor TStoryItemOptions.Destroy;
begin
  if Assigned(FPopup) then
    FPopup.RemoveObject(Self); //must do before FreeAndNil(FPopup) else it fails when we call "inherited" below (the popup seems to kill us though it wasn't our owner, we were just its child/content)

  FreeAndNil(FPopup);

  inherited; //do last
end;

{$ENDREGION}

{$REGION 'PROPERTIES'}

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
    btnToggleHome.IsPressed := Home;
    btnToggleStoryPoint.IsPressed := StoryPoint;
    btnToggleSnapping.IsPressed := Snapping;
    btnToggleAnchored.IsPressed := Anchored;
    btnToggleActionURL.IsPressed := (UrlAction <> '');
    btnToggleTags.IsPressed := (Tags <> '');

    {$IF DEFINED(ANDROID) OR DEFINED(IOS)}
    btnLoad.Visible := false; //TODO: implement some simple Load file dialog for mobile devices (flat list of documents). Should have some button to delete files too
    btnSave.Visible := false; //TODO: implement a dialog to ask for a filename (and ask if want to replace if exists)
    {$ENDIF}
  end;
end;

{$endregion}

{$region 'View'}

function TStoryItemOptions.GetView: TControl;
begin
  result := Self;
end;

{$endregion}

{$ENDREGION PROPERTIES}

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

function TStoryItemOptions.ActLoad_GetFilename: String;
begin
  with OpenDialog do
  begin
    DefaultExt := EXT_READCOM;
    Filter := StoryItem.GetLoadFilesFilter;
    //Options := Options - [TOpenOption.ofAllowMultiSelect]; //Single-selection (set in designer)
    if Execute then //TODO: see if supported on Android and iOS, also see Kastri (https://stackoverflow.com/questions/69138504/why-does-fmx-topendialog-not-work-in-android)
      result := Filename
    else
      result := '';
  end;
end;

function TStoryItemOptions.ActLoad: Boolean;
begin
  var Filename := ActLoad_GetFilename;
  result := (Filename <> '');
  if result then
    result := Assigned(StoryItem.Load(Filename)); //TODO: seems to cause error (on MouseUp at Form) due to MouseCapture (probably from the popup) not having been released for some (child?) item that gets freed. Should try to get the Root (the form) and do SetCapture(nil) on it or similar, or try to get Capture to us here and release immediately (OR MAYBE THERE IS SOME OTHER ERROR AND WE SHOULD TRY TO REPLACE THE WHOLE ITEM VIA ITS PARENT INSTEAD OF LOADING CONTENT IN IT REMOVING ITS CHILDREN FIRST - THAT WAY WE'LL BE ABLE TO REPLACE AN ITEM WITH ANY OTHER ITEM)
end; //TODO: need to change ActLoad to load any file and replace the current one (if not the RootStoryItem should maybe resize to take current bounds), then return the StoryItem instance that was created from that file info (not assume it's same class of StoryItem, TPanelStoryItem in the case of the story [want to load any StoryItem as root - also make sure when RootStoryItem changes the old one is released to not leak]). Can pass true to 2nd optional parameter of load, but need to return the storyitem instead of boolean (can return nil on fail/cancel - also add try/catch maybe?)

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

procedure TStoryItemOptions.ActChangeUrl;
begin
  TDialogServiceAsync.InputQuery(STR_URL, [STR_URL], [StoryItem.GetUrlAction],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if (AResult = mrOk) then
      begin
        var LUrl := Trim(AValues[0]);
        StoryItem.SetUrlAction(LUrl);
        btnToggleActionURL.IsPressed := (LUrl <> '');
      end;
      //ShowPopup; //TODO: doesn't work (popup gets hidden after OK/Cancel). Probably it is executed at other thread (and ignore), haven't tried telling it to do from the UI thread, or try else with a timeout to do it a moment later
    end
  );
  //ShowPopup; //see comment above //doesn't work either (popup shown in the background but closes after OK/Cancel at input prompt)
end;

procedure TStoryItemOptions.ActChangeTags;
begin
  TDialogServiceAsync.InputQuery(STR_TAGS, [STR_TAGS], [StoryItem.GetTags],
    procedure(const AResult: TModalResult; const AValues: array of string)
    begin
      if (AResult = mrOk) then
      begin
        var LTags := Trim(AValues[0]);
        StoryItem.Tags := LTags;
        btnToggleTags.IsPressed := (LTags <> '');
      end;
      //ShowPopup; //TODO: doesn't work (popup gets hidden after OK/Cancel). Probably it is executed at other thread (and ignore), haven't tried telling it to do from the UI thread, or try else with a timeout to do it a moment later
    end
  );
  //ShowPopup; //see comment above //doesn't work either (popup shown in the background but closes after OK/Cancel at input prompt)
end;

///

procedure TStoryItemOptions.actionToggleHomeExecute(Sender: TObject);
begin
  StoryItem.SetHome(btnToggleHome.IsPressed);
  //ShowPopup; //show popup again to make the toggle evident
end;

procedure TStoryItemOptions.actionToggleStoryPointExecute(Sender: TObject);
begin
  StoryItem.SetStoryPoint(btnToggleStoryPoint.IsPressed);
  //ShowPopup; //show popup again to make the toggle evident
end;

procedure TStoryItemOptions.actionToggleSnappingExecute(Sender: TObject);
begin
  StoryItem.SetSnapping(btnToggleSnapping.IsPressed);
  //ShowPopup; //show popup again to make the toggle evident
end;

procedure TStoryItemOptions.actionToggleAnchoredExecute(Sender: TObject);
begin
  StoryItem.SetAnchored(btnToggleAnchored.IsPressed);
  //ShowPopup; //show popup again to make the toggle evident
end;

procedure TStoryItemOptions.actionChangeUrlActionExecute(Sender: TObject);
begin
  actChangeUrl;
end;

procedure TStoryItemOptions.actionChangeTagsExecute(Sender: TObject);
begin
  actChangeTags;
end;

procedure TStoryItemOptions.actionLoadExecute(Sender: TObject);
begin
  actLoad;
end;

procedure TStoryItemOptions.actionSaveExecute(Sender: TObject);
begin
  actSave;
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
      PlacementTarget := FStoryItem.View;
      Placement := TPlacement.Center; //show to center of form
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
