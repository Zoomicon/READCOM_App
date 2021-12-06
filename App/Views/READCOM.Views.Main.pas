unit READCOM.Views.Main;

//TODO: fix issue where if Popup with Options is shown at least once, then save state puts options data in the stored file, failing to load them (since that class isn't declared [doesn't need to after all] with streaming system). MAYBE JUST DESTROY THE POPUP WHEN CLOSED and make sure all are closed before saving

interface

uses
  Zoomicon.Generics.Collections, //for TObjectListEx
  Zoomicon.Zooming.FMX.ZoomFrame, //for TZoomFrame
  READCOM.App.Models, //for IStory, ISToryItem
  READCOM.Views.StoryItem, //for TStoryItem
  READCOM.Views.Menu.HUD,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Objects, FMX.Controls, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Types, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, READCOM.Views.PanelStoryItem,
  READCOM.Views.AudioStoryItem,
  SubjectStand;

type
  TMainForm = class(TForm, IStory)
    HUD: TStoryHUD;
    ZoomFrame: TZoomFrame;
    procedure FormCreate(Sender: TObject);
    procedure FormSaveState(Sender: TObject);
    procedure HUDactionAddExecute(Sender: TObject);
    procedure HUDactionEditExecute(Sender: TObject);
    procedure HUDactionStructureExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

  protected
    procedure LoadSavedStateOrNewStory;
    function LoadSavedState: Boolean;
    {Story}
    function GetStory: IStoryItem;
    procedure SetStory(const Value: IStoryItem);
    {StoryView}
    function GetStoryView: TStoryItem;
    procedure SetStoryView(const Value: TStoryItem);
    procedure StructureViewSelection(Sender: TComponent; Selection: TObject);

  public
    //--- Events
    { StoryMode }
    function GetStoryMode: TStoryMode;
    procedure SetStoryMode(const Value: TStoryMode);
    { Navigation }
    procedure GotoPreviousPanel;
    procedure GotoNextPanel;
    { CurrentPanel }
    function GetCurrentPanel: IPanelStoryItem;
    procedure SetCurrentPanel(const Value: IPanelStoryItem);
    property CurrentPanel: IPanelStoryItem read GetCurrentPanel write SetCurrentPanel;

    procedure ZoomTo(const StoryItem: IStoryItem = nil); //ZoomTo(nil) zooms to all content

  published
    property Story: IStoryItem read GetStory write SetStory stored false;
    property StoryView: TStoryItem read GetStoryView write SetStoryView stored false;
  end;

var
  MainForm: TMainForm;

implementation
  uses
    System.Contnrs, //for TClassList
    CodeSiteLogging,
    Zoomicon.Introspection.FMX.StructureView, //for TStructureView
    Zoomicon.Helpers.RTL.ClassListHelpers, //for TClassList.Create(TClassArray)
    READCOM.Views.VectorImageStoryItem; //TODO: remove

{$R *.fmx}

{$REGION 'Init / Destroy'}

procedure TMainForm.FormCreate(Sender: TObject);

  procedure InitHUD;
  begin
    HUD.BringToFront;
    HUD.BtnMenu.BringToFront;
    HUD.layoutButtons.BringToFront;
  end;

begin
  CodeSite.EnterMethod('FormCreate');

  InitHUD;
  LoadSavedStateOrNewStory;

  CodeSite.ExitMethod('FormCreate');
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  HUD.DrawerFrameStand.CloseAll;
end;

{$ENDREGION}

{$REGION 'Properties'}

{$region 'Story'}

function TMainForm.GetStory: IStoryItem;
begin
  result := StoryView as IStoryItem;
end;

procedure TMainForm.SetStory(const Value: IStoryItem);
begin
  StoryView := Value.GetView as TStoryItem;
  Value.Active := true; //set as the Active StoryItem
end;

{$endregion}

{$region 'StoryView'}

function TMainForm.GetStoryView: TStoryItem;
begin
  result := TObjectListEx<TControl>.GetFirstClass<TStoryItem>(ZoomFrame.ScaledLayout.Controls)
end;

procedure TMainForm.GotoNextPanel;
begin
  //TODO// ActiveStoryItem.GotoNext; ZoomTo(ActiveStoryItem); //zoom to the new one
end;

procedure TMainForm.GotoPreviousPanel;
begin
  //TODO// ActiveStoryItem.GotoPrevious; ZoomTo(ActiveStoryItem); //zoom to the new one
end;

procedure TMainForm.HUDactionAddExecute(Sender: TObject);
begin
  //HUD.actionAddExecute(Sender);

  //TODO: move to StoryItem (with parameter the class to create and/or file to load [see PanelStoryItem code where this came from])
  var StoryItem := TVectorImageStoryItem.Create(Self);

  StoryItem.Name := 'VectorImageStoryItem_' + IntToStr(Random(maxint)); //TODO: use a GUID

  //Center the new item...
  var ItemSize := StoryItem.Size;
  StoryItem.Position.Point := PointF(StoryView.Size.Width/2 - ItemSize.Width/2, StoryView.Size.Height/2 - ItemSize.Height/2); //not creating TPosition objects to avoid leaking (TPointF is a record)

  StoryItem.Parent := StoryView; //TODO: should add to current StoryItem
  StoryItem.BringToFront; //load as front-most
end;

procedure TMainForm.SetStoryView(const Value: TStoryItem);
begin
  //Remove old story
  var TheStory := StoryView;
  if Assigned(TheStory) then
    begin
    //TheStory.Parent := nil; //shouldn't be needed
    FreeAndNil(TheStory); //destroy the old Story
    end;

  //Add new story
  with Value do
  begin
    Align := TAlignLayout.Fit;
    //AutoSize := true; //the Root StoryItem should be expandable //TODO: fix in manipulator to work
    Parent := ZoomFrame.ScaledLayout; //don't use ZoomFrame as direct parent
  end;
end;

{$endregion}

{$region 'IStory'}

{ CurrentPanel }
function TMainForm.GetCurrentPanel: IPanelStoryItem;
begin
  //TODO
end;

procedure TMainForm.SetCurrentPanel(const Value: IPanelStoryItem);
begin
  //TODO
end;

{ StoryMode }

function TMainForm.GetStoryMode: TStoryMode;
begin
  result := Story.StoryMode;
end;

procedure TMainForm.SetStoryMode(const Value: TStoryMode);
begin
  Story.StoryMode := Value;
end;

{ ZoomTo }

procedure TMainForm.ZoomTo(const StoryItem: IStoryItem);
begin
  if Assigned(StoryItem) then
    ZoomFrame.ZoomTo(StoryItem.View)
  else
    ZoomFrame.ZoomTo; //Zoom to all content
end;

{$endregion}

{$ENDREGION}

{$REGION 'Events'}

{$region 'Actions'}

procedure TMainForm.HUDactionEditExecute(Sender: TObject);
begin
  HUD.actionEditExecute(Sender);

  var view := StoryView;
  if Assigned(view) then
    view.EditMode := HUD.actionEdit.Checked;
end;

procedure TMainForm.StructureViewSelection(Sender: TComponent; Selection: TObject);
begin
  ZoomFrame.ZoomTo(TControl(Selection));
end;

procedure TMainForm.HUDactionStructureExecute(Sender: TObject);
begin
  HUD.actionStructureExecute(Sender);

  HUD.DrawerFrameStand.CloseAllExcept(TStructureView);
  var frameInfo := HUD.DrawerFrameStand.GetFrameInfo<TStructureView>;
  with frameInfo.Frame do
  begin
    ShowOnlyClasses := TClassList.Create([TStoryItem]);
    ShowNames := false;
    ShowTypes := false;
    GUIRoot := StoryView;
    OnSelection := StructureViewSelection;
  end;
  frameInfo.Show;
end;

{$endregion}

{$endregion}

{$region 'SavedState'}

procedure TMainForm.LoadSavedStateOrNewStory;
begin
  //if (not LoadSavedState) then
    begin
    var TheStory := TPanelStoryItem.Create(Self);
    TheStory.Size.Size := TSizeF.Create(ZoomFrame.Width, ZoomFrame.Height);
    StoryView := TheStory;
    end;
end;

function TMainForm.LoadSavedState: Boolean;
begin
  result := false;
  With SaveState do
  begin
    //StoragePath := ... //TODO: default is transient, change to make permanent
    if Stream.Size > 0 then
    begin
      var TheStory := TPanelStoryItem.Create(Self);
      try
        TheStory.Load(Stream); //default file format is EXT_READCOM
        {}CodeSite.Send(TheStory.SaveToString);
        StoryView := TheStory; //only set StoryView
        result := true;
      except
        on E: Exception do
          begin
          Stream.Clear; //clear stream if causes loading error //TODO: instead of Clear which doesn't seem to work, try saving instead a new instance of TPanelStoryItem
          CodeSite.SendException(E);
          ShowException(E, @TMainForm.FormCreate);
          FreeAndNil(TheStory); //Free partially loaded - corrupted StoryItem
          end;
      end;
    end;
  end;
end;

procedure TMainForm.FormSaveState(Sender: TObject);
begin
  CodeSite.EnterMethod('SaveState');
  //StoragePath := ... //TODO: default is transient, change to make permanent
  SaveState.Stream.Clear;

  var TheStory := StoryView;
  if Assigned(TheStory) then
    with SaveState do
      try
        TheStory.Save(Stream); //default file format is EXT_READCOM
        {}CodeSite.Send(TheStory.SaveToString);
      except
        On E: Exception do
          begin
          Stream.Clear; //clear stream in case it got corrupted
          CodeSite.SendException(E);
          ShowException(E, @TMainForm.FormCreate);
          end;
    end;
  CodeSite.ExitMethod('SaveState');
end;

{$endregion}

end.
