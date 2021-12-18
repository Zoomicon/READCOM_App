unit READCOM.Views.Main;

//TODO: fix issue where if Popup with Options is shown at least once, then save state puts options data in the stored file, failing to load them (since that class isn't declared [doesn't need to after all] with streaming system). MAYBE JUST DESTROY THE POPUP WHEN CLOSED and make sure all are closed before saving

interface

uses
  FrameStand, //for TFrameInfo
  Zoomicon.Generics.Collections, //for TObjectListEx
  Zoomicon.Introspection.FMX.StructureView, //for TStructureView
  Zoomicon.Zooming.FMX.ZoomFrame, //for TZoomFrame
  READCOM.App.Models, //for IStory, ISToryItem
  READCOM.Views.StoryItem, //for TStoryItem
  READCOM.Views.Menu.HUD,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Objects, FMX.Controls, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Types, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts, READCOM.Views.PanelStoryItem,
  READCOM.Views.AudioStoryItem,
  SubjectStand,
  READCOM.App.Globals;

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
    procedure HUDactionTargetsExecute(Sender: TObject);
    procedure HUDactionLoadExecute(Sender: TObject);
    procedure HUDactionSaveExecute(Sender: TObject);
    procedure HUDactionNewExecute(Sender: TObject);
    procedure HUDactionHomeExecute(Sender: TObject);
    procedure HUDactionPreviousExecute(Sender: TObject);
    procedure HUDactionNextExecute(Sender: TObject);

  protected
    FStoryMode: TStoryMode;
    FStructureViewFrameInfo: FrameStand.TFrameInfo<TStructureView>;

    { SavedState}
    procedure NewRootStoryItem;
    procedure LoadSavedStateOrNewRootStoryItem;
    function LoadSavedState: Boolean;

    { RootStoryItemStoryView }
    function GetRootStoryItemView: TStoryItem;
    procedure SetRootStoryItemView(const Value: TStoryItem);

    { RootStoryItem }
    function GetRootStoryItem: IStoryItem;
    procedure SetRootStoryItem(const Value: IStoryItem);

    { ActiveStoryItem }
    function GetActiveStoryItem: IStoryItem;
    procedure SetActiveStoryItem(const Value: IStoryItem);

    { Navigation }
    procedure ActivatePrevious;
    procedure ActivateNext;

    { StoryMode }
    function GetStoryMode: TStoryMode;
    procedure SetStoryMode(const Value: TStoryMode);

    { StructureView }
    procedure StructureViewSelection(Sender: TObject; Selection: TObject);

    procedure RootStoryItemViewResized(Sender: TObject);

  public
    procedure ZoomTo(const StoryItem: IStoryItem = nil); //ZoomTo(nil) zooms to all content

  published
    property RootStoryItem: IStoryItem read GetRootStoryItem write SetRootStoryItem stored false;
    property RootStoryItemView: TStoryItem read GetRootStoryItemView write SetRootStoryItemView stored false;
    property ActiveStoryItem: IStoryItem read GetActiveStoryItem write SetActiveStoryItem stored false;
  end;

var
  MainForm: TMainForm;

implementation
  uses
    {$IFDEF DEBUG}CodeSiteLogging,{$ENDIF}
    System.Contnrs, //for TClassList
    System.Math, //for Max
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
  InitHUD;
  LoadSavedStateOrNewRootStoryItem;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  HUD.MultiViewFrameStand.CloseAll;
end;

{$ENDREGION}

{$region 'IStory'}

{$region 'RootStoryItem'}

function TMainForm.GetRootStoryItem: IStoryItem;
begin
  result := RootStoryItemView as IStoryItem;
end;

procedure TMainForm.SetRootStoryItem(const Value: IStoryItem);
begin
  RootStoryItemView := Value.GetView as TStoryItem;
  Value.Active := true; //set as the Active StoryItem
end;

{$endregion}

{$region 'RootStoryItemView'}

function TMainForm.GetRootStoryItemView: TStoryItem;
begin
  result := TObjectListEx<TControl>.GetFirstClass<TStoryItem>(ZoomFrame.ScaledLayout.Controls)
end;

procedure TMainForm.SetRootStoryItemView(const Value: TStoryItem);
begin
  //Remove old story
  var TheRootStoryItemView := RootStoryItemView;
  if Assigned(TheRootStoryItemView) and (Value <> TheRootStoryItemView) then //must check that the same one isn't set again to avoid destroying it
    begin
    //TheRootStoryItemView.Parent := nil; //shouldn't be needed
    ActiveStoryItem := nil; //must clear reference to old ActiveStoryItem since all StoryItems will be destroyed
    FreeAndNil(TheRootStoryItemView); //destroy the old RootStoryItem //FREE THE CONTROL, DON'T FREE JUST THE INTERFACE
    end;

  //Add new story, if any
  if Assigned(Value) then //allowing to set the same one to update any zooming/positioning calculations if needed
  begin
    with Value do
    begin
      AutoSize := true; //TODO: the Root StoryItem should be expandable
      OnResized := RootStoryItemViewResized; //listen for resizing to adapt ZoomFrame.ScaledLayout's size //TODO: this doesn't seem to get called

      var newSize := Size.Size;

      {
      var currentZoomerSize := ZoomFrame.Zoomer.Size.Size; //don't get size of zoomFrame itself
      var newZoomerSize := TSizeF.Create(Max(currentZoomerSize.cx, newSize.cx), Max(currentZoomerSize.cy, newSize.cy));
      ZoomFrame.Zoomer.Size.Size := newZoomerSize; //use the next line instead
      ZoomFrame.SetZoomerSize(newZoomerSize); //must use this since it sets other zoomer params too
      }

      BeginUpdate; //TODO: move this working code to TZoomFrame
      Parent := nil; //remove from parent first (needed if we're calling this code to adjust for RootStoryItem resize action)
      ZoomFrame.ScaledLayout.Align := TAlignLayout.None;
      ZoomFrame.ScaledLayout.Size.Size := newSize;
      ZoomFrame.ScaledLayout.OriginalWidth := newSize.cx; //needed to reset the ScalingFactor
      ZoomFrame.ScaledLayout.OriginalHeight := newSize.cy; //needed to reset the ScalingFactor
      ZoomFrame.ScaledLayout.Align := TAlignLayout.Fit;
      EndUpdate;

      Parent := ZoomFrame.ScaledLayout; //don't use ZoomFrame as direct parent
    end;

    ActiveStoryItem := Value; //this will ZoomTo the RootStoryItemView //don't put in the with statement, will call it on the storyitem and won't ZoomTo //TODO: listen to TStoryItem class event for active story item changes instead?
  end;
end;

procedure TMainForm.RootStoryItemViewResized(Sender: TObject); //TODO: this doesn't seem to get called (needed for AutoSize of RootStoryItemView to work)
begin
  RootStoryItemView := RootStoryItemView; //repeat calculations to adapt ZoomFrame.ScaledLayout size
end;

{$endregion}

{$region 'ActiveStoryItem'}

function TMainForm.GetActiveStoryItem: IStoryItem;
begin
  result := TStoryItem.ActiveStoryItem;
end;

procedure TMainForm.SetActiveStoryItem(const Value: IStoryItem);

  procedure RecursiveClearEditMode(const partialRoot: IStoryItem);
  begin
    if Assigned(partialRoot) then
    begin
      TStoryItem(partialRoot).EditMode := false; //TODO: see StoryMode of IStoryItem instead
      TStoryItem(partialRoot).Enabled := true; //re-enable if previously disabled due to EditMode of its parent
      for var StoryItem in partialRoot.StoryItems do
        RecursiveClearEditMode(StoryItem); //do for item's children too if any
    end;
  end;

begin
  TStoryItem.ActiveStoryItem := Value;
  ZoomTo(Value);

  RecursiveClearEditMode(RootStoryItemView); //Clear EditMode from all items recursively

  //Set any current editmode to the newly active item
  if Assigned(Value) then
    begin
    var StoryItem := TStoryItem(Value.View);
    StoryItem.EditMode := HUD.actionEdit.Checked; //TODO: see StoryMode of IStoryItem instead (or move that to the IStory)
    end;
end;

{$endregion}

{$region 'Navigation'}

procedure TMainForm.ActivateNext;
begin
  //TODO// ActiveStoryItem.GotoNext; ZoomTo(ActiveStoryItem); //zoom to the new one
end;

procedure TMainForm.ActivatePrevious;
begin
  //TODO// ActiveStoryItem.GotoPrevious; ZoomTo(ActiveStoryItem); //zoom to the new one
end;

{$endregion}

{$region 'StoryMode'}

function TMainForm.GetStoryMode: TStoryMode;
begin
  result := FStoryMode;
end;

procedure TMainForm.SetStoryMode(const Value: TStoryMode);
begin
  FStoryMode := Value;
  //TODO
end;

{$endregion}

{$region 'ZoomTo'}

procedure TMainForm.ZoomTo(const StoryItem: IStoryItem);
begin
  if Assigned(StoryItem) then
    ZoomFrame.ZoomTo(StoryItem.View)
  else
    ZoomFrame.ZoomTo; //Zoom to all content
end;

{$endregion}

{$endregion}

{$REGION 'Events'}

{$region 'Actions'}

{$region 'File actions'}

procedure TMainForm.HUDactionNewExecute(Sender: TObject);
begin
  //if ConfirmClose then //TODO (should show some image asking?)
    NewRootStoryItem;
end;

procedure TMainForm.HUDactionLoadExecute(Sender: TObject);
begin
  //HUD.actionLoadExecute(Sender);

  RootStoryItem.Options.ActLoad; //assuming this is blocking action

  RootStoryItemView := RootStoryItemView; //repeat calculations to adapt ZoomFrame.ScaledLayout size //TODO: when RootStoryItemViewResized starts working this shouldn't be needed here anymore
end;

procedure TMainForm.HUDactionSaveExecute(Sender: TObject);
begin
  //HUD.actionSaveExecute(Sender);

  RootStoryItem.Options.ActSave;
end;

{$endregion}

{$region 'View actions'}

procedure TMainForm.HUDactionStructureExecute(Sender: TObject);
begin
  HUD.actionStructureExecute(Sender);

  HUD.MultiViewFrameStand.CloseAllExcept(TStructureView);

  if not Assigned (FStructureViewFrameInfo) then
  begin
    FStructureViewFrameInfo := HUD.MultiViewFrameStand.GetFrameInfo<TStructureView>;
    with FStructureViewFrameInfo.Frame do
    begin
      ShowOnlyClasses := TClassList.Create([TStoryItem]); //TStructureView's destructor will FreeAndNil that TClassList instance
      ShowNames := false;
      ShowTypes := false;
      AllowDragDrop := true; //allow moving items in the structure view to change parent or add to same parent again to change their Z-order
      OnSelection := StructureViewSelection;
    end;
  end;

  FStructureViewFrameInfo.Frame.GUIRoot := RootStoryItemView; //in case the RootStoryItem has changed

  FStructureViewFrameInfo.Show;
end;

procedure TMainForm.HUDactionTargetsExecute(Sender: TObject);
begin
  //HUD.actionTargetsExecute(Sender);

  if Assigned(ActiveStoryItem) then
    ActiveStoryItem.TargetsVisible := HUD.actionTargets.Checked;
end;

{$endregion}

{$region 'Edit actions'}

procedure TMainForm.HUDactionEditExecute(Sender: TObject);
begin
  HUD.actionEditExecute(Sender);

  if Assigned(ActiveStoryItem) then
  begin
    var view := ActiveStoryItem.View as TStoryItem;
    if Assigned(view) then
      view.EditMode := HUD.actionEdit.Checked;
  end;
end;

procedure TMainForm.HUDactionAddExecute(Sender: TObject);
begin
  //HUD.actionAddExecute(Sender);

  //TODO: move to StoryItem (with parameter the class to create and/or file to load [see PanelStoryItem code where this came from])
  var StoryItem := TPanelStoryItem.Create(Self);

  StoryItem.Name := 'PanelStoryItem_' + IntToStr(Random(maxint)); //TODO: use a GUID

  //Center the new item...
  var ItemSize := StoryItem.Size;
  StoryItem.Position.Point := PointF(ActiveStoryItem.View.Size.Width/2 - ItemSize.Width/2, ActiveStoryItem.View.Size.Height/2 - ItemSize.Height/2); //not creating TPosition objects to avoid leaking (TPointF is a record)

  StoryItem.Parent := ActiveStoryItem.View; //TODO: should add to current StoryItem
  StoryItem.BringToFront; //load as front-most
end;

{$endregion}

{$region 'Navigation actions'}

procedure TMainForm.HUDactionHomeExecute(Sender: TObject);
begin
  ActiveStoryItem := RootStoryItem;
end;

procedure TMainForm.HUDactionNextExecute(Sender: TObject);
begin
  //TODO
end;

procedure TMainForm.HUDactionPreviousExecute(Sender: TObject);
begin
  //TODO
end;

{$endregion}

{$endregion}

procedure TMainForm.StructureViewSelection(Sender: TObject; Selection: TObject);
begin
  HUD.MultiView.HideMaster; //first close the structure drawer, then do ZoomTo so that any zooming animation will be visible

  //ZoomFrame.ZoomTo(TControl(Selection)); //just zoom

  ActiveStoryItem := TStoryItem(Selection); //Make active (will also zoom to it) - assuming this is a TStoryItem since StructureView was filtering for such class
  //TODO: in EditMode should allow anything to become active, in StoryMode should only allow those items that are Activateable / have some ActivationOrder (maybe rename to FlowOrder and/or add different prescribed flows)
end;

{$ENDREGION}

{$region 'SavedState'}

procedure TMainForm.NewRootStoryItem;
begin
  RootStoryItemView := nil; //must do first to free the previous one (to avoid naming clashes)
  var newRootStoryItemView := TPanelStoryItem.Create(Self);
  with newRootStoryItemView do
    begin
    Size.Size := TSizeF.Create(ZoomFrame.Width, ZoomFrame.Height);
    EditMode := HUD.actionEdit.Checked; //TODO: add EditMode property to IStory or use its originally intended mode one
    end;
  RootStoryItemView := newRootStoryItemView;
end;

procedure TMainForm.LoadSavedStateOrNewRootStoryItem;
begin
  if (not LoadSavedState) then //Note: if it keeps on failing at load comment out this line for one run //TODO: shouldn't need to do that
    NewRootStoryItem;
end;

function TMainForm.LoadSavedState: Boolean;
begin
  result := false;
  With SaveState do
  begin
    //StoragePath := ... //TODO: default is transient, change to make permanent
    if Stream.Size > 0 then
    begin
      var TheRootStoryItemView := TPanelStoryItem.Create(Self);
      try
        TheRootStoryItemView.Load(Stream); //default file format is EXT_READCOM
        {$IFDEF DEBUG}
        try
          CodeSite.Send(TheRootStoryItemView.SaveToString);
        finally
          //NOP
        end;
        {$ENDIF}
        RootStoryItemView := TheRootStoryItemView; //only set StoryView
        result := true;
      except
        on E: Exception do
          begin
          Stream.Clear; //clear stream if causes loading error //TODO: instead of Clear which doesn't seem to work, try saving instead a new instance of TPanelStoryItem
          {$IFDEF DEBUG}CodeSite.SendException(E);{$ENDIF}
          ShowException(E, @TMainForm.FormCreate);
          FreeAndNil(TheRootStoryItemView); //Free partially loaded - corrupted StoryItem
          end;
      end;
    end;
  end;
end;

procedure TMainForm.FormSaveState(Sender: TObject);
begin
  {$IFDEF DEBUG}CodeSite.EnterMethod('SaveState');{$ENDIF}
  //StoragePath := ... //TODO: default is transient, change to make permanent
  SaveState.Stream.Clear;

  var TheRootStoryItemView := RootStoryItemView;
  if Assigned(TheRootStoryItemView) then
    with SaveState do
      try
        TheRootStoryItemView.Save(Stream); //default file format is EXT_READCOM
        {$IFDEF DEBUG}
        try
          CodeSite.Send(TheRootStoryItemView.SaveToString);
        finally
          //NOP
        end;
        {$ENDIF}
      except
        On E: Exception do
          begin
          Stream.Clear; //clear stream in case it got corrupted
          {$IFDEF DEBUG}CodeSite.SendException(E);{$ENDIF}
          ShowException(E, @TMainForm.FormCreate);
          end;
    end;
  {$IFDEF DEBUG}CodeSite.ExitMethod('SaveState');{$ENDIF}
end;

{$endregion}

end.
