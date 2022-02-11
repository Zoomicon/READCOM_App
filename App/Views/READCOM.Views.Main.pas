unit READCOM.Views.Main;

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
  FMX.Layouts,
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
    procedure FormDestroy(Sender: TObject);
    procedure HUDactionLoadExecute(Sender: TObject);
    procedure HUDactionSaveExecute(Sender: TObject);
    procedure HUDactionNewExecute(Sender: TObject);
    procedure HUDactionHomeExecute(Sender: TObject);
    procedure HUDactionPreviousExecute(Sender: TObject);
    procedure HUDactionNextExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    procedure HUDactionDeleteExecute(Sender: TObject);
    procedure HUDactionCopyExecute(Sender: TObject);
    procedure HUDactionPasteExecute(Sender: TObject);
    procedure HUDactionFlipHorizontallyExecute(Sender: TObject);
    procedure HUDactionFlipVerticallyExecute(Sender: TObject);
  private
    function GetStructureView: TStructureView;

  protected
    FStoryMode: TStoryMode;
    FStructureViewFrameInfo: FrameStand.TFrameInfo<TStructureView>;

    {SavedState}
    procedure NewRootStoryItem;
    procedure LoadSavedStateOrNewRootStoryItem;
    function LoadSavedState: Boolean;

    {RootStoryItemStoryView}
    function GetRootStoryItemView: TStoryItem;
    procedure SetRootStoryItemView(const Value: TStoryItem);

    {RootStoryItem}
    function GetRootStoryItem: IStoryItem;
    procedure SetRootStoryItem(const Value: IStoryItem);

    {HomeStoryItem}
    function GetHomeStoryItem: IStoryItem;
    procedure SetHomeStoryItem(const Value: IStoryItem);

    {NAVIGATION}

    {ActiveStoryItem}
    function GetActiveStoryItem: IStoryItem;
    procedure SetActiveStoryItem(const Value: IStoryItem);

    procedure ActivateRootStoryItem;
    procedure ActivateParentStoryItem;
    procedure ActivateHomeStoryItem;
    procedure ActivatePreviousStoryPoint;
    procedure ActivateNextStoryPoint;

    {StoryMode}
    function GetStoryMode: TStoryMode;
    procedure SetStoryMode(const Value: TStoryMode);

    {StructureView}
    procedure StructureViewSelection(Sender: TObject; const Selection: TObject);

    property StructureView: TStructureView read GetStructureView stored false;

    procedure RootStoryItemViewResized(Sender: TObject);

    procedure HUDEditModeChanged(Sender: TObject; const Value: Boolean);
    procedure HUDStructureVisibleChanged(Sender: TObject; const Value: Boolean);
    procedure HUDTargetsVisibleChanged(Sender: TObject; const Value: Boolean);

  public
    procedure ZoomTo(const StoryItem: IStoryItem = nil); //ZoomTo(nil) zooms to all content

  published
    property StoryMode: TStoryMode read GetStoryMode write SetStoryMode stored false;
    property RootStoryItem: IStoryItem read GetRootStoryItem write SetRootStoryItem stored false;
    property RootStoryItemView: TStoryItem read GetRootStoryItemView write SetRootStoryItemView stored false;
    property HomeStoryItem: IStoryItem read GetHomeStoryItem write SetHomeStoryItem stored false;
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
    Zoomicon.Helpers.FMX.Controls.ControlHelpers, //for TControl.FlipHorizontally, TControl.FlipVertically
    READCOM.Views.PanelStoryItem,
    READCOM.Views.TextStoryItem; //TODO: remove

{$R *.fmx}

{$REGION 'Init / Destroy'}

procedure TMainForm.FormCreate(Sender: TObject);

  procedure InitHUD;
  begin
    with HUD do
    begin
      BringToFront;
      BtnMenu.BringToFront;
      layoutButtons.BringToFront;

      EditMode := false;
      StructureVisible := false;
      TargetsVisible := false;

      OnEditModeChanged := HUDEditModeChanged;
      OnStructureVisibleChanged := HUDStructureVisibleChanged;
      OnTargetsVisibleChanged := HUDTargetsVisibleChanged;
    end;
  end;

begin
  InitHUD;
  //ZoomFrame.ScrollBox.AniCalculations.AutoShowing := true; //fade the toolbars when not active //TODO: doesn't work with direct mouse drags near the bottom and right edges (scrollbars do show when scrolling e.g. with mousewheel) since there's other HUD content above them (the navigation and the edit sidebar panes)
  LoadSavedStateOrNewRootStoryItem;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  HUD.MultiViewFrameStand.CloseAll;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  case Key of

   vkEscape:
      if ssShift in Shift then //go to RootStoryItem
        ActivateRootStoryItem
      else
        ActivateParentStoryItem; //go to ParentStoryItem

    vkPrior, vkLeft, vkUp: //go to PreviousStoryPoint
      ActivatePreviousStoryPoint;

    vkNext, vkRight, vkDown: //go to NextStoryPoint
      ActivateNextStoryPoint;

    vkHome: //go to HomeStoryItem
      ActivateHomeStoryItem;

    (*
    vkEnd:
      ActivateEnd; //TODO: for cheaters, go to EndStoryPoint - HOWEVER MAY WANT TO HAVE MULTIPLE ENDSTORYPOINTS, THEIR NEXTSTORYPOINT WOULD ALWAYS BE HOMESTORYITEM [Home may not be a StoryPoint but End always is else it wouldn't be reachable]? (which is the EndStoryItem should we be able to set such?)
    *)

  end;
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
  RootStoryItemView := Value.GetView as TStoryItem; //Important: don't keep any more logic here, keeping all in SetRootStoryItemView
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

      (*
      var currentZoomerSize := ZoomFrame.Zoomer.Size.Size; //don't get size of zoomFrame itself
      var newZoomerSize := TSizeF.Create(Max(currentZoomerSize.cx, newSize.cx), Max(currentZoomerSize.cy, newSize.cy));
      ZoomFrame.Zoomer.Size.Size := newZoomerSize; //use the next line instead
      ZoomFrame.SetZoomerSize(newZoomerSize); //must use this since it sets other zoomer params too
      *)

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

    if not Assigned(HomeStoryItem) then
      HomeStoryItem := RootStoryItem; //set RootStoryItem as the HomeStoryItem if no such assigned

    if not Assigned(ActiveStoryItem) then
      ActiveStoryItem := RootStoryItem; //set RootStoryItem as the ActiveStoryItem if no such is set (e.g. from loaded state). Note this will also try to ZoomTo it

    //ZoomTo(ActiveStoryItem); //zoom to the previously active storyitem after loading //TODO: this doesn't seem to work correctly
    //ActiveStoryItem := ActiveStoryItem; //re-apply ActiveStoryItem to zoom to it and do misc actions //MAYBE BETTER ALTERNATIVE TO JUST ZOOMTO, BUT STILL DOESN'T WORK OK
  end;
end;

procedure TMainForm.RootStoryItemViewResized(Sender: TObject); //TODO: this doesn't seem to get called (needed for AutoSize of RootStoryItemView to work)
begin
  RootStoryItemView := RootStoryItemView; //repeat calculations to adapt ZoomFrame.ScaledLayout size
end;

{$endregion}

{$region 'HomeStoryItem'}

function TMainForm.GetHomeStoryItem: IStoryItem;
begin
  result := TStoryItem.HomeStoryItem;
end;

procedure TMainForm.SetHomeStoryItem(const Value: IStoryItem);
begin
  TStoryItem.HomeStoryItem := Value;
end;

{$endregion}

{$region 'Navigation'}

{$region 'ActiveStoryItem'}

function TMainForm.GetActiveStoryItem: IStoryItem;
begin
  result := TStoryItem.ActiveStoryItem;
end;

procedure TMainForm.SetActiveStoryItem(const Value: IStoryItem); //TODO: should use "ActiveStoryItemChanged" event instead to do this extra logic

  procedure RecursiveClearEditMode(const partialRoot: IStoryItem);
  begin
    if Assigned(partialRoot) then
    begin
      TStoryItem(partialRoot).EditMode := false; //TODO: see StoryMode of IStoryItem instead
      TStoryItem(partialRoot).Enabled := true; //re-enable if previously disabled due to EditMode of its parent

      //Do for item's children too if any
      for var StoryItem in partialRoot.StoryItems do
        RecursiveClearEditMode(StoryItem);
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
    StoryItem.EditMode := HUD.EditMode; //TODO: see StoryMode of IStoryItem instead (or move that to the IStory)
    //StructureView.SelectedObject := Value.View; //TODO: fix, causes some kind of disruption when doing (automatic) drag-drop (maybe schedule to do at end of drag-drop or to cancel if during drag-drop [if we can detect it] or somehow outside of current event handling)
    end
  else
    StructureView.SelectedObject := nil;

  //HUD.actionDelete.Visible := (ActiveStoryItem.View <> RootStoryItem.View); //doesn't seem to work (neither HUD.btnDelete.Visible does), but have implemented delete of RootStoryItem as a call to actionNew.Execute instead
end;

{$endregion}

procedure TMainForm.ActivateRootStoryItem;
begin
  ActiveStoryItem := RootStoryItem;
end;

procedure TMainForm.ActivateParentStoryItem;
begin
  var activeItem := ActiveStoryItem;
  if Assigned(activeItem) then
  begin
    var parentItem := activeItem.ParentStoryItem;
    if Assigned(parentItem) then //don't want to make RootStoryItem (aka the StoryItem without a parent StoryItem) inactive
      ActiveStoryItem := parentItem;
  end;
end;

procedure TMainForm.ActivateHomeStoryItem;
begin
  ActiveStoryItem := HomeStoryItem;
end;

procedure TMainForm.ActivatePreviousStoryPoint;
begin
  if Assigned(ActiveStoryItem) then
     ActiveStoryItem := ActiveStoryItem.PreviousStoryPoint;
end;

procedure TMainForm.ActivateNextStoryPoint;
begin
  if Assigned(ActiveStoryItem) then
    ActiveStoryItem := ActiveStoryItem.NextStoryPoint;
end;

{$endregion}

{$region 'StoryMode'}

function TMainForm.GetStoryMode: TStoryMode;
begin
  result := FStoryMode;
end;

function TMainForm.GetStructureView: TStructureView;
begin
  if not Assigned (FStructureViewFrameInfo) then
  begin
    FStructureViewFrameInfo := HUD.MultiViewFrameStand.GetFrameInfo<TStructureView>;
    with FStructureViewFrameInfo.Frame do
    begin
      ShowOnlyClasses := TClassList.Create([TStoryItem]); //TStructureView's destructor will FreeAndNil that TClassList instance
      ShowNames := false;
      ShowTypes := false;
      DragDropReorder := (StoryMode = EditMode); //allow moving items in the structure view to change parent or add to same parent again to change their Z-order
      DragDropReparent := (StoryMode = EditMode); //allow reparenting //TODO: should do after listening to some event so that the control is scaled/repositioned to show in their parent (note that maybe we should also have parent story items clip their children, esp if their panels)
      DragDropSelectTarget := true; //always select (make active / zoom to) the Target StoryItem after a drag-drop operation in the structure view
      OnSelection := StructureViewSelection;
    end;
  end;

  result := FStructureViewFrameInfo.Frame;
end;

procedure TMainForm.SetStoryMode(const Value: TStoryMode);
begin
  FStoryMode := Value;
  var isEditMode := (Value = EditMode);

  if Assigned(ActiveStoryItem) then
  begin
    var view := ActiveStoryItem.View as TStoryItem;
    if Assigned(view) then
    begin
      view.EditMode := isEditMode; //TODO: should add an EditMode property to the Story?
      ZoomTo(view); //TODO: should we always ZoomTo ActiveStoryItem when switching story mode? (e.g. entering or exiting EditMode? this is useful after having autoloaded saved state)
    end;
  end;

  if Assigned(FStructureViewFrameInfo) then
    with FStructureViewFrameInfo.Frame do
    begin
      DragDropReorder := isEditMode; //allow moving items in the structure view to change parent or add to same parent again to change their Z-order
      DragDropReparent := isEditMode; //allow reparenting //TODO: should do after listening to some event so that the control is scaled/repositioned to show in their parent (note that maybe we should also have parent story items clip their children, esp if their panels)
    end;
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

  if RootStoryItem.Options.ActLoad then //assuming this is blocking action
    begin
    RootStoryItemView := RootStoryItemView; //repeat calculations to adapt ZoomFrame.ScaledLayout size //TODO: when RootStoryItemViewResized starts working this shouldn't be needed here anymore
    ActiveStoryItem := HomeStoryItem; //set the HomeStoryItem (if not any the RootStoryItem will have been set as such by SetRootStoryView) to active (not doing this when loading saved app state)
    end;
end;

procedure TMainForm.HUDactionSaveExecute(Sender: TObject);
begin
  //HUD.actionSaveExecute(Sender);

  RootStoryItem.Options.ActSave;
end;

{$endregion}

{$region 'Edit actions'}

procedure TMainForm.HUDEditModeChanged(Sender: TObject; const Value: Boolean);
begin
  if Value then
    StoryMode := EditMode
  else
    StoryMode := TStoryMode.InteractiveStoryMode; //TODO: should remember previous mode to restore or make EditMode a separate situation
end;

procedure TMainForm.HUDactionAddExecute(Sender: TObject);
begin
  //HUD.actionAddExecute(Sender);

  var OwnerAndParent := ActiveStoryItem.View;

  var StoryItem :=
    //TPanelStoryItem.Create(OwnerAndParent, 'PanelStoryItem');
    //TBitmapImageStoryItem.Create(OwnerAndParent, 'BitmapImageStoryItem');
    //TVectorImageStoryItem.Create(OwnerAndParent, 'VectorImageStoryItem');
    TTextStoryItem.Create(OwnerAndParent, 'TextStoryItem');
      //TODO: testing (should have separate actions for adding such defaults [for prototyping] for various StoryItem classes)

  StoryItem.Size.Size := StoryItem.DefaultSize; //TODO: its constructor should set that
  StoryItem.Parent := OwnerAndParent;

  //Center the new item in its parent...
  var ItemSize := StoryItem.Size.Size;
  StoryItem.Position.Point := PointF(OwnerAndParent.Size.Width/2 - ItemSize.Width/2, OwnerAndParent.Size.Height/2 - ItemSize.Height/2); //not creating TPosition objects to avoid leaking (TPointF is a record)

  StoryItem.BringToFront; //load as front-most
end;

procedure TMainForm.HUDactionDeleteExecute(Sender: TObject);
begin
  if (ActiveStoryItem.View <> RootStoryItem.View) then
    FreeAndNil(ActiveStoryItem.View)
  else
    HUD.actionNew.Execute; //deleting the RootStoryItem via "New" action
end;

procedure TMainForm.HUDactionCopyExecute(Sender: TObject);
begin
  ActiveStoryItem.Copy;
end;

procedure TMainForm.HUDactionPasteExecute(Sender: TObject);
begin
  ActiveStoryItem.Paste;
end;

procedure TMainForm.HUDactionFlipHorizontallyExecute(Sender: TObject);
begin
  ActiveStoryItem.FlippedHorizontally := not ActiveStoryItem.FlippedHorizontally;
end;

procedure TMainForm.HUDactionFlipVerticallyExecute(Sender: TObject);
begin
  ActiveStoryItem.FlippedVertically := not ActiveStoryItem.FlippedVertically;
end;

{$endregion}

{$region 'View actions'}

procedure TMainForm.HUDStructureVisibleChanged(Sender: TObject; const Value: Boolean);
begin
  if Value then
  begin
    HUD.MultiViewFrameStand.CloseAllExcept(TStructureView);

    StructureView.GUIRoot := RootStoryItemView; //in case the RootStoryItem has changed
    FStructureViewFrameInfo.Show; //this will have been assigned by the StructureView getter if it wasn't
  end;
end;

procedure TMainForm.HUDTargetsVisibleChanged(Sender: TObject; const Value: Boolean);
begin
  if HUD.TargetsVisible then
    if Assigned(ActiveStoryItem) then
      ActiveStoryItem.TargetsVisible := Value;
end;

{$endregion}

{$region 'Navigation actions'}

procedure TMainForm.HUDactionHomeExecute(Sender: TObject);
begin
  ActivateHomeStoryItem;
end;

procedure TMainForm.HUDactionPreviousExecute(Sender: TObject);
begin
  ActivatePreviousStoryPoint;
end;

procedure TMainForm.HUDactionNextExecute(Sender: TObject);
begin
  ActivateNextStoryPoint;
end;

procedure TMainForm.StructureViewSelection(Sender: TObject; const Selection: TObject);
begin
  if not (StoryMode = EditMode) then //in non-Edit mode
    HUD.StructureVisible := false; //we want to hide the StructureView before zooming to the item selected

  ActiveStoryItem := TStoryItem(Selection); //Make active (will also zoom to it) - assuming this is a TStoryItem since StructureView was filtering for such class //also accepts "nil" (for no selection)
  //TODO: in EditMode should allow anything to become active, in StoryMode should only allow those items that are StoryPoints (and only show those)
end;

{$endregion}

{$endregion}

{$ENDREGION}

{$region 'SavedState'}

procedure TMainForm.NewRootStoryItem;
begin
  RootStoryItemView := nil; //must do first to free the previous one (to avoid naming clashes)
  var newRootStoryItemView := TPanelStoryItem.Create(Self);
  with newRootStoryItemView do
    begin
    Size.Size := TSizeF.Create(ZoomFrame.Width, ZoomFrame.Height);
    EditMode := HUD.EditMode; //TODO: add EditMode property to IStory or use its originally intended mode one
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
        RootStoryItemView := TheRootStoryItemView; //only set RootStoryItemView (this affects RootStoryItem too)
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
