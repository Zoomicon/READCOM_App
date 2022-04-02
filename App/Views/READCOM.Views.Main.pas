unit READCOM.Views.Main;

interface

uses
  FrameStand, //for TFrameInfo
  Zoomicon.Generics.Collections, //for TObjectListEx
  Zoomicon.Introspection.FMX.StructureView, //for TStructureView
  Zoomicon.Zooming.FMX.ZoomFrame, //for TZoomFrame
  READCOM.App.Globals, //for SVGIconImageList
  READCOM.App.Models, //for IStory, ISToryItem
  READCOM.Views.StoryItem, //for TStoryItem
  READCOM.Views.Menu.HUD,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Objects, FMX.Controls, FMX.Controls.Presentation, FMX.StdCtrls,
  FMX.Types, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Layouts,
  SubjectStand,
  System.Actions, FMX.ActnList;

type

  TMainForm = class(TForm, IStory)
    HUD: TStoryHUD;
    ZoomFrame: TZoomFrame;
    StoryTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure FormSaveState(Sender: TObject);

    {Keyboard}
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);

    procedure HUDactionLoadExecute(Sender: TObject);
    procedure HUDactionSaveExecute(Sender: TObject);
    procedure HUDactionNewExecute(Sender: TObject);
    procedure HUDactionHomeExecute(Sender: TObject);
    procedure HUDactionPreviousExecute(Sender: TObject);
    procedure HUDactionNextExecute(Sender: TObject);
    procedure HUDactionAddBitmapImageStoryItemExecute(Sender: TObject);
    procedure HUDactionAddTextStoryItemExecute(Sender: TObject);
    procedure HUDactionDeleteExecute(Sender: TObject);
    procedure HUDactionCopyExecute(Sender: TObject);
    procedure HUDactionPasteExecute(Sender: TObject);
    procedure HUDactionFlipHorizontallyExecute(Sender: TObject);
    procedure HUDactionFlipVerticallyExecute(Sender: TObject);

    procedure FormResize(Sender: TObject);
    procedure StoryTimerTimer(Sender: TObject);
    procedure HUDactionCutExecute(Sender: TObject);
    procedure HUDactionAddExecute(Sender: TObject);
    procedure HUDcomboForeColorChange(Sender: TObject);
    procedure HUDcomboBackColorChange(Sender: TObject);

  protected
    FTimerStarted: Boolean;
    FStoryMode: TStoryMode;
    FStructureViewFrameInfo: FrameStand.TFrameInfo<TStructureView>;

    {SavedState}
    procedure NewRootStoryItem;
    procedure LoadSavedStateOrDefaultDocumentOrNewRootStoryItem;
    function LoadSavedState: Boolean;
    function LoadDefaultDocument: Boolean;
    procedure SaveCurrentState;

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
    function GetStructureView: TStructureView;
    procedure StructureViewShowFilter(Sender: TObject; const TheObject: TObject; var ShowObject: Boolean);
    procedure StructureViewSelection(Sender: TObject; const Selection: TObject);
    procedure UpdateStructureView;

    procedure RootStoryItemViewResized(Sender: TObject);

    procedure HUDEditModeChanged(Sender: TObject; const Value: Boolean);
    procedure HUDStructureVisibleChanged(Sender: TObject; const Value: Boolean);
    procedure HUDTargetsVisibleChanged(Sender: TObject; const Value: Boolean);
    procedure HUDUseStoryTimerChanged(Sender: TObject; const Value: Boolean);

    procedure HandleActiveStoryItemChanged(Sender: TObject);

    procedure AddChildStoryItem(const TheStoryItemClass: TStoryItemClass; const TheName: String);

  public
    property StructureView: TStructureView read GetStructureView stored false;
    procedure ZoomTo(const StoryItem: IStoryItem = nil); //ZoomTo(nil) zooms to all content
    procedure ZoomToActiveStoryPointOrHome;
    procedure DeleteActiveStoryItem;

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
    {$IFDEF DEBUG}
    {$IFDEF WINDOWS}CodeSiteLogging,{$ENDIF}
    ObjectDebuggerFMXForm,
    {$ENDIF}
    System.Contnrs, //for TClassList
    System.Math, //for Max
    Zoomicon.Helpers.RTL.ClassListHelpers, //for TClassList.Create(TClassArray)
    Zoomicon.Helpers.FMX.Controls.ControlHelpers, //for TControl.FlipHorizontally, TControl.FlipVertically
    READCOM.Views.PanelStoryItem, //TODO: are the following needed to be used here (for deserialization)?
    READCOM.Views.AudioStoryItem,
    READCOM.Views.ImageStoryItem,
    READCOM.Views.BitmapImageStoryItem,
    READCOM.Views.VectorImageStoryItem,
    READCOM.Views.TextStoryItem;

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
      OnUseStoryTimerChanged := HUDUseStoryTimerChanged;

      TStoryItem.OnActiveStoryItemChanged := HandleActiveStoryItemChanged;
    end;
  end;

begin
  Caption := STR_APP_TITLE;

  FTimerStarted := false;
  InitHUD;
  //ZoomFrame.ScrollBox.AniCalculations.AutoShowing := true; //fade the toolbars when not active //TODO: doesn't work with direct mouse drags near the bottom and right edges (scrollbars do show when scrolling e.g. with mousewheel) since there's other HUD content above them (the navigation and the edit sidebar panes)
  LoadSavedStateOrDefaultDocumentOrNewRootStoryItem;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  HUD.MultiViewFrameStand.CloseAll;
end;

procedure TMainForm.FormResize(Sender: TObject);
begin
  ZoomToActiveStoryPointOrHome; //keep the Active StoryPoint or Home in view
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
  end;

  UpdateStructureView;
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

procedure TMainForm.SetActiveStoryItem(const Value: IStoryItem);
begin
  TStoryItem.ActiveStoryItem := nil; //this will make sure the assignment to Value will then trigger "ActiveStoryItemChanged" class event
  TStoryItem.ActiveStoryItem := Value;
end;

procedure TMainForm.HandleActiveStoryItemChanged(Sender: TObject);

  procedure RecursiveClearEditMode(const partialRoot: IStoryItem);
  begin
    if Assigned(partialRoot) then
    begin
      var StoryItem := TStoryItem(partialRoot.View);
      StoryItem.EditMode := false; //TODO: see StoryMode of IStoryItem instead
      StoryItem.Enabled := true; //restore any items that had been left disabled accidentally
      //StoryItem.Enabled := (StoryMode <> EditMode) and (StoryItem <> RootStoryItemView); //TODO: test

      //Do for item's children too if any
      for var ChildStoryItem in partialRoot.StoryItems do
        RecursiveClearEditMode(ChildStoryItem);
    end;
  end;

begin
  //TODO: in non-EditMode should activate closest parentStoryPoint instead

  RecursiveClearEditMode(RootStoryItemView); //Clear EditMode from all items recursively

  var Value := ActiveStoryItem;

  //Set any current editmode to the newly active item
  if Assigned(Value) then
  begin
    var StoryItem := TStoryItem(Value.View);
    with StoryItem do
    begin
      EditMode := HUD.EditMode; //TODO: see StoryMode of IStoryItem instead (or move that to the IStory)
      //AreaSelector := RootStoryItemView.AreaSelector; //re-use RootStoryItem's AreaSelector (so that we don't get drawing artifacts when resizing area selector and is always on top of everything when extending outside of ActiveStoryItem's bounds - since that can have children that are not inside its area, like a speech bubble for a character) //TODO: not working correctly
    end;

    var isTextStoryItem := (StoryItem is TTextStoryItem);
    HUD.comboForeColor.Enabled := isTextStoryItem; //TODO: if we use ITextStoryItem interface instead at that combo's chane event, do similar check here //TODO: also check for other types that may support forecolor (say SVG images could allow to change dominant color) //TODO: Visible doesn't work (keeps combo hidden), using Enabled instead for now
    if isTextStoryItem then //TODO: adapt if other items support fore color to check which one it is
      HUD.comboForeColor.Color := TTextStoryItem(StoryItem).TextColor;

    StructureView.SelectedObject := StoryItem; //Change StructureView selection
  end
  else
    StructureView.SelectedObject := nil;

  //HUD.actionDelete.Visible := (ActiveStoryItem.View <> RootStoryItem.View); //doesn't seem to work (neither HUD.btnDelete.Visible does), but have implemented delete of RootStoryItem as a call to actionNew.Execute instead

  ZoomToActiveStoryPointOrHome;
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
    activeItem.ActivateParentStoryItem;
end;

procedure TMainForm.ActivateHomeStoryItem;
begin
  ActiveStoryItem := HomeStoryItem;
end;

procedure TMainForm.ActivatePreviousStoryPoint;
begin
  var activeItem := ActiveStoryItem;
  if Assigned(activeItem) then
     ActiveStoryItem := activeItem.PreviousStoryPoint;
end;

procedure TMainForm.ActivateNextStoryPoint;
begin
  var activeItem := ActiveStoryItem;
  if Assigned(activeItem) then
    ActiveStoryItem := activeItem.NextStoryPoint;
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

      if (StoryMode <> EditMode) then //if non-Edit mode
        FilterMode := tfFlatten
      else
        FilterMode := tfPrune;

      DragDropReorder := (StoryMode = EditMode); //allow moving items in the structure view to change parent or add to same parent again to change their Z-order
      DragDropReparent := (StoryMode = EditMode); //allow reparenting //TODO: should do after listening to some event so that the control is scaled/repositioned to show in their parent (note that maybe we should also have parent story items clip their children, esp if their panels)
      DragDropSelectTarget := true; //always select (make active / zoom to) the Target StoryItem after a drag-drop operation in the structure view

      OnSelection := StructureViewSelection;
      OnShowFilter := StructureViewShowFilter;
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
      ZoomToActiveStoryPointOrHome; //keep the Active StoryPoint or Home in view //TODO: should we always ZoomTo when switching story mode? (e.g. entering or exiting EditMode? this is useful after having autoloaded saved state)
    end;
  end;

  if Assigned(FStructureViewFrameInfo) then
    with FStructureViewFrameInfo.Frame do
    begin
      DragDropReorder := isEditMode; //allow moving items in the structure view to change parent or add to same parent again to change their Z-order
      DragDropReparent := isEditMode; //allow reparenting //TODO: should do after listening to some event so that the control is scaled/repositioned to show in their parent (note that maybe we should also have parent story items clip their children, esp if their panels)
      UpdateStructureView; //must refresh StructureView contents since we change the FilterMode based on isEditMode
    end;

  if (not isEditMode) and Assigned(ActiveStoryItem) and (not ActiveStoryItem.StoryPoint) then
    ActiveStoryItem := ActiveStoryItem.PreviousStoryPoint;
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

procedure TMainForm.ZoomToActiveStoryPointOrHome;
begin
  var Value := ActiveStoryItem;
  if not (Value.StoryPoint or Value.Home) then //not zooming down to items that are not StoryPoints or Home //must do last before the ZoomTo since we change the "Value" local variable
    Value := Value.GetAncestorStoryPoint; //if it returns nil it will result in zooming to the RootStoryPoint
  ZoomTo(Value);
end;

{$endregion}

{$endregion}

{$REGION 'Events'}

{$region 'StructureView'}

procedure TMainForm.StructureViewShowFilter(Sender: TObject; const TheObject: TObject; var ShowObject: Boolean);
begin
  if (StoryMode <> EditMode) and (TheObject is TStoryItem) then //in non-Edit mode
    ShowObject := TStoryItem(TheObject).StoryPoint; //only show StoryPoints (assuming StructureView's FilterMode=tfFlatten since we're not in Edit mode - this is important to be set
end;

procedure TMainForm.StructureViewSelection(Sender: TObject; const Selection: TObject);
begin
  if not (StoryMode = EditMode) then //in non-Edit mode
    HUD.StructureVisible := false; //we want to hide the StructureView before zooming to the item selected

  ActiveStoryItem := TStoryItem(Selection); //Make active (may also zoom to it) - assuming this is a TStoryItem since StructureView was filtering for such class //also accepts "nil" (for no selection)
end;

{$endregion}

{$region 'Timer'}

procedure TMainForm.StoryTimerTimer(Sender: TObject); //TODO: should show some timer animation at top-right when the story timer is enabled (AnimatedStoryMode)
begin
  //special case used at app startup
  if not FTimerStarted then //TODO: should check if we loaded from saved state and remember if we were playing the timer and continue [see CCR.PrefsIniFile github repo maybe to keep app settings])
  begin
    ZoomToActiveStoryPointOrHome; //needed upon app first loading to ZoomTo Active StoryPoint or Home from loaded saved state
    StoryTimer.Enabled := false;
    exit;
  end;

  ActivateNextStoryPoint;

  if ActiveStoryItem.Home then
    HUD.UseStoryTimer := false; //TODO: should instead define EndStoryPoint(s) and stop the timer once the end is reached
end;

{$endregion}

{$region 'Actions'}

{$region 'File actions'}

procedure TMainForm.HUDactionNewExecute(Sender: TObject);
begin
  //if ConfirmClose then //TODO (should show some image asking?)
  if (not LoadDefaultDocument) then
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
  if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit;

  ActiveStoryItem.Options.ActAdd;
  UpdateStructureView; //TODO: should instead have some notification from inside a StoryItem towards the StructureView that children were added to it (similar to how StructureView listens for children removal)
end;

procedure TMainForm.HUDactionAddBitmapImageStoryItemExecute(Sender: TObject); //TODO: should change to add a VisualStoryItem that will support any kind of visual item
begin
  if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit;

  //AddChildStoryItem(TVectorImageStoryItem, 'VectorImageStoryItem');
  AddChildStoryItem(TBitmapImageStoryItem, 'BitmapImageStoryItem'); //will also update the StructureView //TODO: see why if we use a TVectorImageStoryItem it does show a default Glyph, but using this doesn't (used to show, after descending TBitmapStoryItem from TVectorStoryItem it stopped) - also decide if we do want such default Glyph or want to use those blank images instead of TPanelStoryItem: problem is they don't show border when in non-Edit mode, but could have option to show border for any StoryItem and/or style the border color/width etc.
end;

procedure TMainForm.HUDactionAddTextStoryItemExecute(Sender: TObject);
begin
  if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit;

  AddChildStoryItem(TTextStoryItem, 'TextStoryItem'); //will also update the StructureView
end;

procedure TMainForm.AddChildStoryItem(const TheStoryItemClass: TStoryItemClass; const TheName: String);
begin
  if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit;

  var OwnerAndParent := ActiveStoryItem.View;

  var StoryItem := TheStoryItemClass.Create(OwnerAndParent, TheName); //TODO: should have separate actions for adding such default items (for prototyping) for various StoryItem classes

  with StoryItem do
  begin
    Align := TAlignLayout.Scale; //IMPORTANT: adjust when parent resizes
    Parent := OwnerAndParent;

    var TheAreaSelector := TStoryItem(OwnerAndParent).AreaSelector; //WARNING: Delphi 11 seems to resolve identifiers against the "with" first and THEN the local variables that were defined inside the with block. Don't name this AreaSelector since StoryItem has such property and accessing the Visible property below will get the wrong result...
    if TheAreaSelector.Visible and (TheAreaSelector.Width <> 0) and (TheAreaSelector.Height <> 0) then //...even worse the IDE introspection during debugging shows the object one was expecting (the one from the local variable), but the one from with would be used instead (checking the wrong "Visible" property value)
    begin
      Size.Size := TheAreaSelector.Size.Size;
      Position.Point := TheAreaSelector.Position.Point; //TODO: assuming the AreaSelector has the same parent, if not (say using a global area selector in the future) should have some way for the AreaSelector to give map the coordinates to the wanted parent
    end
    else
    begin
      var ItemSize := DefaultSize;
      Size.Size := ItemSize; //TODO: StoryItem constructor should have set its DefaultSize
      //Center the new item in its parent...
      Position.Point := PointF(OwnerAndParent.Size.Width/2 - ItemSize.Width/2, OwnerAndParent.Size.Height/2 - ItemSize.Height/2); //not creating TPosition objects to avoid leaking (TPointF is a record)
    end;

    BringToFront; //load as front-most
  end;

  UpdateStructureView; //TODO: should instead have some notification from inside a StoryItem towards the StructureView that children were added to it (similar to how StructureView listens for children removal)
end;

procedure TMainForm.DeleteActiveStoryItem;
begin
  if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit;

  if Assigned(ActiveStoryItem) and Assigned(RootStoryItem) and (ActiveStoryItem.View <> RootStoryItem.View) then
    ActiveStoryItem.Delete //this makes ParentStoryItem active
  else
    NewRootStoryItem; //deleting the RootStoryItem via "NewRootStoryItem", but not via "HUD.actionNew.Execute" since that also tries "LoadDefaultDocument" first
end;

procedure TMainForm.HUDactionDeleteExecute(Sender: TObject);
begin
  DeleteActiveStoryItem;
end;

procedure TMainForm.HUDactionCutExecute(Sender: TObject);
begin
  if Assigned(ActiveStoryItem) and Assigned(RootStoryItem) and (ActiveStoryItem.View <> RootStoryItem.View) then
    ActiveStoryItem.Cut //this makes ParentStoryItem active
  else
  begin
    ActiveStoryItem.Copy;
    NewRootStoryItem; //deleting the RootStoryItem via "NewRootStoryItem", but not via "HUD.actionNew.Execute" since that also tries "LoadDefaultDocument" first
  end;
end;

procedure TMainForm.HUDactionCopyExecute(Sender: TObject);
begin
  if Assigned(ActiveStoryItem) then
    ActiveStoryItem.Copy;
end;

procedure TMainForm.HUDactionPasteExecute(Sender: TObject);
begin
  if Assigned(ActiveStoryItem) then
  begin
    ActiveStoryItem.Paste;
    UpdateStructureView; //TODO: should do similar to deletion by somehow notifying from inside the StoryItem itself the StructureView that items have been added
  end;
end;

procedure TMainForm.HUDactionFlipHorizontallyExecute(Sender: TObject);
begin
  if Assigned(ActiveStoryItem) then
  begin
    ActiveStoryItem.FlippedHorizontally := not ActiveStoryItem.FlippedHorizontally;
    UpdateStructureView; //TODO: should maybe only update the tree of thumbs from the ActiveStoryItem up to the root by somehow notifying from inside the StoryItem itself the StructureView our graphics have changed
  end;
end;

procedure TMainForm.HUDactionFlipVerticallyExecute(Sender: TObject);
begin
  if Assigned(ActiveStoryItem) then
  begin
    ActiveStoryItem.FlippedVertically := not ActiveStoryItem.FlippedVertically;
    UpdateStructureView; //TODO: should maybe only update the tree of thumbs from the ActiveStoryItem up to the root by somehow notifying from inside the StoryItem itself the StructureView our graphics have changed
  end;
end;

procedure TMainForm.HUDcomboForeColorChange(Sender: TObject);
begin
  var LActive := ActiveStoryItem;
  if not Assigned(LActive) then exit;

  if LActive is TTextStoryItem then //TODO: could use interfaces instead (ITextStoryItem or some IForegroundColor interface)
    TTextStoryItem(LActive).TextColor := HUD.comboForeColor.Color;

  //TODO: for Vector images could replace dominant color if supported
end;

procedure TMainForm.HUDcomboBackColorChange(Sender: TObject);
begin
  var LActive := ActiveStoryItem;
  if not Assigned(LActive) then exit;

  LActive.BackgroundColor := HUD.comboForeColor.Color; //TODO: could check for special interface instead (some IBackgroundColor interface)
  //TODO: doesn't seem to do something (plus the Border is shown only when items are children of edited activestoryitem, whereas we want to always draw background in that case - need extra background control that is)
end;

{$endregion}

{$region 'View actions'}

procedure TMainForm.HUDStructureVisibleChanged(Sender: TObject; const Value: Boolean);
begin
  if Value then
  begin
    HUD.MultiViewFrameStand.CloseAllExcept(TStructureView);

    if (StoryMode <> EditMode) then //if non-Edit mode
      StructureView.FilterMode := tfFlatten
    else
      StructureView.FilterMode := tfPrune;

    UpdateStructureView; //in case the RootStoryItem has changed

    FStructureViewFrameInfo.Show; //this will have been assigned by the StructureView getter if it wasn't
  end;
end;

procedure TMainForm.UpdateStructureView;
begin
  if (StoryMode <> EditMode) then
    StructureView.FilterMode := tfFlatten
  else
    StructureView.FilterMode := tfPrune;

  StructureView.GUIRoot := RootStoryItemView;

  if Assigned(ActiveStoryItem)
     and (StoryMode = EditMode) //TODO: temp extra check, since StructureView wasn't opening up in non-Edit mode for unknown reason, remove extra check when fixed
  then
    StructureView.SelectedObject := ActiveStoryItem.View;
end;

procedure TMainForm.HUDTargetsVisibleChanged(Sender: TObject; const Value: Boolean);
begin
  if HUD.TargetsVisible then
    if Assigned(ActiveStoryItem) then
      ActiveStoryItem.TargetsVisible := Value;
end;

procedure TMainForm.HUDUseStoryTimerChanged(Sender: TObject; const Value: Boolean);
begin
  with StoryTimer do
  begin
    Enabled := HUD.UseStoryTimer;
    Interval := 8000; //proceed ever 8sec (TODO: should be easily adjustable)
  end;
  FTimerStarted := true;
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

{$endregion}

{$endregion}

{$region 'Keyboard'}

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
begin
  case Key of

    vkEscape:
      if ssShift in Shift then //go to RootStoryItem
        ActivateRootStoryItem
      else
        ActivateParentStoryItem; //go to ParentStoryItem

    vkPrior, vkLeft, vkUp:     //go to PreviousStoryPoint
      ActivatePreviousStoryPoint;

    vkNext, vkRight, vkDown:   //go to NextStoryPoint
      ActivateNextStoryPoint;

    vkHome: //go to HomeStoryItem
      ActivateHomeStoryItem;

    (* //Unsafed, not used - if user is trying to type in TextStoryItem they might remove it altogether by mistake (wrong focus)
    vkDelete:
      DeleteActiveStoryItem;
    *)

    (*
    vkEnd:
      ActivateEnd; //TODO: for cheaters, go to EndStoryPoint - HOWEVER MAY WANT TO HAVE MULTIPLE ENDSTORYPOINTS, THEIR NEXTSTORYPOINT WOULD ALWAYS BE HOMESTORYITEM [Home may not be a StoryPoint but End always is else it wouldn't be reachable]? (which is the EndStoryItem should we be able to set such?)
    *)

    {$IFDEF DEBUG}
    vkF11:
      with ObjectDebuggerFMXForm1 do
        Visible := not Visible; //toggle Object inspector visibility
    {$ENDIF}
  end;
end;

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

procedure TMainForm.LoadSavedStateOrDefaultDocumentOrNewRootStoryItem;
begin
  if (not LoadSavedState) and (not LoadDefaultDocument) then //Note: if it keeps on failing at load comment out this line for one run //TODO: shouldn't need to do that
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
        {$IFDEF DEBUG}{$IFDEF WINDOWS}
        try
          CodeSite.Send(TheRootStoryItemView.SaveToString);
        finally
          //NOP
        end;
        {$ENDIF}{$ENDIF}
        RootStoryItemView := TheRootStoryItemView; //only set RootStoryItemView (this affects RootStoryItem too)
        result := true;
      except
        on E: Exception do
          begin
          Stream.Clear; //clear stream if causes loading error //TODO: instead of Clear which doesn't seem to work, try saving instead a new instance of TPanelStoryItem
          {$IFDEF DEBUG}{$IFDEF WINDOWS}CodeSite.SendException(E);{$ENDIF}{$ENDIF}
          ShowException(E, @TMainForm.FormCreate);
          FreeAndNil(TheRootStoryItemView); //Free partially loaded - corrupted StoryItem
          end;
      end;
    end;
  end;

  with StoryTimer do
  begin
    Interval := 100;
    Enabled := true; //used to re-apply ActiveStoryItem so that we can zoom to it after the form has fully sized (FormShow event doesn't do this properly)
  end;
end;

function TMainForm.LoadDefaultDocument: Boolean;
begin
  result := false; //don't place inside the try, else you get warning that result might be undefined
  try
    var Stream := TResourceStream.Create(HInstance, 'DefaultDocument', RT_RCDATA);
    try
      if Stream.Size > 0 then
      begin
        var TheRootStoryItemView := TPanelStoryItem.Create(Self);
        try
          TheRootStoryItemView.Load(Stream); //default file format is EXT_READCOM
          {$IFDEF DEBUG}{$IFDEF WINDOWS}
          try
            CodeSite.Send(TheRootStoryItemView.SaveToString);
          finally
            //NOP
          end;
          {$ENDIF}{$ENDIF}
          RootStoryItemView := TheRootStoryItemView; //only set RootStoryItemView (this affects RootStoryItem too)
          result := true;
        except
          on E: Exception do
            begin
            {$IFDEF DEBUG}{$IFDEF WINDOWS}CodeSite.SendException(E);{$ENDIF}{$ENDIF}
            ShowException(E, @TMainForm.FormCreate);
            FreeAndNil(TheRootStoryItemView); //Free partially loaded - corrupted StoryItem
            end;
        end;
      end;
    finally
      Stream.Free;
    end;
  except
    //NOP
  end;
end;

procedure TMainForm.SaveCurrentState;
begin
  {$IFDEF DEBUG}{$IFDEF WINDOWS}CodeSite.EnterMethod('SaveState');{$ENDIF}{$ENDIF}
  //StoragePath := ... //TODO: default is transient, change to make permanent
  SaveState.Stream.Clear;

  var TheRootStoryItemView := RootStoryItemView;
  if Assigned(TheRootStoryItemView) then
    with SaveState do
      try
        TheRootStoryItemView.Save(Stream); //default file format is EXT_READCOM
        {$IFDEF DEBUG}{$IFDEF WINDOWS}
        try
          CodeSite.Send(TheRootStoryItemView.SaveToString);
        finally
          //NOP
        end;
        {$ENDIF}{$ENDIF}
      except
        On E: Exception do
          begin
          Stream.Clear; //clear stream in case it got corrupted
          {$IFDEF DEBUG}{$IFDEF WINDOWS}CodeSite.SendException(E);{$ENDIF}{$ENDIF}
          ShowException(E, @TMainForm.FormCreate);
          end;
    end;
  {$IFDEF DEBUG}{$IFDEF WINDOWS}CodeSite.ExitMethod('SaveState');{$ENDIF}{$ENDIF}
end;

procedure TMainForm.FormSaveState(Sender: TObject);
begin
  HUD.EditMode := false; //make sure we exit EditMode, else child items of ActiveStoryItem are saved as disabled
  SaveCurrentState;
end;

{$endregion}

end.

