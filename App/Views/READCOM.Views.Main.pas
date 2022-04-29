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

    //Initialiation
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    //State saving
    procedure FormSaveState(Sender: TObject);

    //Keyboard
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);

    //File actions
    procedure HUDactionNewExecute(Sender: TObject);
    procedure HUDactionLoadExecute(Sender: TObject);
    procedure HUDactionSaveExecute(Sender: TObject);

    //Light-Dark mode
    procedure HUDactionNextThemeExecute(Sender: TObject);

    //Navigation actions
    procedure HUDactionHomeExecute(Sender: TObject);
    procedure HUDactionPreviousExecute(Sender: TObject);
    procedure HUDactionNextExecute(Sender: TObject);
    procedure StoryTimerTimer(Sender: TObject);

    //Add actions
    procedure HUDactionAddExecute(Sender: TObject);
    procedure HUDactionAddBitmapImageStoryItemExecute(Sender: TObject);
    procedure HUDactionAddTextStoryItemExecute(Sender: TObject);

    //Edit actions
    procedure HUDactionDeleteExecute(Sender: TObject);
    procedure HUDactionCutExecute(Sender: TObject);
    procedure HUDactionCopyExecute(Sender: TObject);
    procedure HUDactionPasteExecute(Sender: TObject);

    //Flip actions
    procedure HUDactionFlipHorizontallyExecute(Sender: TObject);
    procedure HUDactionFlipVerticallyExecute(Sender: TObject);

    //Color actions
    procedure HUDcomboForeColorChange(Sender: TObject);
    procedure HUDcomboBackColorChange(Sender: TObject);

    //Scaling
    procedure FormResize(Sender: TObject);

  protected
    FShortcutCut, FShortcutCopy, FShortcutPaste: TShortCut;
    FTimerStarted: Boolean;
    FStoryMode: TStoryMode;
    FStructureViewFrameInfo: FrameStand.TFrameInfo<TStructureView>;

    {SavedState}
    procedure LoadAtStartup;
    function LoadFromStream(const Stream: TStream; const ActivateHome: Boolean = True): Boolean;
    function LoadFromFile(const Filepath: String): Boolean;
    function LoadFromUrl(const Url: String): Boolean;
    function LoadFromFileOrUrl(const PathOrUrl: String): Boolean;
    function LoadCommandLineParameter: Boolean;
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

    {URLs}
    procedure OpenUrl(const Url: string);

    {ActiveStoryItem}
    function GetActiveStoryItem: IStoryItem;
    procedure SetActiveStoryItem(const Value: IStoryItem);
    procedure HandleActiveStoryItemChanged(Sender: TObject);

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

    {HUD}
    procedure HUDEditModeChanged(Sender: TObject; const Value: Boolean);
    procedure HUDStructureVisibleChanged(Sender: TObject; const Value: Boolean);
    procedure HUDTargetsVisibleChanged(Sender: TObject; const Value: Boolean);
    procedure HUDUseStoryTimerChanged(Sender: TObject; const Value: Boolean);

  public
    property StructureView: TStructureView read GetStructureView stored false;
    procedure ZoomTo(const StoryItem: IStoryItem = nil); //ZoomTo(nil) zooms to all content
    procedure ZoomToActiveStoryPointOrHome;

    {Edit actions}
    procedure NewRootStoryItem;
    procedure DeleteActiveStoryItem;
    procedure CutActiveStoryItem;
    procedure AddChildStoryItem(const TheStoryItemClass: TStoryItemClass; const TheName: String);

  published
    property StoryMode: TStoryMode read GetStoryMode write SetStoryMode stored false;
    property RootStoryItem: IStoryItem read GetRootStoryItem write SetRootStoryItem stored false;
    property RootStoryItemView: TStoryItem read GetRootStoryItemView write SetRootStoryItemView stored false;
    property HomeStoryItem: IStoryItem read GetHomeStoryItem write SetHomeStoryItem stored false;
    property ActiveStoryItem: IStoryItem read GetActiveStoryItem write SetActiveStoryItem stored false;
  end;

var
  MainForm: TMainForm;

resourcestring
  MSG_CONFIRM_CLEAR_STORY = 'Clearing story: are you sure?';

implementation
  uses
    System.Contnrs, //for TClassList
    System.Math, //for Max
    System.Net.URLClient, //for TURLStream (Delphi 11.1+)
    FMX.Styles, //for TStyleManager
    Zoomicon.Helpers.RTL.ClassListHelpers, //for TClassList.Create(TClassArray)
    Zoomicon.Helpers.FMX.Controls.ControlHelpers, //for TControl.FlipHorizontally, TControl.FlipVertically
    Zoomicon.Helpers.FMX.Forms.ApplicationHelper, //for TApplication.Confirm
    READCOM.App.Main, //for StorySource
    READCOM.App.URLs, //for OpenURLinBrowser and DownloadFileWithFallbackCache
    READCOM.App.Debugging, //for ToggleObjectDebuggerVisibility
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
      //keep shortcut keys for Cut/Copy/Paste actions to be able to restore them after disabling them when TextStoryItem is active (to not interfere with text editing)
      FShortcutCut := HUD.actionCut.ShortCut;
      FShortcutCopy := HUD.actionCopy.ShortCut;
      FShortcutPaste := HUD.actionPaste.ShortCut;

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
  if not GlobalUseDX then //GlobalUseDX10 (not using that, using fallback to plain GDI)
    Caption := STR_APP_TITLE + ' ' + STR_COMPATIBILITY_MODE
  else
    Caption := STR_APP_TITLE;

  TStoryItem.Story := Self; //provide a way to StoryItems to influence the Story context

  FTimerStarted := false;
  InitHUD;
  //ZoomFrame.ScrollBox.AniCalculations.AutoShowing := true; //fade the toolbars when not active //TODO: doesn't work with direct mouse drags near the bottom and right edges (scrollbars do show when scrolling e.g. with mousewheel) since there's other HUD content above them (the navigation and the edit sidebar panes)
  LoadAtStartup;
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
      Position.Point := TPointF.Zero; //ignore any Position value, want to have the RootStoryItem centered in the zoomable container

      Align := TAlignLayout.Center; //IMPORTANT: Center to zoomable container, if it had "Scaled" it would crash on Zoom in/out. Note that at TStoryItem.Add any .readcom content added as children is set to use "Align := TAlignLayout.Scale", so that the children scale as their parent StoryItem resizes

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

{$region 'URLs'}

procedure TMainForm.OpenUrl(const Url: String);
begin
  if Url.EndsWith(EXT_READCOM, True) then //if it's a URL to a .readcom file (case-insensitive comparison)
    LoadFromUrl(Url)
  else
    OpenURLinBrowser(Url); //else open in system browser
end;

{$endregion}

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

    //Update Foreground color picker
    HUD.comboForeColor.Enabled := isTextStoryItem; //TODO: if we use ITextStoryItem interface instead at that combo's chane event, do similar check here //TODO: also check for other types that may support forecolor (say SVG images could allow to change dominant color) //TODO: Visible doesn't work (keeps combo hidden), using Enabled instead for now
    if isTextStoryItem then HUD.comboForeColor.Color := TTextStoryItem(StoryItem).TextColor; //TODO: if other items support fore color move to some interface based check and cast (and/or add Foreground color to IStoryItem)

    //Update Background color picker
    HUD.comboBackColor.Color := StoryItem.BackgroundColor;

    //Cut-Copy-Paste shortcut keys variation for TextStoryItem //TODO: should maybe disable respective actions in non-Edit mode, even though they do check it to do nothing when not in edit mode
    if isTextStoryItem then
    begin
      HUD.actionCut.ShortCut := TextToShortCut('Ctrl+Shift+X'); //set alternate shortcut while TTextStoryItem is being edited
      HUD.actionCopy.ShortCut := TextToShortCut('Ctrl+Shift+C'); //set alternate shortcut while TTextStoryItem is being edited
      HUD.actionPaste.ShortCut := TextToShortCut('Ctrl+Shift+V'); //set alternate shortcut while TTextStoryItem is being edited
    end
    else
    begin
      HUD.actionCut.ShortCut := FShortcutCut; //TextToShortCut('Ctrl+X'); //Note: instead of hardcoding shortcuts here reading them on form statup and keep them to restore here
      HUD.actionCopy.ShortCut := FShortcutCopy; //TextToShortCut('Ctrl+C');
      HUD.actionPaste.ShortCut := FShortcutPaste; //TextToShortCut('Ctrl+V');
    end;

    if HUD.StructureVisible then
      StructureView.SelectedObject := StoryItem; //Change StructureView selection (ONLY WHEN StructureView is visible)
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

  if (not isEditMode) and Assigned(ActiveStoryItem) and (not ActiveStoryItem.StoryPoint) then //if current ActiveStoryItem isn't a StoryPoint, then when exiting Edit mode...
  begin
    var ancestor := ActiveStoryItem.GetAncestorStoryPoint; //...try to activeate an ancestor StoryPoint
    if Assigned(ancestor) then
      ActiveStoryItem := ancestor
    else
      ActiveStoryItem := RootStoryItem; //...making sure we always end up with an ActiveStoryItem (the RootStoryItem) when no StoryPoints have been defined
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
  if (StoryMode <> EditMode) and (TheObject is TStoryItem) then //when in non-Edit mode...
    with TStoryItem(TheObject) do
      ShowObject := StoryPoint or Home; //...only show StoryPoints and Home //IMPORTANT: assuming StructureView's FilterMode=tfFlatten since we're in non-Edit mode
end;

procedure TMainForm.StructureViewSelection(Sender: TObject; const Selection: TObject);
begin
  ActiveStoryItem := TStoryItem(Selection); //Make active (may also zoom to it) - assuming this is a TStoryItem since StructureView was filtering for such class //also accepts "nil" (for no selection)
end;

{$endregion}

{$region 'Timer'}

procedure TMainForm.StoryTimerTimer(Sender: TObject); //TODO: should show some timer animation at top-right when the story timer is enabled (AnimatedStoryMode)
begin
  //special case used at app startup
  if not FTimerStarted then //TODO: should check if we loaded from saved state and remember if we were playing the timer and continue [see CCR.PrefsIniFile github repo maybe to keep app settings])
  begin
    ZoomTo; //TODO: temp fix, else showing some undrawn text when loading from temp state and till one [un]zooms or resizes (seems 0.3.1 didn't have the issue this fixes, but 0.3.0 and 0.3.2+ had it)
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
  if TApplication.Confirm(MSG_CONFIRM_CLEAR_STORY) and (not LoadDefaultDocument) then //Note: could also use Application.Confirm since Confirm is defined as a class function in ApplicationHelper
    NewRootStoryItem;
end;

procedure TMainForm.HUDactionLoadExecute(Sender: TObject);
begin
  //HUD.actionLoadExecute(Sender);

  var TempStoryItem := TStoryItem.Create(nil); //using TempStoryItem of TStoryItem type to show generic file dialog filter
  try
    var Filename := TempStoryItem.Options.ActLoad_GetFilename; //assuming this is blocking action
    if (Filename <> '') then
      begin
      RootStoryItemView := TStoryItem.LoadNew(Filename);
      //TODO: OLD-CODE-LINE-REMOVE? //RootStoryItemView := RootStoryItemView; //repeat calculations to adapt ZoomFrame.ScaledLayout size //TODO: when RootStoryItemViewResized starts working this shouldn't be needed here anymore
      ActiveStoryItem := HomeStoryItem; //set the HomeStoryItem (if not any the RootStoryItem will have been set as such by SetRootStoryView) to active (not doing this when loading saved app state)
      end;
  finally
    FreeAndNil(TempStoryItem);
  end;
end;

procedure TMainForm.HUDactionSaveExecute(Sender: TObject);
begin
  //HUD.actionSaveExecute(Sender);

  RootStoryItem.Options.ActSave;
end;

{$endregion}

{$region 'Light-Dark mode'}

procedure TMainForm.HUDactionNextThemeExecute(Sender: TObject);
begin
  var switchToLightMode: Boolean;
  with Globals do
  begin
    switchToLightMode := DarkTheme.UseStyleManager; //don't use "LightTheme.UseStyleManager" this seems to return false, even though it's set to true in the designer (the default style is white)
    LightTheme.UseStyleManager := switchToLightMode;
    DarkTheme.UseStyleManager := not switchToLightMode;
  end;

  if switchToLightMode then //the code above isn't enough to switch theme, so switching the MainForm's theme directly //TODO: this won't work for extra forms, try TStyleManager instead
    StyleBook := Globals.LightTheme
  else
    StyleBook := Globals.DarkTheme;
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

{$region 'Action Helpers'}

procedure TMainForm.NewRootStoryItem; //Note: make sure any keyboard actions call the action event handlers since only those do confirmation
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

procedure TMainForm.DeleteActiveStoryItem; //Note: make sure any keyboard actions call the action event handlers since only those do confirmation
begin
  if not Assigned(ActiveStoryItem) then exit;

  if (Assigned(RootStoryItem) and (ActiveStoryItem.View <> RootStoryItem.View)) then
    ActiveStoryItem.Delete //this makes ParentStoryItem active (which updates StructureView)
  else //note: confirmation is only done at "HUDactionCutExecute"
    NewRootStoryItem; //deleting the RootStoryItem via "NewRootStoryItem", but not via "HUD.actionNew.Execute" since that also tries "LoadDefaultDocument" first //RootStoryItem change updates StructureView
end;

procedure TMainForm.CutActiveStoryItem; //Note: make sure any keyboard actions call the action event handlers since only those do confirmation
begin
  if not Assigned(ActiveStoryItem) then exit;

  if (Assigned(RootStoryItem) and (ActiveStoryItem.View <> RootStoryItem.View)) then
    ActiveStoryItem.Cut //this makes ParentStoryItem active (which updates StructureView)
  else //note: confirmation is only done at "HUDactionCutExecute"
    begin
      ActiveStoryItem.Copy; //needed to simulate "Cut"
      NewRootStoryItem; //deleting the RootStoryItem via "NewRootStoryItem", but not via "HUD.actionNew.Execute" since that also tries "LoadDefaultDocument" first //RootStoryItem change updates StructureView
    end;
end;

{$endregion}

procedure TMainForm.HUDactionDeleteExecute(Sender: TObject);
begin
  if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit;

  if (Assigned(RootStoryItem) and (ActiveStoryItem.View <> RootStoryItem.View)) or
     TApplication.Confirm(MSG_CONFIRM_CLEAR_STORY)
  then
    DeleteActiveStoryItem;
end;

procedure TMainForm.HUDactionCutExecute(Sender: TObject);
begin
  if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit;

  if (Assigned(RootStoryItem) and (ActiveStoryItem.View <> RootStoryItem.View)) or
     TApplication.Confirm(MSG_CONFIRM_CLEAR_STORY)
  then
    CutActiveStoryItem;
end;

procedure TMainForm.HUDactionCopyExecute(Sender: TObject);
begin
  if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit;
  ActiveStoryItem.Copy;
end;

procedure TMainForm.HUDactionPasteExecute(Sender: TObject);
begin
  if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit;

  ActiveStoryItem.Paste;
  UpdateStructureView; //TODO: should do similar to deletion by somehow notifying from inside the StoryItem itself the StructureView that items have been added
end;

procedure TMainForm.HUDactionFlipHorizontallyExecute(Sender: TObject);
begin
  if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit;

  ActiveStoryItem.FlippedHorizontally := not ActiveStoryItem.FlippedHorizontally;
  UpdateStructureView; //TODO: should maybe only update the tree of thumbs from the ActiveStoryItem up to the root by somehow notifying from inside the StoryItem itself the StructureView our graphics have changed
end;

procedure TMainForm.HUDactionFlipVerticallyExecute(Sender: TObject);
begin
  if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit;

  ActiveStoryItem.FlippedVertically := not ActiveStoryItem.FlippedVertically;
  UpdateStructureView; //TODO: should maybe only update the tree of thumbs from the ActiveStoryItem up to the root by somehow notifying from inside the StoryItem itself the StructureView our graphics have changed
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

  LActive.BackgroundColor := HUD.comboBackColor.Color; //TODO: could check for special interface instead (some IBackgroundColor interface)
  //TODO: doesn't seem to do something (plus the Border is shown only when items are children of edited activestoryitem, whereas we want to always draw background in that case - need extra background control that is)
end;

{$endregion}

{$region 'View actions'}

procedure TMainForm.HUDStructureVisibleChanged(Sender: TObject; const Value: Boolean);
begin
  if Value then
  begin
    HUD.MultiViewFrameStand.CloseAllExcept(TStructureView); //TODO: ???
    UpdateStructureView; //in case the RootStoryItem has changed
    FStructureViewFrameInfo.Show; //this will have been assigned by the StructureView getter if it wasn't (the side-panel has already opened, show the StructureView frame in it)
  end;

  with StructureView do //TODO: see why we need those for the hidden StructureView to not grab mouse events from the area it was before collapse on the form (Probably a MultiView bug with or without the combination of TFrameStand to put a frame at the side tray)
  begin
    Enabled := Value;
    HitTest := Value;
  end;
end;

procedure TMainForm.UpdateStructureView;
begin
  Log('UpdateStructureView');

  if not HUD.StructureVisible then
  begin
    Log('Ignoring UpdateStructureView, currently hidden');
    exit;
  end;

  if (StoryMode <> EditMode) then
    StructureView.FilterMode := tfFlatten
  else
    StructureView.FilterMode := tfPrune;

  StructureView.GUIRoot := RootStoryItemView;

  if Assigned(ActiveStoryItem) then
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
    FTimerStarted := Value;
    Enabled := Value;
    Interval := 8000; //proceed ever 8sec (TODO: should be easily adjustable)
  end;
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

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState); //also see HUD Actions' shortcuts
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

    vkF11:
      ToggleObjectDebuggerVisibility;
  end;
end;

{$endregion}

{$ENDREGION}

{$region 'SavedState'}

procedure TMainForm.LoadAtStartup;
begin
  if (not LoadCommandLineParameter) and
     (not LoadSavedState) and
     (not LoadDefaultDocument) then //Note: if it keeps on failing at load comment out the if check for one run of NewRootStoryItem //TODO: shouldn't need to do that
    NewRootStoryItem;
end;

function TMainForm.LoadFromStream(const Stream: TStream; const ActivateHome: Boolean = True): Boolean;
begin
  result := false;
  if Stream.Size > 0 then
  begin
    try
      RootStoryItemView := TStoryItem.LoadNew(Stream, EXT_READCOM); //a new instance of the TStoryItem descendent serialized in the Stream will be created //only set RootStoryItemView (this affects RootStoryItem too)
      if ActivateHome then
        ActivateHomeStoryItem;
      result := true;
    except
      on E: Exception do
        begin
        //Stream.Clear; //clear stream if causes loading error //TODO: instead of Clear which doesn't seem to work, try saving instead a new instance of TPanelStoryItem
        Log(E);
        ShowException(E, @TMainForm.LoadFromStream);
        end;
    end;
  end;
end;

function TMainForm.LoadFromFile(const Filepath: string): Boolean;
begin
  var InputFileStream := TFileStream.Create(Filepath,  fmOpenRead);
  try
    result := LoadFromStream(InputFileStream);
  finally
    FreeAndNil(InputFileStream);
  end;
end;

function TMainForm.LoadFromUrl(const Url: String): Boolean; //TODO: should add LoadFromUrl and AddFromUrl to TStoryItem too
begin
  //var memStream := DownloadFileWithFallbackCache(Url); //TODO: this seems to bring empty file
  try
    //result := LoadFromStream(memStream); //tell app to open the fetched memstream as new RootStoryItem, can create .readcom story files that serve as galleries that point to other readcom files via thumbnails - and can use that at the Default.readcom file too that gets loaded on 1st run //TODO: maybe make global RootStoryItem like ActiveStoryItem with change event and app can listen to that

    var loaded := false;
    Log('Before call to TURLStream');
    TURLStream.Create(Url, //TODO: try to fix the downloader (but do check on mobiles too) since this is Delphi 11.1, else make a version of the downloader that can use that to also have caching
      procedure(AStream: TStream)
      begin
        Log('Started download');
        loaded := LoadFromStream(AStream); //probably it can start loading while the content is coming
        Log('Finished download');
      end,
      true, //ASynchronizeProvide: call the anonymous proc (AProvider parameter) in the context of the main thread //IMPORTANT (it not done various AV errors occur later: we shouldn't touch the UI from other than the main thread)
      true //free on completion
    ).AsyncResult.AsyncWaitEvent.WaitFor; //TODO: check if this works properly on Android
    Log('After call to TURLStream'); //TODO: this should be output after "Finished download" but seems to be output immediately after "Before call..." which means the TURLStream WaitFor doesn't work as expected
    result := loaded; //TODO: due to WaitFor not working as expected, make sure the result of LoadFromUrl isn't used for now (will be false)

  finally
    //FreeAndNil(memStream);
  end;
end;

function TMainForm.LoadFromFileOrUrl(const PathOrUrl: String): Boolean;
begin
  if PathOrUrl.StartsWith('http://', true) or PathOrUrl.StartsWith('https://', true) then
    result := LoadFromUrl(PathOrUrl)
  else
    result := LoadFromFile(PathOrUrl);
end;

function TMainForm.LoadCommandLineParameter: Boolean;
begin
  result := (StorySource <> '') and LoadFromFileOrUrl(StorySource); //assuming short-circuit boolean evaluation
end;

function TMainForm.LoadSavedState: Boolean;
begin
  With SaveState do
  begin
    //StoragePath := ... //TODO: default is transient, change to make permanent
    result := LoadFromStream(Stream, false); //don't ActivateHome, keep last Active one (needed in case the OS brought down the app and need to continue from where we were from saved state)
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
    var Stream := TResourceStream.Create(HInstance, 'DefaultDocument', RT_RCDATA); //TODO: added via Project/Deployment, but not sure where the "DefaultDocument" resource name was defined (isn't shown there)
    try
      if Stream.Size > 0 then
      begin
        try
          RootStoryItemView := TStoryItem.LoadNew(Stream, EXT_READCOM); //a new instance of the TStoryItem descendent serialized in the Stream will be created //only set RootStoryItemView (this affects RootStoryItem too)
          result := true;
        except
          on E: Exception do
            begin
            Log(E);
            ShowException(E, @TMainForm.LoadDefaultDocument);
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
  Log('SaveState');
  //StoragePath := ... //TODO: default is transient, change to make permanent
  SaveState.Stream.Clear;

  var TheRootStoryItemView := RootStoryItemView;
  if Assigned(TheRootStoryItemView) then
    with SaveState do
      try
        TheRootStoryItemView.Save(Stream); //default file format is EXT_READCOM
      except
        on E: Exception do
          begin
          Stream.Clear; //clear stream in case it got corrupted
          Log(E);
          ShowException(E, @TMainForm.SaveCurrentState);
          end;
    end;
end;

procedure TMainForm.FormSaveState(Sender: TObject);
begin
  HUD.EditMode := false; //make sure we exit EditMode, else child items of ActiveStoryItem are saved as disabled
  SaveCurrentState;
end;

{$endregion}

end.

