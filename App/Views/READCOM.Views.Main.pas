//Description: READ-COM Main (Story) View
//Author: George Birbilis (http://zoomicon.com)

{-$DEFINE NOSTYLE}

unit READCOM.Views.Main;

interface
  {$region 'Used units'}
  uses
    System.Actions, System.SysUtils, System.Types, System.UITypes,
    System.Classes, System.Variants,
    //
    FMX.Objects, FMX.Controls, FMX.Controls.Presentation, FMX.StdCtrls,
    FMX.Types, //for TLang
    FMX.Forms, FMX.Graphics, FMX.Dialogs,
    FMX.Layouts,
    FMX.ActnList,
    //
    SubjectStand,
    FrameStand, //for TFrameInfo
    //
    Zoomicon.Generics.Collections, //for TObjectListEx
    Zoomicon.Introspection.FMX.StructureView, //for TStructureView
    Zoomicon.ZUI.FMX.ZoomFrame, //for TZoomFrame
    //
    READCOM.Models.Stories, //for IStory, IStoryItem
    READCOM.Views.StoryItems.StoryItem, //for TStoryItem
    READCOM.Views.HUD; //for TStoryHUD
  {$endregion}

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
      procedure HUDactionAddImageStoryItemExecute(Sender: TObject);
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

      //Options action
      procedure HUDactionOptionsExecute(Sender: TObject);

      //Scaling
      procedure FormResize(Sender: TObject);

      procedure FormShow(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);

    protected
      FShiftDown: Boolean; //set by event handlers for OnFormKeyDown, OnFormKeyUp to monitor SHIFT key standalone presses
      FShortcutCut, FShortcutCopy, FShortcutPaste: TShortCut;
      FTimerStarted: Boolean;
      FCheckedCommandLineActions: Boolean;
      FStoryMode: TStoryMode;
      FStructureViewFrameInfo: FrameStand.TFrameInfo<TStructureView>;

      procedure StartInitialZoomTimer(const Delay: Cardinal = 100);

      {Idle}
      procedure Idle(Sender: TObject; var Done: Boolean);

      {Loading / SavedState}
      procedure LoadAtStartup;
      function LoadFromStream(const Stream: TStream; const ActivateHome: Boolean = True): Boolean;
      function LoadFromFile(const Filepath: String): Boolean;
      function LoadFromUrl(const Url: String): Boolean;
      function LoadFromFileOrUrl(const PathOrUrl: String): Boolean;
      function LoadCommandLineParameter: Boolean;
      function LoadDefaultDocument: Boolean;
      function LoadSavedState: Boolean;
      procedure SaveCurrentState;
      procedure CheckCommandLineActions;
      procedure CheckReplaceStoryAllText;

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
      procedure SetActiveStoryItemIfAssigned(const Value: IStoryItem);
      procedure HandleActiveStoryItemChanged(Sender: TObject);

      {Navigation}
      procedure ActivateRootStoryItem;
      procedure ActivateParentStoryItem;
      procedure ActivateHomeStoryItem;
      //
      procedure ActivateAncestorStoryPoint;
      procedure ActivatePreviousStoryPoint;
      procedure ActivateNextStoryPoint;
      //
      procedure NavigateUp;

      {Orientation}
      procedure CheckProperOrientation;

      {Zooming}
      procedure ZoomTo(const StoryItem: IStoryItem = nil); //ZoomTo(nil) zooms to all content
      procedure ZoomToActiveStoryPointOrHome;

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
    MSG_CONFIRM_CLEAR_STORY = 'Clearing Story: are you sure?';
    MSG_CONFIRM_DELETION = 'Deleting ActiveStoryItem: are you sure?';

implementation
  {$region 'Used units'}
  uses
    System.Contnrs, //for TClassList
    System.IOUtils, //for TPath
    System.Math, //for Max
    System.Net.HttpClient, //for HTTP/HTTPS url scheme support in TURLStream (Delphi 11.1+)
    System.Net.URLClient, //for TURLStream (Delphi 11.1+)
    //
    Fmx.Memo, //for TMemo
    //
    Zoomicon.Helpers.RTL.ClassListHelpers, //for TClassList.Create(TClassArray)
    Zoomicon.Helpers.FMX.ActnList, //for SafeTextToShortcut
    Zoomicon.Helpers.FMX.Controls.ControlHelper, //for TControl.Orientation, TControl.FlipHorizontally, TControl.FlipVertically
    Zoomicon.Helpers.FMX.Forms.ApplicationHelper, //for TApplication.Confirm, TApplication.OpenURL, IsURI
    Zoomicon.Helpers.FMX.Forms.FormHelper, //for TForm.Orientation
    Zoomicon.Helpers.FMX.Memo.MemoHelpers, //for TMemo.DisableFontSizeToFit
    Zoomicon.Introspection.FMX.Debugging, //for ToggleObjectDebuggerVisibility
    //
    READCOM.Models, //for EXT_READCOM
    READCOM.App.Main, //for StorySource
    READCOM.App.Messages,
    READCOM.Resources.Themes, //for Themes.LightTheme, Themes.DarkTheme
    READCOM.Views.StoryItems.ImageStoryItem, //for TImageStoryItem
    READCOM.Views.StoryItems.TextStoryItem, //for TTextStoryItem
    READCOM.Views.Dialogs.AllText, //for TAllTextFrame
    READCOM.Views.Prompts.Wait, //for TWaitFrame
    READCOM.Views.Prompts.Lock, //for TLockFrame
    READCOM.Views.Prompts.Rotate; //for TRotateFrame
  {$endregion}

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
    {$IFDEF NOSTYLE}
    StyleBook := nil; //TODO: can we make a style platform agnostic?
    {$ELSE}
    StyleBook := Themes.LightTheme;
    {$ENDIF}

    const StrAppVersion = '(' + Application.AppVersion + ')';
    Caption := STR_APP_TITLE + ' ' + StrAppVersion;

    {$IF DEFINED(MSWINDOWS)}
    if not GlobalUseDX then //GlobalUseDX10 (not using that, using fallback to plain GDI)
      Caption := STR_APP_TITLE + ' ' + STR_COMPATIBILITY_MODE;
    {$ENDIF}

    TStoryItem.Story := Self; //provide a way to StoryItems to influence the Story context
    FTimerStarted := false;
    FCheckedCommandLineActions := false;
    InitHUD;
    //ZoomFrame.ScrollBox.AniCalculations.AutoShowing := true; //fade the toolbars when not active //TODO: doesn't work with direct mouse drags near the bottom and right edges (scrollbars do show when scrolling e.g. with mousewheel) since there's other HUD content above them (the navigation and the edit sidebar panes)

    Application.OnIdle := Idle;
    LoadAtStartup;

    InitObjectDebugger(MainForm);
  end;

  procedure TMainForm.FormShow(Sender: TObject);
  begin
    //NOP
  end;

  procedure TMainForm.FormDestroy(Sender: TObject);
  begin
    HUD.MultiViewFrameStand.CloseAll;
  end;

  procedure TMainForm.FormResize(Sender: TObject);
  begin
    ZoomToActiveStoryPointOrHome; //keep the Active StoryPoint or Home in view
    CheckProperOrientation;
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

        //ClipChildren := true; //Note: not doing here, since if RootStoryItem always clip its children and we load a Template (say a rabbit with its speech bubble), then children of that template will be clipped and when saved the top object in that template will get the ClipChildren=true which is unwanted
      end;

      if not Assigned(HomeStoryItem) then
        HomeStoryItem := RootStoryItem; //set RootStoryItem as the HomeStoryItem if no such assigned

      if not Assigned(ActiveStoryItem) then
        ActiveStoryItem := RootStoryItem; //set RootStoryItem as the ActiveStoryItem if no such is set (e.g. from loaded state). Note this will also try to ZoomTo it

      CheckReplaceStoryAllText;

      CheckProperOrientation; //checking proper orientation based on the StoryItem that is active upon the loading of the story (usually it's the HomeStoryItem, but could be the RootStoryItem if there's no Home set, or be other StoryItem if loading from saved state)
    end;

    UpdateStructureView;
  end;

  procedure TMainForm.RootStoryItemViewResized(Sender: TObject); //TODO: this doesn't seem to get called (needed for AutoSize of RootStoryItemView to work)
  begin
    RootStoryItemView := RootStoryItemView; //repeat calculations to adapt ZoomFrame.ScaledLayout size (this keeps the zoom to fit the new size of the RootStoryItem, plus updates StructureView if open)
    //ZoomTo; //Zoom to the root //doesn't seem to do it
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
    if Url = '-' then
      ActivatePreviousStoryPoint
    else if Url = '+' then
      ActivateNextStoryPoint
    else if Url = '0' then
      ActivateHomeStoryItem
    //TODO: check for other StoryPoint reference to Activate that, needed to be able to make Stories with alternative flows
    //(e.g. "1.3" [if root is storypoint] or "3" in the flattened StructureView tree of non-Edit mode)

    else if Url.EndsWith(EXT_READCOM, True) then //if it's a URL to a .readcom file (case-insensitive comparison)
      LoadFromUrl(Url)

    else
      Application.OpenURL(Url); //else open in system browser
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

  procedure TMainForm.SetActiveStoryItemIfAssigned(const Value: IStoryItem);
  begin
    if Assigned(Value) then
      SetActiveStoryItem(Value);
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

      //Update color pickers
      HUD.comboForeColor.Color := StoryItem.ForegroundColor;
      HUD.comboBackColor.Color := StoryItem.BackgroundColor;

      //Cut-Copy-Paste shortcut keys variation for TextStoryItem //TODO: should maybe disable respective actions in non-Edit mode, even though they do check it to do nothing when not in edit mode
      if (StoryItem is TTextStoryItem) then
      begin
        //Note: don't use plain TextToShortcut, returns -1 on Android at Delphi 11.1, which gives Range Check Error since TCustomAction.Shortcut doesn't accept values <0. Delphi 12.2 seems to have this fixed, returning 0 in that case
        HUD.actionCut.ShortCut := SafeTextToShortCut('Ctrl+Shift+X'); //set alternate shortcut while TTextStoryItem is being edited
        HUD.actionCopy.ShortCut := SafeTextToShortCut('Ctrl+Shift+C'); //set alternate shortcut while TTextStoryItem is being edited
        HUD.actionPaste.ShortCut := SafeTextToShortCut('Ctrl+Shift+V'); //set alternate shortcut while TTextStoryItem is being edited
      end
      else if TAllTextFrame.Shown then //disable editing shortcuts of app when AllTextFrame is shown
      begin
        HUD.actionCut.ShortCut := 0;
        HUD.actionCopy.ShortCut := 0;
        HUD.actionPaste.ShortCut := 0;
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

    //CheckProperOrientation; //don't do here, it gets annoying to show rotation modal popup upon navigation, plus some Stories may have StoryItems that are designed to be partially appear side-by-side to appear as books
  end;

  procedure TMainForm.CheckProperOrientation;
  begin
    var activeItem := ActiveStoryItem;
    if Assigned(activeItem) then
      TRotateFrame.ShowModal(Self, (Orientation <> activeItem.View.Orientation)); //Show or Hide rotation prompt based on orientation difference of MainForm and ActiveStoryItem.View
  end;

  {$endregion}

  procedure TMainForm.ActivateRootStoryItem;
  begin
    ActiveStoryItem := RootStoryItem; //there's always a RootStoryItem
  end;

  procedure TMainForm.ActivateParentStoryItem;
  begin
    var activeItem := ActiveStoryItem;
    if Assigned(activeItem) then
      activeItem.ActivateParentStoryItem; //this checks if ParentStoryItem is nil and does nothing in that case
  end;

  procedure TMainForm.ActivateHomeStoryItem;
  begin
    ActiveStoryItem := HomeStoryItem; //there's always a HomeStoryItem (code that loads the story takes care of that if none found, setting the RootStoryItem as HomeStoryItem)
  end;

  procedure TMainForm.ActivateAncestorStoryPoint;
  begin
    var activeItem := ActiveStoryItem;
    if Assigned(activeItem) then
      SetActiveStoryItemIfAssigned(activeItem.GetAncestorStoryPoint);
  end;

  procedure TMainForm.ActivatePreviousStoryPoint;
  begin
    var activeItem := ActiveStoryItem;
    if Assigned(activeItem) then
      SetActiveStoryItemIfAssigned(activeItem.PreviousStoryPoint);
  end;

  procedure TMainForm.ActivateNextStoryPoint;
  begin
    var activeItem := ActiveStoryItem;
    if Assigned(activeItem) then
      if activeItem.TagsMatched then //also checking if Tags are matched (all moveables with Tags are over non-moveables with same Tags and vice-versa) to proceed
        SetActiveStoryItemIfAssigned(activeItem.NextStoryPoint)
      else
        TLockFrame.ShowModal(Self); //warn user that they can't proceed (till Tags are matched)
  end;

  procedure TMainForm.NavigateUp;
  begin
    if (StoryMode <> EditMode) then //if not in Edit mode...
      ActivateAncestorStoryPoint //go to (nearest) AncestorStoryPoint, if any

    else //if in Edit mode...
      if FShiftDown then //if SHIFT pressed, go to RootStoryItem
        ActivateRootStoryItem
      else
        ActivateParentStoryItem //go to ParentStoryItem
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

  procedure TMainForm.StartInitialZoomTimer(const Delay: Cardinal = 100);
  begin
    with StoryTimer do
    begin
      Enabled := false;
      FTimerStarted := false; //used for timer to detect this is a special case
      Interval := Delay;
      Enabled := true; //used to zoom to Active or Home StoryItem after the form has fully sized (FormShow event doesn't do this properly)
    end;
  end;

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
    if Assigned(Value) then
    begin
      if not (Value.StoryPoint or Value.Home) then //not zooming down to items that are not StoryPoints or Home //must do last before the ZoomTo since we change the "Value" local variable
        Value := Value.GetAncestorStoryPoint; //if it returns nil it will result in zooming to the RootStoryPoint
      ZoomTo(Value);
    end;
  end;

  {$endregion}

  {$endregion}

  {$REGION 'Events'}

  {$region 'File actions'}

  procedure TMainForm.NewRootStoryItem; //Note: make sure any keyboard actions call the action event handlers since only those do confirmation
  begin
    RootStoryItemView := nil; //must do first to free the previous one (to avoid naming clashes)
    var newRootStoryItemView := TImageStoryItem.Create(Self); //Note: very old versions may expect a TBitmapImageStoryItem instead, ignoring them to keep design clean
    with newRootStoryItemView do
      begin
      //ClipChildren := true; //Empty RootStoryItem should maybe clip its children (not doing at SetRootStoryItem since we may have loaded a template there - anyway commenting out here too in case one wants to design a template from scratch)
      Size.Size := TSizeF.Create(ZoomFrame.Width, ZoomFrame.Height);
      EditMode := HUD.EditMode; //TODO: add EditMode property to IStory or use its originally intended mode one
      end;
    RootStoryItemView := newRootStoryItemView;
  end;

  //

  procedure TMainForm.HUDactionNewExecute(Sender: TObject);
  begin
    TApplication.Confirm(MSG_CONFIRM_CLEAR_STORY, //confirmation done only at the action level //Note: could also use Application.Confirm since Confirm is defined as a class function in ApplicationHelper (and those can be called on object instances of the respective class too)
      procedure(Confirmed: Boolean)
      begin
        if Confirmed and (not LoadDefaultDocument) then
          NewRootStoryItem;
      end
    );
  end;

  procedure TMainForm.HUDactionLoadExecute(Sender: TObject);
  begin
    //HUD.actionLoadExecute(Sender);

    var TempStoryItem := TStoryItem.Create(nil); //using TempStoryItem of TStoryItem type to show generic file dialog filter
    try
      var Filename := TempStoryItem.Options.ActLoad_GetFilename; //assuming this is blocking action
      if (Filename <> '') then
        begin
          if LoadFromFile(Filename) then //Note: just doing RootStoryItemView := TStoryItem.LoadNew(Filename) wouldn't work do font resizing since that won't do StartInitialZoomTimer
            ActivateHomeStoryItem; //set the HomeStoryItem (if not any the RootStoryItem will have been set as such by SetRootStoryView) to active (not doing this when loading saved app state)

          //TODO: OLD-CODE-LINE-REMOVE? //RootStoryItemView := RootStoryItemView; //repeat calculations to adapt ZoomFrame.ScaledLayout size //TODO: when RootStoryItemViewResized starts working this shouldn't be needed here anymore
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

  {$region 'Edit-mode actions'}

  procedure TMainForm.HUDEditModeChanged(Sender: TObject; const Value: Boolean);
  begin
    if Value then
      StoryMode := EditMode
    else
      StoryMode := TStoryMode.InteractiveStoryMode; //TODO: should remember previous mode to restore or make EditMode a separate situation
  end;

  {$region 'Add actions'}

  procedure TMainForm.AddChildStoryItem(const TheStoryItemClass: TStoryItemClass; const TheName: String);
  begin
    if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit; //only in Edit mode and when an ActiveStoryItem is assigned

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

  //

  procedure TMainForm.HUDactionAddExecute(Sender: TObject);
  begin
    if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit; //only in Edit mode and when an ActiveStoryItem is assigned

    ActiveStoryItem.Options.ActAdd;
    UpdateStructureView; //TODO: should instead have some notification from inside a StoryItem towards the StructureView that children were added to it (similar to how StructureView listens for children removal)
  end;

  procedure TMainForm.HUDactionAddImageStoryItemExecute(Sender: TObject); //TODO: should change to add a VisualStoryItem that will support any kind of visual item
  begin
    if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit; //only in Edit mode and when an ActiveStoryItem is assigned

    AddChildStoryItem(TImageStoryItem, 'ImageStoryItem'); //will also update the StructureView //Note: very old versions may expect a TBitmapImageStoryItem instead, ignoring them to keep design clean
    //problem is they don't show border when in non-Edit mode, but could have option to show border for any StoryItem and/or style the border color/width, clip children etc. to make like older PanelStoryItem
  end;

  procedure TMainForm.HUDactionAddTextStoryItemExecute(Sender: TObject);
  begin
    if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit; //only in Edit mode and when an ActiveStoryItem is assigned

    AddChildStoryItem(TTextStoryItem, 'TextStoryItem'); //will also update the StructureView
  end;

  {$endregion}

  {$region 'Edit actions'}

  procedure TMainForm.DeleteActiveStoryItem; //Note: make sure any keyboard actions call the action event handlers since only those do confirmation
  begin
    if not Assigned(ActiveStoryItem) then exit;

    if ActiveStoryItem.IsRoot then
      NewRootStoryItem //deleting the RootStoryItem via "NewRootStoryItem", but not via "HUD.actionNew.Execute" since that also tries "LoadDefaultDocument" first //RootStoryItem change updates StructureView //note: confirmation is only done at "HUDactionCutExecute"
    else
      ActiveStoryItem.Delete; //this makes ParentStoryItem active (which updates StructureView)
  end;

  procedure TMainForm.CutActiveStoryItem; //Note: make sure any keyboard actions call the action event handlers since only those do confirmation
  begin
    if not Assigned(ActiveStoryItem) then exit;

    if ActiveStoryItem.IsRoot then
      begin
        ActiveStoryItem.Copy; //needed to simulate "Cut"
        NewRootStoryItem; //deleting the RootStoryItem via "NewRootStoryItem", but not via "HUD.actionNew.Execute" since that also tries "LoadDefaultDocument" first //RootStoryItem change updates StructureView //note: confirmation is only done at "HUDactionCutExecute"
      end
    else
      ActiveStoryItem.Cut; //this makes ParentStoryItem active (which updates StructureView)
  end;

  //

  procedure TMainForm.HUDactionDeleteExecute(Sender: TObject);
  begin
    if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit; //only in Edit mode (plus safety check for ActiveStoryItem)

    var msg: String;
    if ActiveStoryItem.IsRoot then
      msg := MSG_CONFIRM_CLEAR_STORY
    else
      msg := MSG_CONFIRM_DELETION;

    //Always confirming for destructive actions like deletion
    TApplication.Confirm(msg, //confirmation done only at the action level //Note: could also use Application.Confirm since Confirm is defined as a class function in ApplicationHelper (and those can be called on object instances of the respective class too)
      procedure(Confirmed: Boolean)
      begin
        if Confirmed then
         DeleteActiveStoryItem;
      end
    );
  end;

  procedure TMainForm.HUDactionCutExecute(Sender: TObject);
  begin
    if FShiftDown then //if Shift key is pressed...
    begin
      HUDactionDeleteExecute(Sender); //...do Delete instead of Cut
      exit;
    end;

    if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit; //only in Edit mode (plus safety check for ActiveStoryItem)

    //Not considering Cut a destructive action since one can Paste back. However when cutting the RootStoryItem, since it has no parent to be activated and a new RootStoryItem will be created, paste back means it will become a child of the new RootStoryItem, so asking for confirmation only when cutting the RootStoryItem
    if ActiveStoryItem.IsRoot then
      TApplication.Confirm(MSG_CONFIRM_CLEAR_STORY, //confirmation done only at the action level //Note: could also use Application.Confirm since Confirm is defined as a class function in ApplicationHelper (and those can be called on object instances of the respective class too)
        procedure(Confirmed: Boolean)
        begin
          if Confirmed then
            CutActiveStoryItem;
        end
      )
    else
      CutActiveStoryItem; //no confirmation for cutting non-Root StoryItems
  end;

  procedure TMainForm.HUDactionCopyExecute(Sender: TObject);
  begin
    if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit; //only in Edit mode and when an ActiveStoryItem is assigned
    ActiveStoryItem.Copy;
  end;

  procedure TMainForm.HUDactionPasteExecute(Sender: TObject);
  begin
    if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit; //only in Edit mode and when an ActiveStoryItem is assigned

    ActiveStoryItem.Paste;
    UpdateStructureView; //TODO: should do similar to deletion by somehow notifying from inside the StoryItem itself the StructureView that items have been added
  end;

  {$endregion}

  {$region 'Flip actions'}

  procedure TMainForm.HUDactionFlipHorizontallyExecute(Sender: TObject);
  begin
    if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit; //only in Edit mode and when an ActiveStoryItem is assigned

    ActiveStoryItem.FlippedHorizontally := not ActiveStoryItem.FlippedHorizontally;
    UpdateStructureView; //TODO: should maybe only update the tree of thumbs from the ActiveStoryItem up to the root by somehow notifying from inside the StoryItem itself the StructureView our graphics have changed
  end;

  procedure TMainForm.HUDactionFlipVerticallyExecute(Sender: TObject);
  begin
    if not (HUD.EditMode and Assigned(ActiveStoryItem)) then exit; //only in Edit mode and when an ActiveStoryItem is assigned

    ActiveStoryItem.FlippedVertically := not ActiveStoryItem.FlippedVertically;
    UpdateStructureView; //TODO: should maybe only update the tree of thumbs from the ActiveStoryItem up to the root by somehow notifying from inside the StoryItem itself the StructureView our graphics have changed
  end;

  {$endregion}

  {$region 'Color actions'}

  procedure TMainForm.HUDcomboForeColorChange(Sender: TObject);
  begin
    var LActive := ActiveStoryItem;
    if not Assigned(LActive) then exit;

    LActive.ForegroundColor := HUD.comboForeColor.Color; //TODO: could check for some interface (IForegroundColor interface)
  end;

  procedure TMainForm.HUDcomboBackColorChange(Sender: TObject);
  begin
    var LActive := ActiveStoryItem;
    if not Assigned(LActive) then exit;

    LActive.BackgroundColor := HUD.comboBackColor.Color; //TODO: could check for some interface (IBackgroundColor interface)
  end;

  {$endregion}

  {$region 'Options action'}

  procedure TMainForm.HUDactionOptionsExecute(Sender: TObject);
  begin
    if Assigned(ActiveStoryItem) then
      ActiveStoryItem.Options.ShowPopup;
  end;

  {$endregion}

  {$endregion}

  {$region 'View actions'}

  procedure TMainForm.UpdateStructureView;
  begin
    Log('UpdateStructureView');
    StartTiming; //doing nothing in non-DEBUG builds

    if not HUD.StructureVisible then
      Log('Ignoring UpdateStructureView, currently hidden')
    else
    begin
      if (StoryMode <> EditMode) then
        StructureView.FilterMode := tfFlatten
      else
        StructureView.FilterMode := tfPrune;

      StructureView.GUIRoot := RootStoryItemView;

      if Assigned(ActiveStoryItem) then
        StructureView.SelectedObject := ActiveStoryItem.View;
    end;

    StopTiming_msec; //this will Log elapsed msec at DEBUG builds
  end;

  //

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

  procedure TMainForm.HUDTargetsVisibleChanged(Sender: TObject; const Value: Boolean);
  begin
    if HUD.TargetsVisible then
      if Assigned(ActiveStoryItem) then
        ActiveStoryItem.TargetsVisible := Value;
  end;

  {$endregion}

  {$region 'Light-Dark mode actions'}

  procedure TMainForm.HUDactionNextThemeExecute(Sender: TObject);
  begin
    if not Assigned(Themes) then exit; //Note: safety check (useful when debugging with loading of Themes data module commented-out [had some issues loading the main form on Android when that was loaded too])

    var switchToLightMode: Boolean;
    with Themes do
    begin
      switchToLightMode := DarkTheme.UseStyleManager; //don't use "LightTheme.UseStyleManager" this seems to return false, even though it's set to true in the designer (the default style is white)
      LightTheme.UseStyleManager := switchToLightMode;
      DarkTheme.UseStyleManager := not switchToLightMode;
    end;

    if switchToLightMode then //the code above isn't enough to switch theme, so switching the MainForm's theme directly //TODO: this won't work for extra forms, try TStyleManager instead
      StyleBook := Themes.LightTheme
    else
      StyleBook := Themes.DarkTheme;
  end;

  {$endregion}

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

  procedure TMainForm.HUDUseStoryTimerChanged(Sender: TObject; const Value: Boolean);
  begin
    with StoryTimer do
    begin
      FTimerStarted := Value;
      Enabled := Value;
      Interval := 8000; //proceed ever 8sec (TODO: should be easily adjustable)
    end;
  end;

  procedure TMainForm.StoryTimerTimer(Sender: TObject); //TODO: should show some timer animation at top-right when the story timer is enabled (AnimatedStoryMode)
  begin
    //special case used at app startup
    if not FTimerStarted then //TODO: should check if we loaded from saved state and remember if we were playing the timer and continue [see CCR.PrefsIniFile github repo maybe to keep app settings])
    begin
      StoryTimer.Enabled := false;

      ZoomTo; //TODO: temp fix, else showing some undrawn text when loading from temp state and till one [un]zooms or resizes (seems 0.3.1 didn't have the issue this fixes, but 0.3.0 and 0.3.2+ had it)
      ZoomToActiveStoryPointOrHome; //needed upon app first loading to ZoomTo Active StoryPoint or Home from loaded saved state

      //ActivateHomeStoryItem; //apply the home //TODO: text doesn't render yet at this point

      //CheckCommandLineActions;

      exit;
    end;

    ActivateNextStoryPoint;

    if Assigned(ActiveStoryItem) and ActiveStoryItem.Home then
      HUD.UseStoryTimer := false; //TODO: should instead define EndStoryPoint(s) and stop the timer once the end is reached
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

  {$region 'Keyboard'}

  procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState); //also see HUD Actions' shortcuts
  begin
    case Key of
      vkSHIFT:
        FShiftDown := True;

      vkEscape:
        NavigateUp;

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

      vkF1:
        ShowHelp; //needed since the Help key (that has the F1 key accelerator) was moved into About box to save toolbar space on small screens

      vkF2:
        HUDactionSaveExecute(Self);

      vkF3:
        HUDactionLoadExecute(Self);

      vkF4:
        HUDactionNewExecute(Self);

      vkF5:
        HUD.UseStoryTimer := not HUD.UseStoryTimer; //toggle StoryTimer

      vkF6:
        HUD.StructureVisible := not HUD.StructureVisible; //toggle StructureView visibility

      vkF7:
        HUD.EditMode := not HUD.EditMode; //toggle EditMode

      vkF8:
        if ssCtrl in Shift then //Ctrl+F8 - making sure users don't press this by mistake since it will also close the app
        begin
          ActivateRootStoryItem;
          ShowMessage('Exporting to HTML and exiting'); //this seems to be needed (TThread.Queue doesn't do the trick) to always save TextStoryItem text in exported images
          TThread.Queue(nil, procedure
            begin
              RootStoryItem.SaveHTML(StorySource + '.html');
              Application.Terminate;
            end
          )
        end;

      vkF9:
        TAllTextFrame.ShowModal(MainForm, ActiveStoryItem); //has [X] button to close itself //Note: to see all text of the Story can 1st navigate to RootStoryItem in EditMode

      vkF10:
        HUD.BtnMenu.Action.Execute; //toggle buttons visibility

      vkF11:
        ToggleObjectDebuggerVisibility; //only available for DEBUG builds

      //vkF12: //don't use F12 key, when debugging with Delphi IDE it breaks into the debugger

    end;
  end;

  procedure TMainForm.FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
  begin
    case Key of
      vkSHIFT:
        FShiftDown := False;
    end;
  end;

{$endregion}

  {$region 'Idle'}

  procedure TMainForm.Idle(Sender: TObject; var Done: Boolean); //only process command-line actions after app enters idle mode (to be sure its GUI is ready to capture screenshots) //TODO: probably there exists other better event, sometimes have to move the mouse for command-line processing to occur
  begin
    if not FCheckedCommandLineActions then
    begin
      FCheckedCommandLineActions := true; //should process command-line once
      CheckCommandLineActions; //do the processing
    end;
  end;

  {$endregion}

  {$ENDREGION}

  {$region 'Loading / SavedState'}

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

        StartInitialZoomTimer(100); //TODO: maybe the timer value is low //TODO: this still won't fix the font autosizing when story is loaded from saved state with Active=true at some of its StoryPoints

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
    var InputFileStream := TFileStream.Create(Filepath,  fmOpenRead {or fmShareDenyNone}); //TODO: fmShareDenyNote probably needed for Android
    try
      result := LoadFromStream(InputFileStream);
      StorySource := Filepath;
    finally
      FreeAndNil(InputFileStream);
    end;
  end;

  function TMainForm.LoadFromUrl(const Url: String): Boolean; //TODO: should add LoadFromUrl and AddFromUrl to TStoryItem too
  begin
    try
      TWaitFrame.ShowModal(Self);

      TURLStream.Create(Url,
        procedure(AStream: TStream)
        begin
          Log('Started download');
          LoadFromStream(AStream); //probably it can start loading while the content is coming
          Log('Finished download');
          TWaitFrame.ShowModal(Self, false);
          StorySource := Url;
        end,
        true, //ASynchronizeProvide: call the anonymous proc (AProvider parameter) in the context of the main thread //IMPORTANT (if not done various AV errors occur later: we shouldn't touch the UI from other than the main thread)
        true //free on completion
      );

      result := true; //assuming download will progress fine
    except
      on e: Exception do
      begin
        TWaitFrame.ShowModal(Self, false);
        ShowMessageFmt(ERR_DOWNLOAD, [e.Message]);
        result := false; //probably no network connection etc.
      end;
    end;
  end;

  function TMainForm.LoadFromFileOrUrl(const PathOrUrl: String): Boolean;
  begin
    if IsURI(PathOrUrl) then
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
    Log('LoadSavedState');

    With SaveState do
    begin
      Name := 'SavedState.readcom';
      StoragePath := TPath.GetHomePath;
      result := LoadFromStream(Stream, false); //don't ActivateHome, keep last Active one (needed in case the OS brought down the app and need to continue from where we were from saved state)
      StorySource := StoragePath;
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
    Log('SaveCurrentState');

    with SaveState do
    begin
      Name := 'SavedState.readcom';
      StoragePath := TPath.GetHomePath;

      Stream.Clear;

      var TheRootStoryItemView := RootStoryItemView;
      if Assigned(TheRootStoryItemView) then
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
  end;

  procedure TMainForm.FormSaveState(Sender: TObject);
  begin
    HUD.EditMode := false; //make sure we exit EditMode, else child items of ActiveStoryItem are saved as disabled
    SaveCurrentState;
  end;

  {$endregion}

  {$region 'Command-line'}

  procedure TMainForm.CheckCommandLineActions;

    procedure CheckSaveThumbnail;
    begin
      if SaveThumbnail and (StorySource <> '') then
      begin
        var ThumbPath: String;
        if IsURI(StorySource) then
          ThumbPath := 'Thumb.png' //save in current folder
        else
          ThumbPath := StorySource + '.png';

        ActiveStoryItem.SaveThumbnail(ThumbPath, ThumbnailMaxSize, ThumbnailMaxSize); //bounding box for thumbnail fitting is a square
        Application.Terminate; //close the app after saving the thumb
      end;
    end;

    procedure CheckSaveHtml;
    begin
      if SaveHtml and (StorySource <> '') then
      begin
        var HtmlPath: String;
        if IsURI(StorySource) then
          HtmlPath := 'Story.html' //save in current folder
        else
          HtmlPath := StorySource + '.html';

        RootStoryItem.SaveHtml(HtmlPath, HtmlImageMaxSize, HtmlImageMaxSize); //bounding box for HTML image fitting is a square
        Application.Terminate; //close the app after saving the HTML
      end;
    end;

  begin
    TMemo.DisableFontSizeToFit := true; //don't do font fitting, seems to result in some TextStoryItems not rendering in resulting snapshot images
    CheckSaveThumbnail;
    CheckSaveHtml;
  end;

  procedure TMainForm.CheckReplaceStoryAllText;
  begin
    (* //TODO
    if LoadAllText or SaveAllText and (StorySource <> '') then
    begin
      var CaptionsPath: String;
      if IsURI(StorySource) then
        CaptionsPath := 'Captions.txt' //save in current folder
      else
        CaptionsPath := StorySource + '.txt';

      if LoadAllText then
        ActiveStoryItem.LoadAllText(CaptionsPath)
      else if SaveAllText then
        ActiveStoryItem.SaveAllText(CaptionsPath);
      //TODO: add actions and parameters to localize to TARGET language (ideally should autodetect source or allow to define what the SOURCE is)
    end;
    *)
  end;

  {$endregion}

end.

