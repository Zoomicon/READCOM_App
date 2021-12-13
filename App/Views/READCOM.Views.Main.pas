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

  protected
    { StructureView }
    procedure StructureViewSelection(Sender: TComponent; Selection: TObject);

    { SavedState}
    procedure LoadSavedStateOrNewStory;
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
  InitHUD;
  LoadSavedStateOrNewStory;
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
  if Assigned(TheRootStoryItemView) then
    begin
    //TheStory.Parent := nil; //shouldn't be needed
    FreeAndNil(TheRootStoryItemView); //destroy the old RootStoryItem //FREE THE CONTROL, DON'T FREE JUST THE INTERFACE
    end;

  //Add new story
  with Value do
  begin
    //AutoSize := true; //the Root StoryItem should be expandable
    Parent := ZoomFrame.ScaledLayout; //don't use ZoomFrame as direct parent
    Align := TAlignLayout.Contents;
    Align := TAlignLayout.Center;
    ActiveStoryItem := Value;
  end;
end;

{$endregion}

{$region 'ActiveStoryItem'}

function TMainForm.GetActiveStoryItem: IStoryItem;
begin
  result := TStoryItem.ActiveStoryItem;
end;

procedure TMainForm.SetActiveStoryItem(const Value: IStoryItem);
begin
  TStoryItem.ActiveStoryItem := Value;
  ZoomTo(Value);
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
  result := RootStoryItem.StoryMode; //TODO: propage or make class attribute?
end;

procedure TMainForm.SetStoryMode(const Value: TStoryMode);
begin
  RootStoryItem.StoryMode := Value; //TODO: propage or make class attribute?
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

procedure TMainForm.HUDactionLoadExecute(Sender: TObject);
begin
  //HUD.actionLoadExecute(Sender);

  RootStoryItem.Options.ActLoad;
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
  var frameInfo := HUD.MultiViewFrameStand.GetFrameInfo<TStructureView>;
  with frameInfo.Frame do
  begin
    ShowOnlyClasses := TClassList.Create([TStoryItem]);
    ShowNames := false;
    ShowTypes := false;
    GUIRoot := RootStoryItemView;
    OnSelection := StructureViewSelection;
  end;
  frameInfo.Show;
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

{$endregion}

procedure TMainForm.StructureViewSelection(Sender: TComponent; Selection: TObject);
begin
  ZoomFrame.ZoomTo(TControl(Selection));
end;

{$ENDREGION}

{$region 'SavedState'}

procedure TMainForm.LoadSavedStateOrNewStory;
begin
  if (not LoadSavedState) then //Note: if it keeps on failing at load comment out this line for one run //TODO: shouldn't need to do that
    begin
    var TheStory := TPanelStoryItem.Create(Self);
    TheStory.Size.Size := TSizeF.Create(ZoomFrame.Width, ZoomFrame.Height);
    RootStoryItemView := TheStory;
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
