//Description: StructureView Control for FMX
//Author: George Birbilis (http://zoomicon.com)

unit Zoomicon.Introspection.FMX.StructureView;

interface

uses
  System.Classes, //for TComponent, GroupDecendentsWith, RegisterComponents
  System.Contnrs, //for TClassList
  System.Generics.Collections, //for TList
  System.SysUtils,
  System.Types,
  System.Variants,
  System.ImageList,
  FMX.Controls, //for TControl
  FMX.Graphics,
  FMX.Forms, //for TFrame
  FMX.ImgList, //for TImageList
  FMX.Layouts,
  FMX.TreeView,
  FMX.Types; //for RegisterFmxClasses

type
  TTreeFilterMode = (tfFlatten, tfPrune);

const
  {$region 'Defaults'}

  DEFAULT_SHOW_ONLY_VISIBLE = true;
  DEFAULT_SHOW_ONLY_NAMED = true;
  DEFAULT_SHOW_NAMES = false;
  DEFAULT_SHOW_TYPES = false;
  DEFAULT_SHOW_HINT_NAMES = true;
  DEFAULT_SHOW_HINT_TYPES = false;
  DEFAULT_FILTER_MODE = tfPrune;
  DEFAULT_DRAGDROP_REORDER = false; //acting as structure viewer, not editor by default
  DEFAULT_DRAGDROP_REPARENT = false; //acting as structure viewer, not editor by default
  DEFAULT_DRAGDROP_SELECTTARGET = true; //on drop, target is selected

  {$endregion}

type
  //TClassList = TList<TClass>; //using old-style (non Generic) "TClassList" from System.Contnrs instead

  TRestructuringOperation = (roReorderedChildren, roAddedChild, roRemovedChild);

  TSelectionEvent = procedure(Sender: TObject; const Selection: TObject) of object;
  TRestructuringEvent = procedure(Sender: TObject; const RestructuredChild: TObject; const RestructuredParent: TObject; const Operation: TRestructuringOperation) of object;
  TShowFilterEvent = procedure(Sender: TObject; const TheObject: TObject; var ShowObject: Boolean) of object;

  {$REGION 'TStructureView' --------------------------------------------------------}

  TStructureView = class(TFrame, IFreeNotification)
    TreeView: TTreeView;
    ImageList: TImageList;
    procedure TreeViewChange(Sender: TObject);
    procedure TreeViewDragChange(SourceItem, DestItem: TTreeViewItem; var Allow: Boolean);
  protected
    FGUIRoot: TControl;
    {ShowXX}
    FShowOnlyClasses: TClassList;
    FShowOnlyNamed: Boolean;
    FShowOnlyVisible: Boolean;
    FShowNames: Boolean;
    FShowTypes: Boolean;
    FShowHintNames: Boolean;
    FShowHintTypes: Boolean;
    FFilterMode: TTreeFilterMode;
    {DragDropXX}
    FDragDropReorder: Boolean;
    FDragDropReparent: Boolean;
    FDragDropSelectTarget: Boolean;
    {Events}
    FOnSelection: TSelectionEvent;
    FOnRestructuring: TRestructuringEvent;
    FOnShowFilter: TShowFilterEvent;

    {GUIRoot}
    procedure SetGUIRoot(const Value: TControl); virtual;
    procedure LoadTreeView; virtual;
    {ItemHeight}
    function GetItemHeight: Single; virtual;
    procedure SetItemHeight(const Value: Single); virtual;
    {ShowXX}
    function GetShowCheckboxes: Boolean; virtual;
    procedure SetShowCheckboxes(const Value: Boolean); virtual;
    procedure SetShowOnlyClasses(const Value: TClassList); virtual;
    procedure SetShowOnlyVisible(const Value: Boolean); virtual;
    procedure SetShowOnlyNamed(const Value: Boolean); virtual;
    procedure SetShowNames(const Value: Boolean); virtual;
    procedure SetShowTypes(const Value: Boolean); virtual;
    procedure SetShowHintNames(const Value: Boolean); virtual;
    procedure SetShowHintTypes(const Value: Boolean); virtual;
    {DragDropXX}
    procedure SetDragDropReorder(const Value: Boolean); virtual;
    procedure SetDragDropReparent(const Value: Boolean); virtual;
    {SelectedItem}
    function GetSelectedObject: TObject; virtual;
    procedure SetSelectedObject(const Value: TObject); virtual;

    {Notifications}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override; //no need to override the method "FreeNotification", TComponent and TFmxObject implement IFreeNotification, the latter calling Notification with Operation=opRemove

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    //Properties//
    property GUIRoot: TControl read FGUIRoot write SetGUIRoot;

    property ItemHeight: Single read GetItemHeight write SetItemHeight;
    property ShowCheckboxes: Boolean read GetShowCheckboxes write SetShowCheckboxes default false;
    property ShowOnlyClasses: TClassList read FShowOnlyClasses write SetShowOnlyClasses; //default nil
    property ShowOnlyVisible: Boolean read FShowOnlyVisible write SetShowOnlyVisible default DEFAULT_SHOW_ONLY_VISIBLE;
    property ShowOnlyNamed: Boolean read FShowOnlyNamed write SetShowOnlyNamed default DEFAULT_SHOW_ONLY_NAMED;
    property ShowNames: Boolean read FShowNames write SetShowNames default DEFAULT_SHOW_NAMES;
    property ShowTypes: Boolean read FShowTypes write SetShowTypes default DEFAULT_SHOW_TYPES;
    property ShowHintNames: Boolean read FShowHintNames write SetShowHintNames default DEFAULT_SHOW_HINT_NAMES;
    property ShowHintTypes: Boolean read FShowHintTypes write SetShowHintTypes default DEFAULT_SHOW_HINT_TYPES;
    property FilterMode: TTreeFilterMode read FFilterMode write FFilterMode default DEFAULT_FILTER_MODE;

    property DragDropReorder: Boolean read FDragDropReorder write SetDragDropReorder default DEFAULT_DRAGDROP_REORDER;
    property DragDropReparent: Boolean read FDragDropReparent write SetDragDropReparent default DEFAULT_DRAGDROP_REPARENT;
    property DragDropSelectTarget: Boolean read FDragDropSelectTarget write FDragDropSelectTarget default DEFAULT_DRAGDROP_SELECTTARGET;

    property SelectedObject: TObject read GetSelectedObject write SetSelectedObject; //default nil

    //Events//
    property OnShowFilter: TShowFilterEvent read FOnShowFilter write FOnShowFilter;
    property OnSelection: TSelectionEvent read FOnSelection write FOnSelection;
    property OnRestructuring: TRestructuringEvent read FOnRestructuring write FOnRestructuring;
  end;

  {$ENDREGION ......................................................................}

procedure Register;

implementation
  uses
    System.UITypes, //for TDragMode
    System.Rtti, //for TValue
    Zoomicon.Helpers.RTL.ClassListHelpers, //for TClassList.FindClassOf
    Zoomicon.Helpers.FMX.Controls.ControlHelper, //for TControl.MakeThumbnail
    Zoomicon.Helpers.FMX.ImgList.ImageListHelpers, //for TImageListAddBitmapHelper
    Zoomicon.Helpers.FMX.TreeView.TreeViewHelpers; //for TTreeViewItemSearchHelper

{$R *.fmx}

{$REGION 'TStructureView'}

{$region 'Creation / Destruction '}

constructor TStructureView.Create(AOwner: TComponent);

  procedure InitImageList;
  begin
    with ImageList do
    begin
      Stored := false; //don't store state, should use state from designed .FMX resource
      SetSubComponent(true);
    end;
  end;

  procedure InitTreeView;
  begin
    with TreeView do
    begin
      Stored := false; //don't store state, should use state from designed .FMX resource
      SetSubComponent(true);
    end;
  end;

  procedure InitDragDrop; //Note: Delphi 11 doesn't allow us to use "inline" here (can't access "Self")
  begin
    SetDragDropReorder(DEFAULT_DRAGDROP_REORDER);
    SetDragDropReparent(DEFAULT_DRAGDROP_REPARENT);
  end;

  procedure InitFilters;
  begin
    FShowOnlyClasses := nil;
    FShowOnlyVisible := DEFAULT_SHOW_ONLY_VISIBLE;
    FShowOnlyNamed := DEFAULT_SHOW_ONLY_NAMED;
    FShowNames := DEFAULT_SHOW_NAMES;
    FShowTypes := DEFAULT_SHOW_TYPES;
    FShowHintNames := DEFAULT_SHOW_HINT_NAMES;
    FShowHintTypes := DEFAULT_SHOW_HINT_TYPES;
    FFilterMode := DEFAULT_FILTER_MODE;
  end;

begin
  inherited;

  InitImageList;
  InitTreeView;

  InitDragDrop;
  InitFilters;
end;

destructor TStructureView.Destroy;
begin
  TreeView.Clear;
  ImageList.ClearCache;
  //FreeAndNil(TreeView); //should be done automatically when "inherited" is called
  //FreeAndNil(Images); //should be done automatically when "inherited" is called
  FreeAndNil(ShowOnlyClasses); //freeing any TClassList that had been allocated and assigned to the "ShowOnlyClasses" property to avoid external code having to do it (obviously that TClassList shouldn't be assigned elsewhere)

  inherited; //do last
end;

{$endregion}

{$region 'Properties' ---------------------------------------------------}

{$region 'GUIRoot'}

procedure TStructureView.SetGUIRoot(const Value: TControl);
begin
  FGUIRoot := Value;
  LoadTreeView;
end;

{$endregion}

{$region 'ItemSize'}

function TStructureView.GetItemHeight: Single;
begin
  result := TreeView.ItemHeight;
end;

procedure TStructureView.SetItemHeight(const Value: Single);
begin
  TreeView.ItemHeight := Value;
end;

{$endregion}

{$region 'ShowXX'}

function TStructureView.GetShowCheckboxes: Boolean;
begin
  result := TreeView.ShowCheckBoxes;
end;

procedure TStructureView.SetShowCheckboxes(const Value: Boolean);
begin
  TreeView.ShowCheckboxes := Value;
end;

procedure TStructureView.SetShowOnlyClasses(const Value: TClassList);
begin
  FShowOnlyClasses := Value;
  LoadTreeView;
end;

procedure TStructureView.SetShowOnlyVisible(const Value: Boolean);
begin
  FShowOnlyVisible := Value;
  LoadTreeView;
end;

procedure TStructureView.SetShowOnlyNamed(const Value: Boolean);
begin
  FShowOnlyNamed := Value;
  LoadTreeView;
end;

procedure TStructureView.SetShowNames(const Value: Boolean);
begin
  FShowNames := Value;
  LoadTreeView;
end;

procedure TStructureView.SetShowTypes(const Value: Boolean);
begin
  FShowTypes := Value;
  LoadTreeView;
end;

procedure TStructureView.SetShowHintNames(const Value: Boolean);
begin
  FShowHintNames := Value;
  LoadTreeView;
end;

procedure TStructureView.SetShowHintTypes(const Value: Boolean);
begin
  FShowHintTypes := Value;
  LoadTreeView;
end;

{$endregion}

{$region 'DragDropXX'}

procedure TStructureView.SetDragDropReorder(const Value: Boolean);
begin
  FDragDropReorder := Value;
  TreeView.AllowDrag := FDragDropReorder or FDragDropReparent;
end;

procedure TStructureView.SetDragDropReparent(const Value: Boolean);
begin
  FDragDropReparent := Value;
  TreeView.AllowDrag := FDragDropReorder or FDragDropReparent;
end;

{$endregion}

{$region 'SelectedObject'}

function TStructureView.GetSelectedObject: TObject;
begin
  var selectedItem := TreeView.Selected;
  if Assigned(selectedItem) then
    result := selectedItem.TagObject
  else
    result := nil;
end;

procedure TStructureView.SetSelectedObject(const Value: TObject);
begin
  if Assigned(Value) then
    TreeView.Selected := TreeView.FindObject(Value)
  else
    TreeView.Selected := nil; //clear selection (instead of searching for "TagObject = nil" at the tree items
end;

{$endregion}

{$endregion .............................................................}

{$region 'Methods' ------------------------------------------------------}

procedure TStructureView.LoadTreeView;

  procedure LoadTreeItemChild(const TheControl: TControl; const TheParent: TFmxObject; const IconHeight: Single);
  begin
    if not Assigned(TheControl) then exit;

    var LStructureView := Self;

    var FilterOut :=
      (Assigned(FShowOnlyClasses) and (FShowOnlyClasses.Count > 0) and (FShowOnlyClasses.FindClassOf(TheControl, false) < 0)) or //if FShowOnlyClasses is empty ignore it
      (FShowOnlyVisible and (not TheControl.Visible)) or
      (FShowOnlyNamed and (TheControl.Name = '')); //helps ignore style-related controls (unnamed), including their children

    if (not FilterOut) and Assigned(FOnShowFilter) then //do custom filtering if TheControl is not filtered out already
    begin
      var ShowControl := true;
      FOnShowFilter(LStructureView, TheControl, ShowControl);
      FilterOut := not ShowControl;
    end;

    if FilterOut then
    begin
      if (FFilterMode = tfFlatten) then
      begin
        BeginUpdate;
        //Load items recursively (depth-first)//
        for var ChildControl in TheControl.Controls do
          LoadTreeItemChild(ChildControl, TheParent, IconHeight); //skip the item but add its children to the parent (Flatten subtree)
        EndUpdate;
      end;

      exit; //for (FilterMode = tfPrune) we just need to skip this subtree
    end;

    var TreeItem := TTreeViewItem.Create(TheParent); //use Parent tree node as the Owner too
    with TreeItem do
    begin
      //Keep a reference to TheControl at the TreeViewItem
      TagObject := TheControl;

      //Listen for freeing of TheControl to remove from the tree
      //TheControl.RemoveFreeNotify(LStructureView); //make sure we remove if we were previously set as listener (won't throw error if not found as such), else we'll get added multiple times which delays more and more everytime the tree is repopulated
      //TheControl.AddFreeNotify(LStructureView);
      TheControl.RemoveFreeNotification(LStructureView);
      TComponent(TheControl).FreeNotification(LStructureView);

      //Graphics-related code:

      BeginUpdate;

      //Nest the TreeStoryItem as needed
      Parent := TheParent;

      var W := TheControl.Width;
      var H := TheControl.Height;
      if (W <> 0) and (H <> 0) then //If TheControl has non-zero dimensions...
      begin
        //...add screenshot of TheControl to ImageList
        var ThumbnailSize := TSizeF.Create(W*(IconHeight/H), IconHeight);

        var ControlThumbnail := TheControl.MakeThumbnail(Round(ThumbnailSize.Width), Round(ThumbnailSize.Height)); //could use instead of TheControl.MakeScreenshot, to not make a big image even temporarily (however if drawing code caches settings that are lost when changing drawing size it has higher cost and my persist wrong values [say FontSize if Font AutoFitting algorithm is used] if called last)

        //TODO: check which of these two strategies is better (since TextStoryItem caches the last height for which its autofontfit was calculated, it may causing extra autofontfit calculations)

        //var ControlThumbnail := TheControl.MakeScreenshot; //this wastes more memory, but if drawing code costs when drawing resized, then its better to grab a screenshot and existing size...
        //ControlThumbnail.Resize(Round(ThumbnailSize.Width), Round(ThumbnailSize.Height)); //...and then resize so that ImageList doesn't copy the big image and waste memory //Note: there's also CreateThumbnail function, but would need to FreeAndNil the result of MakeScreenshot separately

        try
          var imgIndex := ImageList.Add(ControlThumbnail); //this will copy from the bitmap //Note: this returns -1 if BitmapWith or BitmapHeight is 0

          //...set the TreeViewItem's image from the ImageList, and scale the glyph appropriately
          ImageIndex := imgIndex; //if -1 then won't show image
          if (imgIndex <> -1) then //see note above, items with 0 width or height won't have thumb added to the ImageList
          begin
            //var img := ImageList.Source.Items[imgIndex].MultiResBitmap; //don't need to get this to ask for Size, have already calculated ThumbnailSize above
            StylesData['glyphstyle.Size.Size'] := TValue.From(ThumbnailSize);
                                                  //TValue.From(TSizeF.Create(img.Width*(IconHeight/img.Height), IconHeight));
          end;
        finally
          FreeAndNil(ControlThumbnail); //MUST FREE THE BITMAP ELSE WE HAVE VARIOUS MEMORY LEAKS
        end;
      end;

      //Titles//
      if FShowNames then Text := TheControl.Name;
      if FShowTypes then Text := Text + ': ' + TheControl.ClassName;

      //Hints//
      if FShowHintNames then Hint := TheControl.Name;
      if FShowHintTypes then Hint := Hint + ': ' + TheControl.ClassName;
      ShowHint := true; //always show hint if such has been set (as a result of FShowHintNames or FShowHintTypes)

      //Load items recursively (depth-first)//
      for var ChildControl in TheControl.Controls do
        try
          LoadTreeItemChild(ChildControl, TreeItem, IconHeight);
        except
          on E: Exception do
            ShowException(E, @TStructureView.LoadTreeView); //TODO: maybe should do one try-catch for the whole for loop to avoid multiple error messages?
        end;

      EndUpdate;
    end;
  end;

begin
  BeginUpdate;

  TreeView.Clear;

  with ImageList do
  begin
    Dormant := false; //this is the default

    ClearCache;
    CacheSize := 100; //doesn't allow to set CacheSize to 0

    Source.ClearAndResetID;
    Destination.ClearAndResetID;
  end;

  if Assigned(FGUIRoot) then
    begin
    TreeView.BeginUpdate;
    LoadTreeItemChild(FGUIRoot, TreeView, TreeView.ItemHeight);

    //TreeView.CollapseAll; //try if new images don't show up

    TreeView.ExpandAll;

    TreeView.EndUpdate;
    end;
  EndUpdate;
end;

{$endregion .............................................................}

{$region 'Events' -------------------------------------------------------}

procedure TStructureView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;

  case Operation of

    opInsert:
    begin
      //TODO: opInsert seems only to be sent to components that are being inserted, to add external listerners those components must support it themselves
    end;

    opRemove:
    begin
      if (csDestroying in ComponentState) then exit; //must check if the component is getting destroyed, else TreeView will fail in FindObject when it tries to read any of its properties (even TreeView.ComponentState fails)
      var treeViewItem := TreeView.FindObject(AComponent);
      if Assigned(treeViewItem) then
        FreeAndNil(treeViewItem); //don't just do "treeViewItem.Parent := nil", we have to also delete the TTreeViewItem instance
    end;

  end;
end;

procedure TStructureView.TreeViewChange(Sender: TObject);
begin
  if Assigned(FOnSelection) then
  begin
    var selectedItem := TreeView.Selected;
    if Assigned(selectedItem) then
      FOnSelection(Self, selectedItem.TagObject);
  end;
end;

procedure TStructureView.TreeViewDragChange(SourceItem, DestItem: TTreeViewItem; var Allow: Boolean);

  procedure Restructured(const AChild: TObject; AParent: TObject; const Operation: TRestructuringOperation);
  begin
    if AParent is TControl then
      TControl(AParent).Repaint; //TODO: see why we need to tell to repaint in that case to show the new order of its children (would expect it to be done automatically)

    if Assigned(FOnRestructuring) then
      FOnRestructuring(Self, AChild, AParent, Operation);
  end;

begin
  Allow := (DragDropReorder or DragDropReparent)
           and Assigned(DestItem); //checking we didn't drop onto the empty area of the TTreeView //TODO: could have a default false boolean property to allow switching GUIRoot when we do this action (and reparent the current GUIRoot under the dropped item), but should have event to notify client code about this GUIRoot change
  if not Allow then exit;

  var SourceObject := TFmxObject(SourceItem.TagObject);
  var DestObject := TFmxObject(DestItem.TagObject);
  var SourceObjectParent := SourceObject.Parent;

  if SourceObjectParent = DestObject then //if dropped to the same parent
  begin
    Allow := DragDropReorder;
    if Allow then
    begin
      SourceObject.BringToFront; //places last in the parent (corresponds to top in the Z-Order), as done for the TTreeViewItems by the TTreeView

      Restructured(SourceObject, SourceObjectParent, roReorderedChildren);

      if DragDropSelectTarget then
        TreeView.Selected := DestItem;
    end
  end

  else //if dropped onto other than its parent
  begin
    Allow := DragDropReparent;
    if Allow then
    begin
      SourceObject.Parent := DestObject; //move the FmxObjects the TTreeViewItems point to //assuming TagObject contains a TFmxObject

      Restructured(SourceObject, SourceObjectParent, roRemovedChild);
      Restructured(SourceObject, DestObject, roAddedChild);

      if DragDropSelectTarget then
        TreeView.Selected := DestItem;
    end
  end;
end; //the TTreeViewItems themselves will be reordered by the TTreeView since TreeView.AllowDrag maps to "DragDropReorder or DragDropReparent"

{$endregion .............................................................}

{$ENDREGION}

{$region 'Registration'}

procedure RegisterSerializationClasses;
begin
  RegisterFmxClasses([TStructureView]);
end;

procedure Register;
begin
  GroupDescendentsWith(TStructureView, TControl);
  RegisterSerializationClasses;
  RegisterComponents('Zoomicon', [TStructureView]);
end;

{$endregion}

initialization
  RegisterSerializationClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
