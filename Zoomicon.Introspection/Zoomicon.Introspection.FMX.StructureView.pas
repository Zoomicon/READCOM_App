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

const
  {$region 'Defaults'}

  DEFAULT_SHOW_ONLY_VISIBLE = true;
  DEFAULT_SHOW_ONLY_NAMED = true;
  DEFAULT_SHOW_NAMES = false;
  DEFAULT_SHOW_TYPES = false;
  DEFAULT_SHOW_HINT_NAMES = true;
  DEFAULT_SHOW_HINT_TYPES = false;
  DEFAULT_DRAGDROP_REORDER = false; //acting as structure viewer, not editor by default
  DEFAULT_DRAGDROP_REPARENT = false; //acting as structure viewer, not editor by default
  DEFAULT_DRAGDROP_SELECTTARGET = true; //on drop, target is selected

  {$endregion}

type
  //TClassList = TList<TClass>; //using old-style (non Generic) "TClassList" from System.Contnrs instead

  TRestructuringOperation = (roReorderedChildren, roAddedChild, roRemovedChild);

  TSelectionEvent = procedure(Sender: TObject; const Selection: TObject) of object;
  TRestructuringEvent = procedure(Sender: TObject; const RestructuredChild: TObject; const RestructuredParent: TObject; const Operation: TRestructuringOperation) of object;

  {$REGION 'TStructureView' --------------------------------------------------------}

  TStructureView = class(TFrame)
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
    {DragDropXX}
    FDragDropReorder: Boolean;
    FDragDropReparent: Boolean;
    FDragDropSelectTarget: Boolean;
    {Events}
    FOnSelection: TSelectionEvent;
    FOnRestructuring: TRestructuringEvent;

    {GUIRoot}
    procedure SetGUIRoot(const Value: TControl); virtual;
    procedure LoadTreeView; virtual;
    {ShowXX}
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

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

  published
    //Properties//
    property GUIRoot: TControl read FGUIRoot write SetGUIRoot;
    property ShowOnlyClasses: TClassList read FShowOnlyClasses write SetShowOnlyClasses; //default nil
    property ShowOnlyVisible: Boolean read FShowOnlyVisible write SetShowOnlyVisible default DEFAULT_SHOW_ONLY_VISIBLE;
    property ShowOnlyNamed: Boolean read FShowOnlyNamed write SetShowOnlyNamed default DEFAULT_SHOW_ONLY_NAMED;
    property ShowNames: Boolean read FShowNames write SetShowNames default DEFAULT_SHOW_NAMES;
    property ShowTypes: Boolean read FShowTypes write SetShowTypes default DEFAULT_SHOW_TYPES;
    property ShowHintNames: Boolean read FShowHintNames write SetShowHintNames default DEFAULT_SHOW_HINT_NAMES;
    property ShowHintTypes: Boolean read FShowHintTypes write SetShowHintTypes default DEFAULT_SHOW_HINT_TYPES;
    property DragDropReorder: Boolean read FDragDropReorder write SetDragDropReorder default DEFAULT_DRAGDROP_REORDER;
    property DragDropReparent: Boolean read FDragDropReparent write SetDragDropReparent default DEFAULT_DRAGDROP_REPARENT;
    property DragDropSelectTarget: Boolean read FDragDropSelectTarget write FDragDropSelectTarget default DEFAULT_DRAGDROP_SELECTTARGET;
    property SelectedObject: TObject read GetSelectedObject write SetSelectedObject; //default nil
    //Events//
    property OnSelection: TSelectionEvent read FOnSelection write FOnSelection;
    property OnRestructuring: TRestructuringEvent read FOnRestructuring write FOnRestructuring;
  end;

  {$ENDREGION ......................................................................}

procedure Register;

implementation
  uses
    System.UITypes, //for TDragMode
    System.Rtti, //for TValue
    FMX.Dialogs, //for ShowMessage
    Zoomicon.Helpers.RTL.ClassListHelpers, //for TClassList.FindClassOf
    Zoomicon.Helpers.FMX.ImgList.ImageListHelpers, //for TImageListAddBitmapHelper
    Zoomicon.Helpers.FMX.TreeView.TreeViewHelpers; //for TTreeViewItemSearchHelper

{$R *.fmx}

{$REGION 'TStructureView'}

{$region 'Creation / Destruction '}

constructor TStructureView.Create(AOwner: TComponent);
begin
  inherited;

  FShowOnlyClasses := nil;
  FShowOnlyVisible := DEFAULT_SHOW_ONLY_VISIBLE;
  FShowOnlyNamed := DEFAULT_SHOW_ONLY_NAMED;
  FShowNames := DEFAULT_SHOW_NAMES;
  FShowTypes := DEFAULT_SHOW_TYPES;
  FShowHintNames := DEFAULT_SHOW_HINT_NAMES;
  FShowHintTypes := DEFAULT_SHOW_HINT_TYPES;

  SetDragDropReorder(DEFAULT_DRAGDROP_REORDER);
  SetDragDropReparent(DEFAULT_DRAGDROP_REPARENT);
end;

destructor TStructureView.Destroy;
begin
  TreeView.Clear;
  ImageList.ClearCache;
  //FreeAndNil(TreeView); //should be done automatically when "inherited" is called
  //FreeAndNil(Images); //should be done automatically when "inherited" is called
  FreeAndNil(ShowOnlyClasses); //freeing any TClassList that had been allocated and assigned to the "ShowOnlyClasses" property to avoid external code having to do it (obviously that TClassList shouldn't be assigned elsewhere)
  inherited;
end;

{$endregion}

{$region 'Properties' ---------------------------------------------------}

{$region 'GUIRoot'}

procedure TStructureView.SetGUIRoot(const Value: TControl);
begin
  FGUIRoot := Value;
  LoadTreeView;
end;

procedure TStructureView.LoadTreeView;

  procedure LoadTreeItemChild(const TheControl: TControl; const TheParent: TFmxObject; const IconHeight: Single);
  begin
    if not Assigned(TheControl) or
       (Assigned(FShowOnlyClasses) and (FShowOnlyClasses.Count > 0) and (FShowOnlyClasses.FindClassOf(TheControl, false) < 0)) or //if FShowOnlyClasses is empty ignore it
       (FShowOnlyVisible and (not TheControl.Visible)) or
       (FShowOnlyNamed and (TheControl.Name = '')) //ignore style-related controls (unnamed), including their children
      then exit;

    var TreeItem := TTreeViewItem.Create(Parent);
    with TreeItem do
    begin
      BeginUpdate;
      Parent := TheParent;

      TagObject := TheControl;

      var ControlToBitmap := TheControl.MakeScreenshot; //this seems to leak memory
      var imgIndex := ImageList.Add(ControlToBitmap); //this will copy from the bitmap
      FreeAndNil(ControlToBitmap); //MUST FREE THE BITMAP ELSE WE HAVE VARIOUS MEMORY LEAKS

      ImageIndex := imgIndex;
      var img := ImageList.Source.Items[imgIndex].MultiResBitmap;
      StylesData['glyphstyle.Size.Size']:= TValue.From(TSizeF.Create(img.Width*(IconHeight/img.Height), IconHeight));

      if FShowNames then Text := TheControl.Name;
      if FShowTypes then Text := Text + ': ' + TheControl.ClassName;

      if FShowHintNames then Hint := TheControl.Name;
      if FShowHintTypes then Hint := Hint + ': ' + TheControl.ClassName;
      ShowHint := true;

      for var ChildControl in TheControl.Controls do
        LoadTreeItemChild(ChildControl, TreeItem, IconHeight);

      EndUpdate;
    end;
  end;

begin
  BeginUpdate;
  TreeView.Clear;
  ImageList.ClearCache;
  ImageList.Dormant := true;

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

{$endregion}

{$region 'ShowXX'}

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

{$region 'Events' -------------------------------------------------------}

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
  Allow := DragDropReorder or DragDropReparent;
  if Allow then
  begin
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

  end;
end; //the TTreeViewItems themselves will be reordered by the TTreeView since TreeView.AllowDrag maps to "DragDropReorder or DragDropReparent"

{$endregion .............................................................}

{$ENDREGION}

{$region 'Registration'}

procedure RegisterClasses;
begin
  RegisterFmxClasses([TStructureView]); //register for persistence (in case they're used standalone)
end;

procedure Register;
begin
  GroupDescendentsWith(TStructureView, TControl);
  RegisterClasses;
  RegisterComponents('Zoomicon', [TStructureView]);
end;

{$endregion}

initialization
  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
