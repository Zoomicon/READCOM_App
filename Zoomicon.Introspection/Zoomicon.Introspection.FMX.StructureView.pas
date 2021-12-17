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
  DEFAULT_SHOW_ONLY_VISIBLE = true;
  DEFAULT_SHOW_ONLY_NAMED = true;
  DEFAULT_SHOW_NAMES = false;
  DEFAULT_SHOW_TYPES = false;
  DEFAULT_SHOW_HINT_NAMES = true;
  DEFAULT_SHOW_HINT_TYPES = false;
  DEFAULT_ALLOW_DRAGDROP = false; //acting as structure viewer, not editor by default

type
  //TClassList = TList<TClass>; //using old-style (non Generic) "TClassList" from System.Contnrs instead

  TSelectionEvent = procedure(Sender: TObject; Selection: TObject) of object;

  {$REGION 'TStructureView' --------------------------------------------------------}

  TStructureView = class(TFrame)
    TreeView: TTreeView;
    ImageList: TImageList;
    procedure TreeViewChange(Sender: TObject);
    procedure TreeViewDragChange(SourceItem, DestItem: TTreeViewItem;
      var Allow: Boolean);

  protected
    FGUIRoot: TControl;
    FShowOnlyClasses: TClassList;
    FShowOnlyNamed: Boolean;
    FShowOnlyVisible: Boolean;
    FShowNames: Boolean;
    FShowTypes: Boolean;
    FShowHintNames: Boolean;
    FShowHintTypes: Boolean;
    FOnSelection: TSelectionEvent;

    procedure SetShowOnlyClasses(const Value: TClassList); virtual;
    procedure SetShowOnlyVisible(const Value: Boolean); virtual;
    procedure SetShowOnlyNamed(const Value: Boolean); virtual;
    procedure SetShowNames(const Value: Boolean); virtual;
    procedure SetShowTypes(const Value: Boolean); virtual;
    procedure SetShowHintNames(const Value: Boolean); virtual;
    procedure SetShowHintTypes(const Value: Boolean); virtual;
    function IsAllowDragDrop: Boolean; virtual;
    procedure SetAllowDragDrop(const Value: Boolean); virtual;

    procedure SetGUIRoot(const Value: TControl); virtual;
    procedure LoadTreeView; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property GUIRoot: TControl read FGUIRoot write SetGUIRoot;
    property ShowOnlyClasses: TClassList read FShowOnlyClasses write SetShowOnlyClasses; //default nil
    property ShowOnlyVisible: Boolean read FShowOnlyVisible write SetShowOnlyVisible default DEFAULT_SHOW_ONLY_VISIBLE;
    property ShowOnlyNamed: Boolean read FShowOnlyNamed write SetShowOnlyNamed default DEFAULT_SHOW_ONLY_NAMED;
    property ShowNames: Boolean read FShowNames write SetShowNames default DEFAULT_SHOW_NAMES;
    property ShowTypes: Boolean read FShowTypes write SetShowTypes default DEFAULT_SHOW_TYPES;
    property ShowHintNames: Boolean read FShowHintNames write SetShowHintNames default DEFAULT_SHOW_HINT_NAMES;
    property ShowHintTypes: Boolean read FShowHintTypes write SetShowHintTypes default DEFAULT_SHOW_HINT_TYPES;
    property AllowDragDrop: Boolean read IsAllowDragDrop write SetAllowDragDrop default DEFAULT_ALLOW_DRAGDROP;

  published
    property OnSelection: TSelectionEvent read FOnSelection write FOnSelection;
  end;

  {$ENDREGION ......................................................................}

procedure Register;

implementation
  uses
    System.UITypes, //for TDragMode
    System.Rtti, //for TValue
    FMX.Dialogs, //for ShowMessage
    Zoomicon.Helpers.RTL.ClassListHelpers, //for TClassList.FindClassOf
    Zoomicon.Helpers.FMX.ImgList.ImageListHelpers; //for TImageListAddBitmapHelper

{$R *.fmx}

{$REGION 'TStructureView'}

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

  SetAllowDragDrop(DEFAULT_ALLOW_DRAGDROP);
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

{$region 'Properties'}

{$region 'AllowDragDrop'}

function TStructureView.IsAllowDragDrop: Boolean;
begin
  result := TreeView.AllowDrag;
end;

procedure TStructureView.SetAllowDragDrop(const Value: Boolean);
begin
  TreeView.AllowDrag := Value;
end;

{$endregion}

procedure TStructureView.SetGUIRoot(const Value: TControl);
begin
  FGUIRoot := Value;
  LoadTreeView;
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

{$region 'Events'}

procedure TStructureView.TreeViewChange(Sender: TObject);
begin
  if Assigned(FOnSelection) then
    FOnSelection(Self, TreeView.Selected.TagObject);
end;

procedure TStructureView.TreeViewDragChange(SourceItem, DestItem: TTreeViewItem; var Allow: Boolean);
begin
  Allow := AllowDragDrop;
  if Allow then
    TFmxObject(SourceItem.TagObject).Parent := TFmxObject(DestItem.TagObject); //move the FmxObjects the TTreeViewItems point to //assuming TagObject contains a TFmxObject
end;

{$endregion}

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
