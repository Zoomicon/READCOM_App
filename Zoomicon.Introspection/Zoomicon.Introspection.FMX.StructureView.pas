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

type
  //TClassList = TList<TClass>; //using old-style (non Generic) "TClassList" from System.Contnrs instead

  TSelectionEvent = procedure(Sender: TObject; Selection: TObject) of object;

  {$REGION 'TStructureView' --------------------------------------------------------}

  TStructureView = class(TFrame)
    TreeView: TTreeView;
    ImageList: TImageList;
    procedure TreeViewChange(Sender: TObject);

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

    procedure SetShowOnlyClasses(const Value: TClassList);
    procedure SetShowOnlyVisible(const Value: Boolean);
    procedure SetShowOnlyNamed(const Value: Boolean);
    procedure SetShowNames(const Value: Boolean);
    procedure SetShowTypes(const Value: Boolean);
    procedure SetShowHintNames(const Value: Boolean);
    procedure SetShowHintTypes(const Value: Boolean);

    procedure SetGUIRoot(const Value: TControl);
    procedure LoadTreeView;

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

  published
    property OnSelection: TSelectionEvent read FOnSelection write FOnSelection;
  end;

  {$ENDREGION ......................................................................}

procedure Register;

implementation
  uses
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

  procedure LoadTreeItemChild(const Control: TControl; const Parent: TFmxObject; const IconHeight: Single);
  begin
    if not Assigned(Control) or
       (Assigned(FShowOnlyClasses) and (FShowOnlyClasses.Count > 0) and (FShowOnlyClasses.FindClassOf(Control, false) < 0)) or //if FShowOnlyClasses is empty ignore it
       (FShowOnlyVisible and (not Control.Visible)) or
       (FShowOnlyNamed and (Control.Name = '')) //ignore style-related controls (unnamed), including their children
      then exit;

    var TreeItem := TTreeViewItem.Create(Parent);
    TreeItem.BeginUpdate;
    TreeItem.Parent := Parent;

    TreeItem.Tag := NativeInt(Control);

    var ControlToBitmap := Control.MakeScreenshot; //this seems to leak memory
    var imgIndex := ImageList.Add(ControlToBitmap); //this will copy from the bitmap
    FreeAndNil(ControlToBitmap); //MUST FREE THE BITMAP ELSE WE HAVE VARIOUS MEMORY LEAKS

    TreeItem.ImageIndex := imgIndex;
    var img := ImageList.Source.Items[imgIndex].MultiResBitmap;
    TreeItem.StylesData['glyphstyle.Size.Size']:= TValue.From(TSizeF.Create(img.Width*(IconHeight/img.Height), IconHeight));

    if FShowNames then TreeItem.Text := Control.Name;
    if FShowTypes then TreeItem.Text := TreeItem.Text + ': ' + Control.ClassName;
    TreeItem.ShowHint := true;

    if FShowHintNames then TreeItem.Hint := Control.Name;
    if FShowHintTypes then TreeItem.Hint := TreeItem.Hint + ': ' + Control.ClassName;

    for var ChildControl in Control.Controls do
      LoadTreeItemChild(ChildControl, TreeItem, IconHeight);

    TreeItem.EndUpdate;
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
    FOnSelection(Self, TObject(TreeView.Selected.Tag));
end;

{$endregion}

{$ENDREGION}

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

initialization
  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
