unit Zoomicon.Introspection.StructureView;

interface

uses
  System.Classes, //for TComponent, GroupDecendentsWith, RegisterComponents
  System.SysUtils,
  System.Types,
  System.UITypes,
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
    DEFAULT_SHOW_VISIBLE_ONLY = true;
    DEFAULT_SHOW_NAMED_ONLY = true;
    DEFAULT_SHOW_NAMES = false;
    DEFAULT_SHOW_TYPES = false;
    DEFAULT_SHOW_HINT_NAMES = true;
    DEFAULT_SHOW_HINT_TYPES = false;

type
  TStructureView = class(TFrame)
    TreeView: TTreeView;
    ImageList: TImageList;

  protected
    FGUIRoot: TControl;
    FShowNamedOnly: Boolean;
    FShowTypes: Boolean;
    FShowVisibleOnly: Boolean;
    FShowNames: Boolean;
    FShowHintNames: Boolean;
    FShowHintTypes: Boolean;

    procedure SetShowVisibleOnly(const Value: Boolean);
    procedure SetShowNamedOnly(const Value: Boolean);
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
    property ShowVisibleOnly: Boolean read FShowVisibleOnly write SetShowVisibleOnly default DEFAULT_SHOW_VISIBLE_ONLY;
    property ShowNamedOnly: Boolean read FShowNamedOnly write SetShowNamedOnly default DEFAULT_SHOW_NAMED_ONLY;
    property ShowNames: Boolean read FShowNames write SetShowNames default DEFAULT_SHOW_NAMES;
    property ShowTypes: Boolean read FShowTypes write SetShowTypes default DEFAULT_SHOW_TYPES;
    property ShowHintNames: Boolean read FShowHintNames write SetShowHintNames default DEFAULT_SHOW_HINT_NAMES;
    property ShowHintTypes: Boolean read FShowHintTypes write SetShowHintTypes default DEFAULT_SHOW_HINT_TYPES;
  end;

procedure Register;

implementation
  uses
    System.Rtti, //for TValue
    FMX.MultiResBitmap; //for TSizeKind

{$REGION 'ImageListHelper'}
//based on https://stackoverflow.com/a/43086181/903783

type
  TImageListHelper = class helper for TImageList
    function Add(const aBitmap: TBitmap; const Scale: Single = 1): integer;
  end;

function TImageListHelper.Add(const aBitmap: TBitmap; const Scale: Single = 1): integer;
begin
  Result := -1;
  if (aBitmap.Width = 0) or (aBitmap.Height = 0) then exit;

  // add source bitmap
  var vSource := Source.Add;
  var vBitmapItem: TCustomBitmapItem;
  with vSource.MultiResBitmap do
  begin
    TransparentColor := TColorRec.Fuchsia;
    SizeKind := TSizeKind.Source;
    Width := Round(aBitmap.Width / SCALE);
    Height := Round(aBitmap.Height / SCALE);
    vBitmapItem := ItemByScale(SCALE, True, True);
  end;

  if vBitmapItem = nil then
  begin
    vBitmapItem := vSource.MultiResBitmap.Add;
    vBitmapItem.Scale := SCALE;
  end;
  vBitmapItem.Bitmap.Assign(aBitmap);

  var vDest := Destination.Add;
  var vLayer := vDest.Layers.Add;
  vLayer.SourceRect.Rect := TRectF.Create(TPoint.Zero, vSource.MultiResBitmap.Width, vSource.MultiResBitmap.Height);
  vLayer.Name := vSource.Name;
  Result := vDest.Index;
end;

{$endregion}

{$R *.fmx}

{ TGUIStructure }

constructor TStructureView.Create(AOwner: TComponent);
begin
  inherited;
  FShowVisibleOnly := DEFAULT_SHOW_VISIBLE_ONLY;
  FShowNamedOnly := DEFAULT_SHOW_NAMED_ONLY;
  FShowNames := DEFAULT_SHOW_NAMES;
  FShowTypes := DEFAULT_SHOW_TYPES;
  FShowHintNames := DEFAULT_SHOW_HINT_NAMES;
  FShowHintTypes := DEFAULT_SHOW_HINT_TYPES;
end;

destructor TStructureView.Destroy;
begin
  TreeView.Clear;
  ImageList.ClearCache;
  //FreeAndNil(TreeView);
  //FreeAndNil(Images);
  inherited;
end;

{$region 'Properties'}

procedure TStructureView.SetGUIRoot(const Value: TControl);
begin
  FGUIRoot := Value;
  LoadTreeView;
end;

procedure TStructureView.SetShowVisibleOnly(const Value: Boolean);
begin
  FShowVisibleOnly := Value;
  LoadTreeView;
end;

procedure TStructureView.SetShowNamedOnly(const Value: Boolean);
begin
  FShowNamedOnly := Value;
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
       (FShowVisibleOnly and (not Control.Visible)) or
       (FShowNamedOnly and (Control.Name = '')) //ignore style-related controls (unnamed), including their children
      then exit;

    var TreeItem := TTreeViewItem.Create(Parent);
    TreeItem.BeginUpdate;
    TreeItem.Parent := Parent;

    var imgIndex := ImageList.Add(Control.MakeScreenshot);
    TreeItem.ImageIndex := imgIndex; //TODO: check if causes mem leaks
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
