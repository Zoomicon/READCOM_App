unit uStructureView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.ImageList, FMX.ImgList, FMX.Layouts, FMX.TreeView;

type
  TStructureView = class(TFrame)
    TreeView: TTreeView;
    ImageList: TImageList;
  protected
    FGUIRoot: TControl;

    procedure SetGUIRoot(const Value: TControl);
    procedure LoadTreeView(const OnlyVisible: Boolean = true; const OnlyNamed: Boolean = true; const ShowNames: Boolean = true; const ShowTypes: Boolean = true);
  public
    destructor Destroy; override;
    property GUIRoot: TControl read FGUIRoot write SetGUIRoot;
  end;

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

destructor TStructureView.Destroy;
begin
  TreeView.Clear;
  ImageList.ClearCache;
  //FreeAndNil(TreeView);
  //FreeAndNil(Images);
  inherited;
end;

{$region 'GUIRoot'}

procedure TStructureView.SetGUIRoot(const Value: TControl);
begin
  FGUIRoot := Value;
  LoadTreeView;
end;

{$endregion}

procedure TStructureView.LoadTreeView(const OnlyVisible: Boolean = true; const OnlyNamed: Boolean = true; const ShowNames: Boolean = true; const ShowTypes: Boolean = true);

  procedure LoadTreeItemChild(const Control: TControl; const Parent: TFmxObject; const IconHeight: Single);
  begin
    if not Assigned(Control) or
       (OnlyVisible and (not Control.Visible)) or
       (OnlyNamed and (Control.Name = '')) //ignore style-related controls (unnamed), including their children
      then exit;

    var TreeItem := TTreeViewItem.Create(Parent);
    TreeItem.Parent := Parent;

    var imgIndex := ImageList.Add(Control.MakeScreenshot);
    TreeItem.ImageIndex := imgIndex; //TODO: check if causes mem leaks
    var img := ImageList.Source.Items[imgIndex].MultiResBitmap;
    TreeItem.StylesData['glyphstyle.Size.Size']:= TValue.From(TSizeF.Create(img.Width*(IconHeight/img.Height), IconHeight));

    if ShowNames then TreeItem.Text := Control.Name;
    if ShowTypes then TreeItem.Text := TreeItem.Text + ': ' + Control.ClassName;

    for var ChildControl in Control.Controls do
      LoadTreeItemChild(ChildControl, TreeItem, IconHeight);
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

end.
