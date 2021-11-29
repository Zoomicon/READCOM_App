unit READCOM.Views.Panes.Structure;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.TreeView, FMX.ImgList,
  READCOM.App.Models, System.ImageList; //for IStoryItem

type
  TStructure = class(TFrame)
    Tree: TTreeView;
    Images: TImageList;
  protected
    FStoryItem: IStoryItem;
    procedure LoadTree(const StoryItem: IStoryItem);

    {StoryItem}
    procedure SetStoryItem(const Value: IStoryItem);

  public
    property StoryItem: IStoryItem read FStoryItem write SetStoryItem;
  end;

implementation
  uses FMX.MultiResBitmap;

{$REGION 'ImageListHelper'}
//from https://stackoverflow.com/a/43086181/903783

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

{ TStructure }

procedure TStructure.LoadTree(const StoryItem: IStoryItem);

  procedure LoadTreeItemChild(const StoryItem: IStoryItem; const Parent: TFmxObject);
  begin
    if not Assigned(StoryItem) then exit;

    const View = StoryItem.View;
    if not Assigned(View) then exit;

    var TreeItem := TTreeViewItem.Create(Parent);
    TreeItem.Parent := Parent;

    TreeItem.ImageIndex := Images.Add(View.MakeScreenshot);

    for var ChildStoryItem in StoryItem.StoryItems do
      LoadTreeItemChild(ChildStoryItem, TreeItem);
  end;

begin
  Tree.Clear;
  Images.ClearCache;
  //Images.Dormant := true; //?
  LoadTreeItemChild(StoryItem, Tree);

  Tree.CollapseAll;
  Tree.ExpandAll;
end;

procedure TStructure.SetStoryItem(const Value: IStoryItem);
begin
  FStoryItem := Value;
  LoadTree(Value);
end;

end.
