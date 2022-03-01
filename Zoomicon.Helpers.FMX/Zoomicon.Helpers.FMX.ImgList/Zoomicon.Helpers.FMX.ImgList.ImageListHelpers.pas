unit Zoomicon.Helpers.FMX.ImgList.ImageListHelpers;

interface
  uses
    FMX.Graphics, //for TBitmap
    FMX.ImgList; //for TImageList

  type
    TImageListAddBitmapHelper = class helper for TImageList
      function Add(const aBitmap: TBitmap; const Scale: Single = 1): integer;
    end;

implementation
  uses
    System.Types, //for TRectF
    System.UITypes, //for TColorRec
    FMX.MultiResBitmap; //for TSizeKind

{$REGION 'TImageListAddBitmapHelper'}

function TImageListAddBitmapHelper.Add(const aBitmap: TBitmap; const Scale: Single = 1): integer; //based on https://stackoverflow.com/a/43086181/903783
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
  with vLayer do
  begin
    SourceRect.Rect := TRectF.Create(TPoint.Zero, vSource.MultiResBitmap.Width, vSource.MultiResBitmap.Height);
    Name := vSource.Name;
  end;

  Result := vDest.Index;
end;

{$ENDREGION}

end.
