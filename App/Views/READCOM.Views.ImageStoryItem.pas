//Description: READ-COM ImageStoryItem View
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.ImageStoryItem;

interface

uses
  READCOM.App.Models, //for IImageStoryItem
  READCOM.Views.StoryItem, //for TStoryItem
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ExtCtrls,
  FMX.Clipboard, //for IFMXExtendedClipboardService
  FMX.Objects, //for TImage
  FMX.Surfaces, //for TBitmapSurface
  FMX.SVGIconImage; //for TSVGIconImage


const
  EXT_SVG = '.svg';
  FILTER_VECTOR_IMAGE_TITLE = 'Vector images (*.svg)';
  FILTER_VECTOR_IMAGE_EXTS = '*' + EXT_SVG;
  FILTER_VECTOR_IMAGE = FILTER_VECTOR_IMAGE_TITLE + '|' + FILTER_VECTOR_IMAGE_EXTS;

  EXT_PNG = '.png';
  EXT_JPG = '.jpg';
  EXT_JPEG = '.jpeg';
  FILTER_BITMAP_IMAGE_TITLE = 'Bitmap images (*.png, *.jpg, *.jpeg)';
  FILTER_BITMAP_IMAGE_EXTS = '*' + EXT_PNG + ';*' + EXT_JPG + ';*' + EXT_JPEG;
  FILTER_BITMAP_IMAGE = FILTER_BITMAP_IMAGE_TITLE + '|' + FILTER_BITMAP_IMAGE_EXTS;

  FILTER_IMAGE_TITLE = 'Images (*.svg, *.png, *.jpg, *.jpeg)';
  FILTER_IMAGE_EXTS = FILTER_VECTOR_IMAGE_EXTS + ';' + FILTER_BITMAP_IMAGE_EXTS;
  FILTER_IMAGE = FILTER_IMAGE_TITLE + '|' + FILTER_IMAGE_EXTS;

type

  {$REGION 'TImageStoryItem'}

  TImageStoryItem = class abstract(TStoryItem, IImageStoryItem, IStoryItem, IStoreable)
    ImageControl: TImage;

  //--- Fields ---

  protected
    FStoreSVG: Boolean;


  //--- Methods ---

  protected
    procedure Loaded; override;
    procedure UpdateGlyphVisibility;
    function GetStoreBitmap: Boolean;

    {Z-Order}
    function GetBackIndex: Integer; override;
    procedure SetImageControlZorder; virtual;

    {Clipboard}
    procedure Paste(const Clipboard: IFMXExtendedClipboardService); overload; override;
    procedure PasteImage(const BitmapSurface: TBitmapSurface); override;

    {Image}
    function GetImage: TImage; virtual;
    procedure SetImage(const Value: TImage); virtual;

    {Options}
    function GetOptions: IStoryItemOptions; override;

    {EditMode}
    procedure SetEditMode(const Value: Boolean); override;

    {$region 'SVG'}

    {SVGImage}
    function GetSVGImage: TSVGIconImage; virtual;
    procedure SetSVGImage(const Value: TSVGIconImage); virtual;

    {SVGText}
    function GetSVGText: String;
    procedure SetSVGText(const Value: String);
    {$endregion}

    procedure Resize; override;

  public
    constructor Create(AOwner: TComponent); override;

    {$region 'IStoreable'}
    function GetLoadFilesFilter: String; override;
    function Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM; const CreateNew: Boolean = false): TObject; overload; override;
    function LoadSVG(const Stream: TStream): TObject; virtual;
    function LoadBitmap(const Stream: TStream): TObject; virtual;
    {$endregion}

  //--- Properties ---

  published
    property Image: TImage read GetImage write SetImage stored GetStoreBitmap default nil;
    property SVGImage: TSVGIconImage read GetSVGImage write SetSVGImage stored false default nil;
    property SVGText: String read GetSVGText write SetSVGText stored FStoreSVG;
    property AutoSize default true;
  end;

  TVectorImageStoryItem = class(TImageStoryItem); //for backwards compatibility (note we can't just do "= TImageStoryItem", RegisterFmxClasses doesn't seem to register as separate class names in that case)
  TBitmapImageStoryItem = class(TImageStoryItem); //for backwards compatibility (note we can't just do "= TImageStoryItem", RegisterFmxClasses doesn't seem to register as separate class names in that case)


  {$ENDREGION}

  {$REGION 'TImageStoryItemFactory'}

  TImageStoryItemFactory = class(TInterfacedObject, IStoryItemFactory)
    function New(const AOwner: TComponent = nil): IStoryItem;
  end;

  {$ENDREGION}

implementation
  uses
    READCOM.Views.Options.ImageStoryItemOptions, //for TImageStoryItemOptions
    READCOM.Views.StoryItemFactory, //for StoryItemFactories, StoryItemAddFileFilter
    Zoomicon.Text; //for ReadAllText

{$R *.fmx}

{$REGION 'TImageStoryItem'}

{$region 'Lifetime management'}

constructor TImageStoryItem.Create(AOwner: TComponent);

  procedure InitImageControl;
  begin
    with ImageControl do
    begin
      Stored := false; //don't store state, should use state from designed .FMX resource
      SetSubComponent(true);
      Align := TAlignLayout.Contents;
      WrapMode := TImageWrapMode.Stretch;
      SetImageControlZorder;
      HitTest := false;
    end;
  end;

begin
  inherited;

  InitImageControl;

  Glyph.Visible := true;
  SetGlyphZorder;
end;

procedure TImageStoryItem.UpdateGlyphVisibility;
begin
  var img := ImageControl.Bitmap.Image;
  Glyph.Visible := not (Assigned(img) and (img.Width <> 0) and (img.Height <> 0)); //hide default Glyph if we have a non-empty bitmap image
  FStoreSVG := Glyph.Visible;
  SetGlyphZorder; //keep before SetImageControlZorder to show the Glyph above the bitmap image if due to some error it's appearing together with the bitmap
  SetImageControlZorder;
end;

procedure TImageStoryItem.Loaded;
begin
  inherited;
  UpdateGlyphVisibility;
end;

{$endregion}

{$region 'Z-order'}

function TImageStoryItem.GetBackIndex: Integer;
begin
  result := inherited + 1; //reserve one more place at the bottom for ImageControl
end;

procedure TImageStoryItem.SetImageControlZorder;
begin
  (* //NOT WORKING
  BeginUpdate;
  RemoveObject(ImageControl);
  InsertObject(GetBackIndex - 1, ImageControl);
  EndUpdate;
  *)
  ImageControl.SendToBack;
end;

{$endregion}

{$region 'Clipboard'}

procedure TImageStoryItem.Paste(const Clipboard: IFMXExtendedClipboardService);
begin
  if Clipboard.HasImage then
  begin
    var BitmapSurface := Clipboard.GetImage;
    try
      PasteImage(BitmapSurface);
    finally
      FreeAndNil(BitmapSurface); //must release the TBitmapSurface to not have memory leak
    end;
  end
  else
    inherited; //fallback to ancestor implementation
end;

procedure TImageStoryItem.PasteImage(const BitmapSurface: TBitmapSurface);
begin
  ImageControl.Bitmap.Assign(BitmapSurface);
  UpdateGlyphVisibility;

  if FAutoSize then
    SetSize(ImageControl.Bitmap.Width, ImageControl.Bitmap.Height); //TODO: probably not needed
end;

{$endregion}

{$region 'IStorable'}

function TImageStoryItem.GetLoadFilesFilter: String;
begin
  result := FILTER_IMAGE + '|' + //all images
            FILTER_VECTOR_IMAGE + '|' + //vector images only
            FILTER_BITMAP_IMAGE + '|' + //bitmap images only
            inherited; //filters defined by ancestor (e.g. for readcom files)
end;

function TImageStoryItem.Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM; const CreateNew: Boolean = false): TObject;
begin
  if (ContentFormat = EXT_SVG) then //load EXT_SVG
    result := LoadSVG(Stream)
  else if (ContentFormat = EXT_PNG) or (ContentFormat = EXT_JPG) or (ContentFormat = EXT_JPEG) then //load EXT_PNG, EXT_JPG, EXT_JPEG
    result := LoadBitmap(Stream)
  else
    result := inherited; //load formats supported by ancestor (e.g. EXT_READCOM)
end;

function TImageStoryItem.LoadSVG(const Stream: TStream): TObject;
begin
  if FAutoSize then
    Glyph.Align := TAlignLayout.None;

  //SVGImage.LoadFromStream(Stream); //TODO: should fix to read size info from SVG
  //SVGImage.SVGText := ReadAllText(Stream); //TODO: using this as workaround since LoadFromStream doesn't seem to be compilable anymore //TODO: see why it fails (stack pointer corruption?)

  var s := TStringList.Create(#0, #13);
  try
    s.LoadFromStream(Stream);
    var txt := s.DelimitedText;
    SVGImage.SVGText := txt; //TODO: using this as workaround since LoadFromStream doesn't seem to be compilable anymore
  finally
    FreeAndNil(s);
  end;

  //SVGImage.FixedColor := TAlphaColorRec.Red;

  FStoreSVG := true; //mark that we loaded custom SVG

  if FAutoSize then
    begin
    //SetSize(bitmap.Width, bitmap.Height); //TODO: seems SVG size doesn't get loaded
    SetSize(100,100); //TODO: fix this
    Glyph.Align := TAlignLayout.Contents;
    end;

  result := Self;
end;

function TImageStoryItem.LoadBitmap(const Stream: TStream): TObject;
begin
  ImageControl.Bitmap.LoadFromStream(Stream); //TODO: does it detect PNG and JPEG automatically?
  UpdateGlyphVisibility;

  if FAutoSize then
    SetSize(ImageControl.Bitmap.Width, ImageControl.Bitmap.Height); //TODO: probably not needed

  result := Self;
end;

{$endregion}

{$REGION '--- PROPERTIES ---'}

{$region 'Image'}

function TImageStoryItem.GetStoreBitmap: Boolean;
begin
  result := not FStoreSVG;
end;

function TImageStoryItem.GetImage: TImage;
begin
  if FStoreSVG then
    result := Glyph
  else
    result := ImageControl;
end;

procedure TImageStoryItem.SetImage(const Value: TImage);
begin
  if not Assigned(Value) then exit;

  if Value is TSVGIconImage then
    SVGImage := Value As TSVGIconImage
  else
    ImageControl.Bitmap.Assign(Value.Bitmap); //can't assign TImage directly

  UpdateGlyphVisibility;

  if FAutoSize then
    SetSize(ImageControl.Bitmap.Width, ImageControl.Bitmap.Height); //TODO: probably not needed
end;

{$endregion}

{$region 'SVGImage'}

function TImageStoryItem.GetSVGImage: TSVGIconImage;
begin
  result := Glyph;
end;

procedure TImageStoryItem.SetSVGImage(const Value: TSVGIconImage);
begin
  SVGText := Value.SVGText; //this will also set Size
end;

{$endregion}

{$region 'SVGText'}

function TImageStoryItem.GetSVGText: String;
begin
  if Assigned(Glyph) then
    result := SVGImage.SVGText
  else
    result := '';
end;

procedure TImageStoryItem.SetSVGText(const Value: String);
begin //TODO: should restore default Glyph (keep it to some global/static var once?) if SVGText is set to ''
  if Assigned(Glyph) then
  begin
    if FAutoSize then
      Glyph.Align := TAlignLayout.None;

    SVGImage.SVGText := Value;

    if FAutoSize then //TODO: shouldn't hardcode any size here, item should keep its Width/Height when loading this property
      begin
      //SetSize(bitmap.Width, bitmap.Height); //TODO: seems SVG size doesn't get loaded
      //SetSize(100,100);
      Glyph.Align := TAlignLayout.Contents;
      end;

    FStoreSVG := (Value <> '') and (Value <> '<svg xmlns="http://www.w3.org/2000/svg"></svg>'); //mark that we loaded custom SVG

    //var bitmap := Glyph.MultiResBitmap[0] as TSVGIconFixedBitmapItem;
    //bitmap.DrawSVGIcon;
  end;
end;

{$endregion}

{$region 'Options'}

function TImageStoryItem.GetOptions: IStoryItemOptions;
begin
  if not Assigned(FOptions) then
    begin
    FOptions := TImageStoryItemOptions.Create(nil); //don't set storyitem as owner, seems to always store it (irrespective of "Stored := false")
    FOptions.StoryItem := Self;
    end;

  result := FOptions;
end;

{$endregion}

{$region 'EditMode'}

procedure TImageStoryItem.SetEditMode(const Value: Boolean);
begin
  inherited;
  ImageControl.SendToBack; //make the bitmap image show under the DropTarget
end;

{$endregion}

{$ENDREGION PROPERTIES}

procedure TImageStoryItem.Resize;
begin
  var tmp := SVGText;
  SVGText := '';
  SVGText := tmp; //recalculate bitmap from the SVG //TODO: not sure if the "tmp" step is needed, or if it recalculates the buffer at all (probably doesn't have the new size at this point?)
end;

{$endregion}

{$REGION 'TImageStoryItemFactory'}

function TImageStoryItemFactory.New(const AOwner: TComponent = nil): IStoryItem;
begin
  result := TBitmapImageStoryItem.Create(AOwner); //using TBitmapImageStoryItem instead of TImageStoryItem for forward compatibility (let older app versions load documents saved with newer version)
end;

{$ENDREGION}

{$region 'Registration'}

procedure RegisterSerializationClasses;
begin
  RegisterFmxClasses([
    TImageStoryItem,
    TVectorImageStoryItem, //for backwards compatibility
    TBitmapImageStoryItem //for backwards compatibility
  ]);
end;

procedure Register;
begin
  GroupDescendentsWith(TImageStoryItem, TControl);
  RegisterSerializationClasses;
  RegisterComponents('Zoomicon', [TImageStoryItem]);
end;

{$endregion}

initialization
  StoryItemFactories.Add([EXT_SVG, EXT_PNG, EXT_JPG, EXT_JPEG], TImageStoryItemFactory.Create);
  AddStoryItemFileFilter(FILTER_IMAGE_TITLE, FILTER_IMAGE_EXTS);
  AddStoryItemFileFilter(FILTER_VECTOR_IMAGE_TITLE, FILTER_VECTOR_IMAGE_EXTS);
  AddStoryItemFileFilter(FILTER_BITMAP_IMAGE_TITLE, FILTER_BITMAP_IMAGE_EXTS);

  RegisterSerializationClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
