//Description: READ-COM ImageStoryItem View
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.StoryItems.ImageStoryItem;

interface
  {$region 'Used units'}
  uses
    System.SysUtils,
    System.Types,
    System.UITypes,
    System.Classes,
    System.Variants,
    //
    FMX.Types,
    FMX.Graphics,
    FMX.Controls,
    FMX.Forms,
    FMX.Dialogs,
    FMX.StdCtrls,
    FMX.ExtCtrls,
    FMX.Layouts,
    FMX.Clipboard, //for IFMXExtendedClipboardService
    FMX.Objects, //for TImage
    FMX.Surfaces, //for TBitmapSurface
    FMX.SVGIconImage, //for TSVGIconImage
    //
    Zoomicon.Media.FMX.Models, //for EXT_XX constants
    Zoomicon.Media.FMX.MediaDisplay, //for TMediaDisplay (Glyph)
    //
    READCOM.Models, //for IClipboardEnabled, IStoreable, EXT_READCOM
    READCOM.Models.Stories, //for IStoryItem, IImageStoryItem
    READCOM.Views.StoryItems.StoryItem; //for TStoryItem
  {$endregion}

  {$REGION 'CONSTANTS'}

  const
    FILTER_VECTOR_IMAGE_TITLE = 'Vector images (*.svg)';
    FILTER_VECTOR_IMAGE_EXTS = '*' + EXT_SVG;
    FILTER_VECTOR_IMAGE = FILTER_VECTOR_IMAGE_TITLE + '|' + FILTER_VECTOR_IMAGE_EXTS;

    FILTER_BITMAP_IMAGE_TITLE = 'Bitmap images (*.png, *.jpg, *.jpeg)';
    FILTER_BITMAP_IMAGE_EXTS = '*' + EXT_PNG + ';*' + EXT_JPG + ';*' + EXT_JPEG;
    FILTER_BITMAP_IMAGE = FILTER_BITMAP_IMAGE_TITLE + '|' + FILTER_BITMAP_IMAGE_EXTS;

    FILTER_IMAGE_TITLE = 'Images (*.svg, *.png, *.jpg, *.jpeg)';
    FILTER_IMAGE_EXTS = FILTER_VECTOR_IMAGE_EXTS + ';' + FILTER_BITMAP_IMAGE_EXTS;
    FILTER_IMAGE = FILTER_IMAGE_TITLE + '|' + FILTER_IMAGE_EXTS;

  {$ENDREGION CONSTANTS}

  type

    {$REGION 'TImageStoryItem'}

    TImageStoryItem = class abstract(TStoryItem, IImageStoryItem, IStoryItem, IClipboardEnabled, IStoreable)
    private
      FDummyImage: TImage;

    //--- Methods ---

    protected
      function GetStoreBitmap: Boolean;

      {$region 'Overrides'}

      procedure Loaded; override;

      {Options}
      function GetOptions: IStoryItemOptions; override;

      {$endregion}

      {Image}
      function GetImage: TImage; virtual;
      procedure SetImage(const Value: TImage); overload; virtual;
      procedure SetImage(const Value: TBitmapSurface); overload; virtual;

      {SVGText}
      function GetSVGText: String;
      procedure SetSVGText(const Value: String);

      //procedure Resize; override; //TODO: remove (see comments in implementation)

    public
      constructor Create(AOwner: TComponent); override;

      {$region 'IStoreable'}
      function GetLoadFilesFilter: String; override;
      function Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM; const CreateNew: Boolean = false): TObject; overload; override;
      function LoadSVG(const Stream: TStream): TObject; virtual;
      function LoadBitmap(const Stream: TStream; Const ContentFormat: String): TObject; virtual;
      function Load(const Clipboard: IFMXExtendedClipboardService; const CreateNew: Boolean = false): TObject; overload; override;
      {$endregion}

    //--- Properties ---

    published
      property Image: TImage read GetImage write SetImage stored GetStoreBitmap default nil;
      property SVGText: String read GetSVGText write SetSVGText;
      property AutoSize default true;
    end;

    {$region 'Backwards compatibility'}

    TVectorImageStoryItem = class(TImageStoryItem); //for backwards compatibility (note we can't just do "= TImageStoryItem", RegisterFmxClasses doesn't seem to register as separate class names in that case)
    TBitmapImageStoryItem = class(TImageStoryItem); //for backwards compatibility (note we can't just do "= TImageStoryItem", RegisterFmxClasses doesn't seem to register as separate class names in that case)
    TPanelStoryItem = class(TImageStoryItem); //for backwards compatibility (note we can't just do "= TImageStoryItem", RegisterFmxClasses doesn't seem to register as separate class names in that case)

    {$endregion}

    {$ENDREGION}

    {$REGION 'TImageStoryItemFactory'}

    TImageStoryItemFactory = class(TInterfacedObject, IStoryItemFactory)
      function New(const AOwner: TComponent = nil): IStoryItem;
    end;

    {$ENDREGION}

implementation
  uses
    READCOM.Views.Options.ImageStoryItemOptions, //for TImageStoryItemOptions
    READCOM.Views.StoryItems.StoryItemFactory; //for StoryItemFactories, StoryItemAddFileFilter

  {$R *.fmx}

  {$REGION 'TImageStoryItem'}

  {$region 'Lifetime management'}

  constructor TImageStoryItem.Create(AOwner: TComponent);
  begin
    inherited;
    Glyph.Visible := true;
    SetGlyphZorder;

    FDummyImage := TImage.Create(Self);
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
  begin //TODO: maybe honor CreateNew by making and returning a new instance if true
    if (ContentFormat = EXT_SVG) then //load EXT_SVG
      result := LoadSVG(Stream)
    else if (ContentFormat = EXT_PNG) or (ContentFormat = EXT_JPG) or (ContentFormat = EXT_JPEG) then //load EXT_PNG, EXT_JPG, EXT_JPEG
      result := LoadBitmap(Stream, ContentFormat)
    else
      result := inherited; //load formats supported by ancestor (e.g. EXT_READCOM)
  end;

  function TImageStoryItem.LoadSVG(const Stream: TStream): TObject;
  begin
    Glyph.LoadSVG(Stream);
    result := Self;
  end;

  function TImageStoryItem.LoadBitmap(const Stream: TStream; Const ContentFormat: String): TObject;
  begin
    Glyph.LoadBitmap(Stream, ContentFormat);
    result := Self;
  end;

  function TImageStoryItem.Load(const Clipboard: IFMXExtendedClipboardService; const CreateNew: Boolean = false): TObject;
  begin
    //check for Bitmap image
    if Clipboard.HasImage then
    begin
      var BitmapSurface := Clipboard.GetImage;
      try
        SetImage(BitmapSurface);
        Exit(Self);
      finally
        FreeAndNil(BitmapSurface); //must release the TBitmapSurface to not have memory leak
      end;
    end

    //check for SVG markup
    else if Clipboard.HasText then
    begin
      var LText := TrimLeft(Clipboard.GetText); //Trimming since we may have pasted some SVG markup with extra spaces before and after
      if LText.StartsWith('<svg ') and LText.EndsWith('</svg>') then
      begin
        SVGText := LText;
        Exit(Self);
      end;
    end;

    result := inherited; //fallback to ancestor implementation
  end;

  {$endregion}

  {$REGION '--- PROPERTIES ---'}

  {$region 'Overrides'}

  procedure TImageStoryItem.Loaded;
  begin
    inherited;

    if Assigned(FDummyImage) then
    begin
      if Assigned(FDummyImage.Bitmap.Image) and (FDummyImage.Bitmap.Image.Width <> 0) and (FDummyImage.Bitmap.Height <> 0) then
        SetImage(FDummyImage); //this should copy the bitmap
      FreeAndNil(FDummyImage);
    end;
  end;

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

  {$endregion}

  {$region 'Image'}

  function TImageStoryItem.GetStoreBitmap: Boolean;
  begin
    result := Glyph.HasNonEmptyBitmap;
  end;

  function TImageStoryItem.GetImage: TImage;
  begin
    if (Glyph.Presenter is TImage) then
      result := (Glyph.Presenter as TImage)
    else
      result := FDummyImage; //TODO: ideally should return "nil" but is needed to load old content which has "Image.MultiResBitmap" properties in it
  end;

  procedure TImageStoryItem.SetImage(const Value: TImage);
  begin
    if not Assigned(Value) then
    begin
      Glyph.Bitmap := nil;
      exit;
    end;

    if Value is TSVGIconImage then //special case
      SVGText := (Value as TSVGIconImage).SVGText
    else
      Glyph.Bitmap := Value.Bitmap; //this calls "Assign" internally and copies the bitmap
  end;

  procedure TImageStoryItem.SetImage(const Value: TBitmapSurface);
  begin
    var LBitmap := Glyph.Bitmap;
    LBitmap.Assign(Value);

    Glyph.Bitmap := LBitmap; //TODO: does this work? (check image pasting from clipboard)
  end;

  {$endregion}

  {$region 'SVGText'}

  function TImageStoryItem.GetSVGText: String;
  begin
    result := Glyph.SVGText;
  end;

  procedure TImageStoryItem.SetSVGText(const Value: String);
  begin
    Glyph.SVGText := Value;
  end;

  {$endregion}

  {$ENDREGION PROPERTIES}

  (*
  //TODO: see why it fails to load/show Bitmap images from saved state if this is removed
  procedure TImageStoryItem.Resize;
  begin
    inherited;
    var tmp := SVGText;
    SVGText := '';
    SVGText := tmp; //recalculate bitmap from the SVG //TODO: not sure if the "tmp" step is needed, or if it recalculates the buffer at all (probably doesn't have the new size at this point?)
  end;
  *)

  {$endregion}

  {$REGION 'TImageStoryItemFactory'}

  function TImageStoryItemFactory.New(const AOwner: TComponent = nil): IStoryItem;
  begin
    result := TImageStoryItem.Create(AOwner); //Note: very old versions may expect a TBitmapImageStoryItem instead, ignoring them to keep design clean
  end;

  {$ENDREGION}

  {$region 'Registration'}

  procedure RegisterSerializationClasses;
  begin
    RegisterFmxClasses([
      TImageStoryItem,
      TVectorImageStoryItem, //for backwards compatibility
      TBitmapImageStoryItem, //for backwards compatibility
      TPanelStoryItem //for backwards compatibility
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

