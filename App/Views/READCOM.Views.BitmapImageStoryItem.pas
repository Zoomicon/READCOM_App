//Description: READ-COM BitmapImageStoryItem View
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.BitmapImageStoryItem;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Objects, //for TImage
  FMX.Clipboard, //for IFMXExtendedClipboardService
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ExtCtrls, FMX.Layouts,
  FMX.Surfaces, //for TBitmapSurface
  FMX.SVGIconImage, //for TSVGIconImage
  READCOM.App.Models, //for IBitmapImageStoryItem, IImageStoryItem, IStoryItem, IStoreable
  READCOM.Views.VectorImageStoryItem, READCOM.Views.ImageStoryItem; //for TVectorImageStoryItem

const
  EXT_PNG = '.png';
  EXT_JPG = '.jpg';
  EXT_JPEG = '.jpeg';
  FILTER_BITMAP_IMAGE_TITLE = 'Bitmap images (*.png, *.jpg, *.jpeg)';
  FILTER_BITMAP_IMAGE_EXTS = '*' + EXT_PNG + ';*' + EXT_JPG + ';*' + EXT_JPEG;
  FILTER_BITMAP_IMAGE = FILTER_BITMAP_IMAGE_TITLE + '|' + FILTER_BITMAP_IMAGE_EXTS;

   //TODO: temporarily extending from VectorStoryImage so supporting both vector and bitmap images
  FILTER_IMAGE_TITLE = 'Images (*.svg, *.png, *.jpg, *.jpeg)';
  FILTER_IMAGE_EXTS = FILTER_VECTOR_IMAGE_EXTS + ';' + FILTER_BITMAP_IMAGE_EXTS;
  FILTER_IMAGE = FILTER_IMAGE_TITLE + '|' + FILTER_IMAGE_EXTS;

type

  {$REGION 'TBitmapImageStoryItem' ----------------------------------------------}

  TBitmapImageStoryItem = class(TVectorImageStoryItem, IBitmapImageStoryItem, IVectorImageStoryItem, IImageStoryItem, IStoryItem, IStoreable)
    ImageControl: TImage;

  //--- Methods ---

  protected
    procedure Loaded; override;
    procedure UpdateGlyphVisibility;

    {Z-Order}
    function GetBackIndex: Integer; override;

    {Clipboard}
    procedure Paste(const Clipboard: IFMXExtendedClipboardService); overload; override;
    procedure PasteImage(const BitmapSurface: TBitmapSurface); override;

    {Image}
    function GetImage: TImage; override;
    procedure SetImage(const Value: TImage); override;

    {Options}
    function GetOptions: IStoryItemOptions; override;

    {EditMode}
    procedure SetEditMode(const Value: Boolean); override;

    function GetStoreBitmap: Boolean;

  public
    constructor Create(AOwner: TComponent); override;

    {$region 'IStoreable'}
    function GetLoadFilesFilter: String; override;
    function Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM; const CreateNew: Boolean = false): TObject; overload; override;
    function LoadBitmap(const Stream: TStream): TObject; virtual;
    {$endregion}

  //--- Properties ---
  published
    property Image: TImage read GetImage write SetImage stored GetStoreBitmap default nil; //overrides ancestor's "stored" setting

  end;

  {$ENDREGION ...................................................................}

  {$REGION 'TBitmapImageStoryItemFactory' ---------------------------------------}

  TBitmapImageStoryItemFactory = class(TInterfacedObject, IStoryItemFactory)
    function New(const AOwner: TComponent = nil): IStoryItem;
  end;

  {$ENDREGION ...................................................................}

  procedure Register;

implementation
  uses
    READCOM.Views.StoryItemFactory, //for StoryItemFactories, StoryItemAddFileFilter
    READCOM.Views.Options.BitmapImageStoryItemOptions; //for TBitmapImageStoryItemOptions

{$R *.fmx}

{$REGION 'TBitmapImageStoryItem'}

{$region 'Lifetime management'}

constructor TBitmapImageStoryItem.Create(AOwner: TComponent);

  procedure SetImageControlZorder;
  begin
    BeginUpdate;
    RemoveObject(ImageControl);
    InsertObject(GetBackIndex - 1, ImageControl);
    EndUpdate;
  end;

  procedure InitImageControl;
  begin
    with ImageControl do
    begin
      Stored := false; //don't store state, should use state from designed .FMX resource
      SetSubComponent(true);
      SetImageControlZorder;
      HitTest := false;
    end;
  end;

begin
  inherited;

  InitImageControl;
  Glyph.Visible := true;
end;

procedure TBitmapImageStoryItem.UpdateGlyphVisibility;
begin
  var img := ImageControl.Bitmap.Image;
  Glyph.Visible := not (Assigned(img) and (img.Width <> 0) and (img.Height <> 0)); //hide default Glyph if we have a non-empty bitmap image
end;

procedure TBitmapImageStoryItem.Loaded;
begin
  inherited;
  UpdateGlyphVisibility;
end;

{$endregion}

{$region 'Z-order'}

function TBitmapImageStoryItem.GetBackIndex: Integer;
begin
  result := inherited + 1; //reserve two more places at the bottom for ImageControl
end;

{$endregion}

{$region 'Clipboard'}

procedure TBitmapImageStoryItem.Paste(const Clipboard: IFMXExtendedClipboardService);
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

procedure TBitmapImageStoryItem.PasteImage(const BitmapSurface: TBitmapSurface);
begin
  ImageControl.Bitmap.Assign(BitmapSurface);
  UpdateGlyphVisibility;

  if FAutoSize then
    SetSize(ImageControl.Bitmap.Width, ImageControl.Bitmap.Height); //TODO: probably not needed
end;

{$endregion}

{$region 'IStorable'}

function TBitmapImageStoryItem.GetLoadFilesFilter: String;
begin
  result := FILTER_BITMAP_IMAGE + '|' + inherited;
end;

function TBitmapImageStoryItem.Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM; const CreateNew: Boolean = false): TObject;
begin
  if (ContentFormat = EXT_PNG) or (ContentFormat = EXT_JPG) or (ContentFormat = EXT_JPEG) then //load EXT_PNG, EXT_JPG, EXT_JPEG
    result := LoadBitmap(Stream)
  else
    result := inherited; //load formats supported by ancestor
end;

function TBitmapImageStoryItem.LoadBitmap(const Stream: TStream): TObject;
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

function TBitmapImageStoryItem.GetStoreBitmap: Boolean;
begin
  result := not FStoreSVG;
end;

function TBitmapImageStoryItem.GetImage: TImage;
begin
  result := ImageControl;
end;

procedure TBitmapImageStoryItem.SetImage(const Value: TImage);
begin
  if not Assigned(Value) then exit;

  ImageControl.Bitmap.Assign(Value.Bitmap); //can't assign TImage directly

  UpdateGlyphVisibility;

  if FAutoSize then
    SetSize(ImageControl.Bitmap.Width, ImageControl.Bitmap.Height); //TODO: probably not needed
end;

{$endregion}

{$region 'Options'}

function TBitmapImageStoryItem.GetOptions: IStoryItemOptions;
begin
  if not Assigned(FOptions) then
    begin
    FOptions := TBitmapImageStoryItemOptions.Create(nil); //don't set storyitem as owner, seems to always store it (irrespective of "Stored := false")
    FOptions.StoryItem := Self;
    end;

  result := FOptions;
end;

{$endregion}

{$region 'EditMode'}

procedure TBitmapImageStoryItem.SetEditMode(const Value: Boolean);
begin
  inherited;
  ImageControl.SendToBack; //make the bitmap image show under the DropTarget
end;

{$endregion}

{$ENDREGION}

{$ENDREGION}

{$REGION 'TBitmapImageStoryItemFactory'}

function TBitmapImageStoryItemFactory.New(const AOwner: TComponent = nil): IStoryItem;
begin
  result := TBitmapImageStoryItem.Create(AOwner);
end;

{$ENDREGION}

{$region 'Registration'}

procedure RegisterSerializationClasses;
begin
  RegisterFmxClasses([TBitmapImageStoryItem]);
end;

procedure Register;
begin
  GroupDescendentsWith(TBitmapImageStoryItem, TControl);
  RegisterSerializationClasses;
  RegisterComponents('Zoomicon', [TBitmapImageStoryItem]);
end;

{$endregion}

initialization
  StoryItemFactories.Add([EXT_SVG, EXT_PNG, EXT_JPG, EXT_JPEG], TBitmapImageStoryItemFactory.Create); //TODO: temporarily extending from VectorStoryImage so supporting both vector and bitmap images
  AddStoryItemFileFilter(FILTER_BITMAP_IMAGE_TITLE, FILTER_BITMAP_IMAGE_EXTS);
  AddStoryItemFileFilter(FILTER_IMAGE_TITLE, FILTER_IMAGE_EXTS); //TODO: temporarily extending from VectorStoryImage so supporting both vector and bitmap images

  RegisterSerializationClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
