//Description: READ-COM BitmapImageStoryItem View
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.BitmapImageStoryItem;

interface

uses
  READCOM.App.Models, //for IBitmapImageStoryItem, IImageStoryItem, IStoryItem, IStoreable
  READCOM.Views.ImageStoryItem, //for TImageStoryItem
  FMX.Objects, //for TImage
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ExtCtrls, FMX.Layouts, FMX.SVGIconImage;

const
  EXT_PNG = '.png';
  EXT_JPG = '.jpg';
  EXT_JPEG = '.jpeg';
  FILTER_BITMAP_IMAGE_TITLE = 'Bitmap images (*.png, *.jpg, *.jpeg)';
  FILTER_BITMAP_IMAGE_EXTS = '*' + EXT_PNG + ';*' + EXT_JPG + ';*' + EXT_JPEG;
  FILTER_BITMAP_IMAGE = FILTER_BITMAP_IMAGE_TITLE + '|' + FILTER_BITMAP_IMAGE_EXTS;

type

  {$REGION 'TBitmapImageStoryItem' ----------------------------------------------}

  TBitmapImageStoryItem = class(TImageStoryItem, IBitmapImageStoryItem, IImageStoryItem, IStoryItem, IStoreable)
    ImageControl: TImage;

  //--- Methods ---

  protected
    {Image}
    function GetImage: TImage; override;
    procedure SetImage(const Value: TImage); override;
    {Options}
    function GetOptions: IStoryItemOptions; override;
    {EditMode}
    procedure SetEditMode(const Value: Boolean); override;

    procedure Loaded; override;

  public
    constructor Create(AOwner: TComponent); override;

    {$region 'IStoreable'}
    function GetLoadFilesFilter: String; override;
    procedure Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload; override;
    procedure LoadBitmap(const Stream: TStream); virtual;
    {$endregion}

  //--- Properties ---
  published
    property Image: TImage read GetImage write SetImage stored true default nil; //overrides ancestor's "stored" setting

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

constructor TBitmapImageStoryItem.Create(AOwner: TComponent);
begin
  inherited;

  with ImageControl do
  begin
    Stored := false; //don't store state, should use state from designed .FMX resource
    SetSubComponent(true);
    SendToBack;
    HitTest := false;
  end;
end;

procedure TBitmapImageStoryItem.Loaded;
begin
  inherited;
  Glyph.Visible := not Assigned(ImageControl.Bitmap.Image); //hide default Glyph if we have a bitmap image
end;

{$region 'IStorable'}

function TBitmapImageStoryItem.GetLoadFilesFilter: String;
begin
  result := FILTER_BITMAP_IMAGE;
end;

procedure TBitmapImageStoryItem.Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM);
begin
  if (ContentFormat = EXT_PNG) or (ContentFormat = EXT_JPG) or (ContentFormat = EXT_JPEG) then //load EXT_PNG, EXT_JPG, EXT_JPEG
    LoadBitmap(Stream)
  else
    inherited; //load EXT_READCOM
end;

procedure TBitmapImageStoryItem.LoadBitmap(const Stream: TStream);
begin
  ImageControl.Bitmap.LoadFromStream(Stream); //TODO: does it detect PNG and JPEG automatically?
  if FAutoSize then
    SetSize(ImageControl.Bitmap.Width, ImageControl.Bitmap.Height); //TODO: probably not needed
end;

{$endregion}

{$REGION '--- PROPERTIES ---'}

{$region 'Image'}

function TBitmapImageStoryItem.GetImage: TImage;
begin
  result := ImageControl;
end;

procedure TBitmapImageStoryItem.SetImage(const Value: TImage);
begin
  ImageControl.Bitmap.Assign(Value.Bitmap); //can't assign TImage directly
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

procedure RegisterClasses;
begin
  RegisterFmxClasses([TBitmapImageStoryItem]); //register for persistence
end;

procedure Register;
begin
  GroupDescendentsWith(TBitmapImageStoryItem, TControl);
  RegisterClasses;
  RegisterComponents('Zoomicon', [TBitmapImageStoryItem]);
end;

initialization
  StoryItemFactories.Add([EXT_PNG, EXT_JPG, EXT_JPEG], TBitmapImageStoryItemFactory.Create);
  AddStoryItemFileFilter(FILTER_BITMAP_IMAGE_TITLE, FILTER_BITMAP_IMAGE_EXTS);

  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
