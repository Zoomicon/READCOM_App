unit READCOM.Views.BitmapImageStoryItem;

interface

uses
  READCOM.App.Models, //for IBitmapImageStoryItem, IImageStoryItem, IStoryItem, IStoreable
  READCOM.Views.ImageStoryItem, //for TImageStoryItem
  FMX.Objects, //for TImage
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Zoomicon.Manipulator, FMX.ExtCtrls, FMX.Layouts;

const
  EXT_PNG = '.png';
  EXT_JPG = '.jpg';
  EXT_JPEG = '.jpeg';
  FILTER_PNG = 'PNG bitmap images (*.png)|*.png';
  FILTER_JPEG_JPG = 'JPEG bitmap images (*.jpg, *.jpeg)|*.jpg;*.jpeg';
  FILTER_PNG_JPEG_JPG = FILTER_PNG + '|' + FILTER_JPEG_JPG;

type
  TBitmapImageStoryItem = class(TImageStoryItem, IBitmapImageStoryItem, IImageStoryItem, IStoryItem, IStoreable)
    ImageControl: TImage;

  //--- Methods ---

  protected
    { Image }
    function GetImage: TImage; override;
    procedure SetImage(const Value: TImage); override;

  public
    constructor Create(AOwner: TComponent); override;

  public
    {$region 'IStoreable'}
    function GetLoadFilesFilter: String; override;
    procedure Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload; override;
    procedure LoadBitmap(const Stream: TStream); virtual;
    {$endregion}

  //--- Properties ---
  published
    property Image: TImage read GetImage write SetImage stored true default nil; //overrides ancestor's "stored" setting

  end;

implementation

{$R *.fmx}

constructor TBitmapImageStoryItem.Create(AOwner: TComponent);
begin
  inherited;
  ImageControl.SetSubComponent(true);
  ImageControl.Stored := false; //don't store state, should use state from designed .FMX resource
end;

{ TBitmapImageStoryItem }

{$region 'IStorable'}

function TBitmapImageStoryItem.GetLoadFilesFilter: String;
begin
  result := FILTER_PNG_JPEG_JPG;
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

{$endregion}

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
  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
