unit READCOM.Views.VectorImageStoryItem;

interface

uses
  READCOM.App.Models, //for IVectorImageStoryItem, IImageStoryItem, IStoryItem, IStoreable
  READCOM.Views.ImageStoryItem, //for TImageStoryItem
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.SVGIconImage, Zoomicon.Manipulator,
  FMX.ExtCtrls, FMX.Controls.Presentation;

const
  EXT_SVG = '.svg';
  FILTER_SVG = 'SVG vector images (*.svg)|*.svg';

type
  TVectorImageStoryItem = class(TImageStoryItem, IVectorImageStoryItem, IImageStoryItem, IStoryItem, IStoreable)
    SVGIconImage: TSVGIconImage;

  //--- Methods ---

  protected
    { Image }
    function GetImage: TImage; override;
    procedure SetImage(const Value: TImage); override; //allows only TSVGIconImage

    { SVGImage }
    function GetSVGImage: TSVGIconImage;
    procedure SetSVGImage(const Value: TSVGIconImage);

  public
    {$region 'IStoreable'}
    function GetLoadFilesFilter: String; override;
    procedure Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload; override;
    procedure LoadSVG(const Stream: TStream); virtual;
    {$endregion}

  //--- Properties ---

  published
    property Image: TImage read GetImage stored false; //overrides ancestor's "write" and "stored" settings
    property SVGImage: TSVGIconImage read GetSVGImage write SetSVGImage default nil;

  end;

implementation

{$R *.fmx}

{ TVectorImageStoryItem }

{$region 'IStoreable'}

function TVectorImageStoryItem.GetLoadFilesFilter: String;
begin
  result := FILTER_SVG;
end;

procedure TVectorImageStoryItem.Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM);
begin
  if (ContentFormat = EXT_SVG) then //load EXT_SVG
    LoadSVG(Stream)
  else
    inherited; //load EXT_READCOM
end;

procedure TVectorImageStoryItem.LoadSVG(const Stream: TStream);
begin
  var bitmap := SVGIconImage.MultiResBitmap[0] as TSVGIconFixedBitmapItem;
  bitmap.SVG.LoadFromStream(Stream);
  //bitmap.SVG.FixedColor := TAlphaColorRec.Red;
  bitmap.DrawSVGIcon;
  if FAutoSize then
    SetSize(bitmap.Width, bitmap.Height);
end;

{$endregion}

{$REGION '--- PROPERTIES ---'}

{$region 'Image'}

function TVectorImageStoryItem.GetImage: TImage;
begin
  result := SVGIconImage;
end;

procedure TVectorImageStoryItem.SetImage(const Value: TImage);
begin
  SVGImage := Value As TSVGIconImage;
end;

{$endregion}

{$region 'SVGImage'}

function TVectorImageStoryItem.GetSVGImage: TSVGIconImage;
begin
  result := SVGIconImage;
end;

procedure TVectorImageStoryItem.SetSVGImage(const Value: TSVGIconImage);
begin
  SVGIconImage.Assign(Value);
end;

{$endregion}

{$ENDREGION}

end.