//Description: READ-COM VectorImageStoryItem View
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.VectorImageStoryItem;

interface

uses
  READCOM.App.Models, //for IVectorImageStoryItem, IImageStoryItem, IStoryItem, IStoreable
  READCOM.Views.ImageStoryItem, //for TImageStoryItem
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.SVGIconImage,
  FMX.ExtCtrls, FMX.Controls.Presentation;

const
  EXT_SVG = '.svg';
  FILTER_SVG = 'SVG vector images (*.svg)|*.svg';

type
  TVectorImageStoryItem = class(TImageStoryItem, IVectorImageStoryItem, IImageStoryItem, IStoryItem, IStoreable)
  private
    function GetSVGText: String;
    procedure SetSVGText(const Value: String);

  //--- Methods ---

  protected
    { Image }
    function GetImage: TImage; override;
    procedure SetImage(const Value: TImage); override; //allows only TSVGIconImage

    { SVGImage }
    function GetSVGImage: TSVGIconImage;
    procedure SetSVGImage(const Value: TSVGIconImage);

  public
    constructor Create(AOnwer: TComponent); override;

    {$region 'IStoreable'}
    function GetLoadFilesFilter: String; override;
    procedure Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload; override;
    procedure LoadSVG(const Stream: TStream); virtual;
    {$endregion}

  //--- Properties ---

  published
    property Image: TImage read GetImage stored false; //overrides ancestor's "write" and "stored" settings
    property SVGImage: TSVGIconImage read GetSVGImage write SetSVGImage stored false default nil;
    property SVGText: String read GetSVGText write SetSVGText;
    property AutoSize default true;
  end;

  procedure Register;

implementation

{$R *.fmx}

{ TVectorImageStoryItem }

constructor TVectorImageStoryItem.Create(AOnwer: TComponent);
begin
  inherited;
  FAutoSize := true;
end;

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
  if FAutoSize then
    Glyph.Align := TAlignLayout.None;
  var bitmap := Glyph.MultiResBitmap[0] as TSVGIconFixedBitmapItem;
  bitmap.SVG.LoadFromStream(Stream); //TODO: should fix to read size info from SVG
  //bitmap.SVG.FixedColor := TAlphaColorRec.Red;
  bitmap.DrawSVGIcon;
  if FAutoSize then
    begin
    //SetSize(bitmap.Width, bitmap.Height); //TODO
    SetSize(100,100);
    Glyph.Align := TAlignLayout.Contents;
    end;
end;

{$endregion}

{$REGION '--- PROPERTIES ---'}

{$region 'Image'}

function TVectorImageStoryItem.GetImage: TImage;
begin
  result := Glyph;
end;

procedure TVectorImageStoryItem.SetImage(const Value: TImage);
begin
  SVGImage := Value As TSVGIconImage;
end;

{$endregion}

{$region 'SVGImage'}

function TVectorImageStoryItem.GetSVGImage: TSVGIconImage;
begin
  result := Glyph;
end;

procedure TVectorImageStoryItem.SetSVGImage(const Value: TSVGIconImage);
begin
  SVGText := (Value.MultiResBitmap[0] as TSVGIconFixedBitmapItem).SVGText; //this will also set Size
end;

{$endregion}

{$region 'SVGText'}

function TVectorImageStoryItem.GetSVGText: String;
begin
  result := (Glyph.MultiResBitmap[0] as TSVGIconFixedBitmapItem).SVGText;
end;

procedure TVectorImageStoryItem.SetSVGText(const Value: String);
begin
  if FAutoSize then
    Glyph.Align := TAlignLayout.None;
  (Glyph.MultiResBitmap[0] as TSVGIconFixedBitmapItem).SVGText := Value;
  if FAutoSize then
    begin
    Size.Size := Glyph.Size.Size;
    Glyph.Align := TAlignLayout.Contents;
    end;
end;

{$endregion}

{$ENDREGION}

procedure RegisterClasses;
begin
  RegisterFmxClasses([TVectorImageStoryItem]); //register for persistence
end;

procedure Register;
begin
  GroupDescendentsWith(TVectorImageStoryItem, TControl);
  RegisterClasses;
  RegisterComponents('Zoomicon', [TVectorImageStoryItem]);
end;

initialization
  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
