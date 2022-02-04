//Description: READ-COM VectorImageStoryItem View
//Author: George Birbilis (http://zoomicon.com)

//TODO: merge ImageStoryItem with its children BitmapImageStoryItem and VectorImageStoryItem so that any ImageStoryItem can load its state from other "image" .READCOM file, be it bitmap or vector based (users don't need to understand the difference)

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
  FILTER_VECTOR_IMAGE_TITLE = 'Vector images (*.svg)';
  FILTER_VECTOR_IMAGE_EXTS = '*' + EXT_SVG;
  FILTER_VECTOR_IMAGE = FILTER_VECTOR_IMAGE_TITLE + '|' + FILTER_VECTOR_IMAGE_EXTS;

type
  TVectorImageStoryItem = class(TImageStoryItem, IVectorImageStoryItem, IImageStoryItem, IStoryItem, IStoreable)
  //--- Methods ---

  protected
    FStoreSVG: Boolean;

    {Image}
    function GetImage: TImage; override;
    procedure SetImage(const Value: TImage); override; //allows only TSVGIconImage

    {SVGImage}
    function GetSVGImage: TSVGIconImage;
    procedure SetSVGImage(const Value: TSVGIconImage);

    {SVGText}
    function GetSVGText: String;
    procedure SetSVGText(const Value: String);

    procedure Resize; override;

  public

    {$region 'IStoreable'}
    function GetLoadFilesFilter: String; override;
    procedure Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload; override;
    procedure LoadSVG(const Stream: TStream); virtual;
    {$endregion}

  //--- Properties ---

  published
    property Image: TImage read GetImage stored false; //overrides ancestor's "write" and "stored" settings
    property SVGImage: TSVGIconImage read GetSVGImage write SetSVGImage stored false default nil;
    property SVGText: String read GetSVGText write SetSVGText stored FStoreSVG;
    property AutoSize default true;
  end;

  TVectorImageStoryItemFactory = class(TInterfacedObject, IStoryItemFactory)
    function New(const AOwner: TComponent = nil): IStoryItem;
  end;

  procedure Register;

implementation
  uses
    READCOM.Views.StoryItemFactory; //for StoryItemFactories, StoryItemAddFileFilter

{$R *.fmx}

{$REGION 'TVectorImageStoryItem'}

procedure TVectorImageStoryItem.Resize;
begin
  var tmp := SVGText;
  SVGText := '';
  SVGText := tmp; //recalculate bitmap from the SVG //TODO: not sure if the "tmp" step is needed, or if it recalculates the buffer at all (probably doesn't have the new size at this point?)
end;

{$region 'IStoreable'}

function TVectorImageStoryItem.GetLoadFilesFilter: String;
begin
  result := FILTER_VECTOR_IMAGE;
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
  FStoreSVG := true; //mark that we loaded custom SVG
  bitmap.DrawSVGIcon;
  if FAutoSize then
    begin
    //SetSize(bitmap.Width, bitmap.Height); //TODO: seems SVG size doesn't get loaded
    SetSize(100,100); //TODO: fix this
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
  if Assigned(Glyph) then
    result := (Glyph.MultiResBitmap[0] as TSVGIconFixedBitmapItem).SVGText
  else
    result := '';
end;

procedure TVectorImageStoryItem.SetSVGText(const Value: String);
begin //TODO: should restore default Glyph (keep it to some global/static var once?) if SVGText is set to ''
  if Assigned(Glyph) then
  begin
    if FAutoSize then
      Glyph.Align := TAlignLayout.None;

    var bitmap := Glyph.MultiResBitmap[0] as TSVGIconFixedBitmapItem;
    bitmap.SVGText := Value;
    if FAutoSize then //TODO: shouldn't hardcode any size here, item should keep its Width/Height when loading this property
      begin
      //SetSize(bitmap.Width, bitmap.Height); //TODO: seems SVG size doesn't get loaded
      //SetSize(100,100);
      Glyph.Align := TAlignLayout.Contents;
      end;

    FStoreSVG := true; //mark that we loaded custom SVG
  end;
end;

{$endregion}

{$ENDREGION}

{$ENDREGION}

{$REGION 'TVectorImageStoryItemFactory'}

function TVectorImageStoryItemFactory.New(const AOwner: TComponent = nil): IStoryItem;
begin
  result := TVectorImageStoryItem.Create(AOwner);
end;

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
  StoryItemFactories.Add([EXT_SVG], TVectorImageStoryItemFactory.Create);
  AddStoryItemFileFilter(FILTER_VECTOR_IMAGE_TITLE, FILTER_VECTOR_IMAGE_EXTS);

  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
