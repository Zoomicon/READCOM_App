unit READCOM.Views.StoryItem.SVG;

interface

uses
  READCOM.App.Models, //for ILoadable
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.SVGIconImage, READCOM.Views.StoryItem, Zoomicon.Manipulator,
  FMX.ExtCtrls, FMX.Controls.Presentation;

type
  TStoryItemSVG = class(TStoryItem, ILoadable)
    SVGIconImage: TSVGIconImage;
  public
    {$region 'ILoadable'}
    function GetLoadFilesFilter: String; override;
    procedure Load(const Stream: TStream); overload; override;
    procedure Load(const Filepaths: Array of String); overload; override;
    {$endregion}
  end;

implementation

{$R *.fmx}

{ TStoryItemSVG }

{$region 'ILoadable'}

function TStoryItemSVG.GetLoadFilesFilter: String;
begin
  result := 'SVG vector images (*.svg)|*.svg';
end;

procedure TStoryItemSVG.Load(const Stream: TStream);
begin
  var bitmap := SVGIconImage.MultiResBitmap[0] as TSVGIconFixedBitmapItem;
  bitmap.SVG.LoadFromStream(Stream);
  //bitmap.SVG.FixedColor := TAlphaColorRec.Red;
  bitmap.DrawSVGIcon;
  if FAutoSize then
    SetSize(bitmap.Width, bitmap.Height);
end;

procedure TStoryItemSVG.Load(const Filepaths: Array of String);
begin
  for var f in Filepaths do
  begin
    Load(f);
    exit; //we need just the 1st file in case multiple were dropped
  end;
end;

{$endregion}

end.
