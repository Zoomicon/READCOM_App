unit READCOM.Views.StoryItem.SVG;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.SVGIconImage, READCOM.Views.StoryItem, Zoomicon.Manipulator,
  FMX.ExtCtrls, FMX.Controls.Presentation;

type
  TStoryItemSVG = class(TStoryItem)
    SVGIconImage1: TSVGIconImage;
  public
    function GetDropFilter: String; override;
    procedure LoadFile(const Filepath: String); override;
    procedure LoadFiles(const Files: array of String); override;
  end;

implementation

{$R *.fmx}

{ TStoryItemSVG }

function TStoryItemSVG.GetDropFilter: String;
begin
  result := 'SVG vector images (*.svg)|*.svg';
end;

procedure TStoryItemSVG.LoadFile(const Filepath: String);
begin
  var InputFileStream := TFileStream.Create(Filepath,  fmOpenRead);
  try
    SVGIconImage1.MultiResBitmap.LoadFromStream(InputFileStream);
  finally
    FreeAndNil(InputFileStream);
  end;
end;

procedure TStoryItemSVG.LoadFiles(const Files: array of String);
begin
  for var f in Files do
    begin
    LoadFile(f);
    exit; //we need just the 1st file in case multiple were dropped
    end;
end;

end.
