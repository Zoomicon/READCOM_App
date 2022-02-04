//Description: READ-COM ImageStoryItem View
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.ImageStoryItem;

interface

uses
  READCOM.App.Models, //for IImageStoryItem
  READCOM.Views.StoryItem, //for TStoryItem
  FMX.Objects, //for TImage
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ExtCtrls, FMX.SVGIconImage;

type
  TImageStoryItem = class abstract(TStoryItem, IImageStoryItem, IStoryItem, IStoreable)
  //--- Methods ---
  protected
    {Image}
    function GetImage: TImage; virtual; abstract;
    procedure SetImage(const Value: TImage); virtual; abstract;

  //--- Properties ---
  published
    property Image: TImage read GetImage write SetImage stored false;
  end;

implementation

{$R *.fmx}

end.
