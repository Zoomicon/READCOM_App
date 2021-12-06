//Description: READ-COM PanelStoryItem View
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.PanelStoryItem;

interface

uses
  READCOM.App.Models, //for IPanelStoryItem, IStoreable
  READCOM.Views.StoryItem, //for TStoryItem
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.ExtCtrls, FMX.Objects, FMX.SVGIconImage;

type
  {A PanelStoryItem is the only thing that takes part in navigation [TAB flow] when not in edit mode}
  TPanelStoryItem = class(TStoryItem, IPanelStoryItem, IStoryItem, IStoreable)
    //--- Methods ---

    protected
      procedure DoEditModeChange(const Value: Boolean);
      procedure SetEditMode(const Value: Boolean); override;

    public
      constructor Create(AOwner: TComponent); override;

      { IStoreable }
      function GetLoadFilesFilter: String; override;
      procedure Load(const Filepath: String); override;
      procedure Load(const Filepaths: array of String); override;

      procedure NavigateTo;

      { Active }
      function IsActive: Boolean;
      procedure SetActive(const Value: Boolean);

      { Navigatable }
      function IsNavigatable: Boolean;
      procedure SetNavigatable(const Value: Boolean);

      { NavigationOrder }
      function GetNavigationOrder: Integer;
      procedure SetNavigationOrder(const Value: Integer);

    //--- Events ---

    public
      procedure HandleStoryModeChanged;

    //--- Properties ---

    published
      property Active: Boolean read IsActive write SetActive; //default false
      property Navigatable: Boolean read IsNavigatable write SetNavigatable; //default true
      property NavigationOrder: Integer read GetNavigationOrder write SetNavigationOrder; //default 0
  end;

implementation
  uses
    Zoomicon.Generics.Collections, //for TObjectListEx
    READCOM.Views.BitmapImageStoryItem, //for TBitmapImageStoryItem
    READCOM.Views.VectorImageStoryItem, //for TVectorImageStoryItem
    READCOM.Views.AudioStoryItem, //for TAudioStoryItem
    READCOM.Views.TextStoryItem, //for TTextStoryItem
    System.IOUtils; //for TPath

{$R *.fmx}

{ TPanelStoryItem }

constructor TPanelStoryItem.Create(AOwner: TComponent);
begin
  inherited;
  BorderVisible := true;
  Navigatable := true;
end;

procedure TPanelStoryItem.DoEditModeChange(const Value: Boolean);
begin
  inherited;

  TabStop := true; //always do tab stop navigation between TStoryFrames (irrespective of EditMode)
end;

procedure TPanelStoryItem.SetEditMode(const Value: Boolean);
begin
  inherited;

  BorderVisible := true; //always show Border
end;

{$region 'IStoreable'}

function TPanelStoryItem.GetLoadFilesFilter: String;
begin
  result := 'READ-COM Files, Images, Audio, Text|*.readcom;*.png;*.jpg;*.jpeg;*.svg;*.mp3; *.txt' + '|' + FILTER_READCOM + '|' + FILTER_SVG + '|' + FILTER_PNG_JPEG_JPG + '|' + FILTER_MP3 + '|' + FILTER_TXT;
end;

function RemoveNonAllowedIdentifierChars(const s: String): String;
begin
  var count := s.Length;
  var builder := TStringBuilder.Create(count);
  try
    for var i := 1 to count do //strings are 1-indexed
      begin
      var c := s[i];
      if IsValidIdent(c) then //keep only characters that would be allowed by themselves as an Identifier
        builder.Append(c);
      end;
    result := builder.ToString;
  finally
    FreeAndNil(builder);
  end;
end;

procedure TPanelStoryItem.Load(const Filepath: String);
var StoryItemClass: TStoryItemClass;
begin
  var FileExt := ExtractFileExt(Filepath);

  if (FileExt =  EXT_READCOM) then
    StoryItemClass := TStoryItem //TODO: this is wrong, don't want to add an item that then hosts something in it
  else if (FileExt = EXT_SVG) then
    StoryItemClass := TVectorImageStoryItem
  else if (FileExt = EXT_PNG) or (FileExt = EXT_JPEG) or (FileExt = EXT_JPG) then
    StoryItemClass := TBitmapImageStoryItem
  else if (FileExt = EXT_MP3) then
    StoryItemClass := TAudioStoryItem
  else if (FileExt = EXT_TXT) then
    StoryItemClass := TTextStoryItem
  else
    exit;

  var StoryItem := StoryItemClass.Create(Self);

  StoryItem.Name := RemoveNonAllowedIdentifierChars(TPath.GetFileNameWithoutExtension(Filepath)) + IntToStr(Random(maxint)); //TODO: use a GUID
  StoryItem.Load(Filepath); //this should also set the Size of the control

  //Center the new item...
  var ItemSize := StoryItem.Size;
  StoryItem.Position.Point := PointF(Size.Width/2 - ItemSize.Width/2, Size.Height/2 - ItemSize.Height/2); //not creating TPosition objects to avoid leaking (TPointF is a record)

  StoryItem.Parent := Self;
  StoryItem.BringToFront; //load as front-most
end;

procedure TPanelStoryItem.Load(const Filepaths: array of String);
begin
  for var filepath in Filepaths do
    Load(filepath); //Loading all files one by one
end;

{$endregion}

{$region 'NavigationOrder'}

function TPanelStoryItem.GetNavigationOrder: Integer;
begin
  result := TabOrder;
end;

procedure TPanelStoryItem.SetNavigationOrder(const Value: Integer);
begin
  TabOrder := Value;
end;

{$endregion}

procedure TPanelStoryItem.HandleStoryModeChanged;
begin

end;

function TPanelStoryItem.IsActive: Boolean;
begin

end;

procedure TPanelStoryItem.SetActive(const Value: Boolean);
begin
  //TODO
  NavigateTo;
end;

procedure TPanelStoryItem.NavigateTo;
begin
  //TODO: send message to zoom container or find Story Root? (could search for first ancestor that is ZoomPanel but makes dependency to it, better use messaging or ask to be given an IZoomable)
end;

function TPanelStoryItem.IsNavigatable: Boolean;
begin
  result := CanFocus;
end;

procedure TPanelStoryItem.SetNavigatable(const Value: Boolean);
begin
  CanFocus := Value;
end;

end.
