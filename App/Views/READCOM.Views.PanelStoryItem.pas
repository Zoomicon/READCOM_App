unit READCOM.Views.PanelStoryItem;

interface

uses
  READCOM.App.Models, //for IPanelStoryItem, IStoreable
  READCOM.Views.StoryItem, //for TStoryItem
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Zoomicon.Manipulator, FMX.ExtCtrls;

type
  {A TStoryFrame is a TStoryItem that can host other TStoryItems, including of its own kind [nested TStoryFrames] and is the only thing that takes part in navigation [TAB flow] when not in edit mode}
  TPanelStoryItem = class(TStoryItem, {IPanelStoryItem,} IStoryItem, IStoreable)
    protected
      procedure DoEditModeChange(const Value: Boolean);
    public
      {$region 'IStoreable'}
      function GetLoadFilesFilter: String; override;
      procedure Load(const Filepath: String); override;
      procedure Load(const Filepaths: array of String); override;
      {$endregion}
  end;

implementation
  uses
    READCOM.Views.BitmapImageStoryItem, //for TBitmapImageStoryItem
    READCOM.Views.VectorImageStoryItem, //for TVectorImageStoryItem
    READCOM.Views.AudioStoryItem, //for TAudioStoryItem
    System.IOUtils; //for TPath

{$R *.fmx}

{ TPanelStoryItem }

procedure TPanelStoryItem.DoEditModeChange(const Value: Boolean);
begin
  inherited
  TabStop := true; //always do tab stop navigation between TStoryFrames (irrespective of EditMode)
end;

{$region 'IStoreable'}

function TPanelStoryItem.GetLoadFilesFilter: String;
begin
  result := FILTER_READCOM + '|' + FILTER_SVG + '|' + FILTER_PNG_JPEG_JPG + '|' + FILTER_MP3 {+ '|' + FILTER_TXT};
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
    StoryItemClass := TAudioStoryItem;

  var StoryItem := StoryItemClass.Create(Self);

  StoryItem.Name := TPath.GetFileNameWithoutExtension(Filepath) + IntToStr(Random(maxint));
  StoryItem.Load(Filepath); //this should also set the Size of the control

  //Center the item in its parent...
  var ManipulatorSize := Manipulator.Size;
  var ItemSize := StoryItem.Size;
  StoryItem.Position.Point := TPointF.Create(ManipulatorSize.Width/2 - ItemSize.Width/2, ManipulatorSize.Height/2 - ItemSize.Height/2);
  StoryItem.Parent := Manipulator;
end;

procedure TPanelStoryItem.Load(const Filepaths: array of String);
begin
  for var filepath in Filepaths do
    Load(filepath); //Loading all files one by one
end;

{$endregion}

end.
