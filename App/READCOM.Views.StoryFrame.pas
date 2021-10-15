unit READCOM.Views.StoryFrame;

interface

uses
  READCOM.App.Models, //for ILoadable
  READCOM.Views.StoryItem,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Zoomicon.Manipulator, FMX.ExtCtrls;

type
  {A TStoryFrame is a TStoryItem that can host other TStoryItems, including of its own kind [nested TStoryFrames] and is the only thing that takes part in navigation [TAB flow] when not in edit mode}
  TStoryFrame = class(TStoryItem, IStoreable)
    protected
      procedure DoEditModeChange(const Value: Boolean);
    public
      {$region 'IStoreable'}
      function GetLoadFilesFilter: String; override;
      procedure Load(const Filepath: String); override;
      {$endregion}
  end;

implementation
  uses READCOM.Views.StoryItem.SVG, //for TStoryItemSVG
  System.IOUtils; //for TPath

{$R *.fmx}

{ TStoryFrame }

procedure TStoryFrame.DoEditModeChange(const Value: Boolean);
begin
  inherited
  TabStop := true; //always do tab stop navigation between TStoryFrames (irrespective of EditMode)
end;

{$region 'IStoreable'}

function TStoryFrame.GetLoadFilesFilter: String;
begin
  result := 'SVG vector images (*.svg)|*.svg';
end;

procedure TStoryFrame.Load(const Filepath: String);
begin
  //TODO: check file extensions and create different TStoryItemXX objects (Frames, Images, Sounds, Texts)
  var StoryItemSVG := TStoryItemSVG.Create(Self);
  StoryItemSVG.Name := TPath.GetFileNameWithoutExtension(Filepath) + IntToStr(Random(maxint));

  StoryItemSVG.Load(Filepath); //this should also set the Size of the control

  //Center the item in its parent...
  var ManipulatorSize := Manipulator.Size;
  var ItemSize := StoryItemSVG.Size;
  StoryItemSVG.Position.Point := TPointF.Create(ManipulatorSize.Width/2 - ItemSize.Width/2, ManipulatorSize.Height/2 - ItemSize.Height/2);
  StoryItemSVG.Parent := Manipulator;
end;

{$endregion}

end.
