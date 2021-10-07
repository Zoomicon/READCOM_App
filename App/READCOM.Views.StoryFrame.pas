unit READCOM.Views.StoryFrame;

interface

uses
  READCOM.Views.StoryItem,
  READCOM.Views.StoryItem.SVG, //for TStoryItemSVG
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Zoomicon.Manipulator, FMX.ExtCtrls;

type
  {A TStoryFrame is a TStoryItem that can host other TStoryItems, including of its own kind [nested TStoryFrames] and is the only thing that takes part in navigation [TAB flow] when not in edit mode}
  TStoryFrame = class(TStoryItem)
    StoryItemSVG1: TStoryItemSVG;
    protected
      procedure DoEditModeChange(const Value: Boolean);
    public
      function GetDropFilter: String; override;
      procedure LoadFile(const Filepath: String); override;
  end;

implementation
//  uses READCOM.Views.StoryItem.SVG; //for TStoryItemSVG

{$R *.fmx}

{ TStoryFrame }

procedure TStoryFrame.DoEditModeChange(const Value: Boolean);
begin
  inherited
  TabStop := true; //always do tab stop navigation between TStoryFrames (irrespective of EditMode)
end;

function TStoryFrame.GetDropFilter: String;
begin
  result := 'SVG vector images (*.svg)|*.svg';
end;

procedure TStoryFrame.LoadFile(const Filepath: String);
begin
  var StoryItem := TStoryItemSVG.Create(Self);
  StoryItem.Parent := Manipulator;
end;

end.
