unit uStory;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, uStoryPoint, FMX.Layouts;

type
  TStory = class(TFrame)
    FlowLayout: TFlowLayout;
    StoryPoint1: TStoryPoint;
    StoryPoint2: TStoryPoint;
  end;

implementation

{$R *.fmx}

end.
