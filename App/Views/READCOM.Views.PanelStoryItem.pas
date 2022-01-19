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
      procedure SetEditMode(const Value: Boolean); override;

    public
      constructor Create(AOwner: TComponent); override;
  end;

implementation

{$R *.fmx}

{ TPanelStoryItem }

constructor TPanelStoryItem.Create(AOwner: TComponent);
begin
  inherited;
  BorderVisible := true;
  StoryPoint := true;
  //ClipChildren := true;
end;

procedure TPanelStoryItem.SetEditMode(const Value: Boolean);
begin
  inherited; //this may hide the Border
  BorderVisible := true; //always show Border
end;

end.
