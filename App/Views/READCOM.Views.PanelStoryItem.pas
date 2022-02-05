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

    published
      property StoryPoint default true; //since we set TPanelStoryItem as being StoryPoints by default (whereas ancestor TStoryItem wasn't StoryPoint by default) we need to reflect this here, else "StoryPoint=false" won't be stored and user's setting of a TPanelStoryItem to not be a StoryPoint won't be saved/remembered on load
  end;

  procedure Register;

implementation

{$R *.fmx}

{TPanelStoryItem}

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

procedure RegisterClasses;
begin
  RegisterFmxClasses([TPanelStoryItem]); //register for persistence
end;

procedure Register;
begin
  GroupDescendentsWith(TPanelStoryItem, TControl);
  RegisterClasses;
  RegisterComponents('Zoomicon', [TPanelStoryItem]);
end;

initialization
  //TODO// StoryItemFactories.Add([EXT_READCOM], TPanelStoryItemFactory.Create);
  //TODO// AddStoryItemFileFilter(FILTER_READCOM_TITLE, FILTER_READCOM_EXT);

  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
