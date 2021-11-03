unit READCOM.Views.Options.StoryItemOptions;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts,
  READCOM.App.Models, FMX.Controls.Presentation; //for IStoryItemOptions

type
  TStoryItemOptions = class(TFrame, IStoryItemOptions)
    GridPanelLayout: TGridPanelLayout;
    btnDelete: TSpeedButton;
    btnCloseOptions: TSpeedButton;
    procedure btnCloseOptionsClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  protected
    FPopup: TPopup;
    procedure CheckCreatePopup;
  public
    function GetStoryItem: IStoryItem;
    procedure ShowPopup;
    procedure HidePopup;
    property Popup: TPopup read FPopup write FPopup stored false;
  end;

implementation

{$R *.fmx}

{ TStoryItemOptions }

function TStoryItemOptions.GetStoryItem: IStoryItem;
begin
  result := Owner as IStoryItem; //assuming options frame is owned by an IStoryItem implementor
end;

procedure TStoryItemOptions.btnCloseOptionsClick(Sender: TObject);
begin
  HidePopup;
end;

procedure TStoryItemOptions.btnDeleteClick(Sender: TObject);
begin
  FreeAndNil(GetStoryItem As TComponent);
end;

procedure TStoryItemOptions.CheckCreatePopup;
begin
  if not Assigned(FPopup) then
  begin
    var component := GetStoryItem.GetComponent;
    var popup := TPopup.Create(component);
    var options := Self;
    with popup do
    begin
      //Parent := (component as TFmxObject);
      //PlacementTarget := (component as TControl);
      AddObject(options);
      Placement:=TPlacement.MouseCenter;
      Width := options.Width;
      Height := options.Height;
      //PlacementRectangle:= TBounds.Create(RectF(0, 0, Width, Height));
    end;
    FPopup := popup;
  end;
end;

procedure TStoryItemOptions.ShowPopup;
begin
  CheckCreatePopup;
  if Assigned(FPopup) then
    FPopup.IsOpen := true;
end;

procedure TStoryItemOptions.HidePopup;
begin
  if Assigned(FPopup) then
  begin
    FPopup.IsOpen := false;
    //FreeAndNil(FPopup); //TODO: maybe should do to save resources
  end;
end;

end.
