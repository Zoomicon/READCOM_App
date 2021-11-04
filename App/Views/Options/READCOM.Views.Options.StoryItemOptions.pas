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
    btnLoad: TSpeedButton;
    OpenDialog: TOpenDialog;
    btnSave: TSpeedButton;
    SaveDialog: TSaveDialog;
    procedure btnCloseOptionsClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnLoadClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);

  protected
    FPopup: TPopup;
    procedure CheckCreatePopup;
    {StoryItem}
    function GetStoryItem: IStoryItem;
    {DeleteVisible}
    function IsDeleteVisible: Boolean;
    procedure SetDeleteVisible(const Value: Boolean);

  public
    procedure ShowPopup;
    procedure HidePopup;
    property Popup: TPopup read FPopup write FPopup stored false;

  published
    property StoryItem: IStoryItem read GetStoryItem stored false;
    property DeleteVisible: Boolean read IsDeleteVisible write SetDeleteVisible stored false;
  end;

implementation

{$R *.fmx}

{ TStoryItemOptions }

{$region 'StoryItem'}

function TStoryItemOptions.GetStoryItem: IStoryItem;
begin
  result := Owner as IStoryItem; //assuming options frame is owned by an IStoryItem implementor
end;

{$endregion}

{$region 'DeleteVisible'}

function TStoryItemOptions.IsDeleteVisible: Boolean;
begin
  result := btnDelete.Visible;
end;

procedure TStoryItemOptions.SetDeleteVisible(const Value: Boolean);
begin
  btnDelete.Visible := Value;
end;

{$endregion}

{$region 'Buttons'}

procedure TStoryItemOptions.btnCloseOptionsClick(Sender: TObject);
begin
  HidePopup;
end;

procedure TStoryItemOptions.btnDeleteClick(Sender: TObject);
begin
  FreeAndNil(GetStoryItem As TComponent);
end;

procedure TStoryItemOptions.btnLoadClick(Sender: TObject);
begin
  with OpenDialog do
  begin
    Filter := StoryItem.GetLoadFilesFilter;
    if Execute then //TODO: see if supported on Android (https://stackoverflow.com/questions/69138504/why-does-fmx-topendialog-not-work-in-android)
      StoryItem.Load(Filename);
  end;
end;

procedure TStoryItemOptions.btnSaveClick(Sender: TObject);
begin
  with SaveDialog do
  begin
    DefaultExt := EXT_READCOM;
    Filter := StoryItem.GetSaveFilesFilter;
    if Execute then //TODO: see if supported on Android (https://stackoverflow.com/questions/69138504/why-does-fmx-topendialog-not-work-in-android)
      StoryItem.Save(Filename);
  end;
end;

{$endregion}

procedure TStoryItemOptions.CheckCreatePopup;
begin
  if not Assigned(FPopup) then
  begin
    var component := GetStoryItem.View;
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

{$region 'Popup'}

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

{$endregion}

end.
