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
    FStoryItem: IStoryItem;
    FPopup: TPopup;
    procedure CheckCreatePopup;

    {StoryItem}
    function GetStoryItem: IStoryItem;
    procedure SetStoryItem(const Value: IStoryItem);

    {View}
    function GetView: TControl;

    {DeleteVisible}
    function IsDeleteVisible: Boolean;
    procedure SetDeleteVisible(const Value: Boolean);

  public
    destructor Destroy; override;

    procedure ShowPopup; //TODO: use PopupVisible boolean property instead
    procedure HidePopup;
    property Popup: TPopup read FPopup write FPopup stored false;

  published
    property View: TControl read GetView stored false;
    property StoryItem: IStoryItem read GetStoryItem write SetStoryItem stored false;
    property DeleteVisible: Boolean read IsDeleteVisible write SetDeleteVisible stored false;
  end;

implementation

{$R *.fmx}

{ TStoryItemOptions }

destructor TStoryItemOptions.Destroy;
begin
  if Assigned(FPopup) then
    FPopup.RemoveObject(Self); //must do before FreeAndNil(FPopup) else it fails when we call "inherited" below (the popup seems to kill us though it wasn't our owner, we were just its child/content)

  FreeAndNil(FPopup);

  inherited; //do last
end;

{$region 'StoryItem'}

function TStoryItemOptions.GetStoryItem: IStoryItem;
begin
  result := FStoryItem;
end;

procedure TStoryItemOptions.SetStoryItem(const Value: IStoryItem);
begin
  FStoryItem := Value;
end;

{$endregion}

{$region 'View'}

function TStoryItemOptions.GetView: TControl;
begin
  result := Self;
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
    var popup := TPopup.Create(nil); //don't set StoryItem.View as owner, seems to always store it (irrespective of "Stored := false") //can't set Self as owner either, makes a circular reference
    var options := Self;
    with popup do
    begin
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
