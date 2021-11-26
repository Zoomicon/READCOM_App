unit READCOM.Views.StoryItem;

interface

uses
  READCOM.App.Models, //for IStoryItem
  READCOM.Messages.Models, //for IMessageEditModeChange
  Zoomicon.Manipulator, //for TManipulator
  Zoomicon.Puzzler.Models, //for IHasTarget
  Zoomicon.Puzzler.Classes, //for TControlHasTargetHelper
  iPub.Rtl.Messaging, //for SubscribeAttrible, GMessaging
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.SVGIconImage, FMX.ExtCtrls, FMX.Controls.Presentation;

const
  DEFAULT_AUTOSIZE = true;
  MSG_CONTENT_FORMAT_NOT_SUPPORTED = 'Content format not supported: %s';

type
  IMessageNavigatedTo = IMessageSingleValue<IStoryItem>; //TODO: check that GUID reuse won't cause issues

  TStoryItem = class(TManipulator, IStoryItem, IStoreable, IHasTarget, IMultipleHasTarget) //IHasTarget implemented via TControlHasTargetHelper //IMultipleHasTarget implemented via TControlMultipleHasTargetHelper
    DropTarget: TDropTarget;

  //-- Fields ---

  protected
    FID: TGUID;
    FAutoSize: Boolean;
    FHidden: Boolean;
    FUrlAction: String;
    FOptions: IStoryItemOptions;
    FStoryMode: TStoryMode;

  //--- Methods ---

  protected
    function GetDefaultSize: TSizeF; override;
    procedure SetParent(const Value: TFmxObject); override;
    procedure SetEditMode(const Value: Boolean); override;

    procedure ApplyHidden;
    procedure LoadReadCom(const Stream: TStream); virtual;
    procedure SaveReadCom(const Stream: TStream); virtual;

    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override; //preferring overriden methods instead of event handlers that get stored with saved state
    procedure Tap(const Point: TPointF); override;
    //procedure CanFocus(var ACanFocus: Boolean); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure PlayRandomAudioStoryItem;

    { IStoreable }
    function GetLoadFilesFilter: String; virtual;
    procedure LoadFromString(const Data: String); virtual;
    procedure Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload; virtual;
    procedure Load(const Filepath: string); overload; virtual;
    procedure Load(const Filepaths: array of string); overload; virtual;
    function GetSaveFilesFilter: String; virtual;
    function SaveToString: string; virtual;
    procedure Save(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload; virtual;
    procedure Save(const Filepath: string); overload; virtual;

    { View }
    function GetView: TControl;

    { ParentStoryItem }
    function GetParentStoryItem: IStoryItem;
    procedure SetParentStoryItem(const Value: IStoryItem);

    { StoryItems }
    function GetStoryItems: TIStoryItemList;
    procedure SetStoryItems(const Value: TIStoryItemList);

    { AudioStoryItems }
    function GetAudioStoryItems: TIAudioStoryItemList;

    { Hidden }
    function IsHidden: Boolean;
    procedure SetHidden(const Value: Boolean);

    { Anchored }
    function IsAnchored: Boolean;
    procedure SetAnchored(const Value: Boolean);

    { UrlAction }
    function GetUrlAction: String;
    procedure SetUrlAction(const Value: String);

    { StoryMode }
    function GetStoryMode: TStoryMode;
    procedure SetStoryMode(const Value: TStoryMode);

    { Options }
    function GetOptions: IStoryItemOptions;

  //--- Events ---

  protected
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure DropTargetDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
    procedure DropTargetDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);

  public
    //[Subscribe(TipMessagingThread.Main)]
    //procedure OnNavigatedTo(const AMessage: IMessageNavigatedTo);
    procedure HandleParentNavigatedToChanged;

  //--- Properties ---

  protected
    property AutoSize: Boolean read FAutoSize write FAutoSize default DEFAULT_AUTOSIZE;
    property Options: IStoryItemOptions read GetOptions stored false;

  published
    property ParentStoryItem: IStoryItem read GetParentStoryItem write SetParentStoryItem stored false; //default nil
    property StoryItems: TIStoryItemList read GetStoryItems write SetStoryItems stored false; //default nil
    property AudioStoryItems: TIAudioStoryItemList read GetAudioStoryItems stored false; //default nil
    property Hidden: Boolean read IsHidden write SetHidden; //default false
    property Anchored: Boolean read IsAnchored write SetAnchored; //default false
    property UrlAction: String read GetUrlAction write SetUrlAction; //default nil
    property StoryMode: TStoryMode read GetStoryMode write SetStoryMode stored false;
  end;

  TStoryItemClass = class of TStoryItem;

implementation
  uses
    u_UrlOpen,
    Zoomicon.FMX.Utils, //for TControlFocusHelper.SelectNext
    Zoomicon.Generics.Collections,
    READCOM.Views.Options.StoryItemOptions;

{$R *.fmx}

constructor TStoryItem.Create(AOwner: TComponent);

  procedure InitDropTarget;
  begin
    with DropTarget do
    begin
      Stored := False; //don't store state, should use state from designed .FMX resource
      BringToFront;
      FilterIndex := 1; //this is the default value
      Filter := GetLoadFilesFilter;
      OnDragOver := DropTargetDragOver;
      OnDragDrop := DropTargetDropped;
    end;
  end;

begin
  inherited;
  FID := TGUID.NewGuid; //Generate new statistically unique ID
  FAutoSize := DEFAULT_AUTOSIZE;
  InitDropTarget;
end;

destructor TStoryItem.Destroy;
begin
  if Assigned(FOptions) then
    FreeAndNil(FOptions.View);

  inherited; //do last
end;

function TStoryItem.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(640, 480);
end;

procedure TStoryItem.SetParent(const Value: TFmxObject);
begin
  var IsNewParentIStoryItem := Supports(Value, IStoryItem);
  Options.DeleteVisible := IsNewParentIStoryItem;
  if IsNewParentIStoryItem then
    SetParentStoryItem(Value as IStoryItem)
  else
    inherited; //needed to add the top StoryItem to some container
end;

procedure TStoryItem.SetEditMode(const Value: Boolean);
begin
  inherited;
  DropTarget.Visible := Value;
  DropTarget.SendToBack; //keep always under children (setting to Visible seems to BringToFront)
end;

procedure TStoryItem.PlayRandomAudioStoryItem;
begin
  var RandomAudioStoryItem := AudioStoryItems.GetRandom;
  if RandomAudioStoryItem <> nil then
    RandomAudioStoryItem.Play;
end;

{$REGION '--- PROPERTIES ---'}

{$region 'View'}

function TStoryItem.GetView: TControl;
begin
  result := Self;
end;

{$endregion}

{$region 'ParentStoryItem'}

function TStoryItem.GetParentStoryItem: IStoryItem;
begin
  result := Parent As IStoryItem;
end;

procedure TStoryItem.SetParentStoryItem(const Value: IStoryItem);
begin
  StoryMode := Value.StoryMode;
  inherited SetParent(Value.View); //don't use "InsertComponent" here, won't work //must use "inherited" to avoid infinite loop and stack overflow //"inherited Parent :=" also fails in Delphi11
end;

{$endregion}

{$region 'StoryItems'}

function IsStoryItem(obj: TFmxObject): Boolean;
begin
  Result := Supports(obj, IStoryItem);
end;

function IsAudioStoryItem(obj: TFmxObject): Boolean;
begin
  Result := Supports(obj, IAudioStoryItem);
end;

function TStoryItem.GetStoryItems: TIStoryItemList;
begin
  result := TObjectListEx<TControl>.GetAllInterface<IStoryItem>(Controls);
end;

procedure TStoryItem.SetStoryItems(const Value: TIStoryItemList);
begin
  for var item in Value do
    AddObject(item.GetView As TStoryItem);
end;

{$endregion}

{$region 'AudioStoryItems'}

function TStoryItem.GetAudioStoryItems: TIAudioStoryItemList;
begin
  result := TObjectListEx<TControl>.GetAllInterface<IAudioStoryItem>(Controls);
end;

{$endregion}

{$region 'Hidden'}

function TStoryItem.IsHidden: Boolean;
begin
  result := FHidden;
end;

procedure TStoryItem.SetHidden(const Value: Boolean);
begin
  FHidden := Value;
  ApplyHidden;
end;

procedure TStoryItem.ApplyHidden;
begin
  Visible := (StoryMode = TStoryMode.EditMode) or (not Hidden);
end;

{$endregion}

{$region 'Anchored'}

function TStoryItem.IsAnchored: Boolean;
begin
  result := Locked;
end;

procedure TStoryItem.SetAnchored(const Value: Boolean);
begin
  Locked := Value;
end;

{$endregion}

{$region 'UrlAction'}
function TStoryItem.GetUrlAction: String;
begin
  result := FUrlAction;
end;

procedure TStoryItem.SetUrlAction(const Value: String);
begin
  FUrlAction := Value;
end;

{$region 'StoryMode'}

function TStoryItem.GetStoryMode: TStoryMode;
begin
  result := FStoryMode;
end;

procedure TStoryItem.SetStoryMode(const Value: TStoryMode);
begin
  FStoryMode := Value;
  StoryItems.ForEach(procedure(StoryItem:IStoryItem) //note: can't use "const" parameter here, TProc<IStoryItem> doesn't use such
    begin
    StoryItem.StoryMode := Value;
    end
  );

  ApplyHidden;
end;

{$endregion}

{$region 'Options'}

function TStoryItem.GetOptions: IStoryItemOptions;
begin
  if not Assigned(FOptions) then
    begin
    FOptions := TStoryItemOptions.Create(nil); //don't set storyitem as owner, seems to always store it (irrespective of "Stored := false")
    FOptions.StoryItem := Self;
    end;

  result := FOptions;
end;

{$endregion}

{$ENDREGION}

{$REGION '--- EVENTS ---'}

//TODO: fix to work with items that are to be focused only (depending on mode)
procedure TStoryItem.KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState);
begin
  inherited;

  var CurControl := TControl(Screen.FocusControl);
  case Key of
    vkReturn:
      SelectNext(CurControl);
    vkTab:
      SelectNext(CurControl, not (ssShift in Shift));
  end;
end;

{
procedure TStoryItem.CanFocus(var ACanFocus: Boolean);
begin
  inherited;
  ShowMessage('Focused');
end;
}

procedure TStoryItem.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  Shift := FMouseShift; //TODO: remove if Delphi fixes related bug (more at FMouseShift definition)

  inherited; //fire event handlers

  if not EditMode then
    begin
    if (FUrlAction <> '') then
      url_Open_In_Browser(FUrlAction);
    end
  else
    if (ssRight in Shift) then
      Options.ShowPopup //this will create options and assign to FOptions if it's unassigned
end;

procedure TStoryItem.Tap(const Point: TPointF);
begin
  inherited; //fire event handlers
  if EditMode then
    Options.ShowPopup; //this will create options and assign to FOptions if it's unassigned
end;

procedure TStoryItem.HandleParentNavigatedToChanged;
begin
  //TODO
end;

{$region 'Drop target'}

procedure TStoryItem.DropTargetDragOver(Sender: TObject; const Data: TDragObject; const Point: TPointF; var Operation: TDragOperation);
begin
  inherited;

  if EditMode then
    Operation := TDragOperation.Copy;
end;

procedure TStoryItem.DropTargetDropped(Sender: TObject; const Data: TDragObject; const Point: TPointF);
begin
  inherited;

  if EditMode then
    Load(Data.Files);
end;

{$endregion}

{$ENDREGION}

{$region 'IStoreable'}

{ Load }

function TStoryItem.GetLoadFilesFilter: String;
begin
  result := FILTER_READCOM;
end;

procedure TStoryItem.LoadFromString(const Data: string);
begin
  var StrStream := TStringStream.Create(Data);
  try
    var BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, BinStream);
      BinStream.Seek(0, soFromBeginning);
      LoadReadCom(BinStream);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

procedure TStoryItem.LoadReadCom(const Stream: TStream);
begin
  Stream.ReadComponent(Self);
end;

procedure TStoryItem.Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM);
begin
  if ContentFormat = EXT_READCOM then
    LoadReadCom(Stream)
  else
    raise EInvalidOperation.CreateFmt(MSG_CONTENT_FORMAT_NOT_SUPPORTED, [ContentFormat]);
end;

procedure TStoryItem.Load(const Filepath: String);
begin
  var InputFileStream := TFileStream.Create(Filepath,  fmOpenRead);
  try
    Load(InputFileStream, ExtractFileExt(Filepath));
  finally
    FreeAndNil(InputFileStream);
  end;
end;

procedure TStoryItem.Load(const Filepaths: array of String);
begin
  for var f in Filepaths do
  begin
    Load(f);
    exit; //by default use just the 1st file in case multiple were dropped
  end;
end;

{ Save }

function TStoryItem.GetSaveFilesFilter: String;
begin
  result := FILTER_READCOM;
end;

function TStoryItem.SaveToString: string;
begin
  var BinStream := TMemoryStream.Create;
  var s: String;
  try
    var StrStream := TStringStream.Create(s);
    try
      SaveReadCom(BinStream);
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream);
      StrStream.Seek(0, soFromBeginning);
      result:= StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free
  end;
end;

procedure TStoryItem.SaveReadCom(const Stream: TStream);
begin
  Stream.WriteComponent(Self);
end;

procedure TStoryItem.Save(const Stream: TStream; const ContentFormat: String = EXT_READCOM);
begin
  if ContentFormat = EXT_READCOM then
    SaveReadCom(Stream)
  else
    raise EInvalidOperation.CreateFmt(MSG_CONTENT_FORMAT_NOT_SUPPORTED, [ContentFormat]);
end;

procedure TStoryItem.Save(const Filepath: string);
begin
  var OutputFileStream := TFileStream.Create(Filepath, fmCreate); //or fmShareDenyNone //TODO: may be needed for Android
  try
    Save(OutputFileStream, ExtractFileExt(Filepath));
  finally
    FreeAndNil(OutputFileStream);
  end;
end;

{$endregion}

end.
