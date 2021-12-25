//Description: READ-COM StoryItem View
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.StoryItem;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.SVGIconImage, FMX.ExtCtrls, FMX.Controls.Presentation,
  READCOM.App.Models, //for IStoryItem
  Zoomicon.Manipulation.FMX.CustomManipulator, //for TCustomManipulator
  Zoomicon.Puzzler.Models, //for IHasTarget
  Zoomicon.Helpers.FMX.Controls.ControlHelpers, //for TControlFocusHelper.SelectNext //MUST DECLARE BEFORE Zoomicon.Puzzler.Classes
  Zoomicon.Puzzler.Classes; //for TControlHasTargetHelper //MUST DECLARE AFTER Zoomicon.Helpers.FMX.Controls.ControlHelpers

const
  MSG_CONTENT_FORMAT_NOT_SUPPORTED = 'Content format not supported: %s';

type
  TStoryItem = class(TCustomManipulator, IStoryItem, IStoreable, IHasTarget, IMultipleHasTarget) //IHasTarget implemented via TControlHasTargetHelper //IMultipleHasTarget implemented via TControlMultipleHasTargetHelper
    Border: TRectangle;
    Glyph: TSVGIconImage;

  //-- Fields ---

  protected
    //FID: TGUID;
    FHidden: Boolean;
    FUrlAction: String;
    FOptions: IStoryItemOptions;
    FTargetsVisible: Boolean;

    FStoryItems: TIStoryItemList;
    FAudioStoryItems: TIAudioStoryItemList;

    FOnActiveChanged: TNotifyEvent;

    class var
      FActiveStoryItem: IStoryItem;
      FOnActiveStoryItemChanged: TNotifyEvent;

  //--- Methods ---

  protected
    class destructor Destroy;

    procedure Init; virtual;

    //procedure Loaded; override;
    //procedure Updated; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;

    { DefaultSize }
    function GetDefaultSize: TSizeF; override;

    { Parent }
    procedure SetParent(const Value: TFmxObject); override;

    { EditMode }
    procedure SetEditMode(const Value: Boolean); override;

    { BorderVisible }
    function GetBorderVisible: Boolean;
    procedure SetBorderVisible(const Value: Boolean);

    { ActiveStoryItem }
    class procedure SetActiveStoryItem(const Value: IStoryItem); static; //static means has no "Self" passed to it, required for "class property" accessors

    { View }
    function GetView: TControl;

    { ParentStoryItem }
    function GetParentStoryItem: IStoryItem;
    procedure SetParentStoryItem(const Value: IStoryItem);

    { StoryItems }
    function GetStoryItems: TIStoryItemList; inline;
    procedure SetStoryItems(const Value: TIStoryItemList);

    { AudioStoryItems }
    function GetAudioStoryItems: TIAudioStoryItemList; inline;

    { ActivationOrder }
    function GetActivationOrder: Integer;
    procedure SetActivationOrder(const Value: Integer); //-1 for not taking part in activation chain

    { Active }
    function IsActive: Boolean;
    procedure SetActive(const Value: Boolean);

    { Hidden }
    function IsHidden: Boolean;
    procedure SetHidden(const Value: Boolean);

    { Anchored }
    function IsAnchored: Boolean;
    procedure SetAnchored(const Value: Boolean);

    { UrlAction }
    function GetUrlAction: String;
    procedure SetUrlAction(const Value: String);

    { TargetsVisible }
    function GetTargetsVisible: Boolean;
    procedure SetTargetsVisible(const Value: Boolean);

    { Options }
    function GetOptions: IStoryItemOptions; virtual; //TODO: make methods that are available via properties protected?

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Paint; override;

    procedure PlayRandomAudioStoryItem;

    { IStoreable }
    function GetAddFilesFilter: String; virtual;
    procedure Add(const Filepath: String); overload; virtual;
    procedure Add(const Filepaths: array of string); overload; virtual;

    function GetLoadFilesFilter: String; virtual;
    procedure LoadFromString(const Data: String); virtual;
    procedure Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload; virtual;
    procedure Load(const Filepath: string); overload; virtual;
    procedure LoadReadCom(const Stream: TStream); virtual;
    procedure LoadReadComBin(const Stream: TStream); virtual;

    function GetSaveFilesFilter: String; virtual;
    function SaveToString: string; virtual;
    procedure Save(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload; virtual;
    procedure Save(const Filepath: string); overload; virtual;
    procedure SaveReadCom(const Stream: TStream); virtual;
    procedure SaveReadComBin(const Stream: TStream); virtual;

  //--- Events ---

  protected
    procedure ActiveChanged;

    //procedure CanFocus(var ACanFocus: Boolean); override;
    procedure KeyDown(var Key: Word; var KeyChar: WideChar; Shift: TShiftState); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override; //preferring overriden methods instead of event handlers that get stored with saved state
    procedure DropTargetDropped(const Filepaths: array of string); override;
    procedure Tap(const Point: TPointF); override;

  //--- Properties ---

  public
    property Options: IStoryItemOptions read GetOptions stored false;
    property BorderVisible: Boolean read GetBorderVisible write SetBorderVisible;

    class property ActiveStoryItem: IStoryItem read FActiveStoryItem write SetActiveStoryItem;
    class property OnActiveStoryItemChanged: TNotifyEvent read FOnActiveStoryItemChanged write FOnActiveStoryItemChanged;

  published
    property ParentStoryItem: IStoryItem read GetParentStoryItem write SetParentStoryItem stored false; //default nil
    property StoryItems: TIStoryItemList read GetStoryItems write SetStoryItems stored false; //default nil
    property AudioStoryItems: TIAudioStoryItemList read GetAudioStoryItems stored false; //default nil
    property ActivationOrder: Integer read GetActivationOrder write SetActivationOrder; //default -1 (not taking part in activation order)
    property Active: Boolean read IsActive write SetActive default false;
    property Hidden: Boolean read IsHidden write SetHidden default false;
    property Anchored: Boolean read IsAnchored write SetAnchored default false;
    property UrlAction: String read GetUrlAction write SetUrlAction; //default nil //TODO: or is it ''?
    property TargetsVisible: Boolean read GetTargetsVisible write SetTargetsVisible default false;
  end;

  TStoryItemClass = class of TStoryItem;

implementation
  uses
    u_UrlOpen,
    System.IOUtils, //for TPath
    Zoomicon.Generics.Collections,
    READCOM.Views.StoryItemFactory, //for AddStoryItemFileFilter, StoryItemFileFilters
    READCOM.Views.Options.StoryItemOptions; //for TStoryItemOptions

{$R *.fmx}

{$region 'Create / Init / Destroy'}

class destructor TStoryItem.Destroy;
begin
  ActiveStoryItem := nil;
end;

procedure TStoryItem.Init;

  procedure InitGlyph;
  begin
    with Glyph do
    begin
      Stored := false; //don't store state, should use state from designed .FMX resource
      SetSubComponent(true);
      Align := TAlignLayout.Client;
      SendToBack;
      HitTest := false;
    end;
  end;

  procedure InitBorder;
  begin
    with Border do
    begin
      Stored := false; //don't store state, should use state from designed .FMX resource
      SetSubComponent(true);
      Align := TAlignLayout.Client;
      SendToBack;
      HitTest := false;
      Visible := EditMode; //show only in EditMode
    end;
  end;

  procedure InitDropTarget;
  begin
    with DropTarget do
    begin
      Visible := EditMode;
      FilterIndex := 1; //this is the default value
      Filter := GetAddFilesFilter;
      Align := TAlignLayout.Client;
      HitTest := False; //TODO: done at ancestor anyway?
      SendToBack; //TODO: ??? or done at ancestor anyway? (note order of Inits below will play part in resulting order)
    end;
  end;

begin
  InitDropTarget;
  InitGlyph;
  InitBorder;
end;

constructor TStoryItem.Create(AOwner: TComponent);
begin
  inherited;

  FStoryItems := TIStoryItemList.Create;
  FAudioStoryItems := TIAudioStoryItemList.Create;

  //FID := TGUID.NewGuid; //Generate new statistically unique ID
  Init;
end;

destructor TStoryItem.Destroy;
begin
  Target := nil;
  Active := false; //making sure an IStoryItem reference isn't held by the class var FActiveStoryItem

  if Assigned(FOptions) then
    begin
    FOptions.HidePopup;
    FreeAndNil(FOptions.View);
    end;

  FreeAndNil(FStoryItems);
  FreeAndNil(FAudioStoryItems);

  inherited; //do last
end;

{$endregion}

procedure TStoryItem.DoAddObject(const AObject: TFmxObject);
  procedure AddToStoryItems;
  begin
    var StoryItem: IStoryItem;
    if Supports(AObject, IStoryItem, StoryItem) then
      FStoryItems.Add(StoryItem);
  end;

  procedure AddToAudioStoryItems;
  begin
    var AudioStoryItem: IAudioStoryItem;
    if Supports(AObject, IAudioStoryItem, AudioStoryItem) then
      FAudioStoryItems.Add(AudioStoryItem);
  end;

begin
  inherited;
  AddToStoryItems;
  AddToAudioStoryItems;
end;

procedure TStoryItem.DoInsertObject(Index: Integer; const AObject: TFmxObject);
begin
  inherited;

  FreeAndNil(FStoryItems); //TODO: this will fail if while looping on StoryItems we insert some new StoryItem (BUT THAT IS AN EDGE CASE, NOT DOING)
  FStoryItems := TObjectListEx<TControl>.GetAllInterface<IStoryItem>(Controls); //need to recalculate the list, since the "Index" of objects is different from the one of StoryItems (the TStoryItem contains other FmxObjects too in its UI design)

  FreeAndNil(FAudioStoryItems); //TODO: this will fail if while looping on StoryItems we insert some new AudioStoryItem (BUT THAT IS AN EDGE CASE, NOT DOING)
  FAudioStoryItems := TObjectListEx<TControl>.GetAllInterface<IAudioStoryItem>(Controls); //need to recalculate the list, since the "Index" of objects is different from the one of AudioStoryItems (the TStoryItem contains other FmxObjects too in its UI design)
end;

procedure TStoryItem.DoRemoveObject(const AObject: TFmxObject);

  procedure RemoveFromStoryItems;
  begin
    var StoryItem: IStoryItem;
    if Supports(AObject, IStoryItem, StoryItem) then
      FStoryItems.Remove(StoryItem);
  end;

  procedure RemoveFromAudioStoryItems;
  begin
    var AudioStoryItem: IAudioStoryItem;
    if Supports(AObject, IAudioStoryItem, AudioStoryItem) then
      FStoryItems.Remove(AudioStoryItem);
  end;

begin
  inherited;
  RemoveFromStoryItems;
  RemoveFromAudioStoryItems; //not checking if item was removed from StoryItems, since it may just implement IAudioStoryItem without implementing IStoryItem in Delphi (which follows COM practice), even though IAudioStoryItem extends IStoryItem
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

procedure TStoryItem.Paint;
begin
  inherited;
  PaintTargetLines;
end;

procedure TStoryItem.PlayRandomAudioStoryItem;
begin
  var RandomAudioStoryItem := AudioStoryItems.GetRandom;
  if RandomAudioStoryItem <> nil then
    RandomAudioStoryItem.Play;
end;

{$REGION 'PROPERTIES' ------------}

{$region 'DefaultSize'}

function TStoryItem.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(300, 300);
end;

{$endregion}

{$region 'BorderVisible'}

function TStoryItem.GetBorderVisible: Boolean;
begin
  result := Border.Visible;
end;

procedure TStoryItem.SetBorderVisible(const Value: Boolean);
begin
  if Assigned(Border) then
    Border.Visible := Value;
end;

{$endregion}

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
  inherited SetParent(Value.View); //don't use "InsertComponent" here, won't work //must use "inherited" to avoid infinite loop and stack overflow //"inherited Parent :=" also fails in Delphi11 when using with just "Value"
end;

{$endregion}

{$region 'StoryItems'}

function TStoryItem.GetStoryItems: TIStoryItemList;
begin
  result := FStoryItems;
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
  result := FAudioStoryItems;
end;

{$endregion}

{$region 'ActivationOrder'}

function TStoryItem.GetActivationOrder: Integer;
begin
  if TabStop then
    result := TabOrder
  else
    result := -1;
end;

procedure TStoryITem.SetActivationOrder(const Value: Integer);
begin
  if Value < 0 then
    TabStop := false
  else
    TabOrder := Value;
end;

{$endregion}

{$region 'Active'}

function TStoryItem.IsActive: Boolean;
begin
  result := Assigned(FActiveStoryItem) and (FActiveStoryItem.View = Self);
end;

procedure TStoryItem.SetActive(const Value: Boolean);
begin
  if (Value = IsActive) then exit; //Important

  if (Value) then //make active
    begin
    if Assigned(FActiveStoryItem) then
      FActiveStoryItem.Active := false; //deactivate the previously active StoryItem

    FActiveStoryItem := Self
    end
  else //make inactive
    FActiveStoryItem := nil;

  ActiveChanged;
end;

class procedure TStoryItem.SetActiveStoryItem(const Value: IStoryItem);
begin
  if (Value = FActiveStoryItem) then exit;

  if Assigned(Value) then //not checking if ActivationOrder <> -1, since an out-of-activation-order StoryItem may be activated directly via a target
    Value.Active := true //this will also deactivate the ActiveStoryItem if any
  else {if Assigned(FActiveStoryItem) then} //if SetActiveStoryItem(nil) was called then deactivate ActiveStoryItem (no need to check if it is Assigned [not nil], since the "Value = FActiveStoryItem" check above would have exited)
    FActiveStoryItem.Active := false;
end;

procedure TStoryItem.ActiveChanged;
begin
  if Assigned(FOnActiveChanged) then
    FOnActiveChanged(Self);

  if IsActive and Assigned(FOnActiveStoryItemChanged) then //fire class-level event only for the now active StoryItem
    FOnActiveStoryItemChanged(Self);
end;

{$endregion}

{$region 'EditMode'}

procedure TStoryItem.SetEditMode(const Value: Boolean);
begin
  inherited;

  if Assigned(Border) then
    Border.Visible := Value;

  if Assigned(DropTarget) then
    with DropTarget do
    begin
      Visible := Value;
      SendToBack; //keep always under children (setting to Visible seems to BringToFront)
    end;

  if Assigned(FStoryItems) then
    For var StoryItem in FStoryItems do
      StoryItem.Hidden := StoryItem.Hidden; //reapply logic for child StoryItems' Hidden since it's related to StoryItemParent's EditMode
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
  Visible := (not Hidden) or (Assigned(ParentStoryItem) and (ParentStoryItem.EditMode)); //always reapply this (logic in SetEditMode depends on it), not only if value changed
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

{$endregion}

{$endregion ....................}

{$region 'TargetsVisible'}

function TStoryItem.GetTargetsVisible: Boolean;
begin
  result := FTargetsVisible;
end;

procedure TStoryItem.SetTargetsVisible(const Value: Boolean);
begin
  FTargetsVisible := Value;
  InvalidateRect(BoundsRect);
end;

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

{$region 'Keyboard'}

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

{$endregion}

{$region 'Mouse'}

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

{$endregion}

{$region DragDrop}

procedure TStoryItem.DropTargetDropped(const Filepaths: array of string);
begin
  inherited;
  Add(Filepaths);
end;

{$endregion}

{$region 'Touch'}

procedure TStoryItem.Tap(const Point: TPointF);
begin
  inherited; //fire event handlers
  if EditMode then
    Options.ShowPopup; //this will create options and assign to FOptions if it's unassigned
end;

{$endregion}

{$ENDREGION}

{$region 'IStoreable'}

function RemoveNonAllowedIdentifierChars(const s: String): String;
begin
  var count := s.Length;
  var builder := TStringBuilder.Create(count);
  try
    for var i := 1 to count do //strings are 1-indexed
      begin
      var c := s[i];
      if IsValidIdent(c) then //keep only characters that would be allowed by themselves as an Identifier
        builder.Append(c);
      end;
    result := builder.ToString;
  finally
    FreeAndNil(builder);
  end;
end;

{ Add }

function TStoryItem.GetAddFilesFilter: String;
begin
  var listFilters := TStringList.Create(#0, '|');
  var listExt := TStringList.Create(#0, ';');

  for var Pair in StoryItemFileFilters do
  begin
    listFilters.Add(Pair.Key {+ '(' + Pair.Value.Replace(';', ',') + ')'}); //note: title already contains the exts in parentheses
    listFilters.Add(Pair.Value);
  end;
  listFilters.Insert(0, listExt.DelimitedText);
  listFilters.Insert(0, 'READ-COM StoryItems, Images, Audio, Text');

  result := listFilters.DelimitedText;

  FreeAndNil(listExt);
  FreeAndNil(listFilters);
end;

procedure TStoryItem.Add(const Filepath: String);
begin
  var FileExt := ExtractFileExt(Filepath);

  var view := StoryItemFactories.Get(FileExt).New(Self).View; //TODO: for .READCOM should have special case with a nil object and use Stream.ReadComponent(nil) since we don't know beforehand what exact TStoryItem class the .readcom file contains (unless it allows to load descendents too)
  view.Name := RemoveNonAllowedIdentifierChars(TPath.GetFileNameWithoutExtension(Filepath)) + IntToStr(Random(maxint)); //TODO: use a GUID

  var StoryItem := view as TStoryItem;
  StoryItem.Load(Filepath); //this should also set the Size of the control

  //Center the new item...
  var ItemSize := StoryItem.Size;
  StoryItem.Position.Point := PointF(Size.Width/2 - ItemSize.Width/2, Size.Height/2 - ItemSize.Height/2); //not creating TPosition objects to avoid leaking (TPointF is a record)
  StoryItem.Align := TAlignLayout.Scale; //adjust when parent scales
  StoryItem.Parent := Self;
  StoryItem.BringToFront; //load as front-most
end;

procedure TStoryItem.Add(const Filepaths: array of String);
begin
  for var filepath in Filepaths do
    Add(filepath); //Adding all files one by one
end;

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
      LoadReadComBin(BinStream);
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

procedure TStoryItem.LoadReadCom(const Stream: TStream);

begin
  var reader := TStreamReader.Create(Stream);
  LoadFromString(reader.ReadToEnd);
  FreeAndNil(reader); //the reader doesn't own the stream by default, so this won't close the stream
end;

procedure TStoryItem.LoadReadComBin(const Stream: TStream);

  procedure RemoveStoryItems;
  begin
    var StoryItemViews := TObjectListEx<TControl>.GetAllClass<TStoryItem>(Controls);
    StoryItemViews.FreeAll; //this will end up having RemoveObject called for each StoryItem (which will also remove from StoryItems and AudioStoryItems [if it is such] lists)
    FreeAndNil(StoryItemViews);
  end;

begin
  RemoveStoryItems;
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
      SaveReadComBin(BinStream);
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
  var writer := TStreamWriter.Create(Stream);
  writer.Write(SaveToString);
  FreeAndNil(writer); //the writer doesn't own the stream by default, so this won't close the stream
end;

procedure TStoryItem.SaveReadComBin(const Stream: TStream);
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

initialization
  AddStoryItemFileFilter(FILTER_READCOM_TITLE, FILTER_READCOM_EXTS); //should make sure this is used first

end.
