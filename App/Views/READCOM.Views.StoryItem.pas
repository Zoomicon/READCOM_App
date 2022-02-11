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

resourcestring
  MSG_CONTENT_FORMAT_NOT_SUPPORTED = 'Content format not supported: %s';

type
  TStoryItem = class(TCustomManipulator, IStoryItem, IClipboardEnabled, IStoreable, IHasTarget, IMultipleHasTarget) //IHasTarget implemented via TControlHasTargetHelper //IMultipleHasTarget implemented via TControlMultipleHasTargetHelper
    Border: TRectangle;
    Glyph: TSVGIconImage;

  //-- Fields ---

  protected
    //FID: TGUID;
    FStoryPoint: Boolean;
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

    class var
      FHomeStoryItem: IStoryItem;

  //--- Methods ---

  protected
    class destructor Destroy;

    procedure Init; virtual;

    //procedure Loaded; override;
    //procedure Updated; override;
    procedure DoAddObject(const AObject: TFmxObject); override;
    procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;
    procedure DoRemoveObject(const AObject: TFmxObject); override;

    {Name}
    procedure SetName(const NewName: TComponentName); override;

    {DefaultSize}
    function GetDefaultSize: TSizeF; override;

    {Parent}
    procedure SetParent(const Value: TFmxObject); override;

    {EditMode}
    procedure SetEditMode(const Value: Boolean); override;

    {BorderVisible}
    function IsBorderVisible: Boolean; virtual;
    procedure SetBorderVisible(const Value: Boolean); virtual;

    {View}
    function GetView: TControl;

    {ParentStoryItem}
    function GetParentStoryItem: IStoryItem;
    procedure SetParentStoryItem(const Value: IStoryItem);

    {StoryItems}
    function GetStoryItems: TIStoryItemList; inline;
    procedure SetStoryItems(const Value: TIStoryItemList);

    {AudioStoryItems}
    function GetAudioStoryItems: TIAudioStoryItemList; inline;

    {ActiveStoryItem}
    class procedure SetActiveStoryItem(const Value: IStoryItem); static; //static means has no "Self" passed to it, required for "class property" accessors

    {Active}
    function IsActive: Boolean; virtual;
    procedure SetActive(const Value: Boolean); virtual;

    {HomeStoryItem}
    class procedure SetHomeStoryItem(const Value: IStoryItem); static; //static means has no "Self" passed to it, required for "class property" accessors

    {Home}
    function IsHome: Boolean; virtual;
    procedure SetHome(const Value: Boolean); virtual;

    {StoryPoint}
    function IsStoryPoint: Boolean; virtual;
    procedure SetStoryPoint(const Value: Boolean); virtual;

    {Previous/Next StoryPoint}
    function GetPreviousStoryPoint: IStoryItem;
    function GetNextStoryPoint: IStoryItem;
    //
    function GetAncestorStoryPoint: IStoryItem;
    function GetFirstChildStoryPoint: IStoryItem;
    function GetLastChildStoryPoint: IStoryItem;
    function GetPreviousSiblingStoryPoint: IStoryItem;
    function GetNextSiblingStoryPoint: IStoryItem;

    {FlippedHorizontally}
    function IsFlippedHorizontally: Boolean; virtual;
    procedure SetFlippedHorizontally(const Value: Boolean); virtual;

    {FlippedVertically}
    function IsFlippedVertically: Boolean; virtual;
    procedure SetFlippedVertically(const Value: Boolean); virtual;

    {Hidden}
    function IsHidden: Boolean; virtual;
    procedure SetHidden(const Value: Boolean); virtual;

    {Anchored}
    function IsAnchored: Boolean; virtual;
    procedure SetAnchored(const Value: Boolean); virtual;

    {UrlAction}
    function GetUrlAction: String; virtual;
    procedure SetUrlAction(const Value: String); virtual;

    {TargetsVisible}
    function GetTargetsVisible: Boolean; virtual;
    procedure SetTargetsVisible(const Value: Boolean); virtual;

    {Options}
    function GetOptions: IStoryItemOptions; virtual;

  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; const AName: string); overload; virtual; //TODO: see why still getting message "[dcc32 Warning] READCOM.Views.StoryItem.pas(148): W1010 Method 'Create' hides virtual method of base type 'TCustomManipulator'" even though "virtual" was added here (ancestor has no such constructor to use "override" instead)
    destructor Destroy; override;

    procedure Paint; override;

    procedure PlayRandomAudioStoryItem;

    {IClipboardEnabled}
    procedure Copy;
    procedure Paste;

    {IStoreable}
    procedure ReadState(Reader: TReader); override;
    procedure ReaderError(Reader: TReader; const Message: string; var Handled: Boolean); virtual;

    function GetAddFilesFilter: String; virtual;
    procedure Add(const StoryItem: IStoryItem); overload; virtual;
    procedure AddFromString(const Data: String); virtual;
    procedure Add(const Filepath: String); overload; virtual;
    procedure Add(const Filepaths: array of string); overload; virtual;

    function GetLoadFilesFilter: String; virtual;
    function LoadFromString(const Data: String; const CreateNew: Boolean = false): TObject; virtual;
    procedure Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload; virtual;
    procedure Load(const Filepath: string); overload; virtual;
    //
    function LoadReadCom(const Stream: TStream; const CreateNew: Boolean = false): IStoryItem; virtual;
    function LoadReadComBin(const Stream: TStream; const CreateNew: Boolean = false): IStoryItem; virtual;

    function GetSaveFilesFilter: String; virtual;
    function SaveToString: string; virtual;
    procedure Save(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload; virtual;
    procedure Save(const Filepath: string); overload; virtual;
    //
    procedure SaveReadCom(const Stream: TStream); virtual;
    procedure SaveReadComBin(const Stream: TStream); virtual;

  //--- Events ---

  protected
    procedure ActiveChanged;
    procedure DropTargetDropped(const Filepaths: array of string); override;
    procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override; //preferring overriden methods instead of event handlers that get stored with saved state
    procedure Tap(const Point: TPointF); override;

  //--- Properties ---

  public
    property Options: IStoryItemOptions read GetOptions stored false;
    property BorderVisible: Boolean read IsBorderVisible write SetBorderVisible stored false default false;

    class property ActiveStoryItem: IStoryItem read FActiveStoryItem write SetActiveStoryItem;
    class property OnActiveStoryItemChanged: TNotifyEvent read FOnActiveStoryItemChanged write FOnActiveStoryItemChanged;

    class property HomeStoryItem: IStoryItem read FHomeStoryItem write SetHomeStoryItem;

  published
    property ParentStoryItem: IStoryItem read GetParentStoryItem write SetParentStoryItem stored false; //default nil
    property StoryItems: TIStoryItemList read GetStoryItems write SetStoryItems stored false; //default nil
    property AudioStoryItems: TIAudioStoryItemList read GetAudioStoryItems stored false; //default nil
    property Active: Boolean read IsActive write SetActive default false;
    property Home: Boolean read IsHome write SetHome default false;
    property StoryPoint: Boolean read IsStoryPoint write SetStoryPoint default false;
    property PreviousStoryPoint: IStoryItem read GetPreviousStoryPoint stored false;
    property NextStoryPoint: IStoryItem read GetNextStoryPoint stored false;
    property FlippedHorizontally: Boolean read IsFlippedHorizontally write setFlippedHorizontally stored false default false; //Scale.X stores related info
    property FlippedVertically: Boolean read IsFlippedVertically write setFlippedVertically stored false default false; //Scale.Y stores related info
    property Hidden: Boolean read IsHidden write SetHidden default false;
    property Anchored: Boolean read IsAnchored write SetAnchored default true;
    property UrlAction: String read GetUrlAction write SetUrlAction; //default nil //TODO: or is it ''?
    property TargetsVisible: Boolean read GetTargetsVisible write SetTargetsVisible default false;
  end;

  TStoryItemClass = class of TStoryItem;

implementation
  uses
    {$IFDEF DEBUG}CodeSiteLogging,{$ENDIF}
    u_UrlOpen, //for url_Open_In_Browser
    System.IOUtils, //for TPath
    FMX.Platform, //for TPlatformServices
    FMX.Clipboard, //for IFMXExtendedClipboardService
    Zoomicon.Generics.Collections, //for TObjectListEx
    Zoomicon.Helpers.RTL.ComponentHelpers, //for TComponent.FindSafeName
    READCOM.Views.StoryItemFactory, //for AddStoryItemFileFilter, StoryItemFileFilters
    READCOM.Views.Options.StoryItemOptions; //for TStoryItemOptions

{$R *.fmx}

{$region 'Create / Init / Destroy'}

constructor TStoryItem.Create(AOwner: TComponent);
begin
  FStoryItems := TIStoryItemList.Create;
  FAudioStoryItems := TIAudioStoryItemList.Create;

  inherited; //must create FStoryItems first, since EditMode ancestor's property is overriden and tries to access StoryItems when the ancestor's constructor initializes it

  //FID := TGUID.NewGuid; //Generate new statistically unique ID
  Init;
end;

constructor TStoryItem.Create(AOwner: TComponent; const AName: string);
begin
  Create(nil); //this may initialize the component from a referenced resource that has a Default name (say a TFrame descendent's design) for the newly created component: to avoid conflict with other component instance with same name under the same owner, not specifying an onwer yet //don't use "inherited" here

  SetName(FindSafeNewName(AName, AOwner)); //since there's no owner there will be no naming conflict at this point (unless there's an owned control with the same name)
  AOwner.InsertComponent(Self); //set the owner after changing the (default) Name to the specified one

  //assuming if inherited constructor or other method (say the "Name" setter) called inside this constructor raises an exception the object is destroyed automatically without having to use try/finally and calling a destructor via freeing the new instance
end;

procedure TStoryItem.Init;

  procedure InitGlyph;
  begin
    with Glyph do
    begin
      Stored := false; //don't store state, should use state from designed .FMX resource
      SetSubComponent(true);
      Align := TAlignLayout.Contents;
      SendToBack; //always send to back after setting Visible
      HitTest := false;
    end;
  end;

  procedure InitBorder;
  begin
    with Border do
    begin
      Stored := false; //don't store state, should use state from designed .FMX resource
      SetSubComponent(true);
      Align := TAlignLayout.Contents;
      Visible := EditMode; //show only in EditMode
      SendToBack; //always send to back after setting Visible
      HitTest := false;
    end;
  end;

  procedure InitDropTarget;
  begin
    with DropTarget do
    begin
      FilterIndex := 1; //this is the default value
      Filter := GetAddFilesFilter;

      Stored := false; //don't store state, should use state from designed .FMX resource
      SetSubComponent(true);
      Align := TAlignLayout.Contents;
      Visible := EditMode;
      SendToBack; //TODO: ??? or done at ancestor anyway? (note order of Inits below will play part in resulting order) //always send to back after setting Visible

      HitTest := false; //TODO: done at ancestor anyway?
    end;
  end;

begin
  InitDropTarget;
  InitGlyph;
  InitBorder;

  Anchored := true;
end;

class destructor TStoryItem.Destroy;
begin
  ActiveStoryItem := nil;
  FOnActiveStoryItemChanged := nil; //probably not needed, doesn't hurt to do though

  HomeStoryItem := nil;
end;

destructor TStoryItem.Destroy;
begin
  Target := nil;
  Active := false; //making sure an IStoryItem reference to this object isn't held by the class var FActiveStoryItem
  Home := false; //making sure an IStoryItem reference to this object isn't held by the class var FHomeStoryItem

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
  if Supports(Value, IStoryItem) then
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

{$region 'Name'}

procedure TStoryItem.SetName(const NewName: TComponentName);
begin
  inherited SetName(FindSafeNewName(NewName));
end;

{$endregion}

{$region 'DefaultSize'}

function TStoryItem.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(300, 300);
end;

{$endregion}

{$region 'BorderVisible'}

function TStoryItem.IsBorderVisible: Boolean;
begin
  result := Border.Visible;
end;

procedure TStoryItem.SetBorderVisible(const Value: Boolean);
begin
  if Assigned(Border) then
    with Border do
    begin
      Visible := Value;
      SendToBack; //always send to back after setting Visible
    end;
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
  Supports(Parent, IStoryItem, result); //don't just do "Parent as IStoryItem", will fail if Parent doesn't support the interface, want nil result in that case
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

    FActiveStoryItem := Self;
  end
  else //make inactive
    FActiveStoryItem := nil;

  ActiveChanged;
end;

class procedure TStoryItem.SetActiveStoryItem(const Value: IStoryItem);
begin
  if (Value = FActiveStoryItem) then exit;

  if Assigned(Value) then //not checking if StoryPoint, since a non-StoryPoint may be activated directly via a target if it belongs to other StoryPoint parent
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
  inherited; //call ancestor implementation

  Glyph.SendToBack; //send the glyph even more below the DropTarget ("inherited SetEditMode" sent it to the back)
  Border.SendToBack; //set the border even more below the DropTarget and the Glyph

  for var StoryItem in FStoryItems do
    with StoryItem do
    begin
      BorderVisible := Value;
      Hidden := Hidden; //reapply logic for child StoryItems' Hidden since it's related to StoryItemParent's EditMode
    end;
end;

{$endregion}

{$region 'Home'}

function TStoryItem.IsHome: Boolean;
begin
  result := Assigned(FHomeStoryItem) and (FHomeStoryItem.View = Self);
end;

procedure TStoryItem.SetHome(const Value: Boolean);
begin
  if (Value = IsHome) then exit; //Important

  if (Value) then //make Home
    begin
    if Assigned(FHomeStoryItem) then
      FHomeStoryItem.Home := false; //deactivate the previously Home StoryItem

    FHomeStoryItem := Self
    end
  else //make inHome
    FHomeStoryItem := nil;

  //HomeChanged; //don't need to issue notification if home storyitem changed //TODO: maybe have such and have StructureView somehow mark the HomeStoryItem thumb with an icon (maybe allow various such symbol annotations)
end;

class procedure TStoryItem.SetHomeStoryItem(const Value: IStoryItem);
begin
  if (Value = FHomeStoryItem) then exit;

  if Assigned(Value) then //note the Home (starting) StoryItem is not necesserily a StoryPoint //TODO: decide on this
    Value.Home := true //this will also deactivate the HomeStoryItem if any
  else {if Assigned(FHomeStoryItem) then} //if SetHomeStoryItem(nil) was called then deactivate HomeStoryItem (no need to check if it is Assigned [not nil], since the "Value = FHomeStoryItem" check above would have exited)
    FHomeStoryItem.Home := false;
end;

{$endregion}

{$region 'StoryPoint'}

function TStoryItem.IsStoryPoint: Boolean;
begin
  result := FStoryPoint;
end;

procedure TStoryItem.SetStoryPoint(const Value: Boolean);
begin
  FStoryPoint := Value;
end;

{$endregion}

{$region 'Previous/Next StoryPoint'}

function TStoryItem.GetPreviousStoryPoint: IStoryItem;
begin
  var parentItem := ParentStoryItem;

  //Check if RootStoryItem
  if not Assigned(parentItem) then
  begin
    var lastChildStoryPoint := GetLastChildStoryPoint;
    if Assigned(lastChildStoryPoint) then
      exit(lastChildStoryPoint)
    else
      exit(nil); //none found
  end;

  //Check previous siblings
  var previousSiblingStoryPoint := GetPreviousSiblingStoryPoint;
  if Assigned(previousSiblingStoryPoint) then
    exit(previousSiblingStoryPoint);

  //Check parent, grandparent etc. (parent may not be a StoryPoint if we navigated directly to a StoryItem via a target-link)
  var ancestorStoryPoint := GetAncestorStoryPoint;
  if Assigned(ancestorStoryPoint) then
    exit(ancestorStoryPoint);

  //Loop in siblings (checking next siblings from the end)
  result := parentItem.GetLastChildStoryPoint; //this may end up returning Self
end;

function TStoryItem.GetNextStoryPoint: IStoryItem;
begin
  var parentItem := ParentStoryItem;

  //Check if RootStoryItem
  if not Assigned(parentItem) then
  begin
    var firstChildStoryPoint := GetFirstChildStoryPoint;
    if Assigned(firstChildStoryPoint) then
      exit(firstChildStoryPoint)
    else
      exit(nil); //none found
  end;

  //Check next siblings
  var nextSiblingStoryPoint := GetNextSiblingStoryPoint;
  if Assigned(nextSiblingStoryPoint) then
    exit(nextSiblingStoryPoint);

  //Check parent, grandparent etc. (parent may not be a StoryPoint if we navigated directly to a StoryItem via a target-link)
  var ancestorStoryPoint := GetAncestorStoryPoint;
  if Assigned(ancestorStoryPoint) then
    exit(ancestorStoryPoint);

  //Loop in siblings (checking previous siblings from the start)
  result := parentItem.GetFirstChildStoryPoint; //this may end up returning Self
end;

function TStoryItem.GetAncestorStoryPoint: IStoryItem;
begin
  var parentItem := ParentStoryItem;
  if not Assigned(parentItem) then exit(nil);

  if parentItem.StoryPoint then
    exit(parentItem)
  else
    exit(parentItem.GetAncestorStoryPoint);
end;

function TStoryItem.GetFirstChildStoryPoint: IStoryItem;
begin //TODO: use ListEx with a predicate below
  var children := StoryItems;

  for var i := 0 to children.Count-1 do //0-based array index
  begin
    var child := children.Items[i];
    if child.IsStoryPoint then //note: not considering if result is Hidden, caller should show it if they want to activate it
      exit(child);
  end;

  result := nil;
end;

function TStoryItem.GetLastChildStoryPoint: IStoryItem;
begin //TODO: use ListEx with a predicate below (and add option there for Direction [see Components' Search/Find that has similar and if there's TDirection type])
  var children := StoryItems;

  for var i := children.Count-1 downto 0 do //0-based array index
  begin
    var child := children.Items[i];
    if child.IsStoryPoint then //note: not considering if result is Hidden, caller should show it if they want to activate it
      exit(child);
  end;

  result := nil;
end;

function TStoryItem.GetPreviousSiblingStoryPoint: IStoryItem;
begin
  var parentItem := ParentStoryItem;
  if not Assigned(parentItem) then exit(nil);

  var storyItemIndex := parentItem.StoryItems.IndexOf(Self as IStoryItem); //must use "as" here, else it won't always find it
  var siblings := parentItem.StoryItems;

  for var i := (storyItemIndex - 1) downto 0 do //0-based array index
  begin
    var sibling := siblings.Items[i];
    if sibling.IsStoryPoint then //note: not considering if result is Hidden, caller should show it if they want to activate it
      exit(sibling);
  end;

  result := nil;
end;

function TStoryItem.GetNextSiblingStoryPoint: IStoryItem;
begin
  var parentItem := ParentStoryItem;
  if not Assigned(parentItem) then exit(nil);

  var storyItemIndex := parentItem.StoryItems.IndexOf(Self as IStoryItem); //must use "as" here, else it won't always find it
  var siblings := parentItem.StoryItems;

  for var i := (storyItemIndex + 1) to (siblings.Count - 1) do //0-based array index
  begin
    var sibling := siblings.Items[i];
    if sibling.IsStoryPoint then //note: not considering if result is Hidden, caller should show it if they want to activate it
      exit(sibling);
  end;

  result := nil;
end;

{$endregion}

{$region 'FlippedHorizontally'}

function TStoryItem.IsFlippedHorizontally: Boolean;
begin
  result := (Scale.X < 0);
end;

procedure TStoryItem.SetFlippedHorizontally(const Value: Boolean);
begin
  if Value xor IsFlippedHorizontally then
    FlipHorizontally;
end;

{$endregion

{$region 'FlippedVertically'}

function TStoryItem.IsFlippedVertically: Boolean;
begin
  result := (Scale.Y < 0);
end;

procedure TStoryItem.SetFlippedVertically(const Value: Boolean);
begin
  if Value xor IsFlippedVertically then
    FlipVertically;
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

{$region 'IClipboardEnabled'}

procedure TStoryItem.Copy;
begin
 var svc: IFMXExtendedClipboardService;
 if TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, Svc) then
 begin
   var wasEditing := EditMode;
   try
     EditMode := false;
     Svc.SetText(SaveToString);
   finally
     EditMode := wasEditing;
   end;
 end;
end;

procedure TStoryItem.Paste;
begin
 var svc: IFMXExtendedClipboardService;
 if TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, Svc) then
   AddFromString(Svc.GetText);
end;

{$endregion}

 {$region 'IStoreable'}

procedure TStoryItem.ReadState(Reader: TReader);
begin
  var readerPreviousOnErrorHandler := Reader.OnError;
  try
    Reader.OnError := ReaderError; //install our error handler
    inherited; //let the state loading continue
  finally
    Reader.OnError := readerPreviousOnErrorHandler; //restore previous error handler
  end;
end;

type
  THackReader = class(TReader); //used to access PropName protected property

procedure TStoryItem.ReaderError(Reader: TReader; const Message: string; var Handled: Boolean);
begin
  with THackReader(Reader) do
    begin
    Handled := AnsiSameText(PropName, 'ActivationOrder'); //Ignores removed ActivationOrder property
    {$IFDEF DEBUG}CodeSite.Send('Ignored deprecated property "ActivationOrder"');{$ENDIF}
    end;
end;

//

function RemoveNonAllowedIdentifierChars(const s: String): String; //TODO: move to some utility library?
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

{Add}

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

procedure TStoryItem.Add(const StoryItem: IStoryItem);
begin
  //Center the new item...
  var StoryItemView := StoryItem.View;
  Self.InsertComponent(StoryItemView); //make sure we set Self as owner //TODO: need to call a safe method to do this with rename (see constructor above and extract such method)
  var ItemSize := StoryItemView.Size;
  StoryItemView.Position.Point := PointF(Size.Width/2 - ItemSize.Width/2, Size.Height/2 - ItemSize.Height/2); //not creating TPosition objects to avoid leaking (TPointF is a record)
  StoryItemView.Align := TAlignLayout.Scale; //adjust when parent scales
  StoryItemView.Parent := Self;
  StoryItemView.BringToFront; //load as front-most
end;

procedure TStoryItem.AddFromString(const Data: string);
var StoryItem: IStoryItem;
begin
  if Supports(LoadFromString(Data, true), IStoryItem, StoryItem) then
    Add(StoryItem);
end;

procedure TStoryItem.Add(const Filepath: String);
begin
  var FileExt := ExtractFileExt(Filepath);

  var view := StoryItemFactories.Get(FileExt).New(Self).View; //TODO: for .READCOM should have special case with a nil object and use Stream.ReadComponent(nil) since we don't know beforehand what exact TStoryItem class the .readcom file contains (unless it allows to load descendents too)
  view.Name := RemoveNonAllowedIdentifierChars(TPath.GetFileNameWithoutExtension(Filepath)) + IntToStr(Random(maxint)); //TODO: use a GUID

  var StoryItem := view as TStoryItem;
  StoryItem.Load(Filepath); //this should also set the Size of the control

  Add(StoryItem);
end;

procedure TStoryItem.Add(const Filepaths: array of String);
begin
  for var filepath in Filepaths do
    Add(filepath); //Adding all files one by one
end;

{Load}

function TStoryItem.GetLoadFilesFilter: String;
begin
  result := FILTER_READCOM;
end;

function TStoryItem.LoadFromString(const Data: string; const CreateNew: Boolean = false): TObject;
begin
  var StrStream := TStringStream.Create(Data);
  try
    var BinStream := TMemoryStream.Create;
    try
      ObjectTextToBinary(StrStream, BinStream);
      BinStream.Seek(0, soFromBeginning);
      result := LoadReadComBin(BinStream, CreateNew).View;
    finally
      BinStream.Free;
    end;
  finally
    StrStream.Free;
  end;
end;

function TStoryItem.LoadReadCom(const Stream: TStream; const CreateNew: Boolean = false): IStoryItem;
begin
  var reader := TStreamReader.Create(Stream);
  try
    result := TStoryItem(LoadFromString(reader.ReadToEnd, CreateNew)) as IStoryItem;
  finally
    FreeAndNil(reader); //the reader doesn't own the stream by default, so this won't close the stream
  end;
end;

function TStoryItem.LoadReadComBin(const Stream: TStream; const CreateNew: Boolean = false): IStoryItem;

  procedure RemoveStoryItems; //TODO: add to IStoryItem and implement at TStoryItem level?
  begin
    var StoryItemViews := TObjectListEx<TControl>.GetAllClass<TStoryItem>(Controls);
    StoryItemViews.FreeAll; //this will end up having RemoveObject called for each StoryItem (which will also remove from StoryItems and AudioStoryItems [if it is such] lists)
    FreeAndNil(StoryItemViews);
  end;

var Instance: TStoryItem;
begin
  if CreateNew then
    Instance := nil //create new StoryItem
  else
  begin
    Instance := Self; //load state to this StoryItem
    RemoveStoryItems; //remove existing children
  end;

  result := Stream.ReadComponent(Instance) as IStoryItem; //note that we have overriden ReadState so that it can set a custom Reader error handler to ignore specific deprecated properties
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

{Save}

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
      SaveReadComBin(BinStream); //need to save to binary first (to memory)... //TODO: save to temp file (esp. for memory constrained devices) instead?
      BinStream.Seek(0, soFromBeginning);
      ObjectBinaryToText(BinStream, StrStream); //...then convert to text format
      StrStream.Seek(0, soFromBeginning);
      result:= StrStream.DataString;
    finally
      StrStream.Free;
    end;
  finally
    BinStream.Free;
  end;
end;

procedure TStoryItem.SaveReadCom(const Stream: TStream);
begin
  var writer := TStreamWriter.Create(Stream);
  try
    writer.Write(SaveToString);
  finally
    FreeAndNil(writer); //the writer doesn't own the stream by default, so this won't close the stream
  end;
end;

procedure TStoryItem.SaveReadComBin(const Stream: TStream);
begin
  var oldEditMode := EditMode;
  EditMode := false; //need to clear this since it disabled children and don't want them to be saved with "Enabled=false"

  try
    Stream.WriteComponent(Self);
  finally
    EditMode := oldEditMode; //restore previous EditMode setting
  end;
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
