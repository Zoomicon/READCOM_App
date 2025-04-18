//Description: READ-COM StoryItem View
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.StoryItems.StoryItem;

interface
  {$region 'Used units' ---------------------------------------------------------}
  uses
    System.UITypes,
    System.SysUtils, System.Types, System.Classes, System.Variants,
    //
    FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
    FMX.Objects,
    FMX.ExtCtrls,
    FMX.Controls.Presentation,
    FMX.Surfaces, //for TBitmapSurface
    FMX.Clipboard, //for IFMXExtendedClipboardService
    FMX.Layouts,
    //
    FMX.SVGIconImage,
    //
    Zoomicon.Media.FMX.MediaDisplay, //for TMediaDisplay (Glyph)
    Zoomicon.Manipulation.FMX.CustomManipulator, //for TCustomManipulator
    Zoomicon.Helpers.FMX.Controls.ControlHelper, //for TControl.SelectNext //MUST DECLARE BEFORE Zoomicon.Puzzler.Classes
    //TODO: unused// Zoomicon.Puzzler.Models, //for IHasTarget, IMultipleHasTarget
    //TODO: unused// Zoomicon.Puzzler.Classes, //for TControlHasTargetHelper, TControlMultipleHasTargetHelper //MUST DECLARE AFTER Zoomicon.Helpers.FMX.Controls.ControlHelpers
    //
    READCOM.Models, //for IClipboardEnabled, IStoreable, EXT_READCOM, DEFAULT_THUMB_WIDTH, DEFAULT_THUMB_HEIGHT, DEFAULT_HTML_IMAGE_WIDTH, DEFAULT_HTML_IMAGE_HEIGHT
    READCOM.Models.Stories; //for IStoryItem
  {$endregion}

  resourcestring
    MSG_CONTENT_FORMAT_NOT_SUPPORTED = 'Content format not supported: %s';

  //TODO: override EnabledStored maybe to never store Enabled property (which is used in Edit mode - in that case should disable a child after pasting in edit mode)

  type
    TStoryItem = class(TCustomManipulator, IStoryItem, IClipboardEnabled, IStoreable (*, IHasTarget, IMultipleHasTarget*)) //TODO: from Zoomicon.Puzzler: unused (IHasTarget implemented via TControlHasTargetHelper //IMultipleHasTarget implemented via TControlMultipleHasTargetHelper)
      Border: TRectangle;
      Background: TRectangle;
      Glyph: TMediaDisplay;

    //-- Fields ---

    protected
      //FID: TGUID;
      FContent: TBytes; //opaque content storage (dynamic array of bytes)
      FContentExt: String;
      FStoryPoint: Boolean;
      FHidden: Boolean;
      FSnapping: Boolean;
      FUrlAction: String;
      FOptions: IStoryItemOptions;
      FTargetsVisible: Boolean;
      FDragging: Boolean; //=False
      FDragStart: TPointF; //=TPointF.Zero

      FStoryItems: TIStoryItemList;
      FAudioStoryItems: TIAudioStoryItemList;

      FOnActiveChanged: TNotifyEvent;

      //Global IStory (context) //TODO: talk to that so that we could tell it to open hyperlinks (e.g. http://...) but also special hyperlinks like story:next, story:previous etc. that can invoke methods to navigate in the story (actually could pass the "verb" to the story itself via special method and it would know how to handle the hyperlinks and the special ones [not have any download and url opening code in the StoryItem])
      class var
        FStory: IStory;

      //TODO: maybe move to IStory so that we don't have many class variables and maybe also be able to have side-by-side stories by passing them different context (IStory)
      class var
        FIgnoreActiveStoryItemChanges: Boolean; //=False
        FActiveStoryItem: IStoryItem; //=nil
        FOnActiveStoryItemChanged: TNotifyEvent;

      //TODO: maybe move to IStory so that we don't have many class variables
      class var
        FHomeStoryItem: IStoryItem;

    //--- Methods ---

    protected
      class destructor Destroy;

      procedure Loaded; override;
      //procedure Updated; override;
      procedure DoAddObject(const AObject: TFmxObject); override;
      procedure DoInsertObject(Index: Integer; const AObject: TFmxObject); override;
      procedure DoRemoveObject(const AObject: TFmxObject); override;

      {Z-Order}
      function GetBackIndex: Integer; override;
      procedure SetBackgroundZorder; virtual;
      procedure SetGlyphZorder; virtual;
      procedure SetBorderZorder; virtual;

      {Cursor}
      procedure UpdateCursor;

      {Clipboard}
      procedure Paste(const Clipboard: IFMXExtendedClipboardService); overload; virtual;

      {Name}
      procedure SetName(const NewName: TComponentName); override;

      {DefaultSize}
      function GetDefaultSize: TSizeF; override;

      {Parent}
      procedure SetParent(const Value: TFmxObject); override;

      {EditMode}
      procedure SetEditMode(const Value: Boolean); override;
      procedure ApplyParentEditMode(const StoryItem: IStoryItem); virtual;

      {BorderVisible}
      function IsBorderVisible: Boolean; virtual;
      procedure SetBorderVisible(const Value: Boolean); virtual;

      {View}
      function GetView: TControl; //get a TStoryItem descendant from IStoryItem //Note: could return TStoryItem type here, but maybe other controls implement IStoryItem too in the future

      {ParentStoryItem}
      function GetParentStoryItem: IStoryItem;
      procedure SetParentStoryItem(const Value: IStoryItem);

      {StoryItems}
      function GetStoryItems: TIStoryItemList; inline;
      procedure SetStoryItems(const Value: TIStoryItemList);

      {ImageStoryItems}
      function GetImageStoryItems: TIImageStoryItemList; inline; //Note: need to free returned object when done, isn't cached by the StoryItem

      {AudioStoryItems}
      function GetAudioStoryItems: TIAudioStoryItemList; inline;

      {TextStoryItems}
      function GetTextStoryItems: TITextStoryItemList; inline; //Note: need to free returned object when done, isn't cached by the StoryItem

      {ActiveStoryItem}
      class procedure SetActiveStoryItem(const Value: IStoryItem); static; //static means has no "Self" passed to it, required for "class property" accessors

      {Active}
      function IsActive: Boolean; virtual;
      procedure SetActive(const Value: Boolean); virtual;

      {Root}
      function IsRoot: Boolean;

      {Home}
      class procedure SetHomeStoryItem(const Value: IStoryItem); static; //static means has no "Self" passed to it, required for "class property" accessors
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

      {Content}
      function GetContent: TBytes; virtual;
      procedure SetContent(const Value: TBytes); virtual;

      {ContentExt}
      function GetContentExt: String; virtual;
      procedure SetContentExt(const Value: String); virtual;

      {AllText}
      function GetAllText: TStrings;
      procedure SetAllText(const Value: TStrings);

      {ForegroundColor}
      function GetForegroundColor: TAlphaColor; virtual;
      procedure SetForegroundColor(const Value: TAlphaColor); virtual;

      {BackgroundColor}
      function GetBackgroundColor: TAlphaColor; virtual;
      procedure SetBackgroundColor(const Value: TAlphaColor); virtual;

      {FlippedHorizontally}
      function IsFlippedHorizontally: Boolean; virtual;
      procedure SetFlippedHorizontally(const Value: Boolean); virtual;

      {FlippedVertically}
      function IsFlippedVertically: Boolean; virtual;
      procedure SetFlippedVertically(const Value: Boolean); virtual;

      {Hidden}
      function IsHidden: Boolean; virtual;
      procedure SetHidden(const Value: Boolean); virtual;

      {Snapping}
      function IsSnapping: Boolean; virtual;
      procedure SetSnapping(const Value: Boolean); virtual;
      procedure DoSnapping; virtual;

      {Anchored}
      function IsAnchored: Boolean; virtual;
      procedure SetAnchored(const Value: Boolean); virtual;

      {UrlAction}
      function GetUrlAction: String; virtual;
      procedure SetUrlAction(const Value: String); virtual;

      {Tags}
      function GetTags: String;
      procedure SetTags(const Value: String);
      function AreTagsMatched: Boolean;

      {TargetsVisible}
      function GetTargetsVisible: Boolean; virtual;
      procedure SetTargetsVisible(const Value: Boolean); virtual;

      {Options}
      function GetOptions: IStoryItemOptions; virtual;

      {Serialization}
      procedure DefineProperties(Filer: TFiler); override; //for persisting Content property's binary data
      procedure ReadContent(Stream: TStream);
      procedure WriteContent(Stream: TStream);

    public
      constructor Create(AOwner: TComponent); overload; override;
      constructor Create(AOwner: TComponent; const AName: String); reintroduce; overload; virtual; //TODO: see why if we don't define "reintroduce" we're getting message "[dcc32 Warning] READCOM.Views.StoryItems.StoryItem.pas(148): W1010 Method 'Create' hides virtual method of base type 'TCustomManipulator'" even though "virtual" was added here (ancestor has no such constructor to use "override" instead)
      destructor Destroy; override;

      //procedure Paint; override; //TODO: not doing any custom drawing like target lines

      procedure PlayRandomAudioStoryItem;

      {IClipboardEnabled}
      procedure Delete; virtual;
      procedure Cut; virtual;
      procedure Copy; virtual;
      procedure Paste; overload; virtual;

      {IStoreable}
      procedure ReadState(Reader: TReader); override;
      procedure ReaderError(Reader: TReader; const Message: String; var Handled: Boolean); virtual;
      //
      function GetAddFilesFilter: String; virtual;
      procedure Add(const StoryItem: IStoryItem); overload; virtual;
      procedure AddFromString(const Data: String); virtual;
      procedure Add(const Filepath: String); overload; virtual;
      procedure Add(const Filepaths: array of String); overload; virtual;
      //
      function GetLoadFilesFilter: String; virtual;
      class function LoadNew(const Stream: TStream; const ContentFormat: String = EXT_READCOM): TStoryItem; overload; virtual;
      class function LoadNew(const Filepath: String; const ContentFormat: String = EXT_READCOM): TStoryItem; overload; virtual;
      function Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM; const CreateNew: Boolean = false): TObject; overload; virtual;
      function Load(const Filepath: String; const CreateNew: Boolean = false): TObject; overload; virtual;
      function Load(const Clipboard: IFMXExtendedClipboardService; const CreateNew: Boolean = false): TObject; overload; virtual;
      function LoadFromString(const Data: String; const CreateNew: Boolean = false): TObject; virtual;
      //
      function LoadReadCom(const Stream: TStream; const CreateNew: Boolean = false): IStoryItem; virtual;
      function LoadReadComBin(const Stream: TStream; const CreateNew: Boolean = false): IStoryItem; virtual;

      function GetSaveFilesFilter: String; virtual;
      function SaveToString: String; virtual;
      procedure Save(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload; virtual;
      procedure Save(const Filepath: String); overload; virtual;
      procedure SaveThumbnail(const Filepath: String; const MaxWidth: Integer = DEFAULT_THUMB_WIDTH; const MaxHeight: Integer = DEFAULT_THUMB_HEIGHT); virtual;
      procedure SaveHTML(const Stream: TStream; const ImagesPath: String; const MaxImageWidth: Integer = DEFAULT_HTML_IMAGE_WIDTH; const MaxImageHeight: Integer = DEFAULT_HTML_IMAGE_HEIGHT); overload;
      procedure SaveHTML(const Filepath: String; const MaxImageWidth: Integer = DEFAULT_HTML_IMAGE_WIDTH; const MaxImageHeight: Integer = DEFAULT_HTML_IMAGE_HEIGHT); overload;
      //
      procedure SaveReadCom(const Stream: TStream); virtual;
      procedure SaveReadComBin(const Stream: TStream); virtual;

      {Navigation}
      procedure ActivateRootStoryItem;
      procedure ActivateParentStoryItem;

    //--- Events ---

    protected
      procedure ActiveChanged;
      procedure DropTargetDropped(const Filepaths: array of String); override;
      procedure HandleAreaSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single); override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState;  X, Y: single); override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState;  X, Y: single); override;
      procedure MouseMove(Shift: TShiftState; X, Y: Single); override;
      procedure MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single); override; //preferring overriden methods instead of event handlers that get stored with saved state
      procedure Tap(const Point: TPointF); override;

    //--- Properties ---

    public
      property Options: IStoryItemOptions read GetOptions stored false; //TODO: should we make published? (think it was causing side-effects by getting somehow stored)
      property BorderVisible: Boolean read IsBorderVisible write SetBorderVisible stored false default false;

      class property Story: IStory read FStory write FStory; //Note: class properties can't be published and are not stored

      class property ActiveStoryItem: IStoryItem read FActiveStoryItem write SetActiveStoryItem;
      class property OnActiveStoryItemChanged: TNotifyEvent read FOnActiveStoryItemChanged write FOnActiveStoryItemChanged;

      class property HomeStoryItem: IStoryItem read FHomeStoryItem write SetHomeStoryItem;

    published
      const
        DEFAULT_ACTIVE = false;
        DEFAULT_HOME = false;
        DEFAULT_STORYPOINT = false;
        DEFAULT_FOREGROUND_COLOR = TAlphaColorRec.Null; //claNull
        DEFAULT_BACKGROUND_COLOR = TAlphaColorRec.Null; //claNull
        DEFAULT_SNAPPING = false;
        DEFAULT_ANCHORED = true;
        DEFAULT_FLIPPED_HORIZONTALLY = false;
        DEFAULT_FLIPPED_VERTICALLY = false;
        DEFAULT_HIDDEN = false;
        DEFAULT_TARGETS_VISIBLE = false;

      property ParentStoryItem: IStoryItem read GetParentStoryItem write SetParentStoryItem stored false; //default nil
      property StoryItems: TIStoryItemList read GetStoryItems write SetStoryItems stored false; //default nil
      property ImageStoryItems: TIImageStoryItemList read GetImageStoryItems stored false; //default nil //Note: need to free returned object when done, isn't cached by the StoryItem
      property AudioStoryItems: TIAudioStoryItemList read GetAudioStoryItems stored false; //default nil
      property TextStoryItems: TITextStoryItemList read GetTextStoryItems stored false; //default nil //Note: need to free returned object when done, isn't cached by the StoryItem
      property Active: Boolean read IsActive write SetActive default DEFAULT_ACTIVE;
      property Home: Boolean read IsHome write SetHome default DEFAULT_HOME;
      property StoryPoint: Boolean read IsStoryPoint write SetStoryPoint default DEFAULT_STORYPOINT;
      property PreviousStoryPoint: IStoryItem read GetPreviousStoryPoint stored false;
      property NextStoryPoint: IStoryItem read GetNextStoryPoint stored false;
      property Content: TBytes read GetContent write SetContent stored false; //seems Delphi (at least till v12.2) doesn't stream TBytes, so serializing it via DefineProperties as "ContentBin" //TODO: defining as published instead of public in case a Property Editor is implemented in the future
      property ContentExt: String read GetContentExt write SetContentExt; //default nil
      property AllText: TStrings read GetAllText write SetAllText stored false; //Note: used to replace Text at applicable items in StoryItem's whole subtree (to be used recursively) //Note: need to free returned object when done, isn't cached by the StoryItem
      property ForegroundColor: TAlphaColor read GetForegroundColor write SetForegroundColor default DEFAULT_FOREGROUND_COLOR;
      property BackgroundColor: TAlphaColor read GetBackgroundColor write SetBackgroundColor default DEFAULT_BACKGROUND_COLOR;
      property FlippedHorizontally: Boolean read IsFlippedHorizontally write setFlippedHorizontally stored false default DEFAULT_FLIPPED_HORIZONTALLY; //Scale.X stores related info //Note: default isn't really needed when using stored false
      property FlippedVertically: Boolean read IsFlippedVertically write setFlippedVertically stored false default DEFAULT_FLIPPED_VERTICALLY; //Scale.Y stores related info //Note: default isn't really needed when using stored false
      property Hidden: Boolean read IsHidden write SetHidden default DEFAULT_HIDDEN;
      property Snapping: Boolean read IsSnapping write SetSnapping default DEFAULT_SNAPPING;
      property Anchored: Boolean read IsAnchored write SetAnchored default DEFAULT_ANCHORED;
      property UrlAction: String read GetUrlAction write SetUrlAction; //default '' (implied, not allows to use '')
      property Tags: String read GetTags write SetTags; //default '' (implied, not allows to use '')
      property TagsMatched: Boolean read AreTagsMatched;
      property TargetsVisible: Boolean read GetTargetsVisible write SetTargetsVisible stored false default DEFAULT_TARGETS_VISIBLE; //TODO: not using concept of explicit targets now, but since anchored items with Tags serve as targets could use that property to highlight them (as hint for user). Maybe in that case add the Targets visible toggle button again
    end;

    TStoryItemClass = class of TStoryItem;

implementation
  {$region 'Used units'}
  uses
    System.IOUtils, //for TPath
    //
    FMX.Platform, //for TPlatformServices
    //
    Zoomicon.Generics.Collections, //for TObjectListEx
    Zoomicon.Helpers.RTL.ComponentHelpers, //for TComponent.FindSafeName
    Zoomicon.Helpers.RTL.StreamHelpers, //for TStream.ReadComponent
    Zoomicon.Helpers.RTL.StringsHelpers, //for TStrings.GetLines
    //Zoomicon.Helpers.FMX.Forms.ApplicationHelper, //for IsURI
    Zoomicon.Introspection.FMX.Debugging, //for Log
    Zoomicon.Media.FMX.Models, //for EXT_XX constants
    //
    READCOM.Factories.StoryItemFactory, //for StoryItemFactories, AddStoryItemFileFilter, StoryItemFileFilters
    READCOM.Views.Options.StoryItemOptions; //for TStoryItemOptions
  {$endregion}

  {$R *.fmx}

  {$region 'Lifetime management'}

  constructor TStoryItem.Create(AOwner: TComponent);

    procedure InitBackground;
    begin
      with Background do
      begin
        Stored := false; //don't store state, should use state from designed .FMX resource
        SetSubComponent(true);
        Align := TAlignLayout.Contents;
        SetBackgroundZorder;
        Stroke.Kind := TBrushKind.None; //no borderline for the background (there exists separate Border subcomponent that is above glyphs/bitmaps)
        HitTest := false;
      end;
    end;

    procedure InitGlyph;
    begin
      with Glyph do
      begin
        Stored := false; //don't store state, should use state from designed .FMX resource
        SetSubComponent(true);
        Align := TAlignLayout.Contents;
        WrapMode := TImageWrapMode.Stretch; //stretch the content //TODO: add property WrapMode to TMediaDisplay
        SetGlyphZorder;
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
        SetBorderZorder;
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
        SetDropTargetZorder;

        HitTest := false; //TODO: done at ancestor anyway?
      end;
    end;

  begin
    FStoryItems := TIStoryItemList.Create;
    FAudioStoryItems := TIAudioStoryItemList.Create;

    inherited; //must create FStoryItems first, since ancestor's EditMode property is overriden and causes access to StoryItems property when the ancestor's constructor sets it

    //FID := TGUID.NewGuid; //Generate new statistically unique ID

    InitDropTarget; //will end up at the top
    InitBorder;
    InitGlyph;
    InitBackground; //will end up at the bottom

    //DragMode := TDragMode.dmManual; //no automatic drag (it's the default)
    Anchored := true;
    ForegroundColor := TAlphaColorRec.Null;
    BackgroundColor := TAlphaColorRec.Null;

    //Size.Size := DefaultSize; //set the default size (overriden at descendents) //DO NOT DO, CAUSES NON-LOADING OF VECTOR GRAPHICS OF DEFAULT STORY - SEEMS TO BE DONE INTERNALLY BY FMX ANYWAY SINCE WE OVERRIDE GetDefaultSize
  end;

  constructor TStoryItem.Create(AOwner: TComponent; const AName: String);
  begin
    Create(nil); //this may initialize the component from a referenced resource that has a Default name (say a TFrame descendent's design) for the newly created component: to avoid conflict with other component instance with same name under the same owner, not specifying an onwer yet //don't use "inherited" here

    SetName(FindSafeNewName(AName, AOwner)); //since there's no owner there will be no naming conflict at this point (unless there's an owned control with the same name)
    AOwner.InsertComponent(Self); //set the owner after changing the (default) Name to the specified one

    //assuming if inherited constructor or other method (say the "Name" setter) called inside this constructor raises an exception the object is destroyed automatically without having to use try/finally and calling a destructor via freeing the new instance
  end;

  class destructor TStoryItem.Destroy;
  begin
    ActiveStoryItem := nil;
    FOnActiveStoryItemChanged := nil; //probably not needed, doesn't hurt to do though

    HomeStoryItem := nil;
  end;

  destructor TStoryItem.Destroy;
  begin
    //TODO: Zoomicon.Puzzler unused for now// Target := nil;
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

  procedure TStoryItem.Loaded; //this gets called when component has fully deserialized
  begin
    var LContent := Content;
    if Assigned(LContent) then //if there are saved Content data
    begin
      var LContentStream := TBytesStream.Create(LContent);
      try
        if (ContentExt <> '') then
          Load(LContentStream, ContentExt); //will call overriden method (if any) when at descendant class
          //Note: this should result in Content property being set again by descendent (if they want to store via the Content property)...
          //...we shouldn't store the Content at TStoryItem.Load(Stream) base implementation, since descendents may opt to use other property for more efficient storage instead of keeping the whole assigned file with its headers, metadata etc.
      finally
        FreeAndNil(LContentStream);
      end;
    end;

    inherited;
  end;

  {$endregion}

  procedure TStoryItem.DoAddObject(const AObject: TFmxObject);
    procedure AddToStoryItems;
    var StoryItem: IStoryItem;
    begin
      if Supports(AObject, IStoryItem, StoryItem) then
      begin
        FStoryItems.Add(StoryItem);
        ApplyParentEditMode(StoryItem);
      end;
    end;

    procedure AddToAudioStoryItems;
    var AudioStoryItem: IAudioStoryItem;
    begin
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

    var StoryItem: IStoryItem;
    if Supports(AObject, IStoryItem, StoryItem) then
      ApplyParentEditMode(StoryItem);

    FreeAndNil(FAudioStoryItems); //TODO: this will fail if while looping on StoryItems we insert some new AudioStoryItem (BUT THAT IS AN EDGE CASE, NOT DOING)
    FAudioStoryItems := TObjectListEx<TControl>.GetAllInterface<IAudioStoryItem>(Controls); //need to recalculate the list, since the "Index" of objects is different from the one of AudioStoryItems (the TStoryItem contains other FmxObjects too in its UI design)
  end;

  procedure TStoryItem.DoRemoveObject(const AObject: TFmxObject);

    procedure RemoveFromStoryItems;
    var StoryItem: IStoryItem;
    begin
      if Supports(AObject, IStoryItem, StoryItem) then
        FStoryItems.Remove(StoryItem);
    end;

    procedure RemoveFromAudioStoryItems;
    var AudioStoryItem: IAudioStoryItem;
    begin
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

  (*
  procedure TStoryItem.Paint;
  begin
    inherited;
    PaintTargetLines;
  end;
  *)

  procedure TStoryItem.PlayRandomAudioStoryItem;
  begin
    if not Assigned(FAudioStoryItems) then
      exit;

    var LRandomAudioStoryItem := FAudioStoryItems.GetRandom;
    if Assigned(LRandomAudioStoryItem) then
      LRandomAudioStoryItem.Play;
  end;

  {$region 'Z-order'}

  function TStoryItem.GetBackIndex: Integer;
  begin
    Result := inherited;
    if Assigned(Background) and Background.Visible then
      inc(result); //reserve one more place at the bottom for Background
    if Assigned(Glyph) and Glyph.Visible then
      inc(result); //reserve one more from Glyph
    if Assigned(Border) and Border.Visible then
      inc(result); //reserve one more for Border (if visible)
  end;

  procedure TStoryItem.SetBackgroundZorder;
  begin
    (* //NOT WORKING
    BeginUpdate;
    RemoveObject(Background);
    InsertObject((inherited GetBackIndex) + 3, Background);
    EndUpdate;
    *)
    if Assigned(Background) and Background.Visible then
      Background.SendToBack;
  end;

  procedure TStoryItem.SetGlyphZorder;
  begin
    (* //NOT WORKING
    BeginUpdate;
    RemoveObject(Glyph);
    InsertObject((inherited GetBackIndex) + 2, Glyph);
    EndUpdate;
    *)
    if Assigned(Glyph) and Glyph.Visible then
      Glyph.SendToBack;
  end;

  procedure TStoryItem.SetBorderZorder;
  begin
    (* //NOT WORKING
    BeginUpdate;
    RemoveObject(Border);
    InsertObject((inherited GetBackIndex) + 1, Border);
    EndUpdate;
    *)
    if Assigned(Border) and Border.Visible then
      Border.SendToBack;
  end;

  {$endregion}

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
    Result := Assigned(Border) and Border.Visible;
    //result := Assigned(Border) and Assigned(Border.Stroke); //TODO: allow to hide outline of Border subcomponent (maybe rename it to Background since it will also be used for fill)
  end;

  procedure TStoryItem.SetBorderVisible(const Value: Boolean);
  begin
    if Assigned(Border) then
      with Border do
      begin
        Visible := Value;
        SetBorderZorder; //will end up on top
        SetGlyphZorder;
        SetBackgroundZorder; //will end up at the bottom
      end;
  end;

  {$endregion}

  {$region 'View'}

  function TStoryItem.GetView: TControl;
  begin
    Result := Self;
  end;

  {$endregion}

  {$region 'ParentStoryItem'}

  function TStoryItem.GetParentStoryItem: IStoryItem;
  begin
    result := nil; //Note: if Supports first parameter is nil then it returns false and the last (out) parameter is undefined (not necesserily nil)
    Supports(Parent, IStoryItem, result) //don't just do "Parent as IStoryItem", will fail if Parent doesn't support the interface, want nil result in that case
  end;

  procedure TStoryItem.SetParentStoryItem(const Value: IStoryItem);
  begin
    inherited SetParent(Value.View); //don't use "InsertComponent" here, won't work //must use "inherited" to avoid infinite loop and stack overflow //"inherited Parent :=" also fails in Delphi11 when using with just "Value"
  end;

  {$endregion}

  {$region 'StoryItems'}

  function TStoryItem.GetStoryItems: TIStoryItemList;
  begin
    Result := FStoryItems;
  end;

  procedure TStoryItem.SetStoryItems(const Value: TIStoryItemList);
  begin
    for var item in Value do
      AddObject(item.GetView As TStoryItem);
  end;

  {$endregion}

  {$region 'ImageStoryItems'}

  function TStoryItem.GetImageStoryItems: TIImageStoryItemList;
  begin
    Result := TObjectListEx<TControl>.GetAllInterface<IImageStoryItem>(Controls);
  end;

  {$endregion}

  {$region 'AudioStoryItems'}

  function TStoryItem.GetAudioStoryItems: TIAudioStoryItemList;
  begin
    Result := FAudioStoryItems;
  end;

  {$endregion}

  {$region 'TextStoryItems'}

  function TStoryItem.GetTextStoryItems: TITextStoryItemList;
  begin
    Result := TObjectListEx<TControl>.GetAllInterface<ITextStoryItem>(Controls);
  end;

  {$endregion}

  {$endregion}

  {$region 'Active'}

  class procedure TStoryItem.SetActiveStoryItem(const Value: IStoryItem);
  begin
    if FIgnoreActiveStoryItemChanges or (Value = FActiveStoryItem) then exit;

    if Assigned(Value) then //not checking if StoryPoint, since a non-StoryPoint may be activated directly via a target if it belongs to other StoryPoint parent
      Value.Active := true //this will also deactivate the ActiveStoryItem if any
    else {if Assigned(FActiveStoryItem) then} //if SetActiveStoryItem(nil) was called then deactivate ActiveStoryItem (no need to check if it is Assigned [not nil], since the "Value = FActiveStoryItem" check above would have exited)
      FActiveStoryItem.Active := false;
  end;

  function TStoryItem.IsActive: Boolean;
  begin
    Result := Assigned(FActiveStoryItem) and (FActiveStoryItem.View = Self);
  end;

  procedure TStoryItem.SetActive(const Value: Boolean);
  begin
    if FIgnoreActiveStoryItemChanges or (Value = IsActive) then exit; //Important

    if (Value) then //make active
    begin
      if Assigned(FActiveStoryItem) then
        FActiveStoryItem.Active := false; //deactivate the previously active StoryItem

      FActiveStoryItem := Self;

      PlayRandomAudioStoryItem; //TODO: maybe should play AudioStoryItems in the order they exist in their parent StoryItem (but would need to remember last one played in that case which may be problematic if they are reordered etc.)
      //PlayNextAudioStoryItem;
    end
    else //make inactive
      FActiveStoryItem := nil;

    ActiveChanged;
  end;

  function TStoryItem.IsRoot: Boolean;
  begin
    Result := not Assigned(ParentStoryItem);
  end;

  procedure TStoryItem.ActivateRootStoryItem;
  begin
    if Assigned(Story) then
      Story.ActivateRootStoryItem;
  end;

  procedure TStoryItem.ActivateParentStoryItem;
  begin
    var parentItem := ParentStoryItem;
    if Assigned(parentItem) then //don't want to make RootStoryItem (aka the StoryItem without a parent StoryItem) inactive, so don't set ActiveStoryItem to nil
      parentItem.Active := true;
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

    try
      //BeginUpdate; //TODO: see if it helps or causes text drawing issues

      //send the glyph etc. even more below the DropTarget ("inherited SetEditMode" sent it to the back) //TODO: make this helper method (also done at SetBorderVisible)
      SetBorderZorder; //will end up on top
      SetGlyphZorder;
      SetBackgroundZorder; //will end up at the bottom

      for var StoryItem in FStoryItems do
        ApplyParentEditMode(StoryItem);

    finally
      //EndUpdate;
    end;
  end;

  procedure TStoryItem.ApplyParentEditMode(const StoryItem: IStoryItem);
  begin
    var ParentEditMode := EditMode;
    with StoryItem do
    try
      //View.BeginUpdate; //TODO: see if it helps or causes text drawing issues
      BorderVisible := ParentEditMode;
      Hidden := Hidden; //reapply logic for child StoryItems' Hidden since it's related to StoryItemParent's EditMode
    finally
      //View.EndUpdate;
    end;
  end;

  {$endregion}

  {$region 'Home'}

  function TStoryItem.IsHome: Boolean;
  begin
    Result := Assigned(FHomeStoryItem) and (FHomeStoryItem.View = Self);
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
    Result := FStoryPoint;
  end;

  procedure TStoryItem.SetStoryPoint(const Value: Boolean);
  begin
    FStoryPoint := Value;
  end;

  {$endregion}

  {$region 'Previous/Next StoryPoint'}

  function TStoryItem.GetPreviousStoryPoint: IStoryItem; //TODO: this logic doesn't work ok when there is an isolated StoryPoint deep in the hierarchy (it's ignored)
  begin
    //Check previous siblings //TODO: won't navigate directly into grand*-children if siblings aren't StoryPoints (though we could consider this a feature to be able to create isolated StoryPoints that are navigable only by name)
    var previousSiblingStoryPoint := GetPreviousSiblingStoryPoint;
    if Assigned(previousSiblingStoryPoint) then
      exit(previousSiblingStoryPoint);

    //Check parent, grandparent etc. (parent may not be a StoryPoint if we navigated directly to a StoryItem via a target-link)
    var ancestorStoryPoint := GetAncestorStoryPoint;
    if Assigned(ancestorStoryPoint) then
      exit(ancestorStoryPoint);

    //Loop to end of story
    if Assigned(Story) then //TODO: we should be able to find the RootStoryItem without Story being assigned (though it would need traversing up the tree)
    begin
      var rootStoryItem := Story.RootStoryItem;
      if Assigned(rootStoryItem) then
        if rootStoryItem.StoryPoint then //if RootStoryItem is a StoryPoint return that
          exit(rootStoryItem)
        else
          exit(rootStoryItem.PreviousStoryPoint); //return last StoryPoint found (checking from RootStoryItem)
    end;

    //If we're the topmost StoryPoint
    if StoryPoint then
      result := Self
    else
      result := nil; //no StoryPoint found
  end;

  function TStoryItem.GetNextStoryPoint: IStoryItem; //TODO: this logic doesn't work ok when there is an isolated StoryPoint deep in the hierarchy (it's ignored)
  begin
    //Check children
    var firstChildStoryPoint := GetFirstChildStoryPoint; //TODO: should have option to do it recursively (with param to not go upwards) so that we can find grandchildren
    if Assigned(firstChildStoryPoint) then
      exit(firstChildStoryPoint);

    var parentItem := ParentStoryItem;
    if not Assigned(parentItem) then //If RootStoryItem with no StoryItem children
      exit(nil); //none found

    //Check next siblings //TODO: won't navigate directly into grand*-children if siblings aren't StoryPoints (though we could consider this a feature to be able to create isolated StoryPoints that are navigable only by name)
    var nextSiblingStoryPoint := GetNextSiblingStoryPoint;
    if Assigned(nextSiblingStoryPoint) then
      exit(nextSiblingStoryPoint);

    //Check parent, grandparent etc. (parent may not be a StoryPoint if we navigated directly to a StoryItem via a target-link)
    var ancestorStoryPoint := GetAncestorStoryPoint;
    while Assigned(ancestorStoryPoint) do
    begin
      var ancestorNextSiblingStoryPoint := ancestorStoryPoint.GetNextSiblingStoryPoint;
      if Assigned(ancestorNextSiblingStoryPoint) then
        exit(ancestorNextSiblingStoryPoint)
      else
        ancestorStoryPoint := ancestorStoryPoint.GetAncestorStoryPoint; //try going even more up the Story tree (till we reach the root)
    end;

    //Loop to start of story
    if Assigned(Story) then //TODO: we should be able to find the RootStoryItem without Story being assigned (though it would need traversing up the tree)
    begin
      var rootStoryItem := Story.RootStoryItem;
      if Assigned(rootStoryItem) then
        if rootStoryItem.StoryPoint then //if RootStoryItem is a StoryPoint return that
          exit(rootStoryItem)
        else
          exit(rootStoryItem.NextStoryPoint); //return first StoryPoint found (checking from RootStoryItem)
    end;

    //If we're the topmost StoryPoint
    if StoryPoint then
      result := Self
    else
      result := nil; //no StoryPoint found
  end;

  function TStoryItem.GetAncestorStoryPoint: IStoryItem;
  begin
    var parentItem := ParentStoryItem;
    if not Assigned(parentItem) then
      exit(nil); //reached the RootStoryItem without finding a StoryPoint

    if parentItem.StoryPoint then
      exit(parentItem)
    else
      exit(parentItem.GetAncestorStoryPoint); //recurse outwards till we find an ancestor that is a StoryPoint or reach the root
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

    Result := nil;
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

    Result := nil;
  end;

  function TStoryItem.GetPreviousSiblingStoryPoint: IStoryItem;
  begin
    var parentItem := ParentStoryItem;
    if not Assigned(parentItem) then exit(nil);

    var parentChildren := parentItem.StoryItems; //TODO: could ask only for StoryPoints here
    var storyItemIndex := parentChildren.IndexOf(Self as IStoryItem); //must use "as" here, else it won't always find it

    for var i := (storyItemIndex - 1) downto 0 do //0-based array index
    begin
      var sibling := parentChildren.Items[i];
      if sibling.IsStoryPoint then //note: not considering if result is Hidden, caller should show it if they want to activate it
        exit(sibling);
    end;

    Result := nil;
  end;

  function TStoryItem.GetNextSiblingStoryPoint: IStoryItem;
  begin
    var parentItem := ParentStoryItem;
    if not Assigned(parentItem) then exit(nil);

    var parentChildren := parentItem.StoryItems; //TODO: could ask only for StoryPoints here
    var storyItemIndex := parentChildren.IndexOf(Self as IStoryItem); //must use "as" here, else it won't always find it

    for var i := (storyItemIndex + 1) to (parentChildren.Count - 1) do //0-based array index
    begin
      var sibling := parentChildren.Items[i];
      if sibling.IsStoryPoint then //note: not considering if result is Hidden, caller should show it if they want to activate it
        exit(sibling);
    end;

    Result := nil;
  end;

  {$endregion}

  {$region 'Content'}

  function TStoryItem.GetContent: TBytes;
  begin
    Result := FContent;
  end;

  procedure TStoryItem.SetContent(const Value: TBytes); //Note: descendents should override this method to act upon change of content
  begin
    FContent := Value;
  end;

  {$endregion}

  {$region 'ContentExt'}

  function TStoryItem.GetContentExt: String;
  begin
    Result := FContentExt;
  end;

  procedure TStoryItem.SetContentExt(const Value: String);
  begin
    FContentExt := Value;
  end;

  {$endregion}

  {$region 'AllText'}

  function TStoryItem.GetAllText: TStrings; //Note: the output order of the strings may look strange, it's not in StoryPoint-related groups
  begin
    Result := TStringList.Create(false); //set to not own objects //Note: caller will need to free this TStringList

    //Append our own text
    var LTextStoryItem: ITextStoryItem;
    if Supports(Self, ITextStoryItem, LTextStoryItem) then
    begin
      result.Append(LTextStoryItem.Text);
      result.Append(EXPORT_TEXTSTORYITEM_SEPARATOR);
    end;

    //Append our children's text
    for var LStoryItem in StoryItems do
    begin
      var LStoryItemAllText := LStoryItem.AllText; //does recursion, resulting in a depth-first traversal of the subtree
      try
        if Assigned(LStoryItemAllText) then
          result.AddStrings(LStoryItemAllText); //append strings from child
      finally
        FreeAndNil(LStoryItemAllText); //freeing the TStringList returned by AllText property of child StoryItem
      end;
    end;
  end;

  procedure TStoryItem.SetAllText(const Value: TStrings); //expecting the same order of strings as returned by GetAllText
  begin
    if (Value.Count = 0) then //no more items
      exit;

    //Append our own text
    var LTextStoryItem: ITextStoryItem;
    if Supports(Self, ITextStoryItem, LTextStoryItem) then
    begin
      var LNextSeparatorIndex := Value.IndexOf(EXPORT_TEXTSTORYITEM_SEPARATOR);
      if (LNextSeparatorIndex < 0) then
        LNextSeparatorIndex := Value.Count; //if no separator found, assume it's till the end

      var LText := Value.GetLines(0, LNextSeparatorIndex - 1, false); //skip the separator and don't output a line break for last line
      for var i := LNextSeparatorIndex downto 0 do //Deleting in reverse order, including the found separator
        Value.Delete(i); //TODO: Maybe add some helper method to delete ranges like that
      LTextStoryItem.Text := LText;
    end;

    //Set our children's text
    for var LStoryItem in StoryItems do
    begin
      if (Value.Count = 0) then //no more items (optimization check to avoid looping through any more StoryItems, with all of them doing nothing)
        exit;

      LStoryItem.SetAllText(Value); //Note: even though this is a const parameter, the reference is what is constant, we can still change the contents (aka remove elements) of the TStrings implementation
    end;
  end;

  {$endregion}

  {$region 'ForegroundColor'}

  function TStoryItem.GetForegroundColor: TAlphaColor;
  begin
    Result := Glyph.ForegroundColor;
  end;

  procedure TStoryItem.SetForegroundColor(const Value: TAlphaColor);
  begin
    Glyph.ForegroundColor := Value;
  end;

  {$endregion}

  {$region 'BackgroundColor'}

  function TStoryItem.GetBackgroundColor: TAlphaColor;
  begin
    if Assigned(Background) then
      Result := Background.Fill.Color
    else
      Result := TAlphaColorRec.Null;
  end;

  procedure TStoryItem.SetBackgroundColor(const Value: TAlphaColor);
  begin
    if Assigned(Background) then
      with Background do
      begin
        Fill.Kind := TBrushKind.Solid;
        Fill.Color := Value;
      end;
  end;

  {$endregion}

  {$region 'FlippedHorizontally'}

  function TStoryItem.IsFlippedHorizontally: Boolean;
  begin
    Result := (Scale.X < 0);
  end;

  procedure TStoryItem.SetFlippedHorizontally(const Value: Boolean);
  begin
    if Value xor IsFlippedHorizontally then
      FlipHorizontally;
  end;

  {$endregion}

  {$region 'FlippedVertically'}

  function TStoryItem.IsFlippedVertically: Boolean;
  begin
    Result := (Scale.Y < 0);
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
    Result := FHidden;
  end;

  procedure TStoryItem.SetHidden(const Value: Boolean);
  begin
    FHidden := Value;
    Visible := (not Hidden) or (Assigned(ParentStoryItem) and (ParentStoryItem.EditMode)); //always reapply this (logic in SetEditMode depends on it), not only if value changed
  end;

  {$endregion}

  {$region 'Snapping'}

  function TStoryItem.IsSnapping: Boolean;
  begin
    Result := FSnapping;
  end;

  procedure TStoryItem.SetSnapping(const Value: Boolean);
  begin
    FSnapping := Value;
    //don't apply snapping at this point, it is supposed to be applied when user drags and drops an unanchored StoryItem into the area of a snapping StoryItem which are both children of the ActiveStoryItem. So we need to know what was dropped to check if there's a snapping sibling's area containing the drop point (comparing in absolute coordinates)
  end;

  procedure TStoryItem.DoSnapping;
  begin
    var LParent := ParentStoryItem;
    if not Assigned(LParent) then exit;

    try
      Enabled := false; //disable temporarily
      //Check if our CenterPoint lies inside the bounds of a sibling that has Snapping on
      var LObj := LParent.View.ObjectAtLocalPoint(BoundsRect.CenterPoint, false, false, false, false); //only checking the immediate children (ignoring SubComponents) of our ParentStoryPoint, not checking the disabled ones since we make ourself temporarily disabled to exclude us
      if Assigned(LObj) and (LObj.GetObject is TStoryItem) then
      begin
        var LStoryItemUnderneath := TStoryItem(LObj.GetObject);
        if LStoryItemUnderneath.Snapping then
          Position.Point := LStoryItemUnderneath.BoundsRect.CenterPoint - PointF(Width/2, Height/2); //use same center as the Snapping sibling //Note: should be must faster than BoundsRect := BoundsRect.CenterAt(LStoryItemUnderneath.BoundsRect), at least in Delphi 11.1, where CenterAt calls RectCenter which doesn't seem to be optimized (does 3 OffsetRect operations)
      end;
    finally
      Enabled := true; //make sure we always enable again
    end;
  end;

  {$endregion}

  {$region 'Anchored'}

  function TStoryItem.IsAnchored: Boolean;
  begin
    Result := Locked;
  end;

  procedure TStoryItem.SetAnchored(const Value: Boolean);
  begin
    Locked := Value;
    UpdateCursor;
  end;

  {$endregion}

  {$region 'UrlAction'}

  function TStoryItem.GetUrlAction: String;
  begin
    Result := FUrlAction;
  end;

  procedure TStoryItem.SetUrlAction(const Value: String);
  begin
    FUrlAction := Trim(Value);
    UpdateCursor;
  end;

  {$endregion}

  {$region 'Tags'}

  function TStoryItem.GetTags: String;
  begin
    Result := TagString;
  end;

  procedure TStoryItem.SetTags(const Value: String);
  begin
    TagString := Trim(Value);
  end;

  function TStoryItem.AreTagsMatched: Boolean;

    function GetTagged(const AnchoredValue: Boolean): TIStoryItemList;
    begin
      Result := StoryItems.GetAll(
        function(StoryItem: IStoryItem): Boolean
        begin
          with StoryItem do
            Result := (Tags <> '') and (Anchored = AnchoredValue);
        end
      );
    end;

  begin
    var MoveablesWithTags: TListEx<IStoryItem> := nil; //local vars not auto-initialized, safeguarding FreeAndNil
    var TargetsWithTags: TListEx<IStoryItem> := nil;
    try
      MoveablesWithTags := GetTagged(false); //non-anchored with tags
      Log('MoveablesWithTags.Count=%d', [MoveablesWithTags.Count]);

      TargetsWithTags := GetTagged(true); //anchored with tags (targets)
      Log('TargetsWithTags.Count=%d', [TargetsWithTags.Count]);

      Result :=
        TargetsWithTags.All( //looping over all targets, not over moveables since we'll remove every matched moveables (a moveable can be matched to a single target, but a target can be matched to multiple moveables)
          function(LTarget: IStoryItem): Boolean
            function CheckMatchingTags(const Tags1, Tags2: String): Boolean;
            begin
              Result := (Tags1 = Tags2); //TODO: maybe do more complex matching in the future, allowing lists of tags where a Target can define multiple Moveable item Tags it accepts and a Moveable can have multiple identities defined at its Tags
            end;
          begin
            Result := false; //consider a Target unmatched initially //Note: must set this here outside of the "for" loop, since no MoveablesWithTags may have been configured (by author's mistake) - else compiler will show warning that function may have undefined result
            for var i := MoveablesWithTags.Count - 1 downto 0 do //count backwards since we'll remove each matched moveable //TODO: add Remove function to Generics to do this item removal (counting backwards) using an optional (else remove all) predicate
            begin
              var LMoveable := MoveablesWithTags[i];
              if CheckMatchingTags(LTarget.Tags, LMoveable.Tags) and
                 LTarget.View.BoundsRect.Contains(LMoveable.View.BoundsRect.CenterPoint) then
              begin
                MoveablesWithTags.RemoveItem(LMoveable, TDirection.FromEnd);
                Log('Tag match: "%s" and "%s"', [LTarget.Tags, LMoveable.Tags]);
                Result := true; //a Target has to be matched by at least one Moveable (but need to check and remove all from the MoveablesWithTags list to see when finished if any moveables have been left unmatched to any targets)
              end;
            end;
          end
        )
        and (MoveablesWithTags.Count = 0); //check no moveables are left unmatched with targets after the previous action (if we've reached here, we're sure all targets were processed and matched)

    finally
      //freeing (will check internally if not nil) in reverse order
      FreeAndNil(TargetsWithTags);
      FreeAndNil(MoveablesWithTags);
    end;
  end;

  {$endregion}

  {$region 'TargetsVisible'}

  function TStoryItem.GetTargetsVisible: Boolean;
  begin
    Result := FTargetsVisible;
  end;

  procedure TStoryItem.SetTargetsVisible(const Value: Boolean);
  begin
    FTargetsVisible := Value;
    InvalidateRect(BoundsRect);
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

    Result := FOptions;
  end;

  {$endregion}

  {$region 'Serialization'}

  //based on https://stackoverflow.com/a/60978501/903783 (but without the PByte casting and pointer dereferencing)

  procedure TStoryItem.DefineProperties(Filer: TFiler);
  begin
    inherited DefineProperties(Filer);
    Filer.DefineBinaryProperty('ContentBin', ReadContent, WriteContent, Length(FContent) > 0); //assuming "ContentBin" is not a published property (to avoid any naming conflicts). It should be safe if it's "public" instead of "published", but if one wants to have a property editor in the IDE they need it published, so adding suffix "Bin" and marking the published property "stored false" is safest
  end;

  procedure TStoryItem.ReadContent(Stream: TStream);
  var
    BinSize: Integer;
  begin
    BinSize := Stream.Size;
    SetLength(FContent, BinSize);
    if BinSize > 0 then
      Stream.ReadBuffer(FContent, BinSize);
  end;

  procedure TStoryItem.WriteContent(Stream: TStream);
  begin
    Stream.WriteBuffer(FContent, Length(FContent));
  end;

  {$endregion}

  {$ENDREGION}

  {$REGION '--- EVENTS ---'}

  {$region 'Mouse'}

  procedure TStoryItem.HandleAreaSelectorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Single);
  begin
    inherited; //make sure we call this since it does the bring to front / send to back with double click

    if (ssCtrl in Shift) or (ssRight in Shift) then //either Ctrl+LeftClick or just RightClick
    begin
      var LPoint := PointF(X, Y) + AreaSelector.Position.Point;
      var LObj := ObjectAtLocalPoint(LPoint, false, true, false, false); //only checking the immediate children (ignoring SubComponents) //TODO: this won't work if we reuse an AreaSelector that belongs to other parent
      if Assigned(LObj) and (LObj.GetObject is TStoryItem) then
        TStoryItem(LObj.GetObject).Active := true //make CTRL or RIGHT clicked child the ActiveStoryItem
      else
        Options.ShowPopup; //if no child at this position show options popup
    end;
  end; //TODO: should also do the extra logic from TStoryPoint.MouseDown that can make the parent storyitem active (maybe make reusable methods)

  procedure TStoryItem.MouseDown(Button: TMouseButton; Shift: TShiftState;  X, Y: single);
  begin
    inherited;

    if EditMode or Anchored then exit;

    FDragStart := PointF(X, Y);
    FDragging := true;

    Capture; //Capture the mouse actions
  end;

  procedure TStoryItem.MouseUp(Button: TMouseButton; Shift: TShiftState;  X, Y: single);
  begin
    inherited;

    if EditMode or (not FDragging) then exit;

    FDragging := false;
    FDragStart := TPointF.Zero;

    DoSnapping;

    ReleaseCapture; //Release capture of mouse actions
  end;

  procedure TStoryItem.MouseMove(Shift: TShiftState; X, Y: Single);
  begin
    inherited;

    if EditMode or (not FDragging) then exit;

    var LParent := ParentStoryItem;
    if Assigned(LParent) and LParent.Active then //only when ParentStoryItem is the ActiveStoryItem //Note: never moving the RootStoryItem
    begin
      var LParentView := LParent.View;
      var newPos := PointF(X, Y) - FDragStart;
      var newAbsoluteBounds := LocalToAbsolute(TRectF.Create(newPos, Width, Height)); //TODO: does this take Scale in mind?
      if LParentView.AbsoluteRect.Contains(newAbsoluteBounds) then //only when the new bounds are contained in Parent to avoid user losing the moved StoryItem at the edges of its parent
        Position.Point := LParentView.AbsoluteToLocal(newAbsoluteBounds.TopLeft);
    end;
  end;

  procedure TStoryItem.MouseClick(Button: TMouseButton; Shift: TShiftState; X, Y: Single);

    function HasUrlAction: Boolean;
    begin
      Result := (FUrlAction <> '');
    end;

    function HasActiveChildStoryItem: Boolean;
    begin
      if not Assigned(ActiveStoryItem) then exit(false); //no ActiveStoryItem

      var LActiveStoryItemParent := ActiveStoryItem.ParentStoryItem;
      if not Assigned(LActiveStoryItemParent) then exit(false); //ActiveStoryItem is the RootStoryItem, so we can't be its ParentStoryItem

      Result := (LActiveStoryItemParent.View = Self); //check if we're the parent of the ActiveStoryItem //Note: we can't compare the IStoryPoint interfaces directly, must compare the TStoryPoint objects they wrap (the views)
    end;

    function HasActiveSiblingStoryItem: Boolean;
    begin
      if not Assigned(ActiveStoryItem) then exit(false); //no ActiveStoryItem

      var LActiveStoryItemParent := ActiveStoryItem.ParentStoryItem;
      if not Assigned(LActiveStoryItemParent) then exit(false); //ActiveStoryItem is the RootStoryItem, so we can't be its Sibling

      var LParent := ParentStoryItem;
      if not Assigned(LParent) then exit(false); //we're the RootStoryItem, so we can't have Siblings

      Result := (LActiveStoryItemParent.View = LParent.View); //check if we have the same parent as the ActiveStoryItem //Note: we can't compare the IStoryPoint interfaces directly, must compare the TStoryPoint objects they wrap (the views)
    end;

  begin
    Shift := FMouseShift; //TODO: remove if Delphi fixes related bug (more at FMouseShift definition)

    inherited; //fire event handlers

    if EditMode then //don't use check (Story.StoryMode = TStoryMode.EditMode) aka Global EditMode, in that case would have issue working at a given nesting level (grandchidlren would get in our way, getting activated by accident)
    begin
      if (ssCtrl in Shift) or (ssRight in Shift) then //either Ctrl+LeftClick or just RightClick
      begin
        var LObj := ObjectAtLocalPoint(PointF(X, Y), false, true, false, false); //only checking the immediate children (ignoring SubComponents)
        if Assigned(LObj) and (LObj.GetObject is TStoryItem) then
          TStoryItem(LObj.GetObject).Active := true //make CTRL or RIGHT clicked child the ActiveStoryItem
        else
          Options.ShowPopup; //if no child at this position show options popup
      end
      else
        PlayRandomAudioStoryItem; //TODO: maybe should play AudioStoryItems in the order they exist in their parent StoryItem (but would need to remember last one played in that case which may be problematic if they are reordered etc.)
        //PlayNextAudioStoryItem;
    end

    else //non-Edit mode

    begin
      //TODO: should traverse down parents to find the nearest parent StoryItem in the containment chain and see if it's active or not, if not should ignore all mouse actions (not consume them)
      if (((ssCtrl in Shift) or (ssRight in Shift)) and //either Ctrl+LeftClick or just RightClick //TODO: this allows to bypass riddles, maybe remove? (however selecting from StructureView also allows to bypass a riddle check as done by ActivateNextStoryPoint, could keep as a feature)
          (HasActiveChildStoryItem or HasActiveSiblingStoryItem)) then //and (one of our children is the ActiveStoryItem or we're its Sibling)...
        //TODO: does this bypass riddles (constrained navigation)? It shouldn't
        Active := true //...make us (the parent of the ActiveStoryItem) the Active one (so that we can go back to the parent level by right-clicking it without using the keyboard's ESC key)
      else
      begin
        PlayRandomAudioStoryItem; //TODO: maybe should play AudioStoryItems in the order they exist in their parent StoryItem (but would need to remember last one played in that case which may be problematic if they are reordered etc.)
        //PlayNextAudioStoryItem;

        //var LParent := ParentStoryItem;
        if (HasUrlAction
            and (Story.StoryMode <> TStoryMode.EditMode) //make sure we don't do UrlActions when editing a story //should we use EditMode property instead? it doesn't seem to check StoryMode, but seems to store to local storable field
            {and //TODO: should have URLs clickable only for children of ActiveStoryItem (and for itself if it's the RootStoryItem maybe) //in non-EditMode should disable HitTest though at everything that isn't the current StoryItem or direct child of the ActiveStoryItem apart from the TextStoryItems maybe (could maybe just disble HitTest at all siblings of ActiveStoryItem and have everything under ActiveStoryItem HitTest-enabled)
            ((Assigned(LParent) and LParent.Active) or
            ((not Assigned(LParent)) and Active))}) then //only when ParentStoryItem is the ActiveStoryItem //assuming short-circuit evaluation //if no LParent then it's the RootStoryItem, allowing it to have URLAction too
          FStory.OpenUrl(FUrlAction);
      end;
    end;

  end;

  {$endregion}

  {$region 'DragDrop'}

  procedure TStoryItem.DropTargetDropped(const Filepaths: array of String);
  begin
    //inherited;
    Add(Filepaths);

    DropTarget.HitTest := false; //always do just in case it was missed
  end;

  {$endregion}

  {$region 'Touch'}

  procedure TStoryItem.Tap(const Point: TPointF);
  begin
    inherited; //fire event handlers
    (* //don't use, shows popup too often, should just select maybe (like Click)
    if EditMode then
      Options.ShowPopup; //this will create options and assign to FOptions if it's unassigned
    *)
    //TODO: should we call MouseClick here or is a Tap mapped automatically to a MouseClick? (try on a touch device - also see if active [magnetic] Windows pen has difference from passive [capacitive] pen or finger touch)
  end;

  {$endregion}

  {$ENDREGION}

  {$region 'Clipboard'}

  procedure TStoryItem.Delete;
  begin
    ActivateParentStoryItem; //make ParentStoryItem active before deleting
    FreeAndNil(Self);
  end;

  procedure TStoryItem.Cut;
  begin
    Copy;
    Delete; //this makes ParentStoryItem active
  end;

  procedure TStoryItem.Copy;
  var svc: IFMXExtendedClipboardService;
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, Svc) then
      Svc.SetText(SaveToString);
  end;

  procedure TStoryItem.Paste;
  var Clipboard: IFMXExtendedClipboardService;
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXExtendedClipboardService, Clipboard) then
      Paste(Clipboard);
  end;

  procedure TStoryItem.UpdateCursor;
  begin
    if (UrlAction <> '') then
      Cursor := crHandPoint
    else if (not Anchored) then
      Cursor := crDrag
    else
      Cursor := crDefault;
  end;

  procedure TStoryItem.Paste(const Clipboard: IFMXExtendedClipboardService);
  var FileExt: String;
  begin
    //Check clipboard contents format
    if Clipboard.HasText then
    begin
      var LText := Trim(Clipboard.GetText); //Trimming since we may have pasted an indented object from a .readcom file or some SVG markup with extra spaces before and after
      if LText.StartsWith('object ') then //ignore if not Delphi serialization format (its text-based form), handle other text at descendents like TextStoryItem //TODO: add method to check if text contains object
        FileExt := EXT_READCOM
      else if LText.StartsWith('<svg ') and LText.EndsWith('</svg>') then
        FileExt := EXT_SVG
      else
        FileExt := '.txt' //TODO: should maybe add EXT_TXT to Zoomicon.Media.FMX.Models unit (of respective BOSS package)
    end
    else if Clipboard.HasImage then
      FileExt := EXT_PNG //could set to any image file extension the app knows here, clipboard images are not really in PNG format
    //TODO: would be nice to also have a Clipboard.HasAudio (we'd set FileExt := EXT_MP3 then [or any sound ext], so that TAudioStoryItem gets instantiated to handle that clipboard content)
    else
      raise Exception.Create('Unknown Clipboard format');

    try
      FIgnoreActiveStoryItemChanges := true; //ignore changes to ActiveStoryItem while loading children since "Active" is persisted property

      try
        var StoryItemFactory := StoryItemFactories.Get(FileExt);

        var StoryItem: TStoryItem;
        if Assigned(StoryItemFactory) then //a FileExt that has a specific TStoryItem subclass assigned to it
        begin
          StoryItem := StoryItemFactory.New(Self).View as TStoryItem;
          StoryItem.Name := 'Clipboard' + IntToStr(Random(maxint)); //TODO: use a GUID
          StoryItem.Load(Clipboard); //this should also set the Size of the control
        end
        else //we are adding a ".readcom" file, don't know beforehand what class of TStoryItem descendent it contains serialized
          StoryItem := Load(Clipboard, true) as TStoryItem;

        Add(StoryItem); //note: Add does (StoryItem=nil) check and returns, logging it
      except
        on EListError do
          raise EInvalidOperation.CreateFmt(MSG_CONTENT_FORMAT_NOT_SUPPORTED, [FileExt]);
      end;

    finally
      FIgnoreActiveStoryItemChanges := false; //restore value
    end;
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

  procedure TStoryItem.ReaderError(Reader: TReader; const Message: String; var Handled: Boolean);
  begin
    with THackReader(Reader) do
      begin
      var RemovedActivationOrderProperty := AnsiSameText(PropName, 'ActivationOrder'); //Ignores removed ActivationOrder property
      var ErrorReading := Message.StartsWith('Error reading ');
      var UnknownProperty := ErrorReading and Message.EndsWith(' does not exist'); //Ignores unknown properties //TODO: error starts with 'Error reading XX: ...' (not sure if this is always unlocalized) and also assuming System.RTL.Consts.SUnknownProperty = 'Property %s does not exist' - should have instead a function that can compare a format String with some text and match it (could even extract the format parameters)
      var InvalidProperty := ErrorReading and Message.EndsWith('Invalid property value'); //Ignores unloadable properties (e.g. any stored event handlers) //TODO: find related constant in System.RTL.Consts.SUnknownProperty (see previous comment)

      if RemovedActivationOrderProperty then
        Log('Ignored deprecated property "ActivationOrder"');
      if UnknownProperty then
        Log('Ignored unknown property "%s"', [PropName]);
      if InvalidProperty then
        Log('Ignored invalid value for property "%s"', [PropName]); //TODO: maybe we somehow need to skip the value of that property in the stream (for event handlers that are missing it seems to work, but when assigning say a String to an integer property it fails to skip the invalid value and tries to read the String contents as a property name)

      Handled := RemovedActivationOrderProperty or UnknownProperty or InvalidProperty;
      end;
  end;

  //

  function RemoveNonAllowedIdentifierChars(const s: String): String; //TODO: move to some utility library?
  begin
    Result := '';

    var count := s.Length;
    if (count <> 0) then
      for var i := 1 to count do //strings are 1-indexed
      begin
        var c := s[i];
        if IsValidIdent(result + c) then //keep only characters that don't cause the identifier to be invalid //TODO: this seems to allow Unicode alphabetic characters instead of just English ones (need to somehow convert maybe to English)
          Result := result + c;
      end;

    if (result.Length = 0) then
      Result := 'Item'; //NOTE: don't localize
  end;

  {$region 'Add'}

  function TStoryItem.GetAddFilesFilter: String;
  begin
    var listFilters := TStringList.Create(#0, '|');

    var listExt := TStringList.Create(#0, ';');
    listExt.Sorted := true;
    listExt.Duplicates := dupIgnore; //for "Duplicates" to work, need to have "Sorted := true" in listExt

    for var Pair in StoryItemFileFilters do
    begin
      listFilters.Add(Pair.Key {+ '(' + Pair.Value.Replace(';', ',') + ')'}); //note: title already contains the exts in parentheses
      listFilters.Add(Pair.Value);
      for var ext in Pair.Value.Split([';']) do
        listExt.Add(ext);
    end;

    //Insert first an entry for all supported extensions
    listFilters.Insert(0, listExt.DelimitedText);
    listExt.Delimiter := ','; //There's no way to use ', ' (aka a String separator instead of a char, to also have space after comma), could concatenate with for loop instead, but the entry is long anyway, so skip the extra spaces
    listFilters.Insert(0, 'READ-COM StoryItems, Images, Audio, Text (' + listExt.DelimitedText + ')'); //TODO: ideally the key should only contain the text (not *.xx too) so that we could concatenate the names (even if in singular) instead of hard-coding known type names here //Seems if we don't add the file extensions in parentheses in the name, Delphi or Win11 adds them with ";" format to the text of the entry (so it would be better if we auto-add them with ", ")

    Result := listFilters.DelimitedText;

    FreeAndNil(listExt);
    FreeAndNil(listFilters);
  end;

  procedure TStoryItem.Add(const StoryItem: IStoryItem);
  begin
    if not Assigned(StoryItem) then
    begin
      Log('TStoryItem.Add(nil) called, exiting');
      exit;
    end;

    //Center the new item...
    var StoryItemView := StoryItem.View;
    Self.InsertComponent(StoryItemView); //make sure we set Self as owner //TODO: need to call a safe method to do this with rename (see constructor above and extract such method)

    var OwnerAndParent := Self;
    var TheAreaSelector := AreaSelector; //need our AreaSelector, not the StoryItem's
    with StoryItemView do
    if TheAreaSelector.Visible and (TheAreaSelector.Width <> 0) and (TheAreaSelector.Height <> 0) then
      BoundsRect := TheAreaSelector.SelectedArea //TODO: does this take in mind scale? //TODO: assuming the AreaSelector has the same parent, if not (say using a global area selector in the future) should have some way for the AreaSelector to give map the coordinates to the wanted parent
      //Note: need to use BoundsRect, not Size, else control's children that are set to use "Align=Scale" seem to become larger when object shrinks and vice-versa instead of following its change
    else
    begin
      var ItemSize := Size.Size; //StoryItem constructor sets its DefaultSize, don't set Size to DefaultSize here since the StoryItem may have a different size set
      //Center the new item in its parent...
      Position.Point := PointF(OwnerAndParent.Size.Width/2 - ItemSize.Width/2, OwnerAndParent.Size.Height/2 - ItemSize.Height/2); //not creating TPosition objects to avoid leaking (TPointF is a record)
    end;

    StoryItemView.Align := TAlignLayout.Scale; //IMPORTANT: adjust when parent resizes

    StoryItemView.Parent := Self;
    StoryItemView.BringToFront; //load as front-most
  end;

  procedure TStoryItem.AddFromString(const Data: String);
  var StoryItem: IStoryItem;
  begin
    try
      FIgnoreActiveStoryItemChanges := true; //ignore changes to ActiveStoryItem while loading children since "Active" is persisted property

      if Supports(LoadFromString(Data, true), IStoryItem, StoryItem) then
        Add(StoryItem);

    finally
      FIgnoreActiveStoryItemChanges := false; //restore value
    end;
  end;

  procedure TStoryItem.Add(const Filepath: String);
  begin
    try
      FIgnoreActiveStoryItemChanges := true; //ignore changes to ActiveStoryItem while loading children since "Active" is persisted property

      var FileExt := ExtractFileExt(Filepath).ToLowerInvariant; //make file extension lower case
      try
        var StoryItemFactory := StoryItemFactories.Get(FileExt);

        var StoryItem: TStoryItem;
        if Assigned(StoryItemFactory) then
        begin
          StoryItem := StoryItemFactory.New(Self).View as TStoryItem;
          StoryItem.Name := RemoveNonAllowedIdentifierChars(TPath.GetFileNameWithoutExtension(Filepath)) + IntToStr(Random(maxint)); //TODO: use a GUID
          StoryItem.Load(Filepath); //this should also set the Size of the control
        end
        else //we are adding a ".readcom" file, don't know beforehand what class of TStoryItem descendent it contains serialized
          StoryItem := Load(Filepath, true) as TStoryItem;

        Add(StoryItem);
      except
        on EListError do
          raise EInvalidOperation.CreateFmt(MSG_CONTENT_FORMAT_NOT_SUPPORTED, [FileExt]);
      end;

    finally
      FIgnoreActiveStoryItemChanges := false; //restore value
    end;
  end;

  procedure TStoryItem.Add(const Filepaths: array of String);
  begin
    for var filepath in Filepaths do
      Add(filepath); //Adding all files one by one
  end;

  {$endregion}

  {$region 'Load'}

  function TStoryItem.GetLoadFilesFilter: String;
  begin
    Result := FILTER_READCOM;
  end;

  class function TStoryItem.LoadNew(const Stream: TStream; const ContentFormat: String): TStoryItem;
  begin
    var tempStoryItem: TStoryItem := nil; //local vars not auto-initialized, safeguarding FreeAndNil
    try
      tempStoryItem := TStoryItem.Create(nil); //creating since we need an instance to call Load //TODO: add a class
      Result := TStoryItem(tempStoryItem.Load(Stream, ContentFormat, True)); //passing True to load new TStoryItem descendent instance based on serialization information in the stream
    finally
      FreeAndNil(tempStoryItem); //releasing the tempStoryItem
    end;
  end;

  class function TStoryItem.LoadNew(const Filepath: String; const ContentFormat: String = EXT_READCOM): TStoryItem;
  begin
    var tempStoryItem: TStoryItem := nil; //local vars not auto-initialized, safeguarding FreeAndNil
    try
      tempStoryItem := TStoryItem.Create(nil); //creating since we need an instance to call Load //TODO: add a class
      Result := TStoryItem(tempStoryItem.Load(Filepath, true)); //passing True to load new TStoryItem descendent instance based on serialization information in the stream
    finally
      FreeAndNil(tempStoryItem); //releasing the tempStoryItem
    end;
  end;

  function TStoryItem.LoadFromString(const Data: String; const CreateNew: Boolean = false): TObject;
  begin
    var StrStream: TStringStream := nil; //local vars not auto-initialized, safeguarding FreeAndNil
    var BinStream: TMemoryStream := nil;
    try
      StrStream := TStringStream.Create(Data);
      Log(StrStream.DataString);

      BinStream := TMemoryStream.Create;
      ObjectTextToBinary(StrStream, BinStream); //may throw exception here

      BinStream.Seek(0, soFromBeginning); //position back to start of stream
      Result := LoadReadComBin(BinStream, CreateNew).View;
    finally
      //freeing (will check internally if not nil) in reverse order
      FreeAndNil(BinStream);
      FreeAndNil(StrStream);
    end;
  end;

  function TStoryItem.LoadReadCom(const Stream: TStream; const CreateNew: Boolean = false): IStoryItem;
  begin
    var reader: TStreamReader := nil; //local vars not auto-initialized, safeguarding FreeAndNil
    try
      reader := TStreamReader.Create(Stream);
      Result := TStoryItem(LoadFromString(reader.ReadToEnd, CreateNew)) as IStoryItem;
    finally
      FreeAndNil(reader); //the reader doesn't own the stream by default, so this won't close the stream
    end;
  end;

  function TStoryItem.LoadReadComBin(const Stream: TStream; const CreateNew: Boolean = false): IStoryItem; //TODO: could make this return TObject to support loading any Delphi object stream

    procedure RemoveStoryItems; //TODO: add to IStoryItem and implement at TStoryItem level?
    begin
      var StoryItemViews: TObjectListEx<TStoryItem> := nil; //local vars not auto-initialized, safeguarding FreeAndNil
      try
        StoryItemViews := TObjectListEx<TControl>.GetAllClass<TStoryItem>(Controls); //TODO: make some reusable property?
        StoryItemViews.FreeAll; //this will end up having RemoveObject called for each StoryItem (which will also remove from StoryItems and AudioStoryItems [if it is such] lists)
      finally
        FreeAndNil(StoryItemViews);
      end;
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

    var obj := Stream.ReadComponent(Instance, ReaderError);
    if obj is TStoryItem then //at current implementation only supporting TStoryItems
      Result := obj as IStoryItem //note that we have overriden ReadState so that it can set a custom Reader error handler to ignore specific deprecated properties, but that won't work by itself (so passing ErrorHandler here too via TStreamErrorHelper.CreateComponent method) if "CreateNew=true" is used, since we pass nil in that case so ReadState is called on TComponent, not on TStoryItem
    else
    begin
      FreeAndNil(obj);
      raise Exception.Create('Object is not a StoryItem');
    end;
  end;

  function TStoryItem.Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM; const CreateNew: Boolean = false): TObject;
  begin
    if ContentFormat = EXT_READCOM then //descendents should override this method to handle more ContentFormat (file extensions) values and call inherited if there's any they can't handle (e.g. to handle EXT_READCOM)
      Result := LoadReadCom(Stream, CreateNew).View
    else
      raise EInvalidOperation.CreateFmt(MSG_CONTENT_FORMAT_NOT_SUPPORTED, [ContentFormat]);
  end;

  function TStoryItem.Load(const Filepath: String; const CreateNew: Boolean = false): TObject;
  begin
    var InputFileStream: TFileStream := nil; //local vars not auto-initialized, safeguarding FreeAndNil
    try
      InputFileStream := TFileStream.Create(Filepath, fmOpenRead {or fmShareDenyNone}); //TODO: fmShareDenyNote probably needed for Android
      Result := Load(InputFileStream, ExtractFileExt(Filepath).ToLowerInvariant, CreateNew);
    finally
      FreeAndNil(InputFileStream);
    end;
  end;

  function TStoryItem.Load(const Clipboard: IFMXExtendedClipboardService; const CreateNew: Boolean = false): TObject;
  begin
    //TODO: how do we check for other opaque file formats (filepath?) on clipboard?
    //TODO: add support for pasting a URL from clipboard if there's custom format for it

    if Clipboard.HasText then
    begin
      var LText := Trim(Clipboard.GetText); //Trimming since we may have pasted an indented object from a .readcom file

      if LText.StartsWith('object ') then //ignore if not Delphi serialization format (its text-based form), handle other text at descendents like TextStoryItem
        Exit(LoadFromString(LText, CreateNew)) //Create new object if we don't know from beforehand what exact type it is

      //else if IsURI(LText) then
      //  LoadFromURI(LText); //TODO: see code from MainForm (and move here with callbacks if needed for its custom processing)
    end;

    Result := nil;
  end;

  {$endregion}

  {$region 'Save'}

  function TStoryItem.GetSaveFilesFilter: String;
  begin
    Result := FILTER_READCOM + '|' +
              FILTER_HTML;
  end;

  function TStoryItem.SaveToString: String;
  begin
    var BinStream: TMemoryStream := nil; //local vars not auto-initialized, safeguarding FreeAndNil
    var StrStream: TStringStream := nil; //local vars not auto-initialized, safeguarding FreeAndNil
    try
      BinStream := TMemoryStream.Create;
      SaveReadComBin(BinStream); //need to save to binary first (to memory)... //TODO: save to temp file (esp. for memory constrained devices) instead?

      BinStream.Seek(0, soFromBeginning);
      StrStream := TStringStream.Create('');
      ObjectBinaryToText(BinStream, StrStream); //...then convert to text format

      StrStream.Seek(0, soFromBeginning); //position back to start of stream
      result:= StrStream.DataString; //get as a string
    finally
      //freeing (will check internally if not nil) in reverse order
      FreeAndNil(StrStream);
      FreeAndNil(BinStream);
    end;

    Log(result);
  end;

  procedure TStoryItem.SaveReadCom(const Stream: TStream);
  begin
    var writer: TStreamWriter := nil; //local vars not auto-initialized, safeguarding FreeAndNil
    try
      writer := TStreamWriter.Create(Stream);
      writer.Write(SaveToString); //SaveToString uses SaveReadComBin (then converts to String)
    finally
      FreeAndNil(writer); //the writer doesn't own the stream by default, so this won't close the stream
    end;
  end;

  procedure TStoryItem.SaveReadComBin(const Stream: TStream); //also called by SaveToString
  begin
    if Assigned(FOptions) then FOptions.HidePopup; //Must hide options popup else the TCustomPopupForm FMX is using gets serialized with the StoryItem //Note: don't use Options property to avoid constructing popup

    var wasActive := Active;
    try
      Active := false; //hide any UI related to being active, enable children etc., but also don't store Active=true so that at Paste it won't cause issues
      Stream.WriteComponent(Self); //note: this will result in the RootStoryItem never having Active=true when serialized, but if no StoryItem is Active the HomeStoryItem or the RootStoryItem if no HomeStoryItem is set with become the Active one
    finally
      if wasActive then
        Active := true; //restore previous Active state
    end;
  end;

  procedure TStoryItem.Save(const Stream: TStream; const ContentFormat: String = EXT_READCOM);
  begin
    if ContentFormat = EXT_READCOM then
      SaveReadCom(Stream)
    (* //TODO: maybe also support saving to HTML stream (with images encoded inside it). In that case should have separate content filter though for self-contained HTML, since we now check for EXT_HTML at Save(Filepath), since that needs the filepath to save the images externally to the HTML
    else if ContentFormat = EXT_HTML then
      SaveHTML(Stream)
    *)
    else
      raise EInvalidOperation.CreateFmt(MSG_CONTENT_FORMAT_NOT_SUPPORTED, [ContentFormat]);
  end;

  procedure TStoryItem.Save(const Filepath: String);
  begin
    var OutputFileStream: TFileStream := nil; //local vars not auto-initialized, safeguarding FreeAndNil
    try
     OutputFileStream := TFileStream.Create(Filepath, fmCreate or fmOpenWrite {or fmShareDenyNone}); //TODO: may be needed for Android //Note: fmCreate clears (overwrites) any existing content

     var ContentFormat := ExtractFileExt(Filepath);
     if ContentFormat = EXT_HTML then //must check here instead of at Save(Stream, ContentFormat), since saving to HTML requires a filepath (we don't save images encoded inside the HTML for size reasons)
      SaveHTML(OutputFileStream, Filepath + '_Images')
     else
      Save(OutputFileStream, ContentFormat);

    finally
      FreeAndNil(OutputFileStream);
    end;
  end;

  procedure TStoryItem.SaveThumbnail(const Filepath: String; const MaxWidth: Integer = DEFAULT_THUMB_WIDTH; const MaxHeight: Integer = DEFAULT_THUMB_HEIGHT);
  begin
    const LPreviousActiveStoryItem = ActiveStoryItem; //TODO: remove when thumb generation is fixed and does't need to activate storyitem first
    //ActivateRootStoryItem;
    ActiveStoryItem := Self; //TODO: remove when thumb generation is fixed to not need this
    //Active := true; //TODO: see why if we use this one instead it skips TextStoryItem rendering for StoryItems that aren't in visible area

    //TThread.Queue(nil, procedure
      //begin
        var thumb: TBitmap := nil; //local vars not auto-initialized, safeguarding FreeAndNil
        try
          thumb := MakeThumbnail(MaxWidth, MaxHeight);
          thumb.SaveToFile(Filepath); //Max thumb size 200x200
        finally
          FreeAndNil(thumb);
          LPreviousActiveStoryItem.Active := true; //restore previous ActiveStoryItem //TODO: remove when thumb generation is fixed and does't need to activate storyitem first at SaveHTMLImage
        end;

      //end
    //);
  end;

  procedure TStoryItem.SaveHTML(const Stream: TStream; const ImagesPath: String; const MaxImageWidth: Integer = DEFAULT_HTML_IMAGE_WIDTH; const MaxImageHeight: Integer = DEFAULT_HTML_IMAGE_HEIGHT);
    var LHTMLWriter: TStreamWriter;

    procedure SaveHTMLImage(const AStoryItem: IStoryItem; const AIndex: Integer);
    begin
      const ImageFilename = 'Image' + AIndex.ToString + '.png';
      const LImagePath = TPath.Combine(ImagesPath, ImageFilename);

      TThread.Queue(nil, procedure
        begin
          AStoryItem.SaveThumbnail(LImagePath, MaxImageWidth, MaxImageHeight);
        end
      );

      LHTMLWriter.WriteLine('    <img src="%s" alt="%s" /><br />', [ExtractFileName(ImagesPath) + '/' + ImageFilename, '']); //assuming the Images are placed one level deeper [in a subfolder] than the HTML //Note: must use '/' since it's a partial URL, don't use TPath.Combine
    end;

  begin
    LHTMLWriter := nil; //local vars not auto-initialized, safeguarding FreeAndNil
    try
      LHTMLWriter := TStreamWriter.Create(Stream, TEncoding.UTF8);

      with LHTMLWriter do
      begin
        WriteLine('<html>');
        WriteLine('<!--%s-->', ['Generated by READ-COM: Reading Communities app']);
        WriteLine('  <body>');
      end;

      if TDirectory.Exists(ImagesPath)
        then TDirectory.Delete(ImagesPath, true);
      TDirectory.CreateDirectory(ImagesPath);

      //Export HomeStoryItem (or RootStoryItem if no HomeStoryItem has been set) snapshot

      var LIndex := 1;
      var LStartingStoryItem := HomeStoryItem;
      if not Assigned(LStartingStoryItem) then
        LStartingStoryItem := Story.RootStoryItem;

      SaveHTMLImage(LStartingStoryItem, LIndex);

      //Export StoryPoint snapshots (except HomeStoryItem)

      for var LStoryItem in StoryItems{StoryPoints} do //TODO: need to iterate over StoryPoints //TODO: doesn't go deeper for now, just assumes StoryPoints are children of the RootStoryItem which isn't always the case
      begin
        if (not LStoryItem.StoryPoint) then //TODO: temporary, remove when iterating over storypoints (with nesting)
          continue; //iterates to next StoryItem

        if (LStoryItem.View = LStartingStoryItem.View) then //skip if we already exported this before the loop //Note: must compare the View objects, not the interface pointers
          continue;

        Inc(LIndex);
        SaveHTMLImage(LStoryItem, LIndex);
      end;

      Log('Exported Images: %d', [LIndex]);

      with LHTMLWriter do
      begin
        WriteLine('  </body>');
        WriteLine('</html>');
      end;

    finally
      FreeAndNil(LHTMLWriter);
    end;
  end;

  procedure TStoryItem.SaveHTML(const Filepath: String; const MaxImageWidth: Integer = DEFAULT_HTML_IMAGE_WIDTH; const MaxImageHeight: Integer = DEFAULT_HTML_IMAGE_HEIGHT);
  begin
    var LHtmlFileStream: TFileStream := nil; //local vars not auto-initialized, safeguarding FreeAndNil
    try
      LHtmlFileStream := TFileStream.Create(Filepath, fmCreate or fmOpenWrite {or fmShareDenyNone}); //TODO: fmShareDenyNone may be needed for Android //Note: fmCreate clears (overwrites) any existing content
      SaveHtml(LHtmlFileStream, Filepath + '_Images', MaxImageWidth, MaxImageHeight);
    finally
      FreeAndNil(LHtmlFileStream);
    end;
  end;

  {$endregion}

  {$endregion}

initialization
  StoryItemFactories.Add([EXT_READCOM], nil); //special case, class information is in the serialization stream

  AddStoryItemFileFilter(FILTER_READCOM_TITLE, FILTER_READCOM_EXTS); //should make sure this is used first

end.

