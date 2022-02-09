//Description: READ-COM App Models
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.App.Models;

interface

uses
  Zoomicon.Generics.Collections, //for TListEx
  Zoomicon.Generics.Factories, //for IFactory, IFactoryRegistry
  Zoomicon.Media.FMX, //for TMediaPlayerEx
  System.Classes, //for TStream
  System.Generics.Collections, //for TList
  System.UITypes, //for TAlphaColor
  FMX.Controls, //for TControl
  FMX.Graphics, //for TFont
  FMX.Objects, //for TImage
  FMX.SVGIconImage, //for TSVGIconImage
  FMX.Media; //for TMediaPlayer

{$region 'Storage' ------------------------------------------------------------}

const
  EXT_READCOM = '.readcom';
  FILTER_READCOM_TITLE = 'READ-COM StoryItem (*.readcom)';
  FILTER_READCOM_EXTS = '*' + EXT_READCOM;
  FILTER_READCOM = FILTER_READCOM_TITLE + '|' + FILTER_READCOM_EXTS;

type

  IClipboardEnabled = interface
    ['{FDD22AC7-873A-4127-B200-E99DB4F2DEBF}']
    procedure Copy;
    procedure Paste;
  end;

  IStoreable = interface(IClipboardEnabled)
    ['{A08F7880-FBE5-40C5-B695-FF0F3A18EF3E}']
    //--- Methods ---

    {Add}
    function GetAddFilesFilter: String;
    procedure Add(const Filepath: String); overload;
    procedure Add(const Filepaths: array of string); overload;

    {Load}
    function GetLoadFilesFilter: String;
    procedure Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload;
    procedure Load(const Filepath: string); overload;
    function LoadFromString(const Data: String; const CreateNew: Boolean = false): TObject;

    {Save}
    function GetSaveFilesFilter: String;
    procedure Save(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload;
    procedure Save(const Filepath: string); overload;
    function SaveToString: string; overload;
  end;

{$endregion -------------------------------------------------------------------}

type

  //forward declarations
  IStoryItem = interface;
  IPanelStoryItem = interface;
  IImageStoryItem = interface;
  IBitmapImageStoryItem = interface;
  IVectorImageStoryItem = interface;
  IAudioStoryItem = interface;
  IStoryItemOptions = interface;

  TIStoryItemList = TListEx<IStoryItem>;
  TIAudioStoryItemList = TListEx<IAudioStoryItem>;

  TStoryMode = (AnimatedStoryMode, InteractiveStoryMode, GuidedInteractiveStoryMode, EditMode);

  IStory = interface
    ['{3A6CAD51-3787-4D18-9DA7-A07895BC4661}']
    procedure ZoomTo(const StoryItem: IStoryItem = nil); //ZoomTo(nil) zooms to all content

    {RootStoryItem}
    function GetRootStoryItem: IStoryItem;
    procedure SetRootStoryItem(const Value: IStoryItem);

    {HomeStoryItem}
    function GetHomeStoryItem: IStoryItem;
    procedure SetHomeStoryItem(const Value: IStoryItem);

    {ActiveStoryItem}
    function GetActiveStoryItem: IStoryItem;
    procedure SetActiveStoryItem(const Value: IStoryItem);

    {Navigation}
    procedure ActivateHomeStoryItem;
    procedure ActivateRootStoryItem;
    procedure ActivateParentStoryItem;
    procedure ActivatePreviousStoryPoint;
    procedure ActivateNextStoryPoint;

    {StoryMode}
    function GetStoryMode: TStoryMode;
    procedure SetStoryMode(const Value: TStoryMode);

    property StoryMode: TStoryMode read GetStoryMode write SetStoryMode; //default AnimatedStoryMode
    property RootStoryItem: IStoryItem read GetRootStoryItem write SetRootStoryItem;
    property HomeStoryItem: IStoryItem read GetHomeStoryItem write SetHomeStoryItem;
    property ActiveStoryItem: IStoryItem read GetActiveStoryItem write SetActiveStoryItem;
  end;

  IStoryItemFactory = IFactory<IStoryItem>;
  IStoryItemFactoryRegistry = IFactoryRegistry<String, IStoryItem>;

  IStoryItem = interface(IStoreable)
    ['{238909DD-45E6-463A-9698-C7C6DC1A6DFE}']

    //--- Methods ---
    procedure PlayRandomAudioStoryItem;

    {View}
    function GetView: TControl;

    {ParentStoryItem}
    function GetParentStoryItem: IStoryItem;
    procedure SetParentStoryItem(const Value: IStoryItem);

    {StoryItems}
    function GetStoryItems: TIStoryItemList;
    procedure SetStoryItems(const Value: TIStoryItemList);

    {AudioStoryItems}
    function GetAudioStoryItems: TIAudioStoryItemList;

    {Active}
    function IsActive: Boolean;
    procedure SetActive(const Value: Boolean);

    {EditMode}
    function IsEditMode: Boolean;
    procedure SetEditMode(const Value: Boolean);

    {BorderVisible}
    function IsBorderVisible: Boolean;
    procedure SetBorderVisible(const Value: Boolean);

    {Home}
    function IsHome: Boolean; //note: a Home StoryItem doesn't have to be StoryPoint, could be just the startup instructions that are shown once and not when looping through the StoryPoints
    procedure SetHome(const Value: Boolean);

    {StoryPoint}
    function IsStoryPoint: boolean;
    procedure SetStoryPoint(const Value: boolean);

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
    function IsFlippedHorizontally: Boolean;
    procedure SetFlippedHorizontally(const Value: Boolean);

    {FlippedVertically}
    function IsFlippedVertically: Boolean;
    procedure SetFlippedVertically(const Value: Boolean);

    {Hidden}
    function IsHidden: Boolean;
    procedure SetHidden(const Value: Boolean);

    {Anchored}
    function IsAnchored: Boolean;
    procedure SetAnchored(const Value: Boolean);

    {UrlAction}
    function GetUrlAction: String;
    procedure SetUrlAction(const Value: String);

    {TargetsVisible}
    function GetTargetsVisible: Boolean;
    procedure SetTargetsVisible(const Value: Boolean);

    {Options}
    function GetOptions: IStoryItemOptions;

    {IStoreable extensions}
    procedure Add(const StoryItem: IStoryItem); overload;
    function LoadReadCom(const Stream: TStream; const CreateNew: Boolean = false): IStoryItem;
    function LoadReadComBin(const Stream: TStream; const CreateNew: Boolean = false): IStoryItem;
    procedure SaveReadCom(const Stream: TStream);
    procedure SaveReadComBin(const Stream: TStream);

    //--- Properties ---
    property View: TControl read GetView;
    property ParentStoryItem: IStoryItem read GetParentStoryItem write SetParentStoryItem; //stored false //default nil
    property StoryItems: TIStoryItemList read GetStoryItems write SetStoryItems; //default nil
    property AudioStoryItems: TIAudioStoryItemList read GetAudioStoryItems; //stored false
    property Active: Boolean read IsActive write SetActive; //default false
    property EditMode: Boolean read IsEditMode write SetEditMode; //stored false //default false
    property BorderVisible: Boolean read IsBorderVisible write SetBorderVisible; //stored false default false
    property Home: Boolean read IsHome write SetHome; //default false
    property StoryPoint: Boolean read IsStoryPoint write SetStoryPoint; //default false
    property PreviousStoryPoint: IStoryItem read GetPreviousStoryPoint; //stored false
    property NextStoryPoint: IStoryItem read GetNextStoryPoint; //stored false
    property FlippedHorizontally: Boolean read IsFlippedHorizontally write setFlippedHorizontally; //stored false //default false //Scale.X stores related info
    property FlippedVertically: Boolean read IsFlippedVertically write setFlippedVertically; //stored false //default false //Scale.Y stores related info
    property Hidden: Boolean read IsHidden write SetHidden; //default false
    property Anchored: Boolean read IsAnchored write SetAnchored; //default true
    property TargetsVisible: Boolean read GetTargetsVisible write SetTargetsVisible; //default false
    property Options: IStoryItemOptions read GetOptions; //stored false
  end;

  IStoryItemOptions = interface
    ['{1AEC7512-1E1D-4720-9D74-9A5411A64377}']

    //--- Methods ---
    {View}
    function GetView: TControl;

    {StoryItem}
    function GetStoryItem: IStoryItem;
    procedure SetStoryItem(const Value: IStoryItem);

    {Popup}
    procedure ShowPopup;
    procedure HidePopup;

    {File}
    function ActAdd: Boolean;
    function ActLoad: Boolean;
    function ActSave: Boolean;

    //--- Properties ---
    property View: TControl read GetView; //stored false
    property StoryItem: IStoryItem read GetStoryItem write SetStoryItem; //stored false
  end;

  IBitmapImageStoryItemOptions = interface(IStoryItemOptions)
    ['{DA637418-9648-48C7-A0CB-7475CAFECBAE}']

    {BitmapImageStoryItem}
    function GetBitmapImageStoryItem: IBitmapImageStoryItem;
    procedure SetBitmapImageStoryItem(const Value: IBitmapImageStoryItem);

    //-- Properties --
    property BitmapImageStoryItem: IBitmapImageStoryItem read GetBitmapImageStoryItem write SetBitmapImageStoryItem; //stored false
  end;


  IPanelStoryItem = interface(IStoryItem)
    ['{61292D80-36A5-4330-B52B-685D538C1E52}']
  end;

  IImageStoryItem = interface(IStoryItem)
    ['{26111D6E-A587-4AB5-8CC9-84269C2719DC}']

    //--- Methods ---
    {Image}
    function GetImage: TImage;
    procedure SetImage(const Value: TImage);

    //--- Properties ---
    property Image: TImage read GetImage write SetImage; //stored false //default nil
  end;

  IBitmapImageStoryItem = interface(IImageStoryItem)
    ['{97C577C0-5391-4B1D-8EA9-119D35B91523}']

    //--- Methods ---
    {Image}
    function GetImage: TImage;
    procedure SetImage(const Value: TImage);

    //--- Properties ---
    property Image: TImage read GetImage write SetImage; //stored true //default nil //overrides ancestor's "stored" setting
  end;

  IVectorImageStoryItem = interface(IImageStoryItem)
    ['{6A71E9E3-D0AC-452E-9DF9-6DFC25BFB2CD}']

    //--- Methods ---
    {Image}
    function GetSVGImage: TSVGIconImage;
    procedure SetSVGImage(const Value: TSVGIconImage);

    //--- Properties ---
    property Image: TImage read GetImage; //overrides ancestor's "write" and "stored" settings
    property SVGImage: TSVGIconImage read GetSVGImage write SetSVGImage; //default nil
  end;

  IAudioStoryItem = interface(IStoryItem)
    ['{5C29ED8A-C6D1-47C2-A8F8-F41249C5846B}']

    //--- Methods ---
    procedure Play;

    {Muted}
    function IsMuted: Boolean;
    procedure SetMuted(const Value: Boolean);

    {AutoPlaying}
    function IsAutoPlaying: Boolean;
    procedure SetAutoPlaying(const Value: Boolean);

    {Looping}
    function IsLooping: Boolean;
    procedure SetLooping(const Value: Boolean);

    {PlayOnce}
    function IsPlayOnce: Boolean;
    procedure SetPlayOnce(const Value: Boolean);

    {Audio}
    function GetAudio: TMediaPlayerEx;
    procedure SetAudio(const Value: TMediaPlayerEx);

    //--- Properties ---
    property Muted: Boolean read IsMuted write SetMuted;
    property AutoPlay: Boolean read IsAutoPlaying write SetAutoPlaying;
    property PlayOnce: Boolean read IsPlayOnce write SetPlayOnce;
    property Audio: TMediaPlayerEx read GetAudio write SetAudio; //stored false
  end;

  ITextStoryItem = interface(IStoryItem)
    ['{A05D85F0-F7F6-4EA1-8D4F-0C6FF7BEA572}']

    //--- Methods ---
    {Text}
    function GetText: String;
    procedure SetText(const Value: String);

    {Editable}
    function IsEditable: Boolean;
    procedure SetEditable(const Value: Boolean);

    {InputPrompt}
    function GetInputPrompt: String;
    procedure SetInputPrompt(const Value: String);

    {Font}
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);

    {Color}
    function GetTextColor: TAlphaColor;
    procedure SetTextColor(const Value: TAlphaColor);

    //--- Properties ---
    property Text: String read GetText write SetText; //default ''
    property Editable: Boolean read IsEditable write SetEditable; //default false
    property InputPrompt: String read GetInputPrompt write SetInputPrompt;
    property Font: TFont read GetFont write SetFont; //sets font size, font family (typeface), font style (bold, italic, underline, strikeout)
    property TextColor: TAlphaColor read GetTextColor write SetTextColor;
    //TODO:  (maybe remove and just add filterchar string like in TEdit)
  end;

implementation

end.
