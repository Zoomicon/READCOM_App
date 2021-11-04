unit READCOM.App.Models;

interface

uses
  Zoomicon.Generics.Collections, //for TListEx
  Zoomicon.Media.Classes, //for TMediaPlayerEx
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
  FILTER_READCOM = 'READ-COM StoryItem (*.read-com)|*.readcom';

type
  IStoreable = interface
    ['{A08F7880-FBE5-40C5-B695-FF0F3A18EF3E}']
    //--- Methods ---

    { Load }
    function GetLoadFilesFilter: String;
    procedure Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload;
    procedure Load(const Filepath: string); overload;
    procedure Load(const Filepaths: array of string); overload;
    procedure LoadFromString(const Data: String);

    { Save }
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

  IStoryItem = interface(IStoreable)
    ['{238909DD-45E6-463A-9698-C7C6DC1A6DFE}']
    //--- Methods ---
    procedure PlayRandomAudioStoryItem;

    { Id }
    function GetId: TGUID;
    procedure SetId(const Value: TGUID);

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

    { Target }
    function GetTarget: IStoryItem;
    procedure SetTarget(const Value: IStoryItem);

    { TargetId }
    function GetTargetId: TGUID;
    procedure SetTargetId(const Value: TGUID);

    { StoryMode }
    function GetStoryMode: TStoryMode;
    procedure SetStoryMode(const Value: TStoryMode);

    { Options }
    function GetOptions: IStoryItemOptions;

    //--- Messages ---
    procedure HandleParentNavigatedToChanged;

    //--- Properties ---
    property Id: TGUID read GetId write SetId;
    property View: TControl read GetView;
    property ParentStoryItem: IStoryItem read GetParentStoryItem write SetParentStoryItem; //default nil //stored false
    property StoryItems: TIStoryItemList read GetStoryItems write SetStoryItems; //default nil
    property AudioStoryItems: TIAudioStoryItemList read GetAudioStoryItems; //stored false
    property Hidden: Boolean read IsHidden write SetHidden; //default false
    property Target: IStoryItem read GetTarget write SetTarget; //stored false
    property TargetId: TGUID read GetTargetId write SetTargetId; //default ''
    property StoryMode: TStoryMode read GetStoryMode write SetStoryMode; //default AnimatedStoryMode
    property Options: IStoryItemOptions read GetOptions; //stored false
  end;

  IStory = interface(IStoryItem)
    { StoryMode }
    function GetStoryMode: TStoryMode;
    procedure SetStoryMode(const Value: TStoryMode);
  end;

  IStoryItemOptions = interface
    ['{1AEC7512-1E1D-4720-9D74-9A5411A64377}']
    { StoryItem }
    function GetStoryItem: IStoryItem;

    { DeleteVisible }
    function IsDeleteVisible: Boolean;
    procedure SetDeleteVisible(const Value: Boolean);

    { Popup }
    procedure ShowPopup;
    procedure HidePopup;

    property DeleteVisible: Boolean read IsDeleteVisible write SetDeleteVisible;
  end;

  IPanelStoryItem = interface(IStoryItem)
    ['{61292D80-36A5-4330-B52B-685D538C1E52}']
    //--- Methods ---
    procedure NavigateTo;

    { Active }
    function IsActive: Boolean;
    procedure SetActive(const Value: Boolean);

    { Navigatable }
    function IsNavigatable: Boolean;
    procedure SetNavigatable(const Value: Boolean);

    { NavigationOrder }
    function GetNavigationOrder: Integer;
    procedure SetNavigationOrder(const Value: Integer);

    //--- Events ---
    procedure HandleStoryModeChanged;

    //--- Properties ---
    property Active: Boolean read IsActive write SetActive; //default false
    property Navigatable: Boolean read IsNavigatable write SetNavigatable; //default true
    property NavigationOrder: Integer read GetNavigationOrder write SetNavigationOrder; //default 0
  end;

  IImageStoryItem = interface(IStoryItem)
    ['{26111D6E-A587-4AB5-8CC9-84269C2719DC}']
    //--- Methods ---
    { Image }
    function GetImage: TImage;
    procedure SetImage(const Value: TImage);

    //--- Properties ---
    property Image: TImage read GetImage write SetImage; //stored false //default nil
  end;

  IBitmapImageStoryItem = interface(IImageStoryItem)
    ['{97C577C0-5391-4B1D-8EA9-119D35B91523}']
    //--- Methods ---
    { Image }
    function GetImage: TImage;
    procedure SetImage(const Value: TImage);

    //--- Properties ---
    property Image: TImage read GetImage write SetImage; //stored true //default nil //overrides ancestor's "stored" setting
  end;

  IVectorImageStoryItem = interface(IImageStoryItem)
    ['{6A71E9E3-D0AC-452E-9DF9-6DFC25BFB2CD}']
    //--- Methods ---
    { Image }
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

    { Muted }
    function IsMuted: Boolean;
    procedure SetMuted(const Value: Boolean);

    { AutoPlaying }
    function IsAutoPlaying: Boolean;
    procedure SetAutoPlaying(const Value: Boolean);

    { Looping }
    function IsLooping: Boolean;
    procedure SetLooping(const Value: Boolean);

    { PlayOnce }
    function IsPlayOnce: Boolean;
    procedure SetPlayOnce(const Value: Boolean);

    { Audio }
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
    { Text }
    function GetText: String;
    procedure SetText(const Value: String);

    { Editable }
    function IsEditable: Boolean;
    procedure SetEditable(const Value: Boolean);

    { InputPrompt }
    function GetInputPrompt: String;
    procedure SetInputPrompt(const Value: String);

    { Font }
    function GetFont: TFont;
    procedure SetFont(const Value: TFont);

    { Color }
    function GetTextColor: TAlphaColor;
    procedure SetTextColor(const Value: TAlphaColor);

    //--- Properties ---
    property Text: String read GetText write SetText; //default ''
    property Editable: Boolean read IsEditable write SetEditable; //default false
    property InputPrompt: String read GetInputPrompt write SetInputPrompt;
    property Font: TFont read GetFont write SetFont; //sets font size, font family (typeface), font style (bold, italic, underline, strikeout)
    property TextColor: TAlphaColor read GetTextColor write SetTextColor;
  end;

implementation

end.
