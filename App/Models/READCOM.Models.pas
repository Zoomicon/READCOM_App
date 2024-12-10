//Description: READ-COM App Models
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Models;

interface
  {$region 'Used units'}
  uses
    System.Classes, //for TStream
    System.Generics.Collections, //for TList
    System.UITypes, //for TAlphaColor
    //
    FMX.Clipboard, //for IFMXExtendedClipboardService
    FMX.Controls, //for TControl
    FMX.Graphics, //for TFont
    FMX.Objects, //for TImage
    FMX.Surfaces, //for TBitmapSurface
    FMX.Types, //for TTextAlign
    //
    Zoomicon.Generics.Collections, //for TListEx
    Zoomicon.Generics.Factories; //for IFactory, IFactoryRegistry
  {$endregion}

  {$region 'Storage' ------------------------------------------------------------}

  {$region 'Constants'}

  const
    EXT_READCOM = '.readcom';
    FILTER_READCOM_TITLE = 'READ-COM StoryItem (*.readcom)';
    FILTER_READCOM_EXTS = '*' + EXT_READCOM;
    FILTER_READCOM = FILTER_READCOM_TITLE + '|' + FILTER_READCOM_EXTS;

    //for HTML export
    EXT_HTML = '.html';
    FILTER_HTML_TITLE = 'HTML (*.html)';
    FILTER_HTML_EXTS = '*' + EXT_HTML;
    FILTER_HTML = FILTER_HTML_TITLE + '|' + FILTER_HTML_EXTS;

    DEFAULT_THUMB_WIDTH = 400;
    DEFAULT_THUMB_HEIGHT = 400;

    DEFAULT_HTML_IMAGE_WIDTH = 600;
    DEFAULT_HTML_IMAGE_HEIGHT = 600;

    EXPORT_TEXTSTORYITEM_SEPARATOR = '**********';

  {$endregion}

  type

    IClipboardEnabled = interface
      ['{FDD22AC7-873A-4127-B200-E99DB4F2DEBF}']
      procedure Delete;
      procedure Cut; //does Copy, then Delete
      procedure Copy;
      procedure Paste;
    end;

    IStoreable = interface(IClipboardEnabled)
      ['{A08F7880-FBE5-40C5-B695-FF0F3A18EF3E}']
      //--- Methods ---

      {Add}
      function GetAddFilesFilter: String;
      procedure Add(const Filepath: String); overload;
      procedure Add(const Filepaths: array of String); overload;

      {Load}
      function GetLoadFilesFilter: String;
      function Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM; const CreateNew: Boolean = false): TObject; overload;
      function Load(const Filepath: String; const CreateNew: Boolean = false): TObject; overload;
      function Load(const Clipboard: IFMXExtendedClipboardService; const CreateNew: Boolean = false): TObject; overload;
      function LoadFromString(const Data: String; const CreateNew: Boolean = false): TObject;

      {Save}
      function GetSaveFilesFilter: String;
      function SaveToString: String;
      procedure Save(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload;
      procedure Save(const Filepath: String); overload;
      procedure SaveThumbnail(const Filepath: String; const MaxWidth: Integer = DEFAULT_THUMB_WIDTH; const MaxHeight: Integer = DEFAULT_THUMB_HEIGHT); //TODO: should make constants
      procedure SaveHTML(const Stream: TStream; const ImagesPath: String; const MaxImageWidth: Integer = DEFAULT_HTML_IMAGE_WIDTH; const MaxImageHeight: Integer = DEFAULT_HTML_IMAGE_HEIGHT); overload;
      procedure SaveHTML(const Filepath: String; const MaxImageWidth: Integer = DEFAULT_HTML_IMAGE_WIDTH; const MaxImageHeight: Integer = DEFAULT_HTML_IMAGE_HEIGHT); overload;
    end;

  {$endregion -------------------------------------------------------------------}

implementation

end.
