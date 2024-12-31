//Description: READ-COM TextStoryItem View
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.StoryItems.TextStoryItem;

interface
  {$region 'Used units'}
  uses
    System.SysUtils,
    System.Types,
    System.UITypes,
    System.Classes,
    System.Variants,
    //
    FMX.Types,
    FMX.Graphics,
    FMX.Controls,
    FMX.Forms,
    FMX.Dialogs,
    FMX.StdCtrls,
    FMX.Objects,
    FMX.SVGIconImage,
    FMX.ExtCtrls,
    FMX.Controls.Presentation,
    FMX.ScrollBox,
    FMX.Memo,
    FMX.Memo.Types,
    FMX.Layouts,
    FMX.Clipboard, //for IFMXExtendedClipboardService
    //
    Zoomicon.Media.FMX.MediaDisplay, //for TMediaDisplay (Glyph)
    //
    READCOM.Models, //for IClipboardEnabled, IStoreable, EXT_READCOM
    READCOM.Models.Stories, //for IStoryItem, ITextStoryItem
    READCOM.Views.StoryItems.StoryItem; //for TStoryItem
    {$endregion}

  const
    EXT_TXT = '.txt';
    FILTER_TEXT_TITLE = 'Text (*.txt)';
    FILTER_TEXT_EXTS = '*' + EXT_TXT;
    FILTER_TEXT = FILTER_TEXT_TITLE + '|' + FILTER_TEXT_EXTS;

  resourcestring
    DEFAULT_TEXT = '......' + sLineBreak + '......';

  type

    {$REGION 'TTextStoryItem' ----------------------------------------------------------}

    TTextStoryItem = class(TStoryItem, ITextStoryItem, IStoryItem, IClipboardEnabled, IStoreable)
      Memo: TMemo;
      procedure MemoApplyStyleLookup(Sender: TObject);

    //--- Methods ---

    protected
      FEditable: Boolean;
      FLastMemoSize: TSizeF;

      //procedure Loaded; override;
      procedure MemoChangeTracking(Sender: TObject);

      {Z-Order}
      function GetBackIndex: Integer; override;
      procedure SetMemoZorder;

      {DefaultSize}
      function GetDefaultSize: TSizeF; override;

      {Active}
      procedure SetActive(const Value: Boolean); override;

      {EditMode}
      procedure SetEditMode(const Value: Boolean); override;

      {Text}
      function GetText: String;
      procedure SetText(const Value: String);

      {SelectedText}
      function GetSelectedText: String;

      {Editable}
      function IsEditable: Boolean;
      procedure SetEditable(const Value: Boolean);
      procedure UpdateMemoReadOnly;

      {InputPrompt}
      function GetInputPrompt: String;
      procedure SetInputPrompt(const Value: String);

      {Font}
      function GetFont: TFont;
      procedure SetFont(const Value: TFont);

      {HorzAlign}
      function GetHorzAlign: TTextAlign;
      procedure SetHorzAlign(const Value: TTextAlign);

      {Color}
      function GetForegroundColor: TAlphaColor; override;
      procedure SetForegroundColor(const Value: TAlphaColor); override;

      {Options}
      function GetOptions: IStoryItemOptions; override;

    public
      constructor Create(AOnwer: TComponent); override;
      //procedure SetBounds(X, Y, AWidth, AHeight: Single); override; //note this is called also when the control is moved //TODO: see if we can get first display and subsequent resizes instead
      procedure Painting; override;
      //procedure Resize; override;
      //procedure DoResized; override;

      {$region 'IStoreable'}
      function GetLoadFilesFilter: String; override;
      function Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM; const CreateNew: Boolean = false): TObject; overload; override;
      function LoadTXT(const Stream: TStream): TObject; virtual;
      function Load(const Clipboard: IFMXExtendedClipboardService; const CreateNew: Boolean = false): TObject; overload; override;
      //TODO: add SaveTXT and Save override
      {$endregion}

    //--- Properties ---

    published
      const
        DEFAULT_EDITABLE = false;
        DEFAULT_HORZ_ALIGN = TTextAlign.Center;
        DEFAULT_FOREGROUND_COLOR = TAlphaColorRec.Black;

      property Text: String read GetText write SetText;
      property SelectedText: String read GetSelectedText stored false;
      property Editable: Boolean read IsEditable write SetEditable default DEFAULT_EDITABLE;
      property InputPrompt: String read GetInputPrompt write SetInputPrompt;
      property Font: TFont read GetFont write SetFont; //sets font size, font family (typeface), font style (bold, italic, underline, strikeout)
      property HorzAlign: TTextAlign read GetHorzAlign write SetHorzAlign default DEFAULT_HORZ_ALIGN;
      property TextColor: TAlphaColor read GetForegroundColor write SetForegroundColor stored false; //DEPRECATED, remaped to and storing ForegroundColor instead
      property ForegroundColor: TAlphaColor read GetForegroundColor write SetForegroundColor default DEFAULT_FOREGROUND_COLOR; //redefining default instead of claNull that was in TStoryItem
    end;

    {$ENDREGION ........................................................................}

    {$REGION 'TTextStoryItemFactory' ---------------------------------------------------}

    TTextStoryItemFactory = class(TInterfacedObject, IStoryItemFactory)
      function New(const AOwner: TComponent = nil): IStoryItem;
    end;

    {$ENDREGION ........................................................................}

    procedure Register;

implementation
  {$region 'Used units'}
  uses
    IOUtils, //for TFile
    //
    FMX.Styles.Objects, //for TActiveStyleObject
    //
    //Zoomicon.Helpers.RTL.StreamHelpers, //for TStream.RealAllText
    Zoomicon.Helpers.FMX.Memo.MemoHelpers, //for TMemo.SetFontSizeToFit
    Zoomicon.Introspection.FMX.Debugging, //for Log
    //
    READCOM.Views.Options.TextStoryItemOptions, //for TTextStoryItemOptions
    READCOM.Factories.StoryItemFactory; //for StoryItemFactories, AddStoryItemFileFilter
    {$endregion}

  {$R *.fmx}

  {$REGION 'TTextStoryItem'}

  {$region 'Lifetime management'}

  constructor TTextStoryItem.Create(AOnwer: TComponent);

    procedure InitMemo;
    begin
      if Assigned(Memo) then
        with Memo do
        begin
          Stored := false; //don't store state, should use state from designed .FMX resource
          SetSubComponent(true);
          Align := TAlignLayout.Contents;
          SetMemoZOrder;
          WordWrap := true;
          TextAlign := DEFAULT_HORZ_ALIGN;
          DisableMouseWheel := true;
          EnabledScroll := false;
          ShowScrollBars := false;
          ReadOnly := not DEFAULT_EDITABLE;
          StyledSettings := []; //don't overload any TextSetting with those from Style
          OnChangeTracking := MemoChangeTracking;
        end;
    end;

  begin
    inherited;
    InitMemo;
    Text := DEFAULT_TEXT;
    ForegroundColor := DEFAULT_FOREGROUND_COLOR;
  end;

  (*
  procedure TTextStoryItem.Loaded; //this gets called multiple times when you have and inherited frame, maybe use SetParent instead
  begin
    {//}Log('TTextStoryItem.Loaded %p', [@Self]);
    inherited;
    Memo.SetFontSizeToFit(FLastMemoSize);
  end;
  *)

  (*
  procedure TTextStoryItem.SetBounds(X, Y, AWidth, AHeight: Single); //Note: also gets called when control is moved //TODO: add Logging, this seems to be called too many times (7 - see stack trace of each case and how they differ, also try to group multiple changes)
  begin
    {//}Log('TTextStoryItem.SetBounds %p', [@Self]);
    inherited;
    Memo.SetFontSizeToFit(FLastMemoSize);
  end;
  *)

  procedure TTextStoryItem.Painting;
  begin
    //Log('TTextStoryItem.Painting %p', [@Self]);
    inherited;
    Memo.SetFontSizeToFit(FLastMemoSize);
  end;

  (*
  procedure TTextStoryItem.DoResized; //or "Resize" which is called earlier
  begin
    Log('TTextStoryItem.DoResized');
    inherited;
    try
      Memo.SetFontSizeToFit(FLastMemoSize); //throws exceptions at startup
    Except
      //NOP
    end;
  end;
  *)

  procedure TTextStoryItem.MemoChangeTracking(Sender: TObject);
  begin
    //Log('TTextStoryItem.MemoChangeTracking %p', [@Self]);
    Memo.SetFontSizeToFit(FLastMemoSize);
  end;

  {$endregion}

  {$region 'Z-order'}

  function TTextStoryItem.GetBackIndex: Integer;
  begin
    result := inherited;
    if Assigned(Memo) and Memo.Visible then
      inc(result); //reserve one more place at the bottom for Memo
  end;

  procedure TTextStoryItem.SetMemoZorder;
  begin
    (* //NOT WORKING
    BeginUpdate;
    RemoveObject(Memo);
    InsertObject((inherited GetBackIndex) + 1, Memo);
    EndUpdate;
    *)
    if Assigned(Memo) and Memo.Visible then
      Memo.SendToBack;
  end;

  {$endregion}

  {$region 'Clipboard'}

  function TTextStoryItem.Load(const Clipboard: IFMXExtendedClipboardService; const CreateNew: Boolean = false): TObject;
  begin
    if Clipboard.HasText then
    begin
      var LText := TrimLeft(Clipboard.GetText); //Left-trimming since we may have pasted an indented object from a .readcom file
      if not LText.StartsWith('object ') then //if Delphi serialization format (its text-based form) then don't paste as text (let ancestor TStoryItem handle it)
      begin
        Text := LText; //else replace current text
        Exit(Self);
      end;
    end;

    result := inherited; //fallback to ancestor implementation
  end;

  {$endregion}

  {$region 'Helpers'}

  procedure TTextStoryItem.UpdateMemoReadOnly;
  begin
    Memo.ReadOnly := not (IsEditable or IsEditMode);
    Memo.HitTest := not Memo.ReadOnly; //TODO: should maybe have a mode that swiches between Editable, Selectable and Read-Only/Inactive (so that it's draggable)
    //Memo.Enabled := not Memo.ReadOnly; //this greys out the item, don't use
  end;

  {$endregion}

  {$REGION 'PROPERTIES'}

  {$region 'DefaultSize'}

  function TTextStoryItem.GetDefaultSize: TSizeF;
  begin
    Result := TSizeF.Create(50, 30);
  end;

  {$endregion}

  {$region 'Active'}

  procedure TTextStoryItem.SetActive(const Value: Boolean);
  begin
    inherited;

    if Value then
      Memo.SetFocus
    else
      Memo.ResetFocus;
  end;

  {$endregion}

  {$region 'EditMode'}

  procedure TTextStoryItem.SetEditMode(const Value: Boolean);
  begin
    inherited;
    UpdateMemoReadOnly;
  end;

  {$endregion}

  {$region 'Text'}

  function TTextStoryItem.GetText: String;
  begin
    result := Memo.Text;
  end;

  procedure TTextStoryItem.SetText(const Value: String);
  begin
    if Assigned(Memo) then
      Memo.Text := Value;
  end;

  {$endregion}

  {$region 'SelectedText'}

  function TTextStoryItem.GetSelectedText: String;
  begin
    Memo.Model.SelectedText;
  end;

  {$endregion}

  {$region 'Editable'}

  function TTextStoryItem.IsEditable: Boolean;
  begin
    result := FEditable;
  end;

  procedure TTextStoryItem.SetEditable(const Value: Boolean);
  begin
    FEditable := Value;
    UpdateMemoReadOnly;
  end;

  {$endregion}

  {$region 'InputPrompt'}

  function TTextStoryItem.GetInputPrompt: String;
  begin
    //TODO
  end;

  procedure TTextStoryItem.SetInputPrompt(const Value: String);
  begin
    //TODO
  end;

  {$endregion}

  {$region 'Font'}

  function TTextStoryItem.GetFont: TFont;
  begin
    result := Memo.Font;
  end;

  procedure TTextStoryItem.SetFont(const Value: TFont);
  begin
    Memo.Font := Value;
  end;

  {$endregion}

  {$region 'HorzAlign'}

  function TTextStoryItem.GetHorzAlign: TTextAlign;
  begin
    result := Memo.TextSettings.HorzAlign;
  end;

  procedure TTextStoryItem.SetHorzAlign(const Value: TTextAlign);
  begin
    Memo.TextSettings.HorzAlign := Value;
  end;

  {$endregion}

  {$region 'ForegroundColor'}

  function TTextStoryItem.GetForegroundColor: TAlphaColor;
  begin
    result := Memo.FontColor;
  end;

  procedure TTextStoryItem.SetForegroundColor(const Value: TAlphaColor);
  begin
    if (Value = TAlphaColorRec.Null) then //never apply the null color as foreground, keep the default TextStoryItem ForegroundColor instead
      Memo.FontColor := DEFAULT_FOREGROUND_COLOR
    else
      Memo.FontColor := Value;
  end;

  {$endregion}

  {$region 'Options'}

  function TTextStoryItem.GetOptions: IStoryItemOptions;
  begin
    if not Assigned(FOptions) then
      begin
      FOptions := TTextStoryItemOptions.Create(nil); //don't set storyitem as owner, seems to always store it (irrespective of "Stored := false")
      FOptions.StoryItem := Self;
      end;

    result := FOptions;
  end;

  {$endregion}

  {$ENDREGION PROPERTIES}

  {$region 'IStoreable'}

  function TTextStoryItem.GetLoadFilesFilter: String;
  begin
    result := FILTER_TEXT + '|' + inherited;
  end;

  function TTextStoryItem.Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM; const CreateNew: Boolean = false): TObject;
  begin
    if (ContentFormat = EXT_TXT) then //load EXT_TXT
      result := LoadTXT(Stream)
    else
      result := inherited; //load EXT_TXT
  end;

  function TTextStoryItem.LoadTXT(const Stream: TStream): TObject;
  begin
    //Text := Stream.ReadAllText; //TODO: doesn't seem to work correctly

    var s := TStringList.Create(#0, #13);
    try
      s.LoadFromStream(Stream);
      Text := s.DelimitedText;

      Size.Size := TSizeF.Create(50, 30); //TODO: judge based on text volume?
    finally
      FreeAndNil(s);
    end;

    result := Self;
  end;

  procedure TTextStoryItem.MemoApplyStyleLookup(Sender: TObject);
  begin
    inherited;

    var Obj := Memo.FindStyleResource('background');
    if Assigned(Obj) And (Obj is TActiveStyleObject) Then
       TActiveStyleObject(Obj).Source := Nil;
  end;

  {$endregion}

  {$ENDREGION}

  {$REGION 'TTextStoryItemFactory'}

  function TTextStoryItemFactory.New(const AOwner: TComponent = nil): IStoryItem;
  begin
    result := TTextStoryItem.Create(AOwner);
  end;

  {$ENDREGION}

  {$region 'Registration'}

  procedure RegisterSerializationClasses;
  begin
    RegisterFmxClasses([TTextStoryItem]);
  end;

  procedure Register;
  begin
    GroupDescendentsWith(TTextStoryItem, TControl);
    RegisterSerializationClasses;
    RegisterComponents('Zoomicon', [TTextStoryItem]);
  end;

  {$endregion}

initialization
  StoryItemFactories.Add([EXT_TXT], TTextStoryItemFactory.Create);

  AddStoryItemFileFilter(FILTER_TEXT_TITLE, FILTER_TEXT_EXTS);

  RegisterSerializationClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
