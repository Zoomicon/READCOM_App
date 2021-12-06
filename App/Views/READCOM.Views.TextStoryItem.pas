//Description: READ-COM TextStoryItem View
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.TextStoryItem;

interface

uses
  READCOM.App.Models,
  READCOM.Views.StoryItem, //for TStoryItem
  READCOM.Views.ImageStoryItem, //for TImageStoryItem
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.SVGIconImage,
  FMX.ExtCtrls, FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo, FMX.Memo.Types;

const
  EXT_TXT = '.txt';
  FILTER_TXT = 'Text files (*.txt)|*.txt';

type
  TTextStoryItem = class(TStoryItem, ITextStoryItem, IStoryItem, IStoreable)
    Memo: TMemo;
    procedure MemoApplyStyleLookup(Sender: TObject);

  //--- Methods ---

  protected
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

  public
    constructor Create(AOnwer: TComponent); override;

    {$region 'IStoreable'}
    function GetLoadFilesFilter: String; override;
    procedure Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM); overload; override;
    procedure LoadTXT(const Stream: TStream); virtual;
    {$endregion}

  //--- Properties ---

  published
    property Text: String read GetText write SetText; //default ''
    property Editable: Boolean read IsEditable write SetEditable; //default false
    property InputPrompt: String read GetInputPrompt write SetInputPrompt;
    property Font: TFont read GetFont write SetFont; //sets font size, font family (typeface), font style (bold, italic, underline, strikeout)
    property TextColor: TAlphaColor read GetTextColor write SetTextColor;
  end;

  procedure Register;

implementation
  uses
    IOUtils, //for TFile
    FMX.Styles.Objects, //for TActiveStyleObject
    Zoomicon.Text; //for ReadAllText

{$R *.fmx}

{ TTextStoryItem }

constructor TTextStoryItem.Create(AOnwer: TComponent);
begin
  inherited;
  memo.SetSubComponent(true);
  memo.Stored := false; //don't store state, should use state from designed .FMX resource
end;

{$REGION '--- PROPERTIES ---'}

{$region 'Text'}

function TTextStoryItem.GetText: String;
begin
  result := Memo.Text;
end;

procedure TTextStoryItem.SetText(const Value: String);
begin
  Memo.Text := Value;
end;

{$endregion}

{$region 'Editable'}

function TTextStoryItem.IsEditable: Boolean;
begin
  result := not Memo.ReadOnly;
end;

procedure TTextStoryItem.SetEditable(const Value: Boolean);
begin
  Memo.ReadOnly := not Value;
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

{$region 'TextColor'}

function TTextStoryItem.GetTextColor: TAlphaColor;
begin
  result := Memo.FontColor;
end;

procedure TTextStoryItem.SetTextColor(const Value: TAlphaColor);
begin
  Memo.FontColor := Value;
end;

{$endregion}

{$ENDREGION}

{$region 'IStoreable'}

function TTextStoryItem.GetLoadFilesFilter: String;
begin
  result := FILTER_TXT;
end;

procedure TTextStoryItem.Load(const Stream: TStream; const ContentFormat: String = EXT_READCOM);
begin
  if (ContentFormat = EXT_TXT) then //load EXT_TXT
    LoadTXT(Stream)
  else
    inherited; //load EXT_TXT
end;

procedure TTextStoryItem.LoadTXT(const Stream: TStream);
begin
  //Text := ReadAllText(Stream);
  var s := TStringList.Create(#0, #13);
  s.LoadFromStream(Stream);
  Text := s.DelimitedText;
  FreeAndNil(s);
end;

procedure TTextStoryItem.MemoApplyStyleLookup(Sender: TObject);
begin
  inherited;

  var Obj := Memo.FindStyleResource('background');
  if Assigned(Obj) And (Obj is TActiveStyleObject) Then
     TActiveStyleObject(Obj).Source := Nil;
end;

{$endregion}

procedure RegisterClasses;
begin
  RegisterFmxClasses([TTextStoryItem]); //register for persistence
end;

procedure Register;
begin
  GroupDescendentsWith(TTextStoryItem, TControl);
  RegisterClasses;
  RegisterComponents('Zoomicon', [TTextStoryItem]);
end;

initialization
  RegisterClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
