//Description: READ-COM TextStoryItem Options
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.Options.TextStoryItemOptions;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Actions, FMX.ActnList,
  System.ImageList, FMX.ImgList, FMX.SVGIconImageList, FMX.Edit,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.StdActns,
  FMX.MediaLibrary.Actions,
  FMX.Objects,
  //
  READCOM.Views.Options.StoryItemOptions,
  READCOM.Models.Stories;

type
  TTextStoryItemOptions = class(TStoryItemOptions, ITextStoryItemOptions, IStoryItemOptions)
    btnToggleEditable: TSpeedButton;
    LayoutTextStoryItemButtons: TFlowLayout;
    LayoutTextStoryItemBreak: TFlowLayoutBreak;
    btnToggleAlignLeft: TSpeedButton;
    btnToggleAlignCenter: TSpeedButton;
    btnToggleAlignRight: TSpeedButton;
    LayoutHorzAlign: TFlowLayout;
    LayoutTextStyle: TFlowLayout;
    btnToggleBold: TSpeedButton;
    btnToggleItalic: TSpeedButton;
    btnToggleUnderline: TSpeedButton;
    btnToggleStrikeout: TSpeedButton;
    procedure actionToggleEditableExecute(Sender: TObject);
    procedure btnToggleAlignClick(Sender: TObject);
    procedure btnToggleBoldClick(Sender: TObject);
    procedure btnToggleStrikeoutClick(Sender: TObject);
    procedure btnToggleItalicClick(Sender: TObject);
    procedure btnToggleUnderlineClick(Sender: TObject);

  protected
    {StoryItem}
    procedure SetStoryItem(const Value: IStoryItem); override;

    {TextStoryItem}
    function GetTextStoryItem: ITextStoryItem; virtual;
    procedure SetTextStoryItem(const Value: ITextStoryItem); virtual;

    {HorzAlign}
    procedure SetButtonsFromHorzAlign;

    {FontStyle}
    procedure SetFontStyleFromButton(const StyleChange: TFontStyle; const Button: TSpeedButton);
    procedure SetButtonsFromFontStyle;

  published
    property TextStoryItem: ITextStoryItem read GetTextStoryItem write SetTextStoryItem stored false;
  end;

implementation

{$R *.fmx}

{$REGION 'PROPERTIES'}

{$region 'StoryItem'}

procedure TTextStoryItemOptions.SetStoryItem(const Value: IStoryItem);
begin
  inherited;

  var LTextStoryItem: ITextStoryItem;
  if not Supports(Value, ITextStoryItem, LTextStoryItem) then
    raise EIntfCastError.Create('Expected ITextStoryItem');

  SetTextStoryItem(LTextStoryItem);
end;

{$endregion}

{$region 'TextStoryItem'}

function TTextStoryItemOptions.GetTextStoryItem: ITextStoryItem;
begin
  Supports(FStoryItem, ITextStoryItem, result);
end;

procedure TTextStoryItemOptions.SetTextStoryItem(const Value: ITextStoryItem);
begin
  inherited SetStoryItem(Value); //don't call overriden SetStoryItem, would do infinite loop

  if Assigned(Value) then
  begin
    btnToggleEditable.IsPressed := Value.Editable; //Toggle Editable button
    SetButtonsFromHorzAlign;                       //Toggle HorzAlign buttons
    SetButtonsFromFontStyle;                       //Toggle FontStyle buttons
  end;
end;

{$endregion}

{$ENDREGION PROPERTIES}

{$REGION 'EVENTS'}

{$region 'Editable'}

procedure TTextStoryItemOptions.actionToggleEditableExecute(Sender: TObject);
begin
  inherited;

  var LTextStoryItem := TextStoryItem;
  if Assigned(LTextStoryItem) then
    LTextStoryItem.Editable := btnToggleEditable.IsPressed;
end;

{$endregion}

{$region 'TextAlign'}

procedure TTextStoryItemOptions.btnToggleAlignClick(Sender: TObject);
begin
  inherited;

  var LTextStoryItem := TextStoryItem;
  if not Assigned(LTextStoryItem) then exit;

  var LHorzAlign := TTextAlign.Center;

  if btnToggleAlignLeft.IsPressed then
    LHorzAlign := TTextAlign.Leading
  else if btnToggleAlignCenter.IsPressed then
    LHorzAlign := TTextAlign.Center
  else if btnToggleAlignRight.IsPressed then
    LHorzAlign := TTextAlign.Trailing;

  LTextStoryItem.HorzAlign := LHorzAlign;
end;

procedure TTextStoryItemOptions.SetButtonsFromHorzAlign;
begin
  var LTextStoryItem := TextStoryItem;
  if not Assigned(LTextStoryItem) then exit;

  var LHorzAlign := LTextStoryItem.HorzAlign;

  if LHorzAlign = TTextAlign.Leading then
    btnToggleAlignLeft.IsPressed := true

  else if LHorzAlign = TTextAlign.Center then
    btnToggleAlignCenter.IsPressed := true

  else if LHorzAlign = TTextAlign.Trailing then
    btnToggleAlignRight.IsPressed := true;
end;

{$endregion}

{$region 'TextStyle'}

procedure TTextStoryItemOptions.SetFontStyleFromButton(const StyleChange: TFontStyle; const Button: TSpeedButton);
begin
  var LTextStoryItem := TextStoryItem;
  if not Assigned(LTextStoryItem) then exit;

  with LTextStoryItem.Font do
    if Button.IsPressed then
      Style := Style + [StyleChange]
    else
      Style := Style - [StyleChange];
end;

procedure TTextStoryItemOptions.SetButtonsFromFontStyle;
begin
  var LTextStoryItem := TextStoryItem;
  if not Assigned(LTextStoryItem) then exit;

  var LFontStyle := LTextStoryItem.Font.Style;

  btnToggleBold.IsPressed := (TFontStyle.fsBold in LFontStyle);
  btnToggleItalic.IsPressed := (TFontStyle.fsItalic in LFontStyle);
  btnToggleUnderline.IsPressed := (TFontStyle.fsUnderline in LFontStyle);
  btnToggleStrikeout.IsPressed := (TFontStyle.fsStrikeout in LFontStyle);
end;

procedure TTextStoryItemOptions.btnToggleBoldClick(Sender: TObject);
begin
  inherited;

  SetFontStyleFromButton(TFontStyle.fsBold, btnToggleBold);
end;

procedure TTextStoryItemOptions.btnToggleItalicClick(Sender: TObject);
begin
  inherited;

  SetFontStyleFromButton(TFontStyle.fsItalic, btnToggleItalic);
end;

procedure TTextStoryItemOptions.btnToggleUnderlineClick(Sender: TObject);
begin
  inherited;

  SetFontStyleFromButton(TFontStyle.fsUnderline, btnToggleUnderline);
end;

procedure TTextStoryItemOptions.btnToggleStrikeoutClick(Sender: TObject);
begin
  inherited;

  SetFontStyleFromButton(TFontStyle.fsStrikeout, btnToggleStrikeout);
end;

{$endregion}

{$ENDREGION EVENTS}

end.
