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
  READCOM.Views.Options.StoryItemOptions,
  READCOM.App.Models;

type
  TTextStoryItemOptions = class(TStoryItemOptions, ITextStoryItemOptions, IStoryItemOptions)
    btnToggleEditable: TSpeedButton;
    LayoutTextStoryItemButtons: TFlowLayout;
    LayoutTextStoryItemBreak: TFlowLayoutBreak;
    btnToggleAlignLeft: TSpeedButton;
    btnToggleAlignCenter: TSpeedButton;
    btnToggleAlignRight: TSpeedButton;
    LayoutHorzAlign: TFlowLayout;
    procedure actionToggleEditableExecute(Sender: TObject);
    procedure btnToggleAlignClick(Sender: TObject);

  protected
    {StoryItem}
    procedure SetStoryItem(const Value: IStoryItem); override;

    {TextStoryItem}
    function GetTextStoryItem: ITextStoryItem; virtual;
    procedure SetTextStoryItem(const Value: ITextStoryItem); virtual;

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
    //Toggle Editable button
    btnToggleEditable.IsPressed := Value.Editable;

    //Toggle HorzAlign buttons
    var LHorzAlign := Value.HorzAlign;
    if LHorzAlign = TTextAlign.Leading then
      btnToggleAlignLeft.IsPressed := true
    else if LHorzAlign = TTextAlign.Center then
      btnToggleAlignCenter.IsPressed := true
    else if LHorzAlign = TTextAlign.Trailing then
      btnToggleAlignRight.IsPressed := true;
  end;
end;

{$endregion}

{$ENDREGION PROPERTIES}

{$REGION 'EVENTS'}

procedure TTextStoryItemOptions.actionToggleEditableExecute(Sender: TObject);
begin
  inherited;

  var LTextStoryItem := TextStoryItem;
  if Assigned(LTextStoryItem) then
    LTextStoryItem.Editable := btnToggleEditable.IsPressed;
end;

procedure TTextStoryItemOptions.btnToggleAlignClick(Sender: TObject);
begin
  inherited;

  var LTextStoryItem := TextStoryItem;
  if Assigned(LTextStoryItem) then
  begin
    var LHorzAlign := TTextAlign.Center;

    if btnToggleAlignLeft.IsPressed then
      LHorzAlign := TTextAlign.Leading
    else if btnToggleAlignCenter.IsPressed then
      LHorzAlign := TTextAlign.Center
    else if btnToggleAlignRight.IsPressed then
      LHorzAlign := TTextAlign.Trailing;

    LTextStoryItem.HorzAlign := LHorzAlign;
  end;

end;

{$ENDREGION EVENTS}

end.
