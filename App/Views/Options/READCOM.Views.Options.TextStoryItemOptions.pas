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
  READCOM.Views.Options.StoryItemOptions,
  READCOM.App.Models;

type
  TTextStoryItemOptions = class(TStoryItemOptions, ITextStoryItemOptions, IStoryItemOptions)
    btnToggleEditable: TSpeedButton;
    procedure actionToggleEditableExecute(Sender: TObject);

  protected
    {StoryItem}
    procedure SetStoryItem(const Value: IStoryItem); override;

    {TextStoryItem}
    function GetTextStoryItem: ITextStoryItem; virtual;
    procedure SetTextStoryItem(const Value: ITextStoryItem); virtual;

  public
    constructor Create(AOwner: TComponent); override;

  published
    property TextStoryItem: ITextStoryItem read GetTextStoryItem write SetTextStoryItem stored false;
  end;

implementation

{$R *.fmx}

{$REGION 'LIFETIME MANAGEMENT'}

constructor TTextStoryItemOptions.Create(AOwner: TComponent);
begin
  inherited;
  var LTextStoryItem := TextStoryItem;
  if Assigned(LTextStoryItem) then
    btnToggleEditable.IsPressed := LTextStoryItem.Editable;
end;

{$ENDREGION}

{$REGION 'PROPERTIES'}

{$region 'StoryItem'}

procedure TTextStoryItemOptions.SetStoryItem(const Value: IStoryItem);
begin
  inherited;

  var LTextStoryItem := TextStoryItem;
  if Assigned(LTextStoryItem) then
    btnToggleEditable.IsPressed := LTextStoryItem.Editable;
end;

{$endregion}

{$region 'TextStoryItem'}

function TTextStoryItemOptions.GetTextStoryItem: ITextStoryItem;
begin
  Supports(FStoryItem, ITextStoryItem, result);
end;

procedure TTextStoryItemOptions.SetTextStoryItem(const Value: ITextStoryItem);
begin
  Supports(Value, IStoryItem, FStoryItem); //interface casting also supports interface implementations using aggregated or nested objects
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

{$ENDREGION EVENTS}

end.
