//Description: Text-related helper classes and methods
//Author: George Birbilis (http://zoomicon.com)

unit Zoomicon.Text;

interface
  uses
    System.Classes, //for TStream
    System.SysUtils, //for TBytes
    FMX.Memo; //for TMemo

type

  TMemoExt = class(TMemo) //TODO: maybe move to a Zoomicon.Helpers.FMX.Memo unit under Zoomicon.Helpers.FMX package
    procedure ApplyStyle; override;
  end;

procedure SetMemoFontSizeToFit(const AMemo: TMemo);

function ReadAllBytes(const Stream: TStream): TBytes;
function ReadAllText(const Stream: TStream; const DefaultEncoding: TEncoding = nil; const ForceDefaultEncoding: Boolean = false): string;

function SafeTextToShortCut(Text: string): Integer; //the Delphi 11.1 TextToShortcut returns -1 when platform doesn't support the check (e.g. on Android) instead of 0 (which is what TAction.Shortcut expects for no-shortcut)

implementation
  uses
    RTLConsts, //for SFileTooLong
    System.Types, //for RectF
    System.UIConsts, //for claXX
    FMX.ActnList, //for TextToShortcut
    FMX.Controls, //for TControl
    FMX.Objects, //for TRectangle
    FMX.Types; //for TAlignLayout

procedure TMemoExt.ApplyStyle; //Make Memo transparent
begin
   inherited;

   var Obj := FindStyleResource('background');
   if Assigned(Obj) then
   begin
      TControl(Obj).Margins.Rect := RectF(-1, -1, -1, -1);
      var LRect := TRectangle.Create(Obj);
      Obj.AddObject(LRect);
      with LRect do
      begin
        Align := TAlignLayout.Client;
        Fill.Color := claLightslategrey;
        Stroke.Color := claNull;
        HitTest := False;
        SendToBack;
      end;
   end;
end;

procedure SetMemoFontSizeToFit(const AMemo: TMemo);
const
  Offset = 4; //The diference between ContentBounds and ContentLayout //TODO: info coming from https://stackoverflow.com/a/21993017/903783 - need to verify
begin
  {$IF NOT DEFINED(ANDROID)} //TODO: Seems changing Font.Size doesn't affect ContentBounds in Android implementation so we get an infinite loop (Heigh is never reached)
  with AMemo do
  begin
    //Font.Size := 12; //Don't set initial font size, use the current one (which may be from a previous calculation - this speeds up when resizing and also fixes glitches when loading saved state if recalculation isn't done for some reason after loading)

    if (ContentBounds.Height <> 0) then //must check, else first while will loop for ever since ContentBounds.Height is 0 on load
    begin
      //make font bigger
      while (ContentBounds.Height + Offset < Height) do //using WordWrap, not checking for Width
        with Font do
          Size := Size + 1;

      //make font smaller
      while (ContentBounds.Height + Offset > Height) do //using WordWrap, not checking for Width
        with Font do
          Size := Size - 1;
    end;
  end;
  {$ENDIF}
end;

function ReadAllBytes(const Stream: TStream): TBytes;
begin
  var LFileSize := Stream.Size;
  {$IFDEF CPU32BITS}
  if LFileSize > MaxInt then
    raise EInOutError.CreateRes(@SFileTooLong);
  {$ENDIF}
  SetLength(Result, LFileSize);
  Stream.ReadBuffer(result, Length(result));
end;

function ReadAllText(const Stream: TStream; const DefaultEncoding: TEncoding = nil; const ForceDefaultEncoding: Boolean = false): string;
var FoundEncoding: TEncoding;
begin
  if ForceDefaultEncoding then
    FoundEncoding := DefaultEncoding;
  var Buff := ReadAllBytes(Stream);
  var BOMLength := TEncoding.GetBufferEncoding(Buff, FoundEncoding, DefaultEncoding);
  result := FoundEncoding.GetString(Buff, BOMLength, Length(Buff) - BOMLength);
end;

function SafeTextToShortCut(Text: string): Integer; //TODO: maybe move to a Zoomicon.Helpers.FMX.ActnList or a Zoomicon.Helpers.FMX.Menus unit at Zoomicon.Helpers.FMX package
begin
  result := TextToShortCut(Text);
  if (result < 0) then //the Delphi 11.1 TextToShortcut returns -1 when platform doesn't support the check (e.g. on Android) instead of 0 (which is what TAction.Shortcut expects for no-shortcut)
    result := 0;
end;

end.
