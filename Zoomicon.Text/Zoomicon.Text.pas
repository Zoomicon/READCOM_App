//Description: Text-related helper classes and methods
//Author: George Birbilis (http://zoomicon.com)

unit Zoomicon.Text;

interface
  uses
    System.Classes, //for TStream
    System.SysUtils, //for TBytes
    System.Types, //for RectF, TSizeF
    FMX.Memo; //for TMemo

type

  TMemoExt = class(TMemo) //TODO: maybe move to a Zoomicon.Helpers.FMX.Memo unit under Zoomicon.Helpers.FMX package
    procedure ApplyStyle; override;
  end;

procedure SetMemoFontSizeToFit(const AMemo: TMemo; var LastFontFitSize: TSizeF);

function ReadAllBytes(const Stream: TStream): TBytes;
function ReadAllText(const Stream: TStream; const DefaultEncoding: TEncoding = nil; const ForceDefaultEncoding: Boolean = false): string;

function SafeTextToShortCut(const Text: string): Integer; //the Delphi 11.1 TextToShortcut returns -1 when platform doesn't support the check (e.g. on Android) instead of 0 (which is what TAction.Shortcut expects for no-shortcut)

function GetLines(const AList: TStrings; const AStart, AEnd: Integer; const AddTrailingLineBreak :Boolean = false): string;

implementation
  uses
    RTLConsts, //for SFileTooLong
    System.Math, //for Min
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

procedure SetMemoFontSizeToFit(const AMemo: TMemo; var LastFontFitSize: TSizeF); //TODO: add logging
//const
  //Offset = 0; //The diference between ContentBounds and ContentLayout //TODO: info coming from https://stackoverflow.com/a/21993017/903783 - need to verify
begin
  //AMemo.AutoCalculateContentSize := true; //don't use

  {$IF DEFINED(ANDROID) OR DEFINED(IOS)}
  AMemo.UpdateContentSize; //Recalculates content bounds of a scroll box. Does not calculate content bounds if AutoCalculateContentSize is False or if the state of the scroll box is csLoading or csDestroying
  {$ENDIF}

  var Offset := 10 {* AMemo.AbsoluteScale.Y}; //TODO: not very good
  var LHeight := AMemo.Height;
  if (LHeight = 0) {or (AMemo.Size.Size = LastFontFitSize)} then
    exit; //don't calculate font autofit based on probably not yet available height information, nor when AMemo.Height didn't change from last calculation

  //AMemo.Font.Size := 12; //Don't set initial font size, use the current one (which may be from a previous calculation - this speeds up when resizing and also fixes glitches when loading saved state if recalculation isn't done for some reason after loading)

  var LContentBoundsHeight := AMemo.ContentBounds.Height;
  if (LContentBoundsHeight = 0) then //must check, else first while will loop for ever since ContentBounds.Height is 0 on load
    exit;

  var LFontSize := AMemo.Font.Size;
  LastFontFitSize := AMemo.Size.Size;

  //make font bigger
  while (LContentBoundsHeight + Offset < LHeight) do //using WordWrap, not checking for Width
  begin
    LFontSize := LFontSize + 1;
    AMemo.Font.Size := LFontSize;

    var LContentBoundsNewHeight := AMemo.ContentBounds.Height;
    if LContentBoundsNewHeight = LContentBoundsHeight then
    begin
      AMemo.Font.Size := LFontSize - 1; //undo font size increase since it had no effect
      exit; //TODO: See why changing Font.Size doesn't affect ContentBounds in Android implementation so we get an infinite loop (Height is never reached)
    end;
    LContentBoundsHeight := LContentBoundsNewHeight;
  end;

  //make font smaller
  while (LContentBoundsHeight + Offset > LHeight) do //using WordWrap, not checking for Width
  begin
    LFontSize := LFontSize - 1;
    AMemo.Font.Size := LFontSize;

    var LContentBoundsNewHeight := AMemo.ContentBounds.Height;
    if LContentBoundsNewHeight = LContentBoundsHeight then
    begin
      AMemo.Font.Size := LFontSize + 1; //undo font size decrease since it had no effect
      exit; //TODO: See why changing Font.Size doesn't affect ContentBounds in Android implementation so we get an infinite loop (Height is never reached)
    end;
    LContentBoundsHeight := LContentBoundsNewHeight;
  end;

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

function SafeTextToShortCut(const Text: string): Integer; //TODO: maybe move to a Zoomicon.Helpers.FMX.ActnList or a Zoomicon.Helpers.FMX.Menus unit at Zoomicon.Helpers.FMX package
begin
  result := TextToShortCut(Text);
  if (result < 0) then //the Delphi 11.1 TextToShortcut returns -1 when platform doesn't support the check (e.g. on Android) instead of 0 (which is what TAction.Shortcut expects for no-shortcut)
    result := 0;
end;

function GetLines(const AList: TStrings; const AStart, AEnd: Integer; const AddTrailingLineBreak :Boolean = false): string; //see https://stackoverflow.com/a/11028950
begin
  Result := '';
  if (AStart < AEnd) then
    Exit;

  var LLast := Min(AEnd, AList.Count - 1);
  for var I := AStart to  (LLast - 1) do
    Result := Result + AList[I] + sLineBreak;

  Result := Result + AList[LLast]; //add the last item separately to avoid the extra line-break

  if AddTrailingLineBreak then
    Result := Result + sLineBreak;
end;

end.
