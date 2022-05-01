unit Zoomicon.Text;

interface
  uses
    System.Classes, //for TStream
    System.SysUtils, //for TBytes
    FMX.Memo; //for TMemo

type

  TMemoExt = class(TMemo)
    procedure ApplyStyle; override;
  end;

procedure SetMemoFontSizeToFit(const AMemo: TMemo);

function ReadAllBytes(const Stream: TStream): TBytes;
function ReadAllText(const Stream: TStream; const DefaultEncoding: TEncoding = nil; const ForceDefaultEncoding: Boolean = false): string;

implementation
  uses
    RTLConsts, //for SFileTooLong
    System.Types, //for RectF
    System.UIConsts, //for claXX
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
  with AMemo do
  begin
    //set default font size
    Font.Size := 12;

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

end.
