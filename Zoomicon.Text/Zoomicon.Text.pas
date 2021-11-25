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

procedure TMemoExt.ApplyStyle;
begin
   inherited;

   var Obj := FindStyleResource('background');
   if Assigned(Obj) then
   begin
      TControl(Obj).Margins.Rect := RectF(-1, -1, -1, -1);
      var Rectangle1              := TRectangle.Create(Obj);
      Obj.AddObject(Rectangle1);
      Rectangle1.Align        := TAlignLayout.Client;
      Rectangle1.Fill.Color   := claLightslategrey;
      Rectangle1.Stroke.Color := claNull;
      Rectangle1.HitTest      := False;
      Rectangle1.SendToBack;
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
