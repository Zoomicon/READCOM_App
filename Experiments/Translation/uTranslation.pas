unit uTranslation;

//TODO: old, probably obsolete code - use code from Azure-Cognitive-Services library from "3rdPartyLibs" folder instead

interface

function Translate(const AAppID: string; const AText: string; const InLanguage: string='en'; const OutLanguage: string='el-GR'): string;
function Translate2(const AAppID: string; const AText: string; const InLanguage: string='en'; const OutLanguage: string='el-GR'): string;

implementation
  uses
    System.Classes, //for TStringStream
    System.SysUtils, //for Format
    System.Variants, //for EmptyParam
    MsXML, //for IXMLHttpRequest
    IdHTTP; //for TIdHTTP

//from https://chowdera.com/2021/04/20210429235058371J.html
function Translate(const AAppID: string; const AText: string; const InLanguage: string='en'; const OutLanguage: string='el-GR'): string;
const
  BaseUrl = 'http://api.microsofttranslator.com/V2/http.svc/Translate?appId=%s&text=%s&from=%s&to=%s';
var
  Url: string;
  req: IXMLHTTPRequest;
begin
  Url := Format(BaseUrl, [AAppID, AText, InLanguage, OutLanguage]);
  req := CoXMLHTTP.Create;
  req.open('Get', Url, False, EmptyParam, EmptyParam);
  req.send(EmptyParam);
  Result := req.responseText;
  Result := Copy(Result, 68+1, Length(Result)-68-9); // Remove the front and back labels
end;

//from https://chowdera.com/2021/04/20210429235058371J.html
function Translate2(const AAppID: string; const AText: string; const InLanguage: string='en'; const OutLanguage: string='el-GR'): string;
const
  BaseUrl = 'http://api.microsofttranslator.com/V2/http.svc/Translate?appId=%s&text=%s&from=%s&to=%s';
var
  Url: String;
  stream: TStringStream;
  idHttpObj: TIdHTTP;
begin
  stream := TStringStream.Create;
  idHttpObj := TIdHTTP.Create(nil);
  Url := Format(BaseUrl, [AAppID, Trim(AText), InLanguage, OutLanguage]);
  idHttpObj.Get(Url, stream);
  Result := stream.DataString;
  Result := Copy(Result, 68+1, Length(Result)-68-9); // Remove the front and back labels
  idHttpObj.Free;
  stream.Free;
end;

end.
