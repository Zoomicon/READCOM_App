unit Zoomicon.Helpers.RTL.StreamHelpers;

interface
  uses System.Classes; //for TStream, TComponent

  type

    TStreamErrorHelper = class helper for TStream
      function ReadComponent(const Instance: TComponent; const ReaderError: TReaderError): TComponent;
    end;

implementation

{$REGION 'TStreamErrorHelper'}

function TStreamErrorHelper.ReadComponent(const Instance: TComponent; const ReaderError: TReaderError): TComponent;
begin //based on TStream.ReadComponent (of Delphi 11 RTL, as on 2022-01-24)
  var reader := TReader.Create(Self, 4096);
  reader.OnError := ReaderError; //the error handler can ignore specific not found properties
  try
    result := Reader.ReadRootComponent(Instance); //if we pass nil returns a new instance
  finally
    Reader.Free;
  end;
end;

{$ENDREGION}

end.
