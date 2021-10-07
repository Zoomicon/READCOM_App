unit READCOM.Messages.Classes;

interface
  uses READCOM.Messages.Models;

  type

    TMessageSingleValue<T> = class(TInterfacedObject, IMessageSingleValue<T>)
      protected
        FValue: T;
      public
        constructor Create(const AValue: T);
        function GetValue: T;
        procedure SetValue(const AValue: T);
    end;

    TMessageMenu = class(TInterfacedObject, IMessageMenu);
    TMessageAdd = class(TInterfacedObject, IMessageAdd);
    TMessageEditModeChange = class(TMessageSingleValue<Boolean>, IMessageEditModeChange);

implementation

{ TMessageSingleValue<T> }

constructor TMessageSingleValue<T>.Create(const AValue: T);
begin
  FValue := AValue;
end;

function TMessageSingleValue<T>.GetValue: T;
begin
  result := FValue;
end;

procedure TMessageSingleValue<T>.SetValue(const AValue: T);
begin
  FValue := AValue;
end;

end.
