unit Zoomicon.Generics.Registries;

interface
  uses
    System.Generics.Collections; //for TDictionary

  type
    IRegistry<TKey, TValue> = interface //TValue is a generic symbol here, not related to System.Rtti.TValue
      ['{3BB3277F-1E81-438D-A2BC-9BE63C63AF4B}']
      procedure Add(const Key: TKey; const Value: TValue); overload;
      procedure Add(const Keys: array of TKey; const Value: TValue); overload;
      function Get(const Key: TKey): TValue;
    end;

    TRegistry<TKey, TValue> = class(TInterfacedObject, IRegistry<TKey, TValue>)
      protected
        FDictionary: TDictionary<TKey, TValue>;
        constructor Create(const SkipDictionaryCreation: Boolean = false); overload; virtual;
      public
        constructor Create(const ACapacity: Integer = 0); overload; virtual;
        destructor Destroy; override;
        procedure Add(const Key: TKey; const Value: TValue); overload; virtual;
        procedure Add(const Keys: array of TKey; const Value: TValue); overload; virtual;
        function Get(const Key: TKey): TValue; virtual;
    end;

    TObjectRegistry<TKey, TValue> = class(TRegistry<TKey,TValue>, IRegistry<TKey, TValue>)
      public
        constructor Create(const Ownerships: TDictionaryOwnerships; const ACapacity: Integer = 0); virtual;
    end;

implementation
  uses System.SysUtils; //for FreeAndNil

{$region 'TRegistry<TKey, TValue>'}

constructor TRegistry<TKey, TValue>.Create(const ACapacity: Integer = 0);
begin
  inherited Create;
  FDictionary := TDictionary<TKey, TValue>.Create(ACapacity);
end;

constructor TRegistry<TKey, TValue>.Create(const SkipDictionaryCreation: Boolean = false);
begin
  inherited Create;
end;

destructor TRegistry<TKey, TValue>.Destroy;
begin
  FreeAndNil(FDictionary);
  inherited;
end;

procedure TRegistry<TKey, TValue>.Add(const Key: TKey; const Value: TValue);
begin
  FDictionary.AddOrSetValue(Key, Value);
end;

procedure TRegistry<TKey, TValue>.Add(const Keys: array of TKey; const Value: TValue);
begin
  for var Key in Keys do
    Add(Key, Value);
end;

function TRegistry<TKey, TValue>.Get(const Key: TKey): TValue;
begin
  result := FDictionary.Items[Key];
end;

{$endregion}

{$region TObjectRegistry<TKey, TValue>}

constructor TObjectRegistry<TKey, TValue>.Create(const Ownerships: TDictionaryOwnerships; const ACapacity: Integer = 0);
begin
  inherited Create(false); //needed to avoid FDictionary getting initialized with TDictionary (would leak an object since we replace it below)
  FDictionary := TObjectDictionary<TKey, TValue>.Create(Ownerships, ACapacity);
end;

{$endregion}

end.
