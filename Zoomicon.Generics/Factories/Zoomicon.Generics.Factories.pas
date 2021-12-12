unit Zoomicon.Generics.Factories;

interface
  uses Zoomicon.Generics.Registries;

  type
    IFactory<TType> = interface
      ['{D8177B34-574D-49FB-B2E6-BF02C5A30ADB}']
      function Create: TType;
    end;

    IFactoryRegistry<TKey, TType> = interface(IRegistry<TKey, IFactory<TType>>)
      ['{88541345-6E83-46D0-A909-8DDE40D16C38}']
    end;

    TFactoryRegistry<TKey, TType> = class(TObjectRegistry<TKey, IFactory<TType>>, IFactoryRegistry<TKey, TType>, IRegistry<TKey, IFactory<TType>>) //Delphi doesn't auto-declare ancestor interfaces
    end;

implementation

end.
