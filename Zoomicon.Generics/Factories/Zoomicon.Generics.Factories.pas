unit Zoomicon.Generics.Factories;

interface
  uses
    System.Classes, //for TComponent
    Zoomicon.Generics.Registries; //for IRegistry, TObjectRegistry

  type
    IFactory<TType> = interface
      ['{D8177B34-574D-49FB-B2E6-BF02C5A30ADB}']
      function New(const AOwner: TComponent = nil): TType;
    end;

    IFactoryRegistry<TKey, TType> = interface(IRegistry<TKey, IFactory<TType>>)
      ['{88541345-6E83-46D0-A909-8DDE40D16C38}']
    end;

    TFactoryRegistry<TKey, TType> = class(TObjectRegistry<TKey, IFactory<TType>>, IFactoryRegistry<TKey, TType>, IRegistry<TKey, IFactory<TType>>); //Delphi doesn't auto-declare ancestor interfaces

implementation

end.
