//Description: READ-COM StoryItem Factory
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.StoryItemFactory;

interface
  uses
    System.SysUtils, //for FreeAndNil
    System.Generics.Collections, //for TPair, doOwnsValues (of TDictionaryOwnerships set)
    Zoomicon.Generics.Factories, //for TFactoryRegistry
    READCOM.App.Models; //for IStoryItemFactoryRegistry

  var
    StoryItemFileFilters: TList<TPair<String, String>>;
    StoryItemFactories: IStoryItemFactoryRegistry;

  function AddStoryItemFileFilter(const Title: String; const Exts: String): String;

implementation

type
  TStoryItemFactoryRegistry = TFactoryRegistry<String, IStoryItem>;

function AddStoryItemFileFilter(const Title: String; const Exts: String): String;
begin
  StoryItemFileFilters.Add(TPair<String, String>.Create(Title, Exts));
end;

initialization
  StoryItemFileFilters := TList<TPair<String, String>>.Create;
  StoryItemFactories := TStoryItemFactoryRegistry.Create([{doOwnsValues}]); //assuming the factory objects descend from TComponent //TODO: seems "doOwnsValues" can't be used with Interfaces

finalization
  FreeAndNil(StoryItemFileFilters);
  StoryItemFactories := nil; //interfaces are refcounted

end.
