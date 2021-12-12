//Description: READ-COM StoryItem Factory
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.StoryItemFactory;

interface
  uses
    Zoomicon.Generics.Factories, //for TFactoryRegistry
    READCOM.App.Models; //for IStoryItemFactoryRegistry

  var
    StoryItemAddFileFilter: String;
    StoryItemFactories: IStoryItemFactoryRegistry;

implementation
  uses
    System.Generics.Collections; //for doOwnsValues (of TDictionaryOwnerships set)

  type
    TStoryItemFactoryRegistry = TFactoryRegistry<String, IStoryItem>;

initialization
  StoryItemFactories := TStoryItemFactoryRegistry.Create([{doOwnsValues}]); //assuming the factory objects descend from TComponent //TODO: see why can't use "doOwnsValues"

end.
