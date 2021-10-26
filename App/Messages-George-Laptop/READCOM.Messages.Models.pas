unit READCOM.Messages.Models;

interface

  type

    IMessageSingleValue<T> = interface
      ['{2F1D59E4-BC43-4810-A8FF-6464A3BA8F36}']
      function GetValue: T;
      procedure SetValue(const Value: T);

      property Value: T read GetValue write SetValue;
    end;

    IMessageMenu = interface
      ['{90C0E406-A811-47F5-97B5-90823F52C143}']
    end;

    IMessageAdd = interface
      ['{5FA43981-3A62-4DF3-A2D1-71CDA4368122}']
    end;

    IMessageNavigationPrevious = interface
      ['{F221FB61-B8C0-4749-9E03-6EB652866FC0}']
    end;

    IMessageNavigationNext = interface
      ['{8D631950-77D6-4535-88DE-3C77C143C009}']
    end;

    IMessageEditModeChange = IMessageSingleValue<Boolean>; //TODO: check that GUID reuse won't cause issues

implementation

end.
