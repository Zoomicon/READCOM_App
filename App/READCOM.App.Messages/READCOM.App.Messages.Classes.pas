unit READCOM.App.Messages.Classes;

interface
  uses READCOM.App.Messages.Models;

  type

    TMessageMenu = class(TInterfacedObject, IMessageMenu)
      public
        constructor Create;
    end;

    TMessageAdd = class(TInterfacedObject, IMessageAdd)
      public
        constructor Create;
    end;

implementation

{ TMessageMenu }

constructor TMessageMenu.Create;
begin
  inherited;
end;

{ TMessageAdd }

constructor TMessageAdd.Create;
begin
  inherited;
end;

end.
