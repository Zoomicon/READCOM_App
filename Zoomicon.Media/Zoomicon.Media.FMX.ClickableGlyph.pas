unit Zoomicon.Media.FMX.ClickableGlyph;

interface
  uses
    FMX.ImgList; //for TGlyph

type

  TClickableGlyph = class(TGlyph)
  published
    property Cursor; //TODO: is exposed in designer but doesn't seem to do anything at runtime
    property OnClick; //TODO: is exposed in designer but doesn't seem to do anything at runtime
  end;

procedure Register;

implementation
  uses
    System.Classes, //for GroupDescendentsWith, RegisterComponents
    FMX.Types; //for RegisterFmxClasses

{$REGION 'Registration'}

procedure RegisterSerializationClasses;
begin
  RegisterFmxClasses([TClickableGlyph]);
end;

procedure Register;
begin
  GroupDescendentsWith(TClickableGlyph, TComponent);
  RegisterSerializationClasses;
  RegisterComponents('Zoomicon', [TClickableGlyph]);
end;

{$ENDREGION}

initialization
  RegisterSerializationClasses; //don't call Register here, it's called by the IDE automatically on a package installation (fails at runtime)

end.
