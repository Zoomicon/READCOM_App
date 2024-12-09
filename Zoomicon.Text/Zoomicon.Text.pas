//Description: Text-related helper classes and methods
//Author: George Birbilis (http://zoomicon.com)

unit Zoomicon.Text;

interface
  uses
    FMX.Memo; //for TMemo

  type

    TMemoExt = class(TMemo)
      procedure ApplyStyle; override;
    end;

  function SafeTextToShortCut(const Text: string): Integer; //the Delphi 11.1 TextToShortcut returns -1 when platform doesn't support the check (e.g. on Android) instead of 0 (which is what TAction.Shortcut expects for no-shortcut)

implementation
  {$region 'Used units'}
  uses
    System.Types, //for RectF
    System.UIConsts, //for claXX color constants
    System.Math, //for Max
    //
    FMX.ActnList, //for TextToShortcut
    FMX.Controls, //for TControl
    FMX.Objects, //for TRectangle
    FMX.Types; //for TAlignLayout
  {$endregion}

  procedure TMemoExt.ApplyStyle; //Make Memo transparent
  begin
     inherited;

     var Obj := FindStyleResource('background');
     if Assigned(Obj) then
     begin
        TControl(Obj).Margins.Rect := RectF(-1, -1, -1, -1);
        var LRect := TRectangle.Create(Obj);
        Obj.AddObject(LRect);
        with LRect do
        begin
          Align := TAlignLayout.Client;
          Fill.Color := claLightslategrey;
          Stroke.Color := claNull;
          HitTest := False;
          SendToBack;
        end;
     end;
  end;

  function SafeTextToShortCut(const Text: string): Integer; //TODO: maybe move to a Zoomicon.Helpers.FMX.ActnList unit at Zoomicon.Helpers.FMX package (though this is not technically a method so wouldn't put in a helper, but just in the unit)
  begin
    result := Max(TextToShortCut(Text), 0); //Fixing Delphi's 11.1 TextToShortcut which returns -1 when platform doesn't support the check (e.g. on Android) instead of 0 (which is what TAction.Shortcut expects for no-shortcut)
  end;

end.
