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

  procedure TMemoExt.ApplyStyle; //Make Memo transparent //TODO: does this work? (TTextStoryItem.MemoApplyStyleLookup seems to use another method with a normal TMemo)
  begin
     inherited;

     var Obj := FindStyleResource('background');
     if Assigned(Obj) then
     begin
        TControl(Obj).Margins.Rect := RectF(-1, -1, -1, -1);
        var LRect := TRectangle.Create(Obj);
        Obj.AddObject(LRect); //TODO: does this also pass ownership of LRect? Or could this leak memory?
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

end.
