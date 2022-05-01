//Description: READ-COM ImageStoryItem Options
//Author: George Birbilis (http://zoomicon.com)

unit READCOM.Views.Options.ImageStoryItemOptions;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  System.Messaging, //for TMessage
  System.Actions, FMX.ActnList,
  System.ImageList, FMX.ImgList, FMX.SVGIconImageList, FMX.Edit,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.StdActns,
  FMX.MediaLibrary.Actions,
  READCOM.Views.Options.StoryItemOptions,
  READCOM.App.Models;

type
  TImageStoryItemOptions = class(TStoryItemOptions, IImageStoryItemOptions, IStoryItemOptions)
    TakePhotoFromCameraAction: TTakePhotoFromCameraAction;
    btnCamera: TSpeedButton;
    //procedure actionCameraExecute(Sender: TObject);
    procedure TakePhotoFromCameraActionDidFinishTaking(Image: TBitmap);

  protected
    //procedure DoCameraDidFinish(Image: TBitmap);
    //procedure DoMessageDidFinishTakingImageFromLibrary(const Sender: TObject; const M: TMessage); //for Android in case app restarted: see https://docwiki.embarcadero.com/Libraries/Sydney/en/FMX.MediaLibrary.TMessageDidFinishTakingImageFromLibrary

    {ImageStoryItem}
    function GeTImageStoryItem: IImageStoryItem;
    procedure SeTImageStoryItem(const Value: IImageStoryItem);

  public
    constructor Create(AOwner: TComponent); override;

  published
    property ImageStoryItem: IImageStoryItem read GeTImageStoryItem write SeTImageStoryItem stored false;
  end;

implementation
uses
  FMX.MediaLibrary,
  FMX.Platform;

{$R *.fmx}

constructor TImageStoryItemOptions.Create(AOwner: TComponent);
begin
  inherited;
  //TMessageManager.DefaultManager.SubscribeToMessage(TMessageDidFinishTakingImageFromLibrary, DoMessageDidFinishTakingImageFromLibrary); //see region 'TakePhotoViaCameraService'
end;

{$region 'ImageStoryItem'}

function TImageStoryItemOptions.GeTImageStoryItem: IImageStoryItem;
begin
  Supports(FStoryItem, IImageStoryItem, result);
end;

procedure TImageStoryItemOptions.SeTImageStoryItem(const Value: IImageStoryItem);
begin
  Supports(Value, IStoryItem, FStoryItem); //interface casting also supports interface implementations using aggregated or nested objects
end;

{$endregion}

procedure TImageStoryItemOptions.TakePhotoFromCameraActionDidFinishTaking(Image: TBitmap);
begin
  inherited;
  ImageStoryItem.Image.Bitmap.Assign(Image);
end;

{$region 'TakePhotoViaCameraService'}
//using predefined Camera action instead of FMX platform services (need to implement for Windows) - there's also TCameraComponent
(*
procedure TImageStoryItemOptions.DoCameraDidFinish(Image: TBitmap);
begin
  ImageStoryItem.Image.Bitmap.Assign(Image);
end;

procedure TImageStoryItemOptions.DoMessageDidFinishTakingImageFromLibrary(const Sender: TObject; const M: TMessage);
begin
  if M is TMessageDidFinishTakingImageFromLibrary then
    ImageStoryItem.Image.Bitmap.Assign(TMessageDidFinishTakingImageFromLibrary(M).Value);
end;

procedure TImageStoryItemOptions.actionCameraExecute(Sender: TObject);
begin
  inherited;

  var Service: IFMXCameraService;
  var Params: TParamsPhotoQuery;
  if TPlatformServices.Current.SupportsPlatformService(IFMXCameraService, Service) then
  begin
    with Params do
    begin
      Editable := True;
      NeedSaveToAlbum := True; // Specifies whether to save a picture to device Photo Library
      RequiredResolution := TSize.Create(640, 640);
      OnDidFinishTaking := DoDidFinish;
    end;
    //Service.TakePhoto(SpeedButton1, Params); //TODO
  end
  else
    ShowMessage('This device does not support the camera service');
end;
*)
{$endregion}

end.
