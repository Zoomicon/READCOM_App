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
  FMX.MediaLibrary, //probably needed by FMX.MediaLibrary.Actions (saving the frame in D12.2 puts it into "interface" section's uses clause, causing compilation error if already at "implementation" section's uses clause)
  FMX.MediaLibrary.Actions, //for TTakePhotoFromCameraAction
  FMX.Objects,
  //
  READCOM.Views.Options.StoryItemOptions,
  READCOM.Models.Stories;

type
  TImageStoryItemOptions = class(TStoryItemOptions, IImageStoryItemOptions, IStoryItemOptions)
    TakePhotoFromCameraAction: TTakePhotoFromCameraAction;
    btnCamera: TSpeedButton;
    LayoutImageStoryItemBreak: TFlowLayoutBreak;
    LayoutImageStoryItemButtons: TFlowLayout;
    //procedure actionCameraExecute(Sender: TObject);
    procedure TakePhotoFromCameraActionDidFinishTaking(Image: TBitmap);
    procedure TakePhotoFromCameraActionCanActionExec(Sender: TCustomAction;
      var CanExec: Boolean);

  protected
    //procedure DoCameraDidFinish(Image: TBitmap);
    //procedure DoMessageDidFinishTakingImageFromLibrary(const Sender: TObject; const M: TMessage); //for Android in case app restarted: see https://docwiki.embarcadero.com/Libraries/Sydney/en/FMX.MediaLibrary.TMessageDidFinishTakingImageFromLibrary

    {StoryItem}
    procedure SetStoryItem(const Value: IStoryItem); override;

    {ImageStoryItem}
    function GetImageStoryItem: IImageStoryItem; virtual;
    procedure SetImageStoryItem(const Value: IImageStoryItem); virtual;

  public
    constructor Create(AOwner: TComponent); override;

  published
    property ImageStoryItem: IImageStoryItem read GetImageStoryItem write SetImageStoryItem stored false;
  end;

implementation
  uses
    FMX.Platform,
    System.Permissions; // Required for permission handling

  {$R *.fmx}

  {$REGION 'LIFETIME MANAGEMENT'}

  constructor TImageStoryItemOptions.Create(AOwner: TComponent);
  begin
    inherited;
    //TMessageManager.DefaultManager.SubscribeToMessage(TMessageDidFinishTakingImageFromLibrary, DoMessageDidFinishTakingImageFromLibrary); //see region 'TakePhotoViaCameraService'

    //Resize to not include LayoutImageStoryItemButtons and its break item if Camera button isn't visible (on platforms that don't support it - currently only mobile ones do), since Camera buttons is the only extra button for ImageStoryItem at this point
    if not btnCamera.Visible then
      Height := Height - LayoutImageStoryItemButtons.Height - 5; //note: don't subtract LayoutImageStoryItemBreak.Height
  end;

  {$ENDREGION}

  {$REGION 'PROPERTIES'}

  {$region 'StoryItem'}

  procedure TImageStoryItemOptions.SetStoryItem(const Value: IStoryItem);
  begin
    inherited;

    var LImageStoryItem: IImageStoryItem;
    if not Supports(Value, IImageStoryItem, LImageStoryItem) then
      raise EIntfCastError.Create('Expected IImageStoryItem');

    SetImageStoryItem(LImageStoryItem);
  end;

  {$endregion}

  {$region 'ImageStoryItem'}

  function TImageStoryItemOptions.GetImageStoryItem: IImageStoryItem;
  begin
    Supports(FStoryItem, IImageStoryItem, result);
  end;

  procedure TImageStoryItemOptions.SetImageStoryItem(const Value: IImageStoryItem);
  begin
    inherited SetStoryItem(Value); //don't call overriden SetStoryItem, would do infinite loop
  end;

  {$endregion}

  {$ENDREGION}

  {$region 'TakePhotoFromCameraAction'}

  //TODO: this won't work in D12.2 since TakePhotoFromCameraAction.ExecuteTarget doesn't call CanActionExec (which calls OnActionExec event handler, if assigned) - need to use TTakePhotoFromCameraActionEx from Zoomicon.Media.FMX package
  procedure TImageStoryItemOptions.TakePhotoFromCameraActionCanActionExec(Sender: TCustomAction; var CanExec: Boolean);
  begin
    log.d('Info', Self, 'TakePhotoFromCameraActionCanActionExec', 'Entered');

    CanExec := false; //don't execute the action before checking permission

    PermissionsService.RequestPermissions(['android.permission.CAMERA'],
      procedure(const APermissions: TClassicStringDynArray; const AGrantResults: TClassicPermissionStatusDynArray)
      begin
        if (Length(AGrantResults) > 0) and (AGrantResults[0] = TPermissionStatus.Granted) then
          //execute the action now that permission is granted
          TThread.Queue(nil,
            procedure
            begin
              log.d('Info', Self, 'TakePhotoFromCameraActionCanActionExec', 'Executing action');

              Sender.Execute;  //ensure action executes on the main thread
            end);
      end);
  end;

  procedure TImageStoryItemOptions.TakePhotoFromCameraActionDidFinishTaking(Image: TBitmap);
  begin
    inherited;
    ImageStoryItem.Image.Bitmap.Assign(Image);
  end;

  {$endregion}

  //using predefined Camera action instead of FMX platform services (need to implement for Windows) - there's also TCameraComponent
  {$region 'TakePhotoViaCameraService'}
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
