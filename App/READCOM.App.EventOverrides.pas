unit READCOM.App.EventOverrides; //in a custom app could rename this to XX.App.Events

interface
  uses READCOM.Views.StoryForm; //for TStoryForm, TStory

  type
    TEventHandlers = class
      procedure StoryFormReady(const StoryForm: TStoryForm);
      procedure StoryLoaded(const Story: TStory);
    end;

  var
    EventHandlers: TEventHandlers;

implementation
  uses
    System.SysUtils; //for FreeAndNil

  procedure TEventHandlers.StoryFormReady(const StoryForm: TStoryForm);
  begin
    (* //uncomment to start with UI buttons hidden (apart from menu toggle button that shows/hides the rest)
    StoryForm.HUD.actionMenu.Checked := false; //don't just do StoryForm.HUD.layoutButtons.Visible := false since that would make the menu toggle button unsync
    StoryForm.HUD.layoutButtons.Visible := false;
    *)
  end;

  procedure TEventHandlers.StoryLoaded(const Story: TStory);
  begin
    //add logic here to execute everytime a Story is loaded (including the default story)
  end;

initialization
  EventHandlers := TEventHandlers.Create;

finalization
  FreeAndNil(EventHandlers);

end.
