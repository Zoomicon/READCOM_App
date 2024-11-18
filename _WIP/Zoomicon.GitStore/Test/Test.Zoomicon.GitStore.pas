unit Test.Zoomicon.GitStore; //Delphi DUnit Test Cases

interface
  uses
    Zoomicon.GitStore.Classes, //for TGitStore
    TestFramework;

  type

    // Test methods for class TGitStore

    TestTGitStore = class(TTestCase)
      strict private
        FGitStore: TGitStore;
      public
        procedure SetUp; override;
        procedure TearDown; override;
      published
        procedure TestLoadContents;
        procedure TestLoadContentsHasSubfolders;
        procedure TestLoadContentsHasImagesSubfolders;
    end;

implementation
  uses
    Zoomicon.Cache.Classes,
    FireDAC.Comp.Client,
    SysUtils; //for FreeAndNil

{$region 'SetUp / TearDown'}

procedure TestTGitStore.SetUp;
begin
  FGitStore := TGitStore.Create(nil, 'Zoomicon', 'READCOM_Gallery', 'main', TFileCache.Create()); //the FileCache is used via IContentCache so it is reference counted
end;

procedure TestTGitStore.TearDown;
begin
  FreeAndNil(FGitStore);
end;

{$endregion}

procedure TestTGitStore.TestLoadContents;
begin
  var data := FGitStore.LoadContents;
  Check(Assigned(data) and (data.SourceView.Rows.Count <> 0), 'Failed to Load repository contents list or repository is empty');
  FreeAndNil(data);
end;

procedure TestTGitStore.TestLoadContentsHasSubfolders;
begin
  var data := FGitStore.LoadContents;
  data.Filter := 'type=' + QuotedStr('tree'); //search for subfolders
  data.Filtered := true;
  Check(Assigned(data) and (data.SourceView.Rows.Count <> 0), 'Failed to find subfolders in repository');
  FreeAndNil(data);
end;

procedure TestTGitStore.TestLoadContentsHasImagesSubfolders;
begin
  var data := FGitStore.LoadContents;
  data.Filter := '(type = ' + QuotedStr('tree') + ') and (path LIKE ' + QuotedStr('Gallery/Assets/Images%') + ')'; //search for subfolders
  data.Filtered := true;
  Check(Assigned(data) and (data.SourceView.Rows.Count <> 0), 'Failed to find any subfolders under Gallery/Assets/Images/ in repository');
  FreeAndNil(data);
end;

initialization
  // Register any test cases with the test runner
  RegisterTest(TestTGitStore.Suite);
end.

