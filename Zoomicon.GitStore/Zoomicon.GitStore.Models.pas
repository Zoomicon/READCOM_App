unit Zoomicon.GitStore.Models;

interface
  uses
    Zoomicon.Downloader.Models, //for TDownloadCompletionEvent
    System.Classes, //for TStream
    System.Generics.Collections, //for TList
    System.Net.URLClient; //for TURI

  type

    IGitItem = interface
      ['{42CD39C9-6F83-43CD-80BF-6AB7D9C23C9B}']
      function GetSHA: String;
      function GetPath: String;
    end;

    IGitFile = interface(IGitItem)
      ['{53091685-BCCC-4DC6-BA03-EEC82F3EE204}']
      function GetFileUri: TURI;

      procedure Download(const CompletionHandler: TDownloadCompletionEvent);
    end;

    IGitFolder = interface(IGitItem)
      ['{E8239100-344A-40B8-8E8E-1BC027CDBDE4}']
      function GetItems(const recursive: Boolean = false): TList<IGitItem>;
      function GetFolders(const recursive: Boolean = false): TList<IGitFolder>;
      function GetFiles(const recursive: Boolean = false): TList<IGitFile>;
    end;

    IGitStore = interface
      ['{1EF5B517-4BC2-4337-A2F3-CF77DEA1CF05}']
      function GetRepositoryURI: TURI;
      procedure SetRepositoryURI(const Value: TURI);

      function GetAuthKey: String;
      procedure SetAuthKey(const Value: String);

      function GetBaseFolder: IGitFolder;

      function GetItem(SHA: string): IGitItem;
      function GetFile(SHA: string): IGitFile;
      function GetFolder(SHA: string): IGitFolder;
    end;

implementation

end.
