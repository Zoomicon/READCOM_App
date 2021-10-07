unit READCOM.App.Models;

interface
  uses
    System.Classes; //for TStream

  type

    ILoadable = interface
      ['{A08F7880-FBE5-40C5-B695-FF0F3A18EF3E}']
      function GetLoadFilesFilter: String;
      procedure Load(const Stream: TStream); overload;
      procedure Load(const Filepath: string); overload;
      procedure Load(const Filepaths: array of string); overload;
    end;

    ISaveable = interface
      ['{F32A83D9-B960-4C12-8BD5-2141563E3091}']
      procedure Save(const Stream: TStream); overload;
      procedure Save(const Filepath: string); overload;
      procedure Save(const Directory: string; const FilenamePrefix: string); overload;
    end;

implementation

end.
