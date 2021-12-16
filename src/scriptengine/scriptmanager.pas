unit ScriptManager;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, RegExpr, IniFiles, FileUtil, fgl;

type
  /// <summary>A structure that manages individual script files.</summary>
  TScriptFile = class(TObject)
  private
    FOrder: Integer;
    FFilePath: String;
    FDisplayName: String;
    /// <summary>Get file name (not including directory path).</summary>
    /// <returns>File name.</returns>
    function GetFileName: String;
  public
    /// <summary>Initialize Object</summary>
    /// <param name="ScriptFile">Path of the script file to be loaded.</param>
    /// <param name="Language">Language code.</param>
    constructor Create(ScriptFile: String; Language: String);
    property Order: Integer read FOrder;
    property FilePath: String read FFilePath;
    property FileName: String read GetFileName;
    property DisplayName: String read FDisplayName;
  end;
  TScriptList = specialize TFPGObjectList<TScriptFile>;
  /// <summary>Reads all script files in the specified folder and returns them as an array.</summary>
  /// <param name="Dir">Directory to be read</param>
  /// <param name="Language">Language code.</param>
  /// <returns>An array showing a list of scripts</returns>
  function LoadScriptFiles(Dir: String; Language: String): TScriptList;
  function CompareOrder(const item1,item2: TScriptFile):Integer;

implementation

function LoadScriptFiles(Dir: String; Language: String): TScriptList;
var
  ScriptFiles: TStringList;
  S: TScriptFile;
  F: String;
  Index: Integer;
begin
  Result:= TScriptList.Create;
  ScriptFiles:= TStringList.Create;
  try
    FindAllFiles(ScriptFiles, Dir, '*.py', False);
    for F in ScriptFiles do
    begin
      Result.Add(TScriptFile.Create(F, Language));
    end;
  finally
    ScriptFiles.Free;
  end;
  Result.Sort(@CompareOrder);
end;

constructor TScriptFile.Create(ScriptFile: String; Language: String);
var
  Script, IniBase: TStringList;
  Setting: TMemIniFile;
  Line: String;
  Check: TRegExpr;
begin
  Setting:= TMemIniFile.Create('');
  try
    Script:= TStringList.Create;
    try
      Script.LoadFromFile(ScriptFile, TEncoding.UTF8);
      // Load Ini Section.
      Check:= TRegExpr.Create;
      try
        Check.Expression:= '(\[\w+\]|\w+=.*)';
        IniBase:= TStringList.Create;
        try
          for Line in Script do
          begin
            if Line = '' then Break;
            if Check.Exec(Line) then
            begin
              IniBase.Add(Check.Match[1]);
            end;
          end;
          Setting.SetStrings(IniBase);
        finally
          IniBase.Free;
        end;

      finally
        Check.Free;
      end;
    finally
      Script.Free;
    end;

    Self.FFilePath:=ScriptFile;
    Self.FDisplayName:=Setting.ReadString('caption', Language, FileName);
    Self.FOrder:=Setting.ReadInteger('general', 'order', 100);
  finally
    Setting.Free;
  end;
end;

function TScriptFile.GetFileName: String;
begin
  Result:= ExtractFileName(FFilePath);
end;

function CompareOrder(const Item1, Item2: TScriptFile):Integer;
begin
  Result:= Item1.Order - Item2.Order;
end;

end.

