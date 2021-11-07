unit ScriptManager;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, RegExpr, IniFiles, FileUtil;

type
  /// <summary>A structure that manages individual script files.</summary>
  TScriptFile = class(TObject)
  private
    FOrder: Integer;
    FFilePath: String;
    FDisplayName: String;
    function GetFileName: String;
  public
    constructor Create(ScriptFile: String);
    property Order: Integer read FOrder;
    property FilePath: String read FFilePath;
    property FileName: String read GetFileName;
    property DisplayName: String read FDisplayName;
  end;
  TScriptList = array of TScriptFile;
  function LoadScriptFiles(Dir: String): TScriptList;

implementation

/// <summaryReads all script files in the specified folder and returns them as an array.</summary>
/// <param name="Dir">Directory to be read</param>
/// <returns>An array showing a list of scripts</returns>
function LoadScriptFiles(Dir: String): TScriptList;
var
  ScriptFiles: TStringList;
  S: Pointer;
  F: String;
  Scripts: TList;
  Index: Integer;
begin
  Scripts:= TList.Create;
  try
    ScriptFiles:= TStringList.Create;
    try
      FindAllFiles(ScriptFiles, Dir, '*.py', False);
      for F in ScriptFiles do
        Scripts.Add(TScriptFile.Create(F));
    finally
      ScriptFiles.Free;
    end;
    SetLength(Result, Scripts.Count);
    Index:=0;
    for S in Scripts do
    begin
      Result[Index]:= TScriptFile(S);
      Index:= Index + 1;
    end;
  finally
    Scripts.Free;
  end;
end;

/// <summary>Initialize Object</summary>
/// <param name="ScriptFile">Path of the script file to be loaded.</param>
constructor TScriptFile.Create(ScriptFile: String);
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
    Self.FDisplayName:=Setting.ReadString('caption', 'def', FileName);
    Self.FOrder:=Setting.ReadInteger('general', 'order', 100);
  finally
    Setting.Free;
  end;
end;

/// <summary>Get file name (not including directory path).</summary>
/// <returns>File name.</returns>
function TScriptFile.GetFileName: String;
begin
  Result:= ExtractFileName(FFilePath);
end;

end.

