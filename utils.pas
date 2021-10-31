unit Utils;

{$mode objfpc}{$H+}

interface

uses
 LazFileUtils, FileUtil, Forms, Classes, SysUtils;

function GetScriptRootDir: String;
function GetOnRunScriptDir: String;
procedure CopyToUnderscoreScripts;
procedure CopyFile(const FromFile: String; const ToFile: String);

implementation

const SCRIPT_ROOT = 'Scripts';
const SCRIPT_ONRUN = 'Run';

function GetScriptRootDir: String;
begin
  Result := GetAppConfigDir(False) + SCRIPT_ROOT;
  if not DirectoryExists(Result) then
  begin
    ForceDirectories(Result);
  end;
end;

function GetOnRunScriptDir: String;
begin
  Result := GetAppConfigDir(False) + SCRIPT_ROOT + DirectorySeparator + SCRIPT_ONRUN;
  if not DirectoryExists(Result) then
  begin
    ForceDirectories(Result);
  end;
end;

/// <summary>
/// Copy the script file starting from _ from the project folder to the script folder.
/// </summary>
procedure CopyToUnderscoreScripts;
var
  ScriptRoot, OnRunScriptDir, FN: String;
  FileList: TStringList;
begin
  ScriptRoot:= ExtractFileDir(Application.ExeName) + DirectorySeparator + LowerCase(SCRIPT_ROOT);
  OnRunScriptDir:= GetOnRunScriptDir;
  FileList:= TStringList.Create;
  try
    FindAllFiles(FileList, ScriptRoot + DirectorySeparator + LowerCase(SCRIPT_ONRUN), '*.py', False);
    for FN in FileList do CopyFile(FN, OnRunScriptDir + DirectorySeparator + ExtractFileName(FN));
  finally
    FileList.Free;
  end;
end;

/// <summary>
/// Copy the file.
/// </summary>
/// <param name="FromFile">The path of the file to copy from.</param>
/// <param name="ToFile">The path of the file to copy to.</param>
procedure CopyFile(const FromFile: String; const ToFile: String);
var
  MemBuffer: TMemoryStream;
begin
  MemBuffer:= TMemoryStream.Create;
  try
    MemBuffer.LoadFromFile(FromFile);
    MemBuffer.Position:=0;
    MemBuffer.SaveToFile(ToFile);
  finally
    MemBuffer.Free;
  end;
end;

end.

