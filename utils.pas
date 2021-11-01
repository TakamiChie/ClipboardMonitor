unit Utils;

{$mode objfpc}{$H+}

interface

uses
 LazFileUtils, FileUtil, Forms, Classes, SysUtils;

function GetSettingRootDir: String;
function GetScriptRootDir: String;
function GetOnRunScriptDir: String;
function GetAppDir: String;
procedure CopyToUnderscoreScripts;
procedure CopyFile(const FromFile: String; const ToFile: String);

implementation

const SCRIPT_ROOT = 'Scripts';
const SCRIPT_ONRUN = 'Run';

/// <summary>Get Setting dir</summary>
/// <returns>The directory path where you want to store application configuration information. Contains directory separator characters at the end.</returns>
function GetSettingRootDir: String;
begin
  Result := GetAppConfigDir(False);
  if not DirectoryExists(Result) then
  begin
    ForceDirectories(Result);
  end;
end;

/// <summary>Get Script root dir</summary>
/// <returns>The directory path where you want to store script root. Contains directory separator characters at the end.</returns>
function GetScriptRootDir: String;
begin
  Result := GetAppConfigDir(False) + SCRIPT_ROOT + DirectorySeparator;
  if not DirectoryExists(Result) then
  begin
    ForceDirectories(Result);
  end;
end;

/// <summary>Get On run script dir</summary>
/// <returns>The directory path where you want to store on run script. Contains directory separator characters at the end.</returns>
function GetOnRunScriptDir: String;
begin
  Result := GetScriptRootDir + SCRIPT_ONRUN + DirectorySeparator;
  if not DirectoryExists(Result) then
  begin
    ForceDirectories(Result);
  end;
end;

/// <summary>Get Application EXE dir</summary>
/// <returns>The directory path where the application executable is located. Contains directory separator characters at the end.</returns>
function GetAppDir: String;
begin
  Result:= ExtractFileDir(Application.ExeName) + DirectorySeparator;
end;

/// <summary>
/// Copy the script file starting from _ from the project folder to the script folder.
/// </summary>
procedure CopyToUnderscoreScripts;
var
  ScriptRoot, OnRunScriptDir, FN: String;
  FileList: TStringList;
begin
  ScriptRoot:= GetAppDir + LowerCase(SCRIPT_ROOT) + DirectorySeparator;
  OnRunScriptDir:= GetOnRunScriptDir;
  FileList:= TStringList.Create;
  try
    FindAllFiles(FileList, ScriptRoot + LowerCase(SCRIPT_ONRUN), '*.py', False);
    for FN in FileList do CopyFile(FN, OnRunScriptDir + ExtractFileName(FN));
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

