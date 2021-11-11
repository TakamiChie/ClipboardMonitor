unit Utils;

{$mode objfpc}{$H+}

interface

uses
 LazFileUtils, FileUtil, Forms, Classes, SysUtils;

function GetSettingRootDir: String;
function GetScriptRootDir: String;
function GetOnRunScriptDir: String;
function GetConversionScriptDir: String;
function GetAppDir: String;
function EscapeTags(HTML: string): String;
procedure CopyToUnderscoreScripts;
procedure CopyFile(const FromFile: String; const ToFile: String);

implementation

const SCRIPT_ROOT = 'Scripts';
const SCRIPT_ONRUN = 'Run';
const SCRIPT_CONVERSION = 'Conversion';

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

/// <summary>Get Conversion script dir</summary>
/// <returns>The directory path where you want to store conversion script. Contains directory separator characters at the end.</returns>
function GetConversionScriptDir: String;
begin
  Result := GetScriptRootDir + SCRIPT_CONVERSION + DirectorySeparator;
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

/// <summary>Invalidate HTML tags by converting them to actual references. Some tags will be left intact.</summary>
/// <param name="HTML">HTML for escaping</param>
/// <returns>Escaped HTML</returns>
function EscapeTags(HTML: string): String;
const
  UnescapeTags: array of string = ('p', 'a', 'b', 'i', 'pre', 'ul', 'ol', 'li', 'dl', 'dt', 'dd', 'br', 'span', 'div');
var
  t: String;
begin
  Result:= StringReplace(HTML, '<', '&lt;', [rfReplaceAll]);
  for t in UnescapeTags do
  begin
    Result:= StringReplace(Result, '&lt;' + t, '<' + t, [rfReplaceAll]);
    Result:= StringReplace(Result, '&lt;/' + t, '</' + t, [rfReplaceAll]);
  end;
end;

/// <summary>
/// Copy the script file starting from _ from the project folder to the script folder.
/// </summary>
procedure CopyToUnderscoreScripts;
var
  ScriptRoot, FN: String;
  FileList: TStringList;
  Entry: array[0..1] of String;
  Dirs: array[0..1,0..1] of String;
begin
  Dirs[0][0] := GetOnRunScriptDir;
  Dirs[0][1] := SCRIPT_ONRUN;
  Dirs[1][0] := GetConversionScriptDir;
  Dirs[1][1] := SCRIPT_CONVERSION;
  ScriptRoot:= GetAppDir + LowerCase(SCRIPT_ROOT) + DirectorySeparator;
  FileList:= TStringList.Create;
  try
    for Entry in Dirs do
    begin
      FindAllFiles(FileList, ScriptRoot + LowerCase(Entry[1]), '*.py', False);
      for FN in FileList do CopyFile(FN, Entry[0] + ExtractFileName(FN));
      FileList.Clear;
    end;
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

