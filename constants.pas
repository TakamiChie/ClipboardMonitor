unit Constants;

{$mode objfpc}{$H+}

interface

uses
 LazFileUtils, Classes, SysUtils;

function GetScriptRootDir: String;
function GetOnRunScriptDir: String;

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

end.

