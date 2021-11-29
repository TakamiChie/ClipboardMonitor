unit scriptmanagertestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ScriptManager;

type

  TScriptManagerTestCase= class(TTestCase)
  published
    procedure TestLoadScript;
    procedure TestLoadScriptNoComment;
    procedure TestLoadScriptFilesOrderd;
  end;

implementation

/// <summary>
/// Make sure that files with the following conditions can be read in TScriptFile.Create.
/// * Comments in file: Yes
/// * Language defined in comments in the file: def,ja
/// * Order values defined in comments in the file: undefined
/// </summary>
procedure TScriptManagerTestCase.TestLoadScript;
var
  SF: TScriptFile;
begin
  SF := TScriptFile.Create('scripts\\SFTEST_HasCommentNoOrder.py', 'def');
  try
    AssertEquals(SF.DisplayName, 'Test File');
    AssertEquals(SF.Order, 100)
  finally
    SF.Free;
  end;
end;

/// <summary>
/// Make sure that files with the following conditions can be read in TScriptFile.Create.
/// * Comments in file: No
/// * Language defined in comments in the file: undefined
/// * Order values defined in comments in the file: undefined
/// </summary>
procedure TScriptManagerTestCase.TestLoadScriptNoComment;
var
  SF: TScriptFile;
begin
  SF := TScriptFile.Create('scripts\\SFTEST_HasNoComment.py', 'def');
  try
    AssertEquals(SF.DisplayName, 'SFTEST_HasNoComment.py');
    AssertEquals(SF.Order, 100)
  finally
    SF.Free;
  end;
end;

/// <summary>
/// Check that the files are sorted when the LoadScriptFiles method is called under the following conditions.
/// * Folder: The folder where the Python files are stored.
/// * Specified order of files: 0 20 50 100 101 Unspecified
/// </summary>
procedure TScriptManagerTestCase.TestLoadScriptFilesOrderd;
var
  SFs: TScriptList;
  SF: TScriptFile;
  Orders: array of Integer = (0, 20, 50, 100, 100, 101);
  i: Integer;
begin
  SFs:= LoadScriptFiles('scripts\\LSFTEST\\', 'def');
  try
    for i := 0 to Length(Orders) - 1 do
    begin
      AssertEquals(SFs[i].Order, Orders[i]);
      AssertEquals(SFs[i].Index, i);
    end;
  finally
    for SF in SFs do SF.Free;
  end;
end;

initialization

  RegisterTest(TScriptManagerTestCase);
end.

