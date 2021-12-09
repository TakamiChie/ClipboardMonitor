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
  SF := TScriptFile.Create('tests\\scripts\\SFTEST_HasCommentNoOrder.py', 'def');
  try
    AssertEquals('Test File', SF.DisplayName);
    AssertEquals(100, SF.Order)
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
  SF := TScriptFile.Create('tests\\scripts\\SFTEST_HasNoComment.py', 'def');
  try
    AssertEquals('SFTEST_HasNoComment.py', SF.DisplayName);
    AssertEquals(100, SF.Order)
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
  SFs:= LoadScriptFiles('tests\\scripts\\LSFTEST\\', 'def');
  try
    for i := 0 to Length(Orders) - 1 do
    begin
      AssertEquals(Orders[i], SFs[i].Order);
      AssertEquals(i, SFs[i].Index);
    end;
  finally
    for SF in SFs do SF.Free;
  end;
end;

initialization

  RegisterTest(TScriptManagerTestCase);
end.

