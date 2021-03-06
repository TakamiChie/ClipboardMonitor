unit scriptprocesstestcase;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, fpcunit, testutils, testregistry, ScriptProcess;

type
  TScriptProcessTestCase= class(TTestCase)
  published
    procedure TestLaunchProgram;
    procedure TestLaunchProgramWithText;
    procedure TestLaunchProgramHasError;
  end;

implementation

/// <summary>
/// When ScriptProcess.Execute is called under the following conditions, make sure that the program starts and the results can be obtained.
/// * Program: python
/// * Script: TestScript.py
/// * Timeout: default
/// * Text: Empty
/// </summary>
procedure TScriptProcessTestCase.TestLaunchProgram;
var
  Proc : TScriptProcess;
  StdOut, StdErr: String;
begin
  Proc := TScriptProcess.Create();
  try
    Proc.Execute('tests\\scripts\\SPTEST_TestScript.py', StdOut, StdErr);
    AssertEquals('Hello This Is a TEST' + #13#10, StdOut);
  finally
    Proc.Free;
  end;
end;

/// <summary>
/// When ScriptProcess.Execute is called under the following conditions, make sure that the program starts and the results can be obtained.
/// * Program: python
/// * Script: TestScript.py
/// * Timeout: default
/// * Text: Input TEST
/// </summary>
procedure TScriptProcessTestCase.TestLaunchProgramWithText;
var
  Proc : TScriptProcess;
  StdOut, StdErr: String;
begin
  Proc := TScriptProcess.Create();
  try
    Proc.Text := 'Input Test';
    Proc.Execute('tests\\scripts\\SPTEST_TestScript.py', StdOut, StdErr);
    AssertEquals('Hello This Is a TEST' + #13#10 + 'Input Test' + #13#10, StdOut);
  finally
    Proc.Free;
  end;
end;

/// <summary>
/// When ScriptProcess.Execute is called under the following conditions, make sure that an error occurs and the value is stored in StdErr.
/// * Program: python
/// * Script: TestScript.py
/// * Timeout: default
/// * Text: Empty
/// * Error occurred: Yes
/// </summary>
procedure TScriptProcessTestCase.TestLaunchProgramHasError;
var
  Proc : TScriptProcess;
  StdOut, StdErr: String;
begin
  Proc := TScriptProcess.Create();
  try
    Proc.Execute('"tests\\scripts\\SPTEST_TestScript.py" "--raiseerror"', StdOut, StdErr);
    AssertTrue(Pos('This Is Test Error', StdErr) > 0);
  finally
    Proc.Free;
  end;
end;

initialization
  RegisterTest(TScriptProcessTestCase);
end.

