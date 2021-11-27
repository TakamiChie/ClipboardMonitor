unit ScriptManagerTestCase;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TScriptManagerTestCase= class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
  end;

implementation

procedure TScriptManagerTestCase.TestHookUp;
begin
 Fail('Write your own test');
end;

procedure TScriptManagerTestCase.SetUp;
begin

end;

procedure TScriptManagerTestCase.TearDown;
begin

end;

initialization

 RegisterTest(TScriptManagerTestCase);
end.

