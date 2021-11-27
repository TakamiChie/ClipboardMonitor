unit localizertestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TLocalizerTestCase= class(TTestCase)
  published
    procedure TestHookUp;
  end;

implementation

procedure TLocalizerTestCase.TestHookUp;
begin
 Fail('Write your own test');
end;



initialization

 RegisterTest(TLocalizerTestCase);
end.

