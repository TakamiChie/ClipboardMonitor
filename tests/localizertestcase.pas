unit localizertestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, Localization;

type
  TLocalizerTestCase= class(TTestCase)
  published
    procedure TestGetLanguageTextDefLanguage;
    procedure TestGetLanguageTextJaLanguage;
    procedure TestGetLanguageList;
    procedure TestGetLanguageTextDefLanguageNoPartialKey;
    procedure TestGetLanguageTextJaLanguageNoPartialKey;
    procedure TestGetLanguageTextDefLanguageNonExistsKey;
  end;

implementation

/// <summary>
/// Confirm that the character string is returned when calling the TLocalizer.GetLanguageText method under the following conditions.
/// * Language: def
/// * Keys that exist in the relevant section: def, ja
/// </summary>
procedure TLocalizerTestCase.TestGetLanguageTextDefLanguage;
var
  Localizer: TLocalizer;
begin
  Localizer:=TLocalizer.Create;
  try
    AssertEquals(Localizer.GetLanguageText('test', 'testmessage'), 'This is a TEST')
  finally
    Localizer.Free;
  end;
end;

/// <summary>
/// Confirm that the character string is returned when calling the TLocalizer.GetLanguageText method under the following conditions.
/// * Language: ja
/// * Keys that exist in the relevant section: def, ja
/// </summary>
procedure TLocalizerTestCase.TestGetLanguageTextJaLanguage;
var
  Localizer: TLocalizer;
begin
  Localizer:=TLocalizer.Create;
  try
    Localizer.Language:='ja';
    AssertEquals(Localizer.GetLanguageText('test', 'testmessage'), 'これは日本語のテストです')
  finally
    Localizer.Free;
  end;
end;

/// <summary>
/// TLocalizer.GetLanguageList method under the following conditions, the list of supported languages will be returned.
/// * Keys that exist in the relevant section: def, ja
/// </summary>
procedure TLocalizerTestCase.TestGetLanguageList;
var
  Localizer: TLocalizer;
  LL: TStringArray;
begin
  Localizer:=TLocalizer.Create;
  try
    LL:= Localizer.GetLanguageList('test', 'testmessage');
    AssertEquals(Length(LL), 2);
    AssertEquals(LL[0], 'def');
    AssertEquals(LL[1], 'ja');
  finally
    Localizer.Free;
  end;
end;

/// <summary>
/// Confirm that Exception occurs when TLocalizer.GetLanguageText method is called under the following conditions.
/// * Language: def
/// * Keys that exist in the relevant section: ja
/// </summary>
procedure TLocalizerTestCase.TestGetLanguageTextDefLanguageNoPartialKey;
var
  Localizer: TLocalizer;
begin
  Localizer:=TLocalizer.Create;
  try
    try
      Localizer.GetLanguageText('test', 'testmessagejaonly');
      Fail('No exceptions were thrown.');
    except
      Self.AssertTrue(True);
    end;
  finally
    Localizer.Free;
  end;
end;

/// <summary>
/// Confirm that the character string is returned when calling the TLocalizer.GetLanguageText method under the following conditions.
/// * Language: ja
/// * Keys that exist in the relevant section: ja
/// </summary>
procedure TLocalizerTestCase.TestGetLanguageTextJaLanguageNoPartialKey;
var
  Localizer: TLocalizer;
begin
  Localizer:=TLocalizer.Create;
  try
    Localizer.Language:='ja';
    AssertEquals(Localizer.GetLanguageText('test', 'testmessagejaonly'), 'これは日本語のテストです')
  finally
    Localizer.Free;
  end;
end;

/// <summary>
/// Confirm that Exception occurs when TLocalizer.GetLanguageText method is called under the following conditions.
/// * Language: def
/// * Keys that exist in the relevant section: None
/// </summary>
procedure TLocalizerTestCase.TestGetLanguageTextDefLanguageNonExistsKey;
var
  Localizer: TLocalizer;
begin
  Localizer:=TLocalizer.Create;
  try
    try
      Localizer.GetLanguageText('test', 'unknownkey');
      Fail('No exceptions were thrown.');
    except
      Self.AssertTrue(True);
    end;
  finally
    Localizer.Free;
  end;
end;

initialization

 RegisterTest(TLocalizerTestCase);
end.

