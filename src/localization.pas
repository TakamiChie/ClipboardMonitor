unit Localization;

{$mode objfpc}{$H+}

interface

uses
  Dialogs, Classes, SysUtils, LCLType, IniFiles;

type
  TLocalizer = class(TObject)
  private
    FLocalizeFile: TMemIniFile;
    FLanguage: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GetLanguageText(Section: String; Key: String): String;
    function LanguageTextExists(Section: String; Key: String): Boolean;
    property Language: String read FLanguage write FLanguage;
  end;

var
  Localizer: TLocalizer = nil;
implementation

constructor TLocalizer.Create;
var
  r: TResourceStream;
  s: TStringList;
begin
  FLanguage:='def';
  r := TResourceStream.Create(HINSTANCE, 'LANG', RT_RCDATA);
  try
    s := TStringList.Create;
    try
      FLocalizeFile := TMemIniFile.Create('');
      s.LoadFromStream(r, TEncoding.UTF8);
      FLocalizeFile.SetStrings(s);
    finally
      s.Free;
    end;
  finally
    r.Free;
  end;

end;

destructor TLocalizer.Destroy;
begin
  FLocalizeFile.Free;
end;

/// <summary>Get the text from the localization file.</summary>
/// <param name='Section'>Section on language data.</param>
/// <param name='Key'>Key on language data.</param>
/// <returns>Localization string.</returns>
/// <exceptions>Localization string does not exist.</exceptions>
function TLocalizer.GetLanguageText(Section: String; Key: String): String;
begin
  if LanguageTextExists(Section, Key) then
    Result:= FLocalizeFile.ReadString(Section + '_' + Key, Language, '')
  else
    raise Exception.Create('Localize File:' + Section + '_' + Key + ' Not found!');
end;

/// <summary>Check to see if any keys are present in the localization file.</summary>
/// <param name='Section'>Section on language data.</param>
/// <param name='Key'>Key on language data.</param>
/// <returns>A value indicating whether or not a localization string exists.</returns>
function TLocalizer.LanguageTextExists(Section: String; Key: String): Boolean;
begin
  Result:= FLocalizeFile.ValueExists(Section + '_' + Key, Language);
end;

end.

