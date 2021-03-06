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
    /// <summary>Get the text from the localization file.</summary>
    /// <param name='Section'>Section on language data.</param>
    /// <param name='Key'>Key on language data.</param>
    /// <returns>Localization string.</returns>
    /// <exceptions>Localization string does not exist.</exceptions>
    function GetLanguageText(Section: String; Key: String): String;
    /// <summary>Get a list of languages that exist in the specified key.</summary>
    /// <param name='Section'>Section on language data.</param>
    /// <param name='Key'>Key on language data.</param>
    /// <returns>List of supported languages.</returns>
    function GetLanguageList(Section: String; Key: String): TStringArray;
    /// <summary>Get a list of key strings contained in the specified section.</summary>
    /// <param name='Section'>Section on language data.</param>
    /// <returns>List of key strings to be included in the section.</returns>
    function GetSectionKeys(Section: String): TStringArray;
    /// <summary>Check to see if any keys are present in the localization file.</summary>
    /// <param name='Section'>Section on language data.</param>
    /// <param name='Key'>Key on language data.</param>
    /// <returns>A value indicating whether or not a localization string exists.</returns>
    function LanguageTextExists(Section: String; Key: String): Boolean;
    property Language: String read FLanguage write FLanguage;
  end;

implementation

constructor TLocalizer.Create;
var
  r: TResourceStream;
begin
  FLanguage:='def';
  r := TResourceStream.Create(HINSTANCE, 'LANG', RT_RCDATA);
  try
    FLocalizeFile := TMemIniFile.Create(r, []);
  finally
    r.Free;
  end;
end;

destructor TLocalizer.Destroy;
begin
  FLocalizeFile.Free;
end;

function TLocalizer.GetLanguageText(Section: String; Key: String): String;
begin
  if LanguageTextExists(Section, Key) then
    Result:= FLocalizeFile.ReadString(Section + '_' + Key, Language, '')
  else
    raise Exception.Create('Localize File:' + Section + '_' + Key + ' Not found!');
end;

function TLocalizer.GetLanguageList(Section: String; Key: String): TStringArray;
var
  List: TStringList;
begin
  List:= TStringList.Create;
  try
    FLocalizeFile.ReadSection(Section + '_' + Key, List);
    Result:= List.ToStringArray;
  finally
    List.Free;
  end;
end;

function TLocalizer.GetSectionKeys(Section: String): TStringArray;
var
  List: TStringList;
  i: Integer;
begin
  List:= TStringList.Create;
  try
    FLocalizeFile.ReadSections(List);
    for i := List.Count - 1 downto 0 do
    begin
      if List[i].StartsWith(Section, True) then
        List[i] := List[i].Remove(0, Section.Length + 1)
      else
        List.Delete(i);
    end;
    Result:= List.ToStringArray;
  finally
    List.Free;
  end;

end;

function TLocalizer.LanguageTextExists(Section: String; Key: String): Boolean;
begin
  Result:= FLocalizeFile.ValueExists(Section + '_' + Key, Language);
end;

end.

