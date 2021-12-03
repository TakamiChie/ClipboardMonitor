unit Preferences;

{$mode objfpc}{$H+}

interface

uses
  Utils, Localization, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, ComCtrls, IniFiles;

type

  { TPreferenceForm }

  TPreferenceForm = class(TForm)
    LanguageSelector: TComboBox;
    Label3: TLabel;
    Register4Startup: TButton;
    Label2: TLabel;
    TransparencyValue: TLabel;
    PythonPath: TEditButton;
    Label1: TLabel;
    CancelButton: TButton;
    InterpreterGroups: TGroupBox;
    OKButton: TButton;
    Panel1: TPanel;
    Transparency: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PythonPathButtonClick(Sender: TObject);
    procedure TransparencyChange(Sender: TObject);
  private

  public

  end;

var
  PreferenceForm: TPreferenceForm;

implementation

{$R *.lfm}

const
  SECTION_GENERAL = 'general';

{ TPreferenceForm }

procedure TPreferenceForm.FormCreate(Sender: TObject);
var
  ini: TMemIniFile;
  L: TLocalizer;
  S, CurLanguage: String;
  LanguageIndex, i: Integer;
begin
  ini := TMemIniFile.Create(GetSettingRootDir + 'setting.ini');
  try
    PythonPath.Text := ini.ReadString(SECTION_GENERAL, 'PythonPath', 'python');
    Transparency.Position:=ini.ReadInteger(SECTION_GENERAL, 'FormTransparency', 255);
    CurLanguage:= ini.ReadString(SECTION_GENERAL, 'Language', 'def');
  finally
    ini.Free;
  end;
  LanguageIndex:=0;
  L:= TLocalizer.Create;
  try
    i:=0;
    for S in L.GetLanguageList('gui', 'SupportedLanguages') do
    begin
      L.Language:=S;
      if S = CurLanguage then LanguageIndex:=i;
      LanguageSelector.Items.Add(S + ':' + L.GetLanguageText('gui', 'SupportedLanguages'));
      Inc(i);
    end;
  finally
    L.Free;
  end;
  LanguageSelector.ItemIndex:=LanguageIndex;
end;

procedure TPreferenceForm.PythonPathButtonClick(Sender: TObject);
var
  OFD: TOpenDialog;
begin
  OFD := TOpenDialog.Create(Self);
  try
    OFD.Filter:='Python|python.exe';
    if OFD.Execute then PythonPath.Text:=OFD.FileName;
  finally
    OFD.Free;
  end;
end;

procedure TPreferenceForm.TransparencyChange(Sender: TObject);
begin
  TransparencyValue.Caption := IntToStr(trunc((Transparency.Position / Transparency.Max) * 100)) + '%';
  Self.AlphaBlend:= Transparency.Position < 255;
  Self.AlphaBlendValue:=Transparency.Position;
end;

procedure TPreferenceForm.OKButtonClick(Sender: TObject);
var
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(GetSettingRootDir + 'setting.ini');
  try
    ini.WriteString(SECTION_GENERAL, 'PythonPath', PythonPath.Text);
    ini.WriteInteger(SECTION_GENERAL, 'FormTransparency', Transparency.Position);
    ini.WriteString(SECTION_GENERAL, 'Language', String(LanguageSelector.Text).Split([':'])[0]);
  finally
    ini.Free;
  end;
end;

end.

