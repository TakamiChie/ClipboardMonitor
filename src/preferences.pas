unit Preferences;

{$mode objfpc}{$H+}

interface

uses
  Windows, Utils, Localization, Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, ComCtrls, IniFiles, LazFileUtils, shlobj, ActiveX, ComObj;

type

  { TPreferenceForm }

  TPreferenceForm = class(TForm)
    LanguageSelector: TComboBox;
    LanguageSelectorLabel: TLabel;
    Register4Startup: TButton;
    TransparencyLabel: TLabel;
    TransparencyValue: TLabel;
    PythonPath: TEditButton;
    Label1: TLabel;
    CancelButton: TButton;
    InterpreterGroups: TGroupBox;
    OKButton: TButton;
    Panel1: TPanel;
    Transparency: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure PythonPathButtonClick(Sender: TObject);
    procedure Register4StartupClick(Sender: TObject);
    procedure TransparencyChange(Sender: TObject);
  private
    FLanguage: TLocalizer;
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
  S, CurLanguage: String;
  LanguageIndex, i: Integer;
begin
  FLanguage:=TLocalizer.Create;
  ini := TMemIniFile.Create(GetSettingRootDir + 'setting.ini');
  try
    PythonPath.Text := ini.ReadString(SECTION_GENERAL, 'PythonPath', 'python');
    Transparency.Position:=ini.ReadInteger(SECTION_GENERAL, 'FormTransparency', 255);
    CurLanguage:= ini.ReadString(SECTION_GENERAL, 'Language', 'def');
  finally
    ini.Free;
  end;
  LanguageIndex:=0;
  i := 0;
  for S in FLanguage.GetLanguageList('gui', 'SupportedLanguages') do
  begin
    FLanguage.Language:=S;
    if S = CurLanguage then LanguageIndex:=i;
    LanguageSelector.Items.Add(S + ':' + FLanguage.GetLanguageText('gui', 'SupportedLanguages'));
    Inc(i);
  end;
  LanguageSelector.ItemIndex:=LanguageIndex;
  // Load Language
  Self.Caption:=FLanguage.GetLanguageText('preferencedlg', 'PreferenceDialog');
  for S in FLanguage.GetSectionKeys('preferencedlg') do
    if Self.FindChildControl(S) <> nil then
      Self.FindChildControl(S).Caption:= FLanguage.GetLanguageText('preferencedlg', S);
end;

procedure TPreferenceForm.FormDestroy(Sender: TObject);
begin
  FLanguage.Free;
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

procedure TPreferenceForm.Register4StartupClick(Sender: TObject);
  function Query(TextKey: String): Integer;
  begin
    Result:= QuestionDlg(FLanguage.GetLanguageText('prompt', 'confirmcaption'),
      FLanguage.GetLanguageText('prompt', TextKey),
      mtConfirmation,
      [mrYes, mrNo], 0);
  end;
  procedure Msg(TextKey: String);
  begin
    MessageDlg(FLanguage.GetLanguageText('prompt', 'information'),
      FLanguage.GetLanguageText('prompt', TextKey),
      mtInformation, [mbOK], 0);
  end;
var
  path: WideString;
  ISlink: IShellLink;
  IPFile: IPersistFile;
begin
  path := WideString(SHGetFolderPathUTF8(CSIDL_STARTUP) + 'CMon.lnk');
  if FileExists(path) then
  begin
    if Query('unregister_startup') = mrYes then
    begin
      if DeleteFile(path) then
        Msg('startup_success')
      else
        Msg('startup_failed')
    end
  end
  else
  begin
    if Query('register_startup') = mrYes then
    begin
      ISlink:=CreateComObject(CLSID_ShellLink) as IShellLink;
      IPFile:= ISlink as IPersistFile;
      ISlink.SetPath(PChar(Application.ExeName));
      ISlink.SetWorkingDirectory(PChar(ExtractFileDir(Application.ExeName)));
      if Succeeded(IPFile.Save(PWChar(path), False)) then
        Msg('startup_success')
      else
        Msg('startup_failed')
    end;
  end;


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

