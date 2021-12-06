unit AboutDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, StrUtils, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, LCLIntf, Clipbrd,
  IniFiles, FileInfo, LCLType, ValEdit, Grids, Utils, Localization;

type

  { TAboutDialog }

  TAboutDialog = class(TForm)
    CopyToClipboard: TButton;
    Donate: TLabel;
    OKButton: TButton;
    URLLinkLabel: TLabel;
    URL: TLabel;
    Image1: TImage;
    URLLinkLabel1: TLabel;
    VersionValues: TValueListEditor;
    procedure CopyToClipboardClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure URLLinkLabelClick(Sender: TObject);
  private

  public

  end;

implementation

{$R *.lfm}

{ TAboutDialog }

procedure TAboutDialog.FormCreate(Sender: TObject);
var
  Language: TLocalizer;
  VersionInfo: TVersionInfo;
  S: String;
  ini: TMemIniFile;
  BuildInfo: TResourceStream;
  SL: TStringList;
const
  SECTION_GENERAL = 'general';
begin
  ini := TMemIniFile.Create(GetSettingRootDir + 'setting.ini');
  try
    Language:= TLocalizer.Create;
    try
      VersionInfo := TVersionInfo.Create;
      try
        Language.Language:= ini.ReadString(SECTION_GENERAL, 'Language', 'def');
        BuildInfo:= TResourceStream.Create(HINSTANCE, 'BUILDINFO', RT_RCDATA);
        try
          SL:= TStringList.Create;
          try
            SL.LoadFromStream(BuildInfo, TEncoding.UTF8);
            ini.SetStrings(SL);
          finally
            SL.Free;
          end;
        finally
          BuildInfo.Free;
        end;
        VersionInfo.Load(HINSTANCE);
        Self.Caption:=Language.GetLanguageText('aboutdlg', 'Caption').Format([Application.Title]);
        Self.VersionValues.InsertRow(
          Language.GetLanguageText('aboutdlg', 'VersionString'),
          '%d.%d.%d.%d'.Format([VersionInfo.FixedInfo.FileVersion[0],
            VersionInfo.FixedInfo.FileVersion[1],
            VersionInfo.FixedInfo.FileVersion[2],
            VersionInfo.FixedInfo.FileVersion[3]]), True);
        Self.VersionValues.InsertRow(
          Language.GetLanguageText('aboutdlg', 'CommitHash'),
          ini.ReadString('data', 'commit_hash', ''), True);
        Self.VersionValues.InsertRow(
          Language.GetLanguageText('aboutdlg', 'BuildDate'),
          ini.ReadString('data', 'build_date', ''), True);

        for S in Language.GetSectionKeys('aboutdlg') do
          if Self.FindChildControl(S) <> nil then
            Self.FindChildControl(S).Caption:= Language.GetLanguageText('aboutdlg', S);
      finally
        VersionInfo.Free;
      end;
    finally
      Language.Free;
    end;
  finally
    ini.Free;
  end;
end;

procedure TAboutDialog.URLLinkLabelClick(Sender: TObject);
begin
  OpenURL(TLabel(Sender).Caption);
end;

procedure TAboutDialog.CopyToClipboardClick(Sender: TObject);
var
  CopyString: TStringList;
  i: Integer;
begin
  CopyString:= TStringList.Create;
  try
    CopyString.Add(Self.Caption);
    CopyString.Add(DupeString('-', 30));
    for i := 0 to Self.VersionValues.RowCount - 1 do
      CopyString.Add(Self.VersionValues.Rows[i].CommaText.Replace(',', ':'));
    Clipboard.AsText:= CopyString.Text;
  finally
    CopyString.Free;
  end;
end;

procedure TAboutDialog.OKButtonClick(Sender: TObject);
begin
  Self.Close;
end;

end.

