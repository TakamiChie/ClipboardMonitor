unit AboutDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, StrUtils, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, LCLIntf, Clipbrd,
  IniFiles, FileInfo, Utils, Localization;

type

  { TAboutDialog }

  TAboutDialog = class(TForm)
    CommitHash: TLabel;
    BuildDate: TLabel;
    CopyToClipboard: TButton;
    Donate: TLabel;
    OKButton: TButton;
    URLLinkLabel: TLabel;
    URL: TLabel;
    Image1: TImage;
    URLLinkLabel1: TLabel;
    VersionString: TLabel;
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
        VersionInfo.Load(HINSTANCE);
        Self.Caption:=Language.GetLanguageText('aboutdlg', 'Caption').Format([Application.Title]);
        Self.VersionString.Caption:=Language.GetLanguageText('aboutdlg', 'VersionStringBase').
          Format([VersionInfo.FixedInfo.FileVersion[0],
            VersionInfo.FixedInfo.FileVersion[1],
            VersionInfo.FixedInfo.FileVersion[2],
            VersionInfo.FixedInfo.FileVersion[3]]);
        Self.CommitHash.Caption:= Language.GetLanguageText('aboutdlg', 'CommitHashBase');
        Self.BuildDate.Caption:= Language.GetLanguageText('aboutdlg', 'BuildDateBase');

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
begin
  Clipboard.AsText:=Self.Caption + #13#10 +
    DupeString('-', 30) + #13#10 +
    VersionString.Caption + #13#10 +
    CommitHash.Caption + #13#10 +
    BuildDate.Caption + #13#10;
end;

procedure TAboutDialog.OKButtonClick(Sender: TObject);
begin
  Self.Close;
end;

end.

