unit Settings;

{$mode objfpc}{$H+}

interface

uses
  ScriptManager, ActnList, Forms, Utils, Classes, Menus, SysUtils, IniFiles;

/// <summary>Read the window information from the Ini file.</summary>
/// <param name="Window">Main Window object</param>
/// <param name="GeneralOnly">
/// Specify True when not reading values other than the GENERAL section
/// when reading configuration dialog settings.
/// Default False.
/// </param>
procedure SetupWindow(Window: TForm; GeneralOnly: Boolean=False);
/// <summary>Update the check state of the script menu when copying.</summary>
/// <param name="MenuRoot">OnrunScript Menu Root object</param>
procedure SetupOnRunMenu(MenuRoot: TMenuItem);
/// <summary>Save Ini file</summary>
/// <param name="Window">Main Window object</param>
procedure SaveSettings(Window: TForm);

implementation

uses
  Main;

const
  SECTION_GENERAL = 'general';
  SECTION_WINDOW = 'window';
  SECTION_SCRIPTSTATE_ONRUN = 'onrunstate';

function GetIniFile: TMemIniFile;
begin
  Result:= TMemIniFile.Create(GetSettingRootDir + 'setting.ini');
end;

procedure SetupWindow(Window: TForm; GeneralOnly: Boolean=False);
var
  MainForm: TMainForm;
  i: Integer;
  Ini: TMemIniFile;
begin
  Ini:= GetIniFile;
  try
    MainForm:= TMainForm(Window);
    if not GeneralOnly then
    begin
      if Ini.ValueExists(SECTION_WINDOW, 'Left') then Window.Left:= Ini.ReadInt64(SECTION_WINDOW, 'Left', Window.Left);
      if Ini.ValueExists(SECTION_WINDOW, 'Top') then Window.Top:= Ini.ReadInt64(SECTION_WINDOW, 'Top', Window.Top);
      if Ini.ValueExists(SECTION_WINDOW, 'Width') then Window.Width:= Ini.ReadInt64(SECTION_WINDOW, 'Width', Window.Width);
      if Ini.ValueExists(SECTION_WINDOW, 'Height') then Window.Height:= Ini.ReadInt64(SECTION_WINDOW, 'Height', Window.Height);
      if Ini.ReadBool(SECTION_WINDOW, 'TopMost', False) then MainForm.WindowTopMost.Execute;
      if Ini.ValueExists(SECTION_WINDOW, 'SplitterPosition') then
        MainForm.FMonitor.Height:= Ini.ReadInt64(SECTION_WINDOW, 'SplitterPosition', MainForm.FMonitor.Height);
    end;
    MainForm.Language.Language:=Ini.ReadString(SECTION_GENERAL, 'Language', 'def');
    MainForm.PythonInterpreter:=Ini.ReadString(SECTION_GENERAL, 'PythonPath', 'python');
    MainForm.AlphaBlendValue:=Ini.ReadInteger(SECTION_GENERAL, 'FormTransparency', 255);
    MainForm.AlphaBlend:=MainForm.AlphaBlendValue<255;
    MainForm.PlaySoundEnabled:= ini.ReadBool(SECTION_GENERAL, 'PlaySoundOnCopy', False);
    MainForm.PlaySoundPath:= ini.ReadString(SECTION_GENERAL, 'PlaySoundPath', '');
    MainForm.ResidesTT:=ini.ReadBool(SECTION_GENERAL, 'ResidesTasktray', True);
    MainForm.FStatusBar.Panels[0].Text:= MainForm.Language.GetLanguageText('status', 'ready');;
    MainForm.ConversionScriptsRoot.Caption:=MainForm.Language.GetLanguageText('gui', 'ConversionScriptsRoot');
    MainForm.RunOnCopyMenuRoot.Caption:=MainForm.Language.GetLanguageText('gui', 'RunOnCopyMenuRoot');
    for i := 0 to MainForm.FActionList.ActionCount - 1 do
      TAction(MainForm.FActionList.Actions[i]).Caption:=
        MainForm.Language.GetLanguageText('gui', MainForm.FActionList.Actions[i].Name);
  finally
    Ini.Free;
  end;
end;

procedure SetupOnRunMenu(MenuRoot: TMenuItem);
var
  MI: TMenuItem;
  Ini: TMemIniFile;
  i: Integer;
begin
  Ini:= GetIniFile;
  try
    i := 0;
    for MI in MenuRoot do
    begin
      MI.Checked:= Ini.ReadBool(SECTION_SCRIPTSTATE_ONRUN, MainForm.OnRunScripts[MI.Tag].FileName, True);
      Inc(i);
    end;
  finally
    Ini.Free;
  end;
end;

procedure SaveSettings(Window: TForm);
var
  MainForm: TMainForm;
  Ini: TMemIniFile;
  i: Integer;
begin
  Ini:= GetIniFile;
  try
    MainForm:= TMainForm(Window);
    Ini.WriteInt64(SECTION_WINDOW, 'Left', Window.Left);
    Ini.WriteInt64(SECTION_WINDOW, 'Top', Window.Top);
    Ini.WriteInt64(SECTION_WINDOW, 'Width', Window.Width);
    Ini.WriteInt64(SECTION_WINDOW, 'Height', Window.Height);
    Ini.WriteBool(SECTION_WINDOW, 'TopMost', MainForm.WindowTopMost.Checked);
    Ini.WriteInt64(SECTION_WINDOW, 'SplitterPosition', MainForm.FMonitor.Height);
    for i := 0 to MainForm.RunOnCopyMenuRoot.Count - 1 do
    begin
      Ini.WriteBool(SECTION_SCRIPTSTATE_ONRUN, MainForm.OnRunScripts[i].FileName, MainForm.RunOnCopyMenuRoot[i].Checked)
    end;
  finally
    Ini.Free;
  end;
end;

end.

