unit Settings;

{$mode objfpc}{$H+}

interface

uses
 ScriptManager, Forms, Utils, Classes, Menus, SysUtils, IniFiles;

type
  TSetting = class(TObject)
  private
    FIniFile: TMemIniFile;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SetupWindow(Window: TForm);
    procedure SetupOnRunMenu(MenuRoot: TMenuItem);
    procedure SaveSettings(Window: TForm);
  end;


implementation

uses
 Main;

const
  SECTION_GENERAL = 'general';
  SECTION_WINDOW = 'window';
  SECTION_SCRIPTSTATE_ONRUN = 'onrunstate';

constructor TSetting.Create;
begin
  FIniFile:= TMemIniFile.Create(GetSettingRootDir + 'setting.ini');
end;

destructor TSetting.Destroy;
begin
  FIniFile.Free;
end;

/// <summary>Read the window information from the Ini file.</summary>
/// <param name="Window">Main Window object</param>
procedure TSetting.SetupWindow(Window: TForm);
var
  MainForm: TMainForm;
begin
  MainForm:= TMainForm(Window);
  if FIniFile.ValueExists(SECTION_WINDOW, 'Left') then Window.Left:= FIniFile.ReadInt64(SECTION_WINDOW, 'Left', Window.Left);
  if FIniFile.ValueExists(SECTION_WINDOW, 'Top') then Window.Top:= FIniFile.ReadInt64(SECTION_WINDOW, 'Top', Window.Top);
  if FIniFile.ValueExists(SECTION_WINDOW, 'Width') then Window.Width:= FIniFile.ReadInt64(SECTION_WINDOW, 'Width', Window.Width);
  if FIniFile.ValueExists(SECTION_WINDOW, 'Height') then Window.Height:= FIniFile.ReadInt64(SECTION_WINDOW, 'Height', Window.Height);
  if FIniFile.ReadBool(SECTION_WINDOW, 'TopMost', False) then MainForm.WindowTopMost.Execute;
  if FIniFile.ValueExists(SECTION_WINDOW, 'SplitterPosition') then
    MainForm.FMonitor.Height:= FIniFile.ReadInt64(SECTION_WINDOW, 'SplitterPosition', MainForm.FMonitor.Height);
  MainForm.Language.Language:=FIniFile.ReadString(SECTION_GENERAL, 'language', 'def');
end;

/// <summary>Update the check state of the script menu when copying.</summary>
/// <param name="MenuRoot">OnrunScript Menu Root object</param>
procedure TSetting.SetupOnRunMenu(MenuRoot: TMenuItem);
var
  MI: TMenuItem;
begin
  for MI in MenuRoot do
  begin
    MI.Checked:= FIniFile.ReadBool(SECTION_SCRIPTSTATE_ONRUN, MainForm.OnRunScripts[MI.Tag].FileName, True)
  end;
end;

/// <summary>Save Ini file</summary>
/// <param name="Window">Main Window object</param>
procedure TSetting.SaveSettings(Window: TForm);
var
  MI: TMenuItem;
  MainForm: TMainForm;
begin
  MainForm:= TMainForm(Window);
  FIniFile.WriteInt64(SECTION_WINDOW, 'Left', Window.Left);
  FIniFile.WriteInt64(SECTION_WINDOW, 'Top', Window.Top);
  FIniFile.WriteInt64(SECTION_WINDOW, 'Width', Window.Width);
  FIniFile.WriteInt64(SECTION_WINDOW, 'Height', Window.Height);
  FIniFile.WriteBool(SECTION_WINDOW, 'TopMost', MainForm.WindowTopMost.Checked);
  FIniFile.WriteInt64(SECTION_WINDOW, 'SplitterPosition', MainForm.FMonitor.Height);
  for MI in MainForm.RunOnCopyMenuRoot do
  begin
    FIniFile.WriteBool(SECTION_SCRIPTSTATE_ONRUN, MainForm.OnRunScripts[MI.Tag].FileName, MI.Checked)
  end;
  FIniFile.UpdateFile;
end;

end.

