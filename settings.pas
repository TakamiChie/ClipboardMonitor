unit Settings;

{$mode objfpc}{$H+}

interface

uses
 Forms, Utils, Classes, Menus, SysUtils, IniFiles;

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
begin
  if FIniFile.ValueExists(SECTION_WINDOW, 'Left') then Window.Left:= FIniFile.ReadInt64(SECTION_WINDOW, 'Left', Window.Left);
  if FIniFile.ValueExists(SECTION_WINDOW, 'Top') then Window.Top:= FIniFile.ReadInt64(SECTION_WINDOW, 'Top', Window.Top);
  if FIniFile.ValueExists(SECTION_WINDOW, 'Width') then Window.Width:= FIniFile.ReadInt64(SECTION_WINDOW, 'Width', Window.Width);
  if FIniFile.ValueExists(SECTION_WINDOW, 'Height') then Window.Height:= FIniFile.ReadInt64(SECTION_WINDOW, 'Height', Window.Height);
end;

/// <summary>Update the check state of the script menu when copying.</summary>
/// <param name="MenuRoot">OnrunScript Menu Root object</param>
procedure TSetting.SetupOnRunMenu(MenuRoot: TMenuItem);
var
  MI: TMenuItem;
begin
  for MI in MenuRoot do
  begin
    MI.Checked:= FIniFile.ReadBool(SECTION_SCRIPTSTATE_ONRUN, MI.Caption, True)
  end;
end;

/// <summary>Save Ini file</summary>
/// <param name="Window">Main Window object</param>
procedure TSetting.SaveSettings(Window: TForm);
var
  MI: TMenuItem;
begin
  FIniFile.WriteInt64(SECTION_WINDOW, 'Left', Window.Left);
  FIniFile.WriteInt64(SECTION_WINDOW, 'Top', Window.Top);
  FIniFile.WriteInt64(SECTION_WINDOW, 'Width', Window.Width);
  FIniFile.WriteInt64(SECTION_WINDOW, 'Height', Window.Height);
  for MI in TMainForm(Window).RunOnCopyMenuRoot do
  begin
    FIniFile.WriteBool(SECTION_SCRIPTSTATE_ONRUN, MI.Caption, MI.Checked)
  end;
  FIniFile.UpdateFile;
end;

end.

