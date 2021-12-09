program CMon;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Main, ClipboardListener, Utils, Settings,
  Localization, Preferences, ScriptManager, ScriptProcess, AboutDialog;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='ClipboardMonitor';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.

