program CMon;

{$mode objfpc}{$H+}

uses
 {$IFDEF UNIX}{$IFDEF UseCThreads}
 cthreads,
 {$ENDIF}{$ENDIF}
 Interfaces, // this includes the LCL widgetset
 Forms, Main, ClipboardListener, Utils, ScriptProcess, Settings, ScriptManager,
 Localization, Preferences;

{$R *.res}

begin
 RequireDerivedFormResource:=True;
 Application.Scaled:=True;
 Application.Initialize;
 Application.CreateForm(TMainForm, MainForm);
 Application.Run;
end.

