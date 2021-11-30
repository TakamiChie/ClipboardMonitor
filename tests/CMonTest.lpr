program CMonTest;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, scriptprocesstestcase, localizertestcase,
  scriptmanagertestcase;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

