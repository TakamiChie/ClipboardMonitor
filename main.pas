unit Main;

{$mode objfpc}{$H+}

interface

uses
 ClipboardListener, ScriptProcess, Settings, Utils, FileUtil, IpHtml, SysUtils,
 Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Clipbrd, Menus,
 ActnList, Classes, Process;

type

 { TMainForm }

 TMainForm = class(TForm)
  FStatus: TIpHtmlPanel;
  UpdateScriptMenu: TAction;
  MenuItem2: TMenuItem;
  OpenScriptDir: TAction;
  FActionList: TActionList;
  FMonitor: TMemo;
  MenuItem1: TMenuItem;
  RunOnCopyMenuRoot: TMenuItem;
  FPopupMenu: TPopupMenu;
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  procedure FormCreate(Sender: TObject);
  procedure FormDestroy(Sender: TObject);
  procedure UpdateScriptMenuExecute(Sender: TObject);
  procedure OpenScriptDirExecute(Sender: TObject);
 private
  FClipboardListener: TClipboardListener;
  FSetting: TSetting;
  procedure ClipboardChanged(Sender: TObject);
  procedure LoadScriptMenus;
  procedure UpdateStatus(HTML: String);
 public

 end;

var
 MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FClipboardListener:= TClipboardListener.Create;
  FClipboardListener.OnClipboardChange := @ClipboardChanged;
  FSetting:= TSetting.Create;
  CopyToUnderscoreScripts; // DONE: Added a process of collectively copying Python scripts named from the underscore to OnRunScriptDir.
  LoadScriptMenus; // DONE: Add a menu that calls this method at any timing.
  FSetting.SetupWindow(Self);
  ClipboardChanged(Sender);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FSetting.SaveSettings(Self);
  CloseAction:=caFree;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FClipboardListener.Free;
end;

procedure TMainForm.ClipboardChanged(Sender: TObject);
var
  OnRunScriptDir, StatusHTML: String;
  StdOut: String;
  StdErr: String;
  MI: TMenuItem;
  Script: TScriptProcess;
begin
  FMonitor.Text:= Clipboard.AsText;
  StatusHTML:= '';
  OnRunScriptDir:= GetOnRunScriptDir;
  // DONE: Externalization because it is long
  for MI in RunOnCopyMenuRoot do
  begin
    if MI.Checked then
    begin
      Script:= TScriptProcess.Create;
      try
        Script.Text:= FMonitor.Text;
        Script.Execute(OnRunScriptDir + MI.Caption, StdOut, StdErr);
        if StdOut <> '' then
        begin
          StatusHTML:= StdOut;
          Break;
        end;
      finally
        Script.Free;
      end;
    end;
  end;
  UpdateStatus(StatusHTML);
end;

/// <summary>
/// Initialize the Run Script menu at copy time and the Extended Copy Script menu.
/// </summary>
procedure TMainForm.LoadScriptMenus;
var
  OnRunScriptDir, s: String;
  OnRunScriptFiles: TStringList;
  MI: TMenuItem;
begin
  OnRunScriptDir:= GetOnRunScriptDir;
  OnRunScriptFiles:= TStringList.Create;
  try
    FindAllFiles(OnRunScriptFiles, OnRunScriptDir, '*.py', False);
    RunOnCopyMenuRoot.Clear;
    for s in OnRunScriptFiles do
    begin
      MI := TMenuItem.Create(RunOnCopyMenuRoot);
      MI.Caption:= ExtractFileName(s);
      MI.Checked:= True; // DONE: Memory of check status
      MI.AutoCheck:= True;
      RunOnCopyMenuRoot.Add(MI);
    end;
    FSetting.SetupOnRunMenu(RunOnCopyMenuRoot);
  finally
    OnRunScriptFiles.Free;
  end;
end;

/// <summary>Update Status text</summary>
/// <param name="HTML">HTML Text</param>
procedure TMainForm.UpdateStatus(HTML: String);
begin
  try
    FStatus.SetHtmlFromStr('<html>' +
      '<head>' +
      '<meta http-equiv="content-type" content="text/html; charset=UTF-8">'+
      '</head>'+
      '<body>' +
      HTML +
      '</body>' +
      '</html>'
      );
  except
    on E: Exception do MessageDlg('Error:'+ E.Message, mtError, [mbCancel], 0);
  end;
end;

{ Begin ActionList }

/// <summary>
/// Open the scripts folder
/// </summary>
procedure TMainForm.OpenScriptDirExecute(Sender: TObject);
begin
  ExecuteProcess('explorer', '"' + GetScriptRootDir + '"');
end;

/// <summary>
/// Update script menu
/// </summary>
procedure TMainForm.UpdateScriptMenuExecute(Sender: TObject);
begin
  LoadScriptMenus;
end;

end.

