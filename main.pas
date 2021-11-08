unit Main;

{$mode objfpc}{$H+}

interface

uses
 ClipboardListener, ScriptProcess, ScriptManager, Settings, Utils, LResources, FileUtil, IpHtml, SysUtils,
 Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Clipbrd, Menus, LCLType,
 ActnList, ComCtrls, Classes;

type

 { TMainForm }

 TMainForm = class(TForm)
  WindowTopMost: TAction;
  FStatus: TIpHtmlPanel;
  FSplitter: TSplitter;
  FStatusBar: TStatusBar;
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
  procedure WindowTopMostExecute(Sender: TObject);
 private
  FClipboardListener: TClipboardListener;
  FOnRunScripts: TScriptList;
  FSetting: TSetting;
  procedure SetOnRunScripts(Value: TScriptList);
  procedure ClipboardChanged(Sender: TObject);
  procedure LoadScriptMenus;
  procedure UpdateStatus(HTML: String);
 public
  property OnRunScripts: TScriptList read FOnRunScripts write SetOnRunScripts;
 end;

const
 STATUS_READY = 'Ready';
 STATUS_INPROGRESS = 'In progress';
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
  FStatusBar.Panels[0].Text:= STATUS_READY;
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
  StatusHTML: String;
  StdOut: String;
  StdErr: String;
  MI: TMenuItem;
  Script: TScriptProcess;
begin
  FStatusBar.Panels[0].Text:= STATUS_INPROGRESS;
  FMonitor.Text:= Clipboard.AsText;
  StatusHTML:= '';
  // DONE: Externalization because it is long
  for MI in RunOnCopyMenuRoot do
  begin
    if MI.Checked then
    begin
      FStatusBar.Panels[1].Text:= TScriptFile(MI.Tag).FileName;
      Application.ProcessMessages;
      Script:= TScriptProcess.Create;
      try
        Script.Text:= FMonitor.Text;
        Script.Execute(TScriptFile(MI.Tag).FilePath, StdOut, StdErr);
        if StdOut <> '' then
        begin
          // TODO:Is it possible to classify the display by script name?
          StatusHTML:= StatusHTML + StdOut;
        end;
      finally
        Script.Free;
      end;
    end;
  end;
  FStatusBar.Panels[0].Text:= STATUS_READY;
  FStatusBar.Panels[1].Text:= '';
  UpdateStatus(StatusHTML);
end;

procedure TMainForm.SetOnRunScripts(Value: TScriptList);
var
  S: TScriptFile;
begin
  for S in FOnRunScripts do S.Free;
  FOnRunScripts:= Value;
end;

/// <summary>
/// Initialize the Run Script menu at copy time and the Extended Copy Script menu.
/// </summary>
procedure TMainForm.LoadScriptMenus;
var
  S: TScriptFile;
  MI: TMenuItem;
begin
  OnRunScripts := LoadScriptFiles(GetOnRunScriptDir);
  RunOnCopyMenuRoot.Clear;
  for S in OnRunScripts do
  begin
    // DONE: Is it possible to set a clear title (instead of a file name) for the script?
    MI := TMenuItem.Create(RunOnCopyMenuRoot);
    MI.Caption:= S.DisplayName;
    MI.Checked:= True; // DONE: Memory of check status
    MI.AutoCheck:= True;
    MI.Tag:=PtrInt(S);
    RunOnCopyMenuRoot.Add(MI);
  end;
  FSetting.SetupOnRunMenu(RunOnCopyMenuRoot);
end;

/// <summary>Update Status text</summary>
/// <param name="HTML">HTML Text</param>
procedure TMainForm.UpdateStatus(HTML: String);
var
  r: TResourceStream;
  s: TStringStream;
begin
  try
    r := TResourceStream.Create(HINSTANCE, 'STYLE', RT_RCDATA);
    try
      s := TStringStream.Create();
      try
        s.LoadFromStream(r);
        FStatus.SetHtmlFromStr('<html>' +
          '<head>' +
          '<meta http-equiv="content-type" content="text/html; charset=UTF-8">'+
          '<style type="text/css">' +
          s.DataString +
          '</style>' +
          '</head>'+
          '<body>' +
          EscapeTags(HTML) +
          '</body>' +
          '</html>'
          );
      finally
        s.Free;
      end;
    finally
    end;
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

/// <summary>Always toggle the display state to the forefront.</summary>
procedure TMainForm.WindowTopMostExecute(Sender: TObject);
begin
  WindowTopMost.Checked:= Not WindowTopMost.Checked;
  if WindowTopMost.Checked then
    FormStyle:= fsSystemStayOnTop
  else
    FormStyle:= fsNormal;
end;


end.

