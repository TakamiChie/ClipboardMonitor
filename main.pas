unit main;

{$mode objfpc}{$H+}

interface

uses
 clipboardlistener, Constants, FileUtil, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
 ExtCtrls, Clipbrd, Menus, ActnList, Classes;

type

 { TMainForm }

 TMainForm = class(TForm)
  OpenScriptDir: TAction;
  FActionList: TActionList;
  FMonitor: TMemo;
  FStatus: TMemo;
  MenuItem1: TMenuItem;
  RunOnCopyMenuRoot: TMenuItem;
  FPopupMenu: TPopupMenu;
  procedure FormCreate(Sender: TObject);
  procedure FormDestroy(Sender: TObject);
  procedure OpenScriptDirExecute(Sender: TObject);
 private
  FClipboardListener: TClipboardListener;
  procedure ClipboardChanged(Sender: TObject);
  procedure LoadScriptMenus;
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
  LoadScriptMenus;
  ClipboardChanged(Sender);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  FClipboardListener.Free;
end;

procedure TMainForm.ClipboardChanged(Sender: TObject);
begin
  FMonitor.Text:= Clipboard.AsText;
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
      MI.Caption:=s;
      MI.RadioItem:= True;
    end;
  finally
    OnRunScriptFiles.Free;
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

end.

