unit Main;

{$mode objfpc}{$H+}

interface

uses
 ClipboardListener, ScriptProcess, ScriptManager, Settings, Utils, Localization, Preferences, LResources, FileUtil, IpHtml, SysUtils,
 Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Clipbrd, Menus, LCLType,
 ActnList, ComCtrls, Classes;

type

 { TMainForm }

 TMainForm = class(TForm)
   ShowPreferences: TAction;
  MenuItem3: TMenuItem;
  MenuItem4: TMenuItem;
  ConversionScriptsRoot: TMenuItem;
  MenuItem5: TMenuItem;
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
  procedure ConversionScriptsClick(Sender: TObject);
  procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
  procedure FormCreate(Sender: TObject);
  procedure FormDestroy(Sender: TObject);
  procedure FStatusBarClick(Sender: TObject);
  procedure FStatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
   const Rect: TRect);
  procedure ShowPreferencesExecute(Sender: TObject);
  procedure UpdateScriptMenuExecute(Sender: TObject);
  procedure OpenScriptDirExecute(Sender: TObject);
  procedure WindowTopMostExecute(Sender: TObject);
 private
  FClipboardListener: TClipboardListener;
  FOnRunScripts: TScriptList;
  FConversionScripts: TScriptList;
  FSetting: TSetting;
  FLastError: String;
  FLanguage: TLocalizer;
  procedure SetOnRunScripts(Value: TScriptList);
  procedure SetConversionScripts(Value: TScriptList);
  procedure ClipboardChanged(Sender: TObject);
  procedure LoadScriptMenus;
  procedure UpdateStatus(HTML: String);
 public
  property OnRunScripts: TScriptList read FOnRunScripts write SetOnRunScripts;
  property ConversionScripts: TScriptList read FConversionScripts write SetConversionScripts;
  property Language: TLocalizer read FLanguage write FLanguage;
 end;

var
 MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FClipboardListener:= TClipboardListener.Create;
  FSetting:= TSetting.Create;
  FLanguage:= TLocalizer.Create;
  FSetting.SetupWindow(Self);
  CopyToUnderscoreScripts; // DONE: Added a process of collectively copying Python scripts named from the underscore to OnRunScriptDir.
  LoadScriptMenus; // DONE: Add a menu that calls this method at any timing.
  FClipboardListener.OnClipboardChange := @ClipboardChanged;
  ClipboardChanged(Sender);
  FStatusBar.Panels[0].Text:= Language.GetLanguageText('status', 'ready');;
  ConversionScriptsRoot.Caption:=Language.GetLanguageText('gui', 'ConversionScriptsRoot');
  RunOnCopyMenuRoot.Caption:=Language.GetLanguageText('gui', 'RunOnCopyMenuRoot');
  for i := 0 to FActionList.ActionCount - 1 do
    TAction(FActionList.Actions[i]).Caption:=Language.GetLanguageText('gui', FActionList.Actions[i].Name);
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

procedure TMainForm.FStatusBarClick(Sender: TObject);
begin
  if FLastError <> '' then
  begin
    MessageDlg(FLastError, mtError, [mbOK], 0);
  end;
end;

procedure TMainForm.FStatusBarDrawPanel(StatusBar: TStatusBar;
 Panel: TStatusPanel; const Rect: TRect);
var
  ts: TTextStyle;
begin
  ts:= StatusBar.Canvas.TextStyle;
  ts.EndEllipsis:=True;
  ts.Wordbreak:= False;
  StatusBar.Canvas.Brush.Color:= clDefault;
  if FLastError <> '' then StatusBar.Canvas.Font.Color := clRed else StatusBar.Canvas.Font.Color := clDefault;
  StatusBar.Canvas.FillRect(Rect);
  StatusBar.Canvas.TextRect(Rect, 2 + Rect.Left, 2 + Rect.Top, Panel.Text, ts);
end;

procedure TMainForm.ClipboardChanged(Sender: TObject);
var
  StatusHTML: String;
  StdOut: String;
  StdErr: String;
  MI: TMenuItem;
  S: TScriptFile;
  Script: TScriptProcess;
begin
  FStatusBar.Panels[0].Text:= Language.GetLanguageText('status', 'inprogress');
  Application.ProcessMessages;
  FMonitor.Text:= Clipboard.AsText;
  StatusHTML:= '';
  FLastError:= '';
  // DONE: Externalization because it is long
  for MI in RunOnCopyMenuRoot do
  begin
    if MI.Checked then
    begin
      S := FOnRunScripts[MI.Tag];
      FStatusBar.Panels[1].Text:= S.FileName;
      Application.ProcessMessages;
      Script:= TScriptProcess.Create;
      try
        Script.Text:= FMonitor.Text;
        Script.Execute(S.FilePath, StdOut, StdErr);
        if StdOut <> '' then
        begin
          // TODO:Is it possible to classify the display by script name?
          StatusHTML:= StatusHTML + StdOut;
        end;
        if StdErr <> '' then
        begin
          FLastError:= FLastError + '[' + S.FileName + ']' + #13#10 + StdErr + #13#10;
        end;
      finally
        Script.Free;
      end;
    end;
  end;
  FStatusBar.Panels[0].Text:= Language.GetLanguageText('status', 'ready');
  if FLastError <> '' then FStatusBar.Panels[1].Text:=Language.GetLanguageText('status', 'haserror') else FStatusBar.Panels[1].Text:= '';
  UpdateStatus(StatusHTML);
end;

procedure TMainForm.SetOnRunScripts(Value: TScriptList);
var
  S: TScriptFile;
begin
  for S in FOnRunScripts do S.Free;
  FOnRunScripts:= Value;
end;

procedure TMainForm.SetConversionScripts(Value: TScriptList);
var
  S: TScriptFile;
begin
  for S in FConversionScripts do S.Free;
  FConversionScripts := Value;
end;

/// <summary>
/// Initialize the Run Script menu at copy time and the Extended Copy Script menu.
/// </summary>
procedure TMainForm.LoadScriptMenus;
var
  S: TScriptFile;
  MI, MR: TMenuItem;
  Menus: array[0..1] of TMenuItem;
begin
  Menus[0]:= RunOnCopyMenuRoot;
  Menus[1]:= ConversionScriptsRoot;
  OnRunScripts := LoadScriptFiles(GetOnRunScriptDir, Language.Language);
  ConversionScripts := LoadScriptFiles(GetConversionScriptDir, Language.Language);
  RunOnCopyMenuRoot.Tag := Int64(Pointer(OnRunScripts));
  ConversionScriptsRoot.Tag := Int64(Pointer(ConversionScripts));
  for MR in Menus do
  begin
    MR.Clear;
    for S in TScriptList(MR.Tag) do
    begin
      MI := TMenuItem.Create(MR);
      MI.Caption:= S.DisplayName;
      if MR = RunOnCopyMenuRoot then MI.AutoCheck:= True;
      if MR = ConversionScriptsRoot then MI.OnClick:=@ConversionScriptsClick;
      MI.Tag:=S.Index;
      MR.Add(MI);
    end;
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
      r.Free;
    end;
  except
    on E: Exception do MessageDlg('Error:'+ E.Message, mtError, [mbCancel], 0);
  end;
end;

/// <summary>Run the conversion script.</summary>
procedure TMainForm.ConversionScriptsClick(Sender: TObject);
var
  SF: TScriptFile;
  Script: TScriptProcess;
  StdOut, StdErr: String;
begin
  FLastError:= '';
  SF:= FConversionScripts[TMenuItem(Sender).Tag];
  Script:= TScriptProcess.Create;
  try
    Script.Text:= FMonitor.Text;
    Script.Execute(SF.FilePath, StdOut, StdErr);
    if StdOut <> '' then Clipboard.AsText:=StdOut;
    if StdErr <> '' then FLastError:= StdErr;
  finally
    Script.Free;
  end;
  if FLastError <> '' then FStatusBar.Panels[1].Text:=Language.GetLanguageText('status', 'haserror') else FStatusBar.Panels[1].Text:= '';
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

/// <summary>Show Preference</summary>
procedure TMainForm.ShowPreferencesExecute(Sender: TObject);
var
  PreferenceForm: TPreferenceForm;
begin
  PreferenceForm:= TPreferenceForm.Create(Self);
  try
    PreferenceForm.ShowModal;
  finally
    PreferenceForm.Free;
  end;
end;

end.

