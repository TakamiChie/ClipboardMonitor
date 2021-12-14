unit Main;

{$mode objfpc}{$H+}

interface

uses
  ClipboardListener, ScriptProcess, ScriptManager, Settings, Utils, Localization, Preferences, AboutDialog,
  LResources, FileUtil, IpHtml, SysUtils,
  Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Clipbrd, Menus, LCLType,
  ActnList, ComCtrls, Classes, MMSystem, Windows;

type
  { TMainForm }
  TMainForm = class(TForm)
    AboutThisApp: TAction;
    AppExit: TAction;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    ShowPreferences: TAction;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    ConversionScriptsRoot: TMenuItem;
    MenuItem5: TMenuItem;
    TrayIcon: TTrayIcon;
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
    procedure FStatusBarClick(Sender: TObject);
    procedure FStatusBarDrawPanel(StatusBar: TStatusBar; Panel: TStatusPanel;
     const Rect: TRect);
    /// <summary>Run the conversion script.</summary>
    procedure ConversionScriptsClick(Sender: TObject);
    procedure TrayIconDblClick(Sender: TObject);
    /// <summary>Open the scripts folder</summary>
    procedure OpenScriptDirExecute(Sender: TObject);
    /// <summary>Update script menu</summary>
    procedure UpdateScriptMenuExecute(Sender: TObject);
    /// <summary>Always toggle the display state to the forefront.</summary>
    procedure WindowTopMostExecute(Sender: TObject);
    /// <summary>Show Preference</summary>
    procedure ShowPreferencesExecute(Sender: TObject);
    /// <summary>Displaying the About dialog.</summary>
    procedure AboutThisAppExecute(Sender: TObject);
    /// <summary>Exit the application.</summary>
    procedure AppExitExecute(Sender: TObject);
  private
    FClipboardListener: TClipboardListener;
    FOnRunScripts: TScriptList;
    FConversionScripts: TScriptList;
    FLastError: String;
    FPythonInterpreter: String;
    FPlaySoundEnabled: Boolean;
    FPlaySoundPath: String;
    FLanguage: TLocalizer;
    FDisplayedTBLong: LongInt; // WindowLong in the state that the icon is displayed on the taskbar
    FTasktrayIconLong: LongInt; // WindowLong of the resident state in the task tray
    procedure SetOnRunScripts(Value: TScriptList);
    procedure SetConversionScripts(Value: TScriptList);
    procedure ClipboardChanged(Sender: TObject);
    /// <summary>
    /// Initialize the Run Script menu at copy time and the Extended Copy Script menu.
    /// </summary>
    procedure LoadScriptMenus;
    /// <summary>Update Status text</summary>
    /// <param name="HTML">HTML Text</param>
    procedure UpdateStatus(HTML: String);
    /// <summary>Method called to get a value indicating whether or not to reside in the task tray.</summary>
    /// <returns>Value indicating whether the application should be resident in the task tray.</returns>
    function GetResidesTT: Boolean;
    /// <summary>Method called when the value indicating whether or not to reside in the task tray is updated.</summary>
    /// <params name="Value">Value indicating whether the application should be resident in the task tray.</param>
    procedure SetResidesTT(Value: Boolean);
  public
    property OnRunScripts: TScriptList read FOnRunScripts write SetOnRunScripts;
    property ConversionScripts: TScriptList read FConversionScripts write SetConversionScripts;
    property Language: TLocalizer read FLanguage write FLanguage;
    property PythonInterpreter: String read FPythonInterpreter write FPythonInterpreter;
    property PlaySoundEnabled: Boolean read FPlaySoundEnabled write FPlaySoundEnabled;
    property PlaySoundPath: String read FPlaySoundPath write FPlaySoundPath;
    property ResidesTT: Boolean read GetResidesTT write SetResidesTT;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FClipboardListener:= TClipboardListener.Create;
  FLanguage:= TLocalizer.Create;
  TrayIcon.Hint:= Self.Caption;
  FDisplayedTBLong:=GetWindowLong(Application.Handle, GWL_EXSTYLE);
  FTasktrayIconLong:=FDisplayedTBLong or WS_EX_TOOLWINDOW and not WS_EX_APPWINDOW;
  SetupWindow(Self);
  CopyToUnderscoreScripts; // DONE: Added a process of collectively copying Python scripts named from the underscore to OnRunScriptDir.
  LoadScriptMenus; // DONE: Add a menu that calls this method at any timing.
  FClipboardListener.OnClipboardChange := @ClipboardChanged;
  ClipboardChanged(Sender);
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if ResidesTT then
  begin
    CloseAction:=caNone;
    Self.Hide;
  end
  else
  begin
    SaveSettings(Self);
    CloseAction:=caFree;
  end;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  OnRunScripts:= nil;
  ConversionScripts:= nil;
  FLanguage.Free;
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
  if PlaySoundEnabled and FileExists(PlaySoundPath) then
    sndPlaySound(PChar(PlaySoundPath), SND_ASYNC + SND_FILENAME + SND_NODEFAULT);
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
      Script:= TScriptProcess.Create(PythonInterpreter);
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
  SetupOnRunMenu(RunOnCopyMenuRoot);
end;

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

function TMainForm.GetResidesTT: Boolean;
begin
  Result:= TrayIcon.Visible;
end;

procedure TMainForm.SetResidesTT(Value: Boolean);
begin
  TrayIcon.Visible := Value;
  if Value then
  begin
    Self.BorderIcons:=[biSystemMenu];
    SetWindowLong(Application.Handle,
      GWL_EXSTYLE, FTasktrayIconLong);
  end
  else
  begin
    Self.BorderIcons:=[biMinimize, biSystemMenu];
    SetWindowLong(Application.Handle,
      GWL_EXSTYLE, FDisplayedTBLong);
  end;
end;

procedure TMainForm.ConversionScriptsClick(Sender: TObject);
var
  SF: TScriptFile;
  Script: TScriptProcess;
  StdOut, StdErr: String;
begin
  FLastError:= '';
  SF:= FConversionScripts[TMenuItem(Sender).Tag];
  Script:= TScriptProcess.Create(PythonInterpreter);
  try
    Script.Text:= FMonitor.Text;
    Script.Execute(SF.FilePath, StdOut, StdErr);
    if StdOut <> '' then Clipboard.AsText:=StdOut.TrimRight([#13, #10]);
    if StdErr <> '' then FLastError:= StdErr;
  finally
    Script.Free;
  end;
  if FLastError <> '' then FStatusBar.Panels[1].Text:=Language.GetLanguageText('status', 'haserror') else FStatusBar.Panels[1].Text:= '';
end;

procedure TMainForm.TrayIconDblClick(Sender: TObject);
begin
  if Self.Showing then
    Self.Hide
  else
    Self.Show;
end;

{ Begin ActionList }

procedure TMainForm.OpenScriptDirExecute(Sender: TObject);
begin
  ExecuteProcess('explorer', '"' + GetScriptRootDir + '"');
end;

procedure TMainForm.UpdateScriptMenuExecute(Sender: TObject);
begin
  LoadScriptMenus;
end;

procedure TMainForm.WindowTopMostExecute(Sender: TObject);
begin
  WindowTopMost.Checked:= Not WindowTopMost.Checked;
  if WindowTopMost.Checked then
    FormStyle:= fsSystemStayOnTop
  else
    FormStyle:= fsNormal;
end;

procedure TMainForm.ShowPreferencesExecute(Sender: TObject);
var
  PreferenceForm: TPreferenceForm;
begin
  PreferenceForm:= TPreferenceForm.Create(Self);
  try
    if PreferenceForm.ShowModal = mrOK then SetupWindow(Self, True);
  finally
    PreferenceForm.Free;
  end;
end;

procedure TMainForm.AboutThisAppExecute(Sender: TObject);
var
  Dialog: TAboutDialog;
begin
  Dialog:=TAboutDialog.Create(Self);
  try
    Dialog.ShowModal;
  finally
    Dialog.Free;
  end;
end;

procedure TMainForm.AppExitExecute(Sender: TObject);
begin
  SaveSettings(Self);
  Application.Terminate;
end;

end.

