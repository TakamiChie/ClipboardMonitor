unit clipboardlistener;

{$mode objfpc}{$H+}

interface

uses
 Windows, Classes, Messages, SysUtils;

type
  { TClipboardListener }

  TClipboardListener = class(TObject)
  strict private
    FOnClipboardChange: TNotifyEvent;
    FWnd: HWND;
    class function GetSupported: Boolean; static;
    procedure WindowProc(var Msg: TMessage);
  public
    constructor Create;
    destructor Destroy; override;
    property OnClipboardChange: TNotifyEvent read FOnClipboardChange write FOnClipboardChange;
    class property Supported: Boolean read GetSupported;
  end;

implementation

uses
  LCLIntf;

var
  AddClipboardFormatListener: function(Wnd: HWND): BOOL; stdcall;
  RemoveClipboardFormatListener: function(Wnd: HWND): BOOL; stdcall;

procedure InitClipboardFormatListener;
var HUser32: HModule;
begin
  HUser32:= GetModuleHandle(user32);
  Pointer(AddClipboardFormatListener) := GetProcAddress(HUser32, 'AddClipboardFormatListener');
  Pointer(RemoveClipboardFormatListener) := GetProcAddress(HUser32, 'RemoveClipboardFormatListener');
end;

{ TClipboardListener }

constructor TClipboardListener.Create;
begin
  inherited;
  if GetSupported then
  begin
    FWnd:= LCLIntf.AllocateHWnd(@WindowProc);
    if not AddClipboardFormatListener(FWnd) then
      RaiseLastOSError;
  end;
end;

destructor TClipboardListener.Destroy;
begin
  if FWnd <> 0 then
  begin
    RemoveClipboardFormatListener(FWnd);
    LCLIntf.DeallocateHWnd(FWnd);
  end;
  inherited;
end;

class function TClipboardListener.GetSupported: Boolean;
begin
  Result := Assigned(AddClipboardFormatListener) and Assigned(RemoveClipboardFormatListener);
end;

procedure TClipboardListener.WindowProc(var Msg: TMessage);
begin
  if (Msg.msg = WM_CLIPBOARDUPDATE) and Assigned(FOnClipboardChange) then
  begin
    Msg.Result:= 0;
    FOnClipboardChange(Self);
  end;
end;

initialization
  InitClipboardFormatListener;
end.

