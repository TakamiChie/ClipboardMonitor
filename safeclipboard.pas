unit SafeClipboard;

{$mode objfpc}{$H+}

interface

uses
 Dialogs, Classes, SysUtils, Clipbrd;

type
  TSafeClipboard = class(TObject)
  private
    function GetText: String;
    procedure SetText(Value: String);
  public
    constructor Create;
    destructor Destroy; override;
    property Text: String read GetText write SetText;
  end;

function GetClipboardText: String;

const RETRY_COUNT = 10;
const RETRY_INTERVAL= 100;

implementation

constructor TSafeClipboard.Create;
var Retry: Integer;
begin
  Retry:= RETRY_COUNT;
  while Retry > 0 do
  begin
    try
      //Clipboard.Open;
      Break;
    except
      on E: Exception do
      begin
        Dec(Retry);
        ShowMessage(E.Message);
        Sleep(RETRY_INTERVAL);
      end;
    end;
  end;
end;

destructor TSafeClipboard.Destroy;
begin
  //Clipboard.Close;
end;

function TSafeClipboard.GetText: String;
begin
  Sleep(100);
  Result:= Clipboard.AsText;
end;

procedure TSafeClipboard.SetText(Value: String);
begin
  Clipboard.AsText:= Value;
end;

/// <summary>Get the text from the clipboard.</summary>
/// <returns>Clipboard text.</returns>
function GetClipboardText: String;
var
  C: TSafeClipboard;
begin
  C:= TSafeClipboard.Create;
  try
    Result:= C.Text;
  finally
    C.Free;
  end;
end;

end.

