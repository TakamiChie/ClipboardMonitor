unit main;

{$mode objfpc}{$H+}

interface

uses
 clipboardlistener, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Clipbrd;

type

 { TMainForm }

 TMainForm = class(TForm)
  FMonitor: TMemo;
  FStatus: TMemo;
  procedure FormCreate(Sender: TObject);
  procedure FormDestroy(Sender: TObject);
 private
  FClipboardListener: TClipboardListener;
  procedure ClipboardChanged(Sender: TObject);
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

end.

