unit Preferences;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  EditBtn, ComCtrls;

type

  { TPreferenceForm }

  TPreferenceForm = class(TForm)
    Register4Startup: TButton;
    Label2: TLabel;
    TransparencyValue: TLabel;
    PythonPath: TEditButton;
    Label1: TLabel;
    CancelButton: TButton;
    InterpreterGroups: TGroupBox;
    OKButton: TButton;
    Panel1: TPanel;
    Transparency: TTrackBar;
  private

  public

  end;

var
  PreferenceForm: TPreferenceForm;

implementation

{$R *.lfm}

end.

