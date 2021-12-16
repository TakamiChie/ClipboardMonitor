unit ScriptProcess;

{$mode objfpc}{$H+}

interface

uses
 Classes, SysUtils, Process;

type
  /// <summary>
  /// A class that summarizes the script execution process.
  /// Any script can be executed with an interpreter.
  /// </summary>
  TScriptProcess = class(TObject)
  private
    FTimeout: Integer;
    FInterpreter: String;
    FText: String;
    FProcess: TProcess;
  public
    /// <summary>Constructer</summary>
    /// <param name="Interpreter">Specify the interpreter name and path to run the script. The default value is Python.</param>
    constructor Create(const Interpreter: String='python');
    /// <summary>Destructor</summary>
    destructor Destroy; override;
    /// <summary>Run the script.</summary>
    /// <param name="ScriptFile">The path of the script file to run.</param>
    /// <param name="StdOut">Standard output.</param>
    /// <param name="StdOut">Standard error.</param>
    /// <returns>Exit Code.</returns>
    function Execute(ScriptFile: String; out StdOut: String; out StdErr: String): Integer;
    property Timeout: Integer read FTimeout write FTimeout;
    property Text: String read FText write FText;
    property Interpreter: String read FInterpreter write FInterpreter;
  end;

implementation

constructor TScriptProcess.Create(const Interpreter: String='python');
begin
  FInterpreter:=Interpreter;
  FTimeout:= 10000;
  FText:='';
end;

destructor TScriptProcess.Destroy;
begin
end;

function TScriptProcess.Execute(ScriptFile: String; out StdOut:String; out StdErr: String): Integer;
var
  InText: String;
  TextResult: TStringList;
  Proc: TProcess;
begin
  StdOut:= '';
  StdErr:= '';
  Proc:= TProcess.Create(nil);
  try
    Proc.Executable:=FInterpreter;
    Proc.Parameters.Add(ScriptFile);
    Proc.Options:= FProcess.Options + [poUsePipes];
    Proc.ShowWindow:=swoHIDE;
    Proc.Execute;
    if FText.Length > 0 then
    begin
      InText:= FText;
      Proc.Input.Write(InText[1], Length(InText));
    end;
    Proc.CloseInput;
    Proc.WaitOnExit(FTimeout);
    TextResult:= TStringList.Create;
    try
      if Proc.Output.NumBytesAvailable > 0 then
      begin
        TextResult.LoadFromStream(Proc.Output, TEncoding.UTF8);
        StdOut:=TextResult.Text;
      end;
      if Proc.Stderr.NumBytesAvailable > 0 then
      begin
        TextResult.LoadFromStream(Proc.Stderr, TEncoding.UTF8);
        StdErr:=TextResult.Text;
      end;
    finally
      TextResult.Free;
    end;
    Result:=Proc.ExitCode;
  finally
    Proc.Free;
  end;
end;

end.

