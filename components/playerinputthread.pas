unit PlayerInputThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ThreadQueue, Pipes;

type

  { TPlayerInputThread }

  TPlayerInputThread = class(TThread)
  private
    FInputStream: TOutputPipeStream;
    FQueue: TThreadQueue; // // queue of eoln-terminated pstrings
  protected
    procedure Execute; override; // try the best to write to FInputStream
  public
    function CanWriteLn: Boolean;
    procedure WriteLn(S: AnsiString);
    constructor Create(AInputStream: TOutputPipeStream);
    destructor Destroy; override;
  end;

implementation

{ TPlayerInputThread }

procedure TPlayerInputThread.Execute;
var
  Item: PString;
begin
  repeat
    if FQueue.Count > 0 then
    begin
      Item := PString(FQueue.Front); FQueue.Pop;
      FInputStream.Write(Item^[1], Length(Item^));
      DisposeStr(Item);
    end
    else
      Sleep(20);
  until Terminated;
end;

function TPlayerInputThread.CanWriteLn: Boolean;
begin
  Result := Assigned(Self);
end;

procedure TPlayerInputThread.WriteLn(S: AnsiString);
begin
  FQueue.Push(NewStr(S+LineEnding));
end;

constructor TPlayerInputThread.Create(AInputStream: TOutputPipeStream);
begin
  inherited Create(False);
  FQueue := TThreadQueue.Create;
  FInputStream := AInputStream;
  FreeOnTerminate := False;
end;

destructor TPlayerInputThread.Destroy;
begin
  FreeAndNil(FQueue);
  inherited Destroy;
end;

end.

