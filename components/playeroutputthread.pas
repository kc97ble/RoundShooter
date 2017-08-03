unit PlayerOutputThread;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ThreadQueue, Pipes, math;

type

  { TPlayerOutputThread }

  TPlayerOutputThread = class(TThread)
  private
    FOutputStream: TInputPipeStream;
    FQueue: TThreadQueue; // // queue of eoln-terminated pstrings
  protected
    procedure Execute; override; // try the best to read from FOutputStream
  public
    function CanReadLn: Boolean;
    function ReadLn: String;
    constructor Create(AOutputStream: TInputPipeStream);
    destructor Destroy; override;
  end;

implementation

{ TPlayerOutputThread }

procedure TPlayerOutputThread.Execute;
var
  Current: String = '';

  procedure ExtendCurrent;
  var
    t0: Integer;
    Buffer: String[192];
  begin
    t0 := Min(192, FOutputStream.NumBytesAvailable);
    Buffer[0] := Char(FOutputStream.Read(Buffer[1], t0));
    Current += Buffer;
  end;

  function FetchLine: String;
  var
    t0: Integer;
  begin
    t0 := Pos(LineEnding, Current);
    if t0 = 0 then
      exit('');
    t0 += Length(LineEnding)-1;
    Result := LeftStr(Current, t0);
    Delete(Current, 1, t0);
  end;

var
  Item: String = '';
begin
  repeat
    if FOutputStream.NumBytesAvailable > 0 then
    begin
      ExtendCurrent;
      repeat
        Item := FetchLine;
        if Item = '' then
          break;
        FQueue.Push(NewStr(Item));
      until False;
    end
    else
      Sleep(20);
  until Terminated;
end;

function TPlayerOutputThread.CanReadLn: Boolean;
begin
  Result := Assigned(Self) and (FQueue.Count > 0);
end;

function TPlayerOutputThread.ReadLn: String;
var
  Item: PString;
begin
  Item := PString(FQueue.Front); FQueue.Pop;
  Result := Item^;
  if RightStr(Result, Length(LineEnding)) = LineEnding then
    Result := LeftStr(Result, Length(Result) - Length(LineEnding));
  UniqueString(Result);
  DisposeStr(Item);
end;

constructor TPlayerOutputThread.Create(AOutputStream: TInputPipeStream);
begin
  inherited Create(False);
  FQueue := TThreadQueue.Create;
  FOutputStream := AOutputStream;
  FreeOnTerminate := False;
end;

destructor TPlayerOutputThread.Destroy;
begin
  FreeAndNil(FQueue);
  inherited Destroy;
end;

end.

