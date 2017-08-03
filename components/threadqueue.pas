unit ThreadQueue;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TThreadQueue }

  TThreadQueue = class(TThreadList)
  public
    procedure Push(Item: Pointer);
    function Front: Pointer;
    function Count: Integer;
    procedure Pop;
  end;

implementation

{ TThreadQueue }

procedure TThreadQueue.Push(Item: Pointer);
var
  List: TList;
begin
  List := LockList;
  try
    List.Add(Item);
  finally
    UnlockList;
  end;
end;

function TThreadQueue.Front: Pointer;
var
  List: TList;
begin
  List := LockList;
  try
    Result := List.First;
  finally
    UnlockList;
  end;
end;

function TThreadQueue.Count: Integer;
var
  List: TList;
begin
  List := LockList;
  try
    Result := List.Count;
  finally
    UnlockList;
  end;
end;

procedure TThreadQueue.Pop;
var
  List: TList;
begin
  List := LockList;
  try
    List.Delete(0);
  finally
    UnlockList;
  end;
end;

end.

