unit Games;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GameObject, contnrs, GamePoints, strutils;

const
  R0 = 1/8; // Player's radius
  R1 = R0/8; // Button's radius
  V0 = R0/8; // Player's max velocity
  V1 = V0*4; // Button's velocity
  Z1 = 8; // Minimal number of frames between shootings

type

  TGame = class;

  { TPlayer }

  TPlayer = class(TGameObject)
  public
    Buttons: TObjectList; //
    Game: TGame;
    Opponent: TPlayer;
    HP, C1: Integer;
    procedure Init;
    function Response(Request: String): String;
    function ResponseShoot(a1, a2: String): String;
    function ResponseVelocity(a1, a2: String): String;
    function ResponseGetPlayersPartially: String;
    function ResponseGetButtons: String;
    function ResponseGetConstant(a2: String): String;
    procedure ApplyDamage; // calculate new self HP
    procedure NextFrame;
    function GetSummaryText: String;
    constructor Create;
    destructor Destroy; override;
  end;

  { TGame }

  TGame = class(TGameObject)
  public
    Player1, Player2: TPlayer; //
    State: Integer;
    Winner: Integer;
    FrameNumber: Integer;
    procedure Init;
    procedure NextFrame;
    procedure ApplyDamage;
    constructor Create;
    destructor Destroy; override;
  end;

var
  Game: TGame;

implementation

{ TPlayer }

procedure TPlayer.Init;
begin
  O := GamePoint(0, 0);
  V := GamePoint(0, 0);
  R := R0;
  HP := 100;
  Buttons.Clear;
end;

function TPlayer.Response(Request: String): String;
var
  a0, a1, a2, t0, t1: String;
begin
  a0 := ExtractWord(1, Request, [' ']);
  a1 := ExtractWord(2, Request, [' ']);
  a2 := ExtractWord(3, Request, [' ']);

  if a0 = 'VELOCITY' then
    exit(ResponseVelocity(a1, a2));

  if Request = 'GET PLAYERS' then
    exit(Format('%d%s%s%s%s', [Game.FrameNumber, LineEnding,
      ResponseGetPlayersPartially, LineEnding,
      Opponent.ResponseGetPlayersPartially]));

  if Request = 'GET BUTTONS' then
    exit(ResponseGetButtons);

  if (a0 = 'GET') and (a1 = 'CONSTANT') then
    exit(ResponseGetConstant(a2));

  if a0 = 'SHOOT' then
    exit(ResponseShoot(a1, a2));
end;

function TPlayer.ResponseShoot(a1, a2: String): String;
var
  T: TGamePoint;
  OK: Boolean = True;
begin
  if not TryStrToFloat(a1, T.X) then
    OK := False;
  if not TryStrToFloat(a2, T.Y) then
    OK := False;
  if (C1 > 0) or (Abs(O - T) < 1e-9) then
    OK := False;
  if not OK then
    exit(Format('%d %d', [Game.FrameNumber, 1]));

  Buttons.Add(TGameObject.Create(O, EnsureAbs(T-O, V1, V1), R1));
  C1 := Z1;
  exit(Format('%d %d', [Game.FrameNumber, 0]));
end;

function TPlayer.ResponseVelocity(a1, a2: String): String;
begin
  VX := StrToFloatDef(a1, VX);
  VY := StrToFloatDef(a2, VY);
  V := EnsureAbs(V, 0, V0);
  exit(Format('%d %.9f %.9f', [Game.FrameNumber, VX, VY]));
end;

function TPlayer.ResponseGetPlayersPartially: String;
begin
  Result := Format('%.9f %.9f %.9f %.9f %d %d', [OX, OY, VX, VY, HP, C1]);
end;

function TPlayer.ResponseGetButtons: String;
var
  Button: TGameObject;
begin
  Result := Format('%d %d', [Game.FrameNumber, Opponent.Buttons.Count]);
  for Pointer(Button) in Opponent.Buttons do
    with Button do
    Result += LineEnding + Format('%.9f %.9f %.9f %.9f', [OX, OY, VX, VY]);
end;

function TPlayer.ResponseGetConstant(a2: String): String;
begin
  case a2 of
    'R0': Result := Format('%.9f', [R0]);
    'V0': Result := Format('%.9f', [V0]);
    'V1': Result := Format('%.9f', [V1]);
    'Z1': Result := Format('%d', [Z1]);
  otherwise
    Result := '-404';
  end;
  Result := IntToStr(Game.FrameNumber) + ' ' + Result;
end;

procedure TPlayer.ApplyDamage;
var
  Button: TGameObject;
begin
  for Pointer(Button) in Opponent.Buttons do
    if Abs(O - Button.O) <= R + 1e-9 then
      HP -= 5;
end;

procedure TPlayer.NextFrame;
begin
  O := O + V;
  if abs(O) > Game.R - R then
    O := O * ((Game.R - R) / abs(O));

  if C1 > 0 then
    C1 -= 1;
end;

function TPlayer.GetSummaryText: String;
begin
  Result := Format('HP=%d C1=%d', [HP, C1]);
  Result += LineEnding + Format('X=%f Y=%f', [OX, OY]);
  Result += LineEnding + Format('VX=%f VY=%f', [VX, VY]);
end;

constructor TPlayer.Create;
begin
  Buttons := TObjectList.Create;
  Buttons.OwnsObjects := True;
end;

destructor TPlayer.Destroy;
begin
  FreeAndNil(Buttons);
  inherited Destroy;
end;


{ TGame }

procedure TGame.Init;
begin
  FrameNumber := 0;
  O.Assign(0, 0);
  V.Assign(0, 0);
  R := 1;
  Player1.Init;
  Player2.Init;
  Player1.OX -= 0.8;
  Player2.OX += 0.8;
end;

procedure TGame.NextFrame;
var
  Item: TGameObject;
begin
  Player1.NextFrame;
  Player2.NextFrame;

  for Pointer(Item) in Player1.Buttons do
  begin
    Item.O := Item.O + Item.V;
    if Abs(Item.O) > 1.1 then
      Player1.Buttons[Player1.Buttons.IndexOf(Item)] := nil;
  end;
  Player1.Buttons.Pack;

  for Pointer(Item) in Player2.Buttons do
  begin
    Item.O := Item.O + Item.V;
    if Abs(Item.O) > 1.1 then
      Player2.Buttons[Player2.Buttons.IndexOf(Item)] := nil;
  end;
  Player2.Buttons.Pack;

  FrameNumber += 1;
end;

procedure TGame.ApplyDamage;
begin
  Player1.ApplyDamage;
  Player2.ApplyDamage;
  if (Player1.HP <= 0) and (Player2.HP <= 0) then
    Winner := 0
  else if Player1.HP <= 0 then
    Winner := 2
  else if Player2.HP <= 0 then
    Winner := 1;
  if (Player1.HP <= 0) or (Player2.HP <= 0) then
    Game.State := 3; // after playing
end;

constructor TGame.Create;
begin
  Player1 := TPlayer.Create;
  Player2 := TPlayer.Create;

  Player1.Game := Self;
  Player1.Opponent := Player2;
  Player2.Game := Self;
  Player2.Opponent := Player1;
end;

destructor TGame.Destroy;
begin
  FreeAndNil(Player1);
  FreeAndNil(Player2);
  inherited Destroy;
end;

initialization
  Game := TGame.Create;

finalization
  FreeAndNil(Game);

end.

