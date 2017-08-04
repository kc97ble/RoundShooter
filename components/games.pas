unit Games;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GameObject, contnrs, GamePoints, strutils, math;

const
  R0 = 1/8;
  VH0 = 1/64;
  HPH = 100;

  R1 = 1/64; // Button's radius for displaying only
  V1 = 1/16;
  CH1 = 8;
  D1 = 5;

  R2 = 1/4;
  CH2 = 128;
  TH2 = 32;
  DH2 = 40;
  DM2 = 20;
  DL2 = 10;

  R3 = 1/2;
  CH3 = 256;
  TH3 = 64;

type

  TGame = class;

  { TPlayer }

  TPlayer = class(TGameObject)
  public
    Buttons: TObjectList; //
    Game: TGame;
    Opponent: TPlayer;
    HP, C1, C2, C3: Integer;
    procedure Init;
    function Response(Request: String): String;
  private
    function ResponseShoot(a1, a2: String): String;
    function ResponseHE(a1, a2: String): String;
    function ResponseSG(a1, a2: String): String;
    function ResponseVelocity(a1, a2: String): String;
    function ResponseGetPlayersPartially: String;
    function ResponseGetPlayersExPartially: String;
    function ResponseGetButtons: String;
    function ResponseGetHE: String;
    function ResponseGetSG: String;
  public
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
    HEs: TObjectList; //
    SGs: TObjectList; //
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

{$i players.inc}

constructor TPlayer.Create;
begin
  Buttons := TObjectList.Create(True);
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
  HEs.Clear;
  SGs.Clear;
end;

procedure TGame.NextFrame;
var
  i: Integer;
begin
  Player1.NextFrame;
  Player2.NextFrame;
  FrameNumber += 1;

  for i := 0 to HEs.Count-1 do
    if TGameObject(HEs[i]).T = 0 then
      HEs[i] := nil
    else
      TGameObject(HEs[i]).NextFrame;
  HEs.Pack;

  for i := 0 to SGs.Count-1 do
    if TGameObject(SGs[i]).T = 0 then
      SGs[i] := nil
    else
      TGameObject(SGs[i]).NextFrame;
  SGs.Pack;
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
  HEs := TObjectList.Create(True);
  SGs := TObjectList.Create(True);

  Player1.Game := Self;
  Player1.Opponent := Player2;
  Player2.Game := Self;
  Player2.Opponent := Player1;
end;

destructor TGame.Destroy;
begin
  FreeAndNil(Player1);
  FreeAndNil(Player2);
  FreeAndNil(HEs);
  FreeAndNil(SGs);
  inherited Destroy;
end;

initialization
  Game := TGame.Create;

finalization
  FreeAndNil(Game);

end.

