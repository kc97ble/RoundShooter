
procedure TPlayer.Init;
begin
  O := GamePoint(0, 0);
  V := GamePoint(0, 0);
  R := R0;
  HP := 100;
  C1 := 0;
  C2 := 0;
  C3 := 0;
  Buttons.Clear;
end;

function TPlayer.Response(Request: String): String;
var
  a0, a1, a2, t0, t1: String;
begin
  a0 := ExtractWord(1, Request, [' ']);
  a1 := ExtractWord(2, Request, [' ']);
  a2 := ExtractWord(3, Request, [' ']);

  case a0 of
    'VELOCITY': exit(ResponseVelocity(a1, a2));
    'HE': exit(ResponseHE(a1, a2));
    'SG': exit(ResponseSG(a1, a2));
    'SHOOT': exit(ResponseShoot(a1, a2));
  end;

  case Request of
    'GET PLAYERS':
      exit(Format('%d%s%s%s%s', [Game.FrameNumber, LineEnding,
        ResponseGetPlayersPartially, LineEnding,
        Opponent.ResponseGetPlayersPartially]));

    'GET PLAYERS EX':
      exit(Format('%d%s%s%s%s', [Game.FrameNumber, LineEnding,
        ResponseGetPlayersExPartially, LineEnding,
        Opponent.ResponseGetPlayersExPartially]));

    'GET BUTTONS': exit(ResponseGetButtons);
    'GET HE': exit(ResponseGetHE);
    'GET SG': exit(ResponseGetSG);
  end;
end;

function TPlayer.ResponseShoot(a1, a2: String): String;
var
  Target: TGamePoint;
  OK: Boolean = True;
begin
  if not TryStrToFloat(a1, Target.X) then
    OK := False;
  if not TryStrToFloat(a2, Target.Y) then
    OK := False;
  if (C1 > 0) or (Abs(O - Target) < 1e-9) then
    OK := False;
  if not OK then
    exit(Format('%d %d', [Game.FrameNumber, 1]));

  Buttons.Add(TGameObject.Create(O, EnsureAbs(Target-O, V1, V1), R1));
  C1 := CH1;
  exit(Format('%d %d', [Game.FrameNumber, 0]));
end;

function TPlayer.ResponseHE(a1, a2: String): String;
var
  Target: TGamePoint;
begin
  if TryStrToFloat(a1, Target.X) and TryStrToFloat(a2, Target.Y) and (C2 = 0) then
  begin
    Game.HEs.Add(TGameObject.Create(Target, GamePoint(0, 0), R2, TH2));
    C2 := CH2;
    exit(Format('%d %d %d', [Game.FrameNumber, 0, C2]));
  end
  else
    exit(Format('%d %d %d', [Game.FrameNumber, 1, C2]));
end;

function TPlayer.ResponseSG(a1, a2: String): String;
var
  Target: TGamePoint;
begin
  if TryStrToFloat(a1, Target.X) and TryStrToFloat(a2, Target.Y) and (C3 = 0) then
  begin
    Game.SGs.Add(TGameObject.Create(Target, GamePoint(0, 0), R3, TH3));
    C3 := CH3;
    exit(Format('%d %d %d', [Game.FrameNumber, 0, C3]));
  end
  else
    exit(Format('%d %d %d', [Game.FrameNumber, 1, C3]));
end;

function TPlayer.ResponseVelocity(a1, a2: String): String;
begin
  VX := StrToFloatDef(a1, VX);
  VY := StrToFloatDef(a2, VY);
  V := EnsureAbs(V, 0, VH0);
  exit(Format('%d %.9f %.9f', [Game.FrameNumber, VX, VY]));
end;

function TPlayer.ResponseGetPlayersPartially: String;
var
  Item: TGameObject;
begin
  for Pointer(Item) in Game.SGs do
    if Abs(Item.O - O) <= Item.R - R + 1e-9 then
      exit(Format('%.9f %.9f 0 0 0 0', [Item.OX, Item.OY]));
  Result := Format('%.9f %.9f %.9f %.9f %d %d', [OX, OY, VX, VY, HP, C1]);
end;

function TPlayer.ResponseGetPlayersExPartially: String;
var
  Item: TGameObject;
begin
  for Pointer(Item) in Game.SGs do
    if Abs(Item.O - O) <= Item.R - R + 1e-9 then
      exit(Format('%.9f %.9f 0 0 0 0 0 0', [Item.OX, Item.OY]));
  Result := Format('%.9f %.9f %.9f %.9f %d %d %d %d', [OX, OY, VX, VY, HP, C1, C2, C3]);
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

function TPlayer.ResponseGetHE: String;
var
  Item: TGameObject;
begin
  Result := Format('%d %d', [Game.FrameNumber, Game.HEs.Count]);
  for Pointer(Item) in Game.HEs do
    Result += LineEnding + Format('%.9f %.9f %d', [Item.OX, Item.OY, Item.T]);
end;

function TPlayer.ResponseGetSG: String;
var
  Item: TGameObject;
begin
  Result := Format('%d %d', [Game.FrameNumber, Game.SGs.Count]);
  for Pointer(Item) in Game.SGs do
    Result += LineEnding + Format('%.9f %.9f %d', [Item.OX, Item.OY, Item.T]);
end;

procedure TPlayer.ApplyDamage;
var
  Item: TGameObject;
begin
  for Pointer(Item) in Opponent.Buttons do
    if Abs(O - Item.O) <= R + 1e-9 then
      HP -= D1;
  for Pointer(Item) in Game.HEs do
    if Item.T = 0 then
      if Abs(O - Item.O) <= Item.R - R + 1e-9 then
        HP -= DH2
      else if Abs(O - Item.O) <= Item.R + 1e-9 then
        HP -= DM2
      else if Abs(O - Item.O) <= Item.R + R + 1e-9 then
        HP -= DL2;
end;

procedure TPlayer.NextFrame;
var
  i: Integer;
begin
  inherited NextFrame;
  if abs(O) > Game.R - R then
    O := O * ((Game.R - R) / abs(O));

  for i := 0 to Buttons.Count-1 do
    if Abs(TGameObject(Buttons[i]).O) > 1.0 then
      Buttons[i] := nil
    else
      TGameObject(Buttons[i]).NextFrame;
  Buttons.Pack;

  C1 := Max(C1-1, 0);
  C2 := Max(C2-1, 0);
  C3 := Max(C3-1, 0);
end;

function TPlayer.GetSummaryText: String;
begin
  Result := Format('HP=%d C1=%d', [HP, C1]);
  Result += LineEnding + Format('X=%f Y=%f', [OX, OY]);
  Result += LineEnding + Format('VX=%f VY=%f', [VX, VY]);
  Result += LineEnding + Format('C2=%d C3=%d', [C2, C3]);
end;
