unit GameObject;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, GamePoints, strutils, contnrs;

type

  { TGameObject }

  TGameObject = class
  private
    function GetO: TGamePoint;
    function GetV: TGamePoint;
    procedure SetO(AValue: TGamePoint);
    procedure SetV(AValue: TGamePoint);
  public
    OX, OY, R: Double;
    VX, VY: Double;
    property O: TGamePoint read GetO write SetO;
    property V: TGamePoint read GetV write SetV;
    constructor Create(AOX, AOY, AVX, AVY, AR: Double);
    constructor Create(AO, AV: TGamePoint; AR: Double);
  end;

implementation

{ TGameObject }

function TGameObject.GetO: TGamePoint;
begin
  Result.X := OX;
  Result.Y := OY;
end;

function TGameObject.GetV: TGamePoint;
begin
  Result.X := VX;
  Result.Y := VY;
end;

procedure TGameObject.SetO(AValue: TGamePoint);
begin
  OX := AValue.X;
  OY := AValue.Y;
end;

procedure TGameObject.SetV(AValue: TGamePoint);
begin
  VX := AValue.X;
  VY := AValue.Y;
end;

constructor TGameObject.Create(AOX, AOY, AVX, AVY, AR: Double);
begin
  OX := AOX; OY := AOY;
  VX := AVX; VY := AVY;
  R := AR;
end;

constructor TGameObject.Create(AO, AV: TGamePoint; AR: Double);
begin
  O := AO;
  V := AV;
  R := AR;
end;

end.

