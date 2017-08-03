unit GamePoints;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, math;

type

  { TGamePoint }

  TGamePoint = object
  public
    X, Y: Double;
    procedure Assign(AX, AY: Double);
  end;

function GamePoint(X, Y: Double): TGamePoint;
operator + (A, B: TGamePoint) C: TGamePoint;
operator - (A, B: TGamePoint) C: TGamePoint;
operator*(A: TGamePoint; X: Double)C: TGamePoint;
function abs(A: TGamePoint): Double;
function EnsureAbs(A: TGamePoint; MinAbs, MaxAbs: Double): TGamePoint;

implementation

function GamePoint(X, Y: Double): TGamePoint;
begin
  Result.X := X;
  Result.Y := Y;
end;

operator+(A, B: TGamePoint)C: TGamePoint;
begin
  C.X := A.X + B.X;
  C.Y := A.Y + B.Y;
end;

operator-(A, B: TGamePoint)C: TGamePoint;
begin
  C.X := A.X - B.X;
  C.Y := A.Y - B.Y;
end;

operator*(A: TGamePoint; X: Double)C: TGamePoint;
begin
  C.X := A.X * X;
  C.Y := A.Y * X;
end;

function abs(A: TGamePoint): Double;
begin
  Result := hypot(A.X, A.Y);
end;

function EnsureAbs(A: TGamePoint; MinAbs, MaxAbs: Double): TGamePoint;
begin
  if abs(A) < MinAbs then
    exit(A * (MinAbs / abs(A)));
  if abs(A) > MaxAbs then
    exit(A * (MaxAbs / abs(A)));
  exit(A);
end;

{ TGamePoint }

procedure TGamePoint.Assign(AX, AY: Double);
begin
  X := AX;
  Y := AY;
end;

end.

