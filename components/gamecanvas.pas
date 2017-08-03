unit GameCanvas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, GameObject, Controls;

procedure Ellipse(Control: TGraphicControl; A: TGameObject);

implementation

function Convert(X: Double; W: Integer): Integer;
begin
  Result := Trunc((X+1)/2 * W);
end;

procedure Ellipse(Control: TGraphicControl; A: TGameObject);
var
  X1, X2, Y1, Y2: Integer;
begin
  X1 := Convert(A.OX - A.R, Control.ClientWidth);
  X2 := Convert(A.OX + A.R, Control.ClientWidth);
  Y1 := Convert(A.OY - A.R, Control.ClientHeight);
  Y2 := Convert(A.OY + A.R, Control.ClientHeight);
  Control.Canvas.Ellipse(X1, Y1, X2, Y2);
end;

end.

