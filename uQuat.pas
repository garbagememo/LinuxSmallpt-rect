unit uQuat;
{$MODE objfpc}{$H+}
{$INLINE ON}
{$modeswitch advancedrecords}
interface

uses
    sysutils,math,uVect;
type
    QuatRecord=record
        x,y,z,w:real;
        function conj:QuatRecord;
        function dot(q:QuatRecord):real;
        procedure CreateRotate(Axis:VecRecord;Angle:real);
        function Rotate(p:VecRecord):VecRecord;
        function QuatMul(q:QuatRecord):QuatRecord;
    end;
const
  QuatUnit:QuatRecord=(x:0;y:0;z:0;w:1);
  QuatZero:QuatRecord=(x:0;y:0;z:0;w:0);
function CreateQuat(x_,y_,z_,w_:real):QuatRecord;
function FromRote(v:VecRecord;rad:real):QuatRecord;
function FromRoteX(v:VecRecord;rad:real):QuatRecord;
function FromRoteY(v:VecRecord;rad:real):QuatRecord;
function FromRoteZ(v:VecRecord;rad:real):QuatRecord;

implementation
function CreateQuat(x_,y_,z_,w_:real):QuatRecord;
begin
  result.x:=x_;result.y:=y_;result.z:=z_;result.w:=w_;
end;
function FromRote(v:VecRecord;rad:real):QuatRecord;
var
 ss,cc:real;
begin
  sincos(rad/2,ss,cc);
  result:=CreateQuat(v.x*ss,v.y*ss,v.z*ss,cc);
end;
function FromRoteX(v:VecRecord;rad:real):QuatRecord;
var
 ss,cc:real;
begin
  sincos(rad/2,ss,cc);
  result:=CreateQuat(ss,0,0,cc);
end;
function FromRoteY(v:VecRecord;rad:real):QuatRecord;
var
 ss,cc:real;
begin
  sincos(rad/2,ss,cc);
  result:=CreateQuat(0,ss,0,cc);
end;
function FromRoteZ(v:VecRecord;rad:real):QuatRecord;
var
 ss,cc:real;
begin
  sincos(rad/2,ss,cc);
  result:=CreateQuat(0,0,ss,cc);
end;

function QuatRecord.conj:QuatRecord;
begin
  result:=CreateQuat(-x,-y,-z,w);
end;
function QuatRecord.dot(q:QuatRecord):real;
begin
  result:=x*q.x+y*q.y+z*q.z+w*q.w;
end;

procedure QuatRecord.CreateRotate(Axis:VecRecord;Angle:real);
var
  ss,cc:real;
begin
  sincos(DegToRad(Angle)/2,ss,cc);
  x:=Axis.x*ss;y:=Axis.y*ss; z:=Axis.z*ss;w:=cc;
end;
function QuatRecord.Rotate(p:VecRecord):VecRecord;
var
  x1,y1,z1,w1,xx,yy,zz,ww:real;
begin
  x1:=x;y1:=y;z1:=z;w1:=w;
  xx := (w1 * p.x + y1 * p.z) - (z1 * p.y);
  yy := (w1 * p.y + z1 * p.x) - (x1 * p.z);
  zz := (w1 * p.z + x1 * p.y) - (y1 * p.x);
  ww := (x1 * p.x + y1 * p.y) - (z1 * p.z);
  result:=CreateVec(((ww * x1 + xx * w1) -yy * z1) + zz * y1,
                    ((ww * y1 + yy * w1) -zz * x1) + xx * z1,
                    ((ww * z1 + zz * w1) -xx * y1) + yy * x1);

end;
function QuatRecord.QuatMul(q:QuatRecord):QuatRecord;
var
  x1,y1,z1,w1:real;
begin
  x1:=x;y1:=y;z1:=z;w1:=w;
 
  result:=CreateQuat(w1*q.x + x1  * q.w + y1 * q.z - z1 * q.y,
                     w1*q.y + q.y * q.w + z1 * q.x - x1 * q.z,
                     w1*q.z + z1  * q.w + x1 * q.y - y1 * q.x,
                     w1*q.w - x1  * q.x - y1 * q.y - z1 * q.z);
end;

begin
end.

