unit uVect;
{$MODE objfpc}{$H+}
{$INLINE ON}
{$modeswitch advancedrecords}
interface

uses
    sysutils,math;
type
   rgbColor=record b,g,r:byte; end;

   RefType=(DIFF,SPEC,REFR);// material types, used in radiance()
{
    DIFFUSE,    // 完全拡散面。いわゆるLambertian面。
    SPECULAR,   // 理想的な鏡面。
    REFRACTION, // 理想的なガラス的物質。
}
    RectAxisType=(XY,YZ,XZ);(*平面がどっち向いているか*)

    VecRecord=record
        x,y,z:real;
    end;

    uvwVecRecord=record
      u,v,w:VecRecord;
    end;

    RayRecord=record
       o, d:VecRecord;
     end;
   function CreateRay(o_,d_:VecRecord):RayRecord;

   function ClampVector(v:VecRecord):VecRecord;
   function ColToRGB(v:VecRecord):rgbColor;
const
   BackGroundColor:VecRecord = (x:0;y:0;z:0);
   ZeroVec:VecRecord = (x:0;y:0;z:0);
   OneVec:VecRecord=(x:1;y:1;z:1);
   
function CreateVec(x_,y_,z_:real):VecRecord;
function VecMul(const V1,V2:VecRecord):VecRecord;inline;
function VecNeg(const V:VecRecord):VecRecord;inline;
function VecSQR(const V:VecRecord):real;inline;
function Veclen(V:VecRecord):real;inline;
function VecNorm(V:VecRecord):VecRecord;inline;
function VecDot(const V1,V2 :VecRecord):real;//内積
function VecCross(const V1,V2 :VecRecord):VecRecord;//外積
function VecAdd3(const V1,V2,V3:VecRecord):VecRecord;inline;
procedure VecWriteln(V:VecRecord);
function uvwVecGet(const l:VecRecord):uvwVecRecord;inline;
function VecSphereRef(const w:VecRecord):VecRecord;inline;(*vを法線に半球状に分布する光線を求める*)

operator * (const v1:VecRecord;const r:real)v:VecRecord;inline;
operator / (const v1:VecRecord;const r:real)v:VecRecord;inline;
operator * (const v1,v2:VecRecord)r:real;inline;//内積
operator / (const v1,v2:VecRecord)v:VecRecord;inline;//外積

operator + (const v1,v2:VecRecord)v:VecRecord;inline;
operator - (const v1,v2:VecRecord)v:VecRecord;inline;
operator + (const v1:VecRecord;const r:real)v:VecRecord;inline;
operator - (const v1:VecRecord;const r:real)v:VecRecord;inline;

implementation

function CreateVec(x_,y_,z_:real):VecRecord;
begin
  result.x:=x_;result.y:=y_;result.z:=z_;
end;
function CreateRay(o_,d_:VecRecord):RayRecord;
begin
  result.o:=o_;
  result.d:=d_;
end;

function VecMul(const V1,V2:VecRecord):VecRecord;inline;
begin
  result.x:=V1.x*V2.x;
  result.y:=V1.y*V2.y;
  result.z:=V1.z*V2.z;
end;

function VecNeg(const V:VecRecord):VecRecord;inline;
begin
  result.x:=-V.x;
  result.y:=-V.y;
  result.z:=-V.z;
end;
function VecSQR(const V:VecRecord):real;inline;
begin
  result:=V.x*V.x+V.y*V.y+V.z*V.z;
end;
function Veclen(V:VecRecord):real;inline;
begin
  result:=sqrt(V.x*V.x+V.y*V.y+V.z*V.z);
end;

function VecNorm(V:VecRecord):VecRecord;inline;
begin
  result:=V/VecLen(V) ;
end;
function VecDot(const V1,V2 :VecRecord):real;//内積
begin
  result:=v1.x*v2.x+v1.y*v2.y+v1.z*v2.z;
end;
function VecCross(const V1,V2 :VecRecord):VecRecord;//外積
begin
  result.x:=V1.y * v2.z - v2.y * V1.z;
  result.y:=V1.z * v2.x - v2.z * V1.x;
  result.z:=V1.x * v2.y - v2.x * V1.y;
end;
function VecAdd3(const V1,V2,V3:VecRecord):VecRecord;inline;
begin
  result.x:=V1.x+V2.x+V3.x;
  result.y:=V1.y+V2.y+V3.y;
  result.z:=V1.z+V2.z+V3.z;
  
end;
function FtoSF(r:real):string;
var
  i,j:longint;
begin
  i:=5;j:=5;
  result:=FloatToStrf(r,ffFixed,I,J);
end;

procedure VecWriteln(V:VecRecord);
begin
  writeln(v.x:8:2,' : ',v.y:8:2,' : ',v.z:8:2);
end;

function uvwVecGet(const l:VecRecord):uvwVecRecord;inline;
begin
  result.w:=l;
  if abs(result.w.x)>0.1 then
    result.u:=VecNorm(CreateVec(0,1,0)/result.w) 
  else
    result.u:=VecNorm(CreateVec(1,0,0)/result.w) ;
  result.v:=result.w/result.u;
end;
function VecSphereRef(const w:VecRecord):VecRecord;inline;
var
  r1,r2,r2s:real;
  uvw:uvwVecRecord;
  u,v:VecRecord;
begin
  uvw:=uvwVecGet(w);
  r1:=2*PI*random;r2:=random;r2s:=sqrt(r2);
  {  
     if abs(w.x)>0.1 then
     u:=VecNorm(CreateVec(0,1,0)/w) 
     else
     u:=VecNorm(CreateVec(1,0,0)/w) ;
     v:=w/u;
  }
  result := VecNorm(uvw.u*cos(r1)*r2s + uvw.v*sin(r1)*r2s + uvw.w*sqrt(1-r2));
end;



operator * (const v1:VecRecord;const r:real)v:VecRecord;inline;
begin
  v.x:=v1.x*r;
  v.y:=v1.y*r;
  v.z:=v1.z*r;
end;

operator / (const v1:VecRecord;const r:real)v:VecRecord;inline;
begin
  v.x:=v1.x/r;
  v.y:=v1.y/r;
  v.z:=v1.z/r;
end;

operator * (const v1,v2:VecRecord)r:real;inline;//内積
begin
  r:=v1.x*v2.x+v1.y*v2.y+v1.z*v2.z;
end;

operator / (const v1,v2:VecRecord)v:VecRecord;inline; //外積
begin
  v.x:=V1.y * v2.z - v2.y * V1.z;
  v.y:=V1.z * v2.x - v2.z * V1.x;
  v.z:=V1.x * v2.y - v2.x * V1.y;
end;

operator + (const v1,v2:VecRecord)v:VecRecord;inline;
begin
  v.x:=v1.x+v2.x;
  v.y:=v1.y+v2.y;
  v.z:=v1.z+v2.z;
end;

operator - (const v1,v2:VecRecord)v:VecRecord;inline;
begin
  v.x:=v1.x-v2.x;
  v.y:=v1.y-v2.y;
  v.z:=v1.z-v2.z;
end;

operator + (const v1:VecRecord;const r:real)v:VecRecord;inline;
begin
  v.x:=v1.x+r;
  v.y:=v1.y+r;
  v.z:=v1.z+r;
end;
operator - (const v1:VecRecord;const r:real)v:VecRecord;inline;
begin
  v.x:=v1.x-r;
  v.y:=v1.y-r;
  v.z:=v1.z-r;
end;


function Clamp(x:real):real;inline;
begin
  if x<0 then exit(0);
  if x>1 then exit(1);
  exit(x);
end;

function ClampVector(v:VecRecord):VecRecord;
begin
  result.x:=clamp(v.x);
  result.y:=clamp(v.y);
  result.z:=clamp(v.z);
end;
function ColToByte(x:real):byte;inline;
begin
  result:=trunc(power(x,1/2.2)*255+0.5);
end;
function ColToRGB(v:VecRecord):rgbColor;
begin
  result.r:=ColToByte(v.x);
  result.g:=ColToByte(v.y);
  result.b:=ColToByte(v.z);
end;

begin
end.
