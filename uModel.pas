unit uModel;
{$MODE objfpc}
{$INLINE ON}
{$modeswitch advancedrecords}
interface
uses SysUtils,Classes,uVect,uQuat,math;

//NEEの場合、cosθ*cosφ*天球中の立法角で光源の影響を受けるとする
//cosθ*cosφ×A^2/dist^2/π 球だとcosθ*πr^2/dist^2/π またはcosθ*2(1-cos_a_max)

const
  eps            = 1e-4;
  INF            = 1e20;
  M_1_PI         = 1/pi;
  M_2PI          = 2*pi;
  DefaultSamples = 16;
type

  VertexRecord=record
    isActive:boolean;(*当面Sphere以外実装できないので・・・*)
    cf:VecRecord;
    p,n:VecRecord;
    rad2:real;
    id:integer;
    procedure monitor;
  //omega,ts,tr,preTR,preRad2:real;//debug
  end;

  uvwVecRecord=record
    u,v,w:VecRecord;
  end;


  ModelClass=class
    p,e,c:VecRecord;// position. emission,color
    refl:RefType;
    isLight:boolean;
    uvwRef:uvwVecRecord;
    lp:VecRecord;
    constructor Create(p_,e_,c_:VecRecord;refl_:RefType);
    function intersect(const r:RayRecord):real;virtual;abstract;
    function GetNorm(x:VecRecord):VecRecord;virtual;abstract;
    function GetLightPath(const x:VecRecord):VecRecord;virtual;abstract;
    function omega_1_pi(const l:VecRecord):real;virtual;abstract;//半球に占める立法角の割合
    function DeepCopy:ModelClass;virtual;abstract;
    function CreateLight(id:integer;o:VecRecord):VertexRecord;virtual;abstract;
    function GetLightOMEGA(const vert:VertexRecord;x:VecRecord):real;virtual;abstract;
    function GetVertexOMEGA(const vert:VertexRecord;x:VecRecord):real;virtual;abstract;
  end;

  SphereClass=class(ModelClass)
    rad:real;       //radius
    rad2,tanR:real;
    cos_a_max:real;
    constructor Create(rad_:real;p_,e_,c_:VecRecord;refl_:RefType);
    function DeepCopy:ModelClass;override;
    function intersect(const r:RayRecord):real;override;
    function GetNorm(x:VecRecord):VecRecord;override;
    function GetLightPath(const x:VecRecord):VecRecord;override;
    function omega_1_pi(const l:VecRecord):real;override;
    function CreateLight(id:integer;o:VecRecord):VertexRecord;override;
    function GetLightOMEGA(const vert:VertexRecord;x:VecRecord):real;override;
    function GetVertexOMEGA(const vert:VertexRecord;x:VecRecord):real;override;
  end;

  RectClass=class(ModelClass)
    H1,H2,V1,V2,w,h,area,dist,tempR:Real;
    RA:RectAxisType;
    nl,hv,wv:VecRecord;
    constructor Create(RA_:RectAxisType;H1_,H2_,V1_,V2_:real;p_,e_,c_:VecRecord;refl_:RefType);
    function DeepCopy:ModelClass;override;
    function intersect(const r:RayRecord):real;override;
    function GetNorm(x:VecRecord):VecRecord;override;
    function GetLightPath(const x:VecRecord):VecRecord;override;
    function omega_1_pi(const l:VecRecord):real;override;
    function CreateLight(id:integer;o:VecRecord):VertexRecord;override;
    function GetLightOMEGA(const vert:VertexRecord;x:VecRecord):real;override;
    function GetVertexOMEGA(const vert:VertexRecord;x:VecRecord):real;override;
  end;

  RectAngleClass=class(ModelClass)
    RAary:array[0..5] of RectClass;
    HitID:integer;
    NeeID:integer;    //NEE用変数
    LPid:integer;//LightPath用変数
    RACenter:VecRecord;
    TotalArea,XAreaP,YAreaP,ZAreaP,XpYAreaP:real;
    constructor Create(p1,p2,e_,c_:VecRecord;refl_:RefType);
    function DeepCopy:ModelClass;override;
    function intersect(const r:RayRecord):real;override;
    function GetNorm(x:VecRecord):VecRecord;override;
    function GetLightPath(const x:VecRecord):VecRecord;override;
    function omega_1_pi(const l:VecRecord):real;override;
    function CreateLight(id:integer;o:VecRecord):VertexRecord;override;
    function GetLightOMEGA(const vert:VertexRecord;x:VecRecord):real;override;
    function GetVertexOMEGA(const vert:VertexRecord;x:VecRecord):real;override;
  end;

  RotateRecAngleClass=class(RectAngleClass)
    Quat,RevQuat:QuatRecord;
    OrgDeg:real;
    OrgAxis:VecRecord;
    constructor Create(Axis:VecRecord;deg:real;p1,p2,e_,c_:VecRecord;refl_:RefType);
    function DeepCopy:ModelClass;override;
    function intersect(const r:RayRecord):real;override;
    function GetNorm(x:VecRecord):VecRecord;override;
  end;

  CameraRecord=record
    o,d,cx,cy : VecRecord;
    dist      : real;
    w,h       : integer;
    ratio     : real;
    samples   : integer;
    procedure Setup(o_,d_: VecRecord;w_,h_:integer;ratio_,dist_:real);
    procedure ReWidth(w_:integer);
    procedure SetSamples(sam :integer);
    function Ray(x,y,sx,sy : integer):RayRecord;
  end;

  SceneRecord=record
    SceneName : string;
    mdl       : TList;
    cam       : CameraRecord;
  end;

  function uvwVecGet(const l:VecRecord):uvwVecRecord;inline;
  function VecSphereRef(const w:VecRecord;var uvw:uvwVecRecord):VecRecord;(*vを法線に半球状に分布する光線を求める*)


implementation


function uvwVecGet(const l:VecRecord):uvwVecRecord;inline;
begin
  result.w:=l;
  if abs(result.w.x)>0.1 then
    result.u:=VecNorm(CreateVec(0,1,0)/result.w) 
  else
    result.u:=VecNorm(CreateVec(1,0,0)/result.w) ;
  result.v:=result.w/result.u;
end;

function VecSphereRef(const w:VecRecord;var uvw:uvwVecRecord):VecRecord;inline;
var
  r1,r2,r2s:real;
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

procedure CameraRecord.Setup(o_,d_: VecRecord;w_,h_:integer;ratio_,dist_:real);
begin
  ratio:=ratio_;dist:=dist_;w:=w_;h:=h_;
  o:=o_;d:=VecNorm(d_);
  cx:=CreateVec(ratio*w_/h_,0,0);
  cy:=VecNorm(cx/d_)*ratio;
  samples:=DefaultSamples;
end;
procedure CameraRecord.ReWidth(w_:integer);
begin
  h:=h*w_ div w;
  w:=w_;
end;

procedure CameraRecord.SetSamples(sam :integer );
begin
  samples:=sam;
end;

function CameraRecord.Ray(x,y,sx,sy:integer):RayRecord;
var
  r1,r2,dx,dy:real;
  td:VecRecord;
begin
  r1:=2*random;
  if r1<1 then dx:=sqrt(r1)-1 else dx:=1-sqrt(2-r1);
  r2:=2*random;
  if (r2 < 1) then dy := sqrt(r2)-1 else dy := 1-sqrt(2-r2);
  td:= cy*(((sy + 0.5 + dy)/2 + (h-y-1))/h - 0.5)+cx*(((sx + 0.5 + dx)/2 + x)/w - 0.5)+d;
  td:=VecNorm(td);
  result.o:= td*dist+ o;
  result.d := td;
end;



constructor ModelClass.Create(p_,e_,c_:VecRecord;refl_:RefType);
begin
  p:=p_;e:=e_;c:=c_;refl:=refl_;if VecSQR(e)>0 then isLight:=TRUE else isLight:=false;
end;

constructor SphereClass.Create(rad_:real;p_,e_,c_:VecRecord;refl_:RefType);
begin
  rad:=rad_;rad2:=rad*rad; inherited create(p_,e_,c_,refl_);
end;

function SphereClass.DeepCopy:ModelClass;
begin
  result:=SphereClass.Create(rad,p,e,c,refl);
end;

function SphereClass.intersect(const r:RayRecord):real;
var
  op:VecRecord;
  t,b,det:real;
begin
  op:=p-r.o;
  t:=eps;b:=op*r.d;
  det:=b*b-op*op+rad*rad;
  if det<0 then 
    result:=INF
  else begin
    det:=sqrt(det); t:=b-det;
    if t>eps then 
      result:=t
    else begin
      t:=b+det;
      if t>eps then 
        result:=t
      else
        result:=INF;
    end;
  end;
end;

function SphereClass.GetNorm(x:VecRecord):VecRecord;
begin
  result:=VecNorm(x-p)
end;

function SphereClass.CreateLight(id:integer;o:VecRecord):VertexRecord;
VAR
  pr,t,st:real;
  n:VecRecord;
BEGIN
  result.cf:=e;
  pr:=2.0*PI*random;t:=2.0*arccos(sqrt(1.0-random));
  st:=sin(t);
  result.n:=VecNorm(CreateVec(cos(pr)*st,cos(t),sin(pr)*st) );
  result.p:=p+(result.n*rad);
  result.rad2:=rad2;
  result.id:=id;
  IF (result.p-o)*result.n<0 THEN result.n:=result.n*-1;
END;


function SphereClass.GetLightPath(const x:VecRecord):VecRecord;
var
  eps1,eps2,eps2s,ss,cc:real;
  cos_a,sin_a,phi:real;
  uvw:uvwVecRecord;
begin
  lp:=p-x;
  uvw:=uvwVecGet(lp);
  tanR:=rad2/VecSQR(lp);
  if tanR>1 then begin
    (*半球の内外=cos_aがマイナスとsin_aが＋、－で場合分け*)
    (*半球内部なら乱反射した寄与全てを取ればよい・・はず*)

    eps1:=2*pi*random;eps2:=random;eps2s:=sqrt(eps2);
    sincos(eps1,ss,cc);
    result:=VecNorm(uvwRef.u*(cc*eps2s)+uvwRef.v*(ss*eps2s)+uvwRef.w*sqrt(1-eps2));
  end
  else begin //半球外部の場合;
    cos_a_max := sqrt(1-tanR );
    eps1 := random; eps2:=random;
    cos_a := 1-eps1+eps1*cos_a_max;
    sin_a := sqrt(1-cos_a*cos_a);
    if (1-2*random)<0 then sin_a:=-sin_a; 
    phi := M_2PI*eps2;
    result:=VecNorm(uvw.u*(cos(phi)*sin_a)+uvw.v*(sin(phi)*sin_a)+uvw.w*cos_a);
  end;
end;

function SphereClass.omega_1_pi(const l:VecRecord):real;
begin
  if tanR>1 then begin
    result:=1;
  end
  else begin
    cos_a_max := sqrt(1-tanR );
    result:=2*PI/PI*(1-cos_a_max);//result:=rad2/d^2
  end;
end;

function SphereClass.GetLightOMEGA(const vert:VertexRecord;x:VecRecord):real;
var
  tr:real;
begin
//  tr:=vert.rad2/(VecSQR(vert.p-x));
  tr:=rad2/(VecSQR(p-x));//これはtVertで指定されているのでpで良い
  if tr>1 then
    result:=1
  else begin
    result:=2*PI/PI*(1-sqrt(1-tr) );
  end;
end;

function SphereClass.GetVertexOMEGA(const vert:VertexRecord;x:VecRecord):real;
var
  tr:real;
begin
  tr:=Vert.rad2/VecSQR(Vert.p-x);//ここが怖いところ。LightOMEGAと向きが逆な罠
//  tr:=rad2/(VecSQR(p-x));//上下のどっちかを生かすと長方形の絵が変わるのが怖すぎ
  if tr>1 then 
    result:=1
  else
    result:=2*pi/pi*(1-sqrt(1-tr));
end;
          
constructor RectClass.Create(RA_:RectAxisType;H1_,H2_,V1_,V2_:real;p_,e_,c_:VecRecord;refl_:RefType);
begin
  RA:=RA_;H1:=H1_;H2:=H2_;V1:=V1_;V2:=V2_;h:=H2-H1;w:=V2-V1;
  case RA of
    XY:begin p_.x:=H1; p_.y:=V1; hv:=CreateVec(H2-H1,0,0);wv:=CreateVec(0,V2-V1,0);end;
    XZ:begin p_.x:=H1; p_.z:=V1; hv:=CreateVec(H2-H1,0,0);wv:=CreateVec(0,0,V2-V1);end;
    YZ:begin p_.y:=H1; p_.z:=V1; hv:=CreateVec(0,H2-H1,0);wv:=CreateVec(0,0,v2-v1);end;
  end;
  nl:=VecNorm(VecCross(hv,wv))*-1;
  area:=w*h;
//  writeln('Area=',Area:5:0,' w:h=',w:4:0,':',h:4:0);
  inherited create(p_,e_,c_,refl_);
//  writeln('nl=');VecWriteln(nl);
end;

function RectClass.DeepCopy:ModelClass;
begin
  result:=RectClass.Create(RA,H1,H2,V1,V2,p,e,c,refl);
end;

function RectClass.intersect(const r:RayRecord):real;
var
  t:real;
  pt:VecRecord;
begin
  (**光線と平行に近い場合の処理が必要だが・・・**)
  case RA of
    xy:begin
         result:=INF;
         if abs(r.d.z)<eps then exit;
         t:=(p.z-r.o.z)/r.d.z;
         if t<eps then exit;//result is INF
         pt:=r.o+r.d*t;
         if (pt.x<H2) and (pt.x>H1) and (pt.y<V2)and (pt.y>V1) then result:=t;
       end;(*xy*)
    xz:begin
         result:=INF;
         if abs(r.d.y)<eps then exit;
         t:=(p.y-r.o.y)/r.d.y;
         if t<eps then exit;//result is INF
         pt:=r.o+r.d*t;
         if (pt.x<H2) and (pt.x>H1) and (pt.z<V2)and (pt.z>V1) then result:=t;
       end;(*xz*)
    yz:begin
         result:=INF;
         if abs(r.d.y)<eps then exit;
         t:=(p.x-r.o.x)/r.d.x;
         if t<eps then exit;//result is INF
         pt:=r.o+r.d*t;
         if (pt.y<H2) and (pt.y>H1) and (pt.z<V2)and (pt.z>V1) then result:=t;
       end;(*yz*)
  end;(*case*)
end;

function RectClass.GetNorm(x:VecRecord):VecRecord;
begin
  result:=nl;
end;
function RectClass.CreateLight(id:integer;o:VecRecord):VertexRecord;
var
  uvw:uvwVecRecord;
begin
  result.n:=VecSphereRef(nl,uvw);
  result.p:=p+hv*random+wv*random;
  result.id:=id;
  result.cf:=e;
  result.rad2:=Area;
   //RectAngle時にうまく動かないのでコメントアウトするしか・・??
//  if random>0.5 then result.n:=result.n*-1;
end;

function RectClass.GetLightPath(const x:VecRecord):VecRecord;
var
  eps1,eps2:real;
  r:VecRecord;
begin
  eps1:=random;eps2:=random;
  case RA of
    XY:begin r.x:=p.x+h*eps1;r.y:=p.y+w*eps2; r.z:=p.z end;
    XZ:begin r.x:=p.x+h*eps1;r.z:=p.z+w*eps2; r.y:=p.y end;
    YZ:begin r.y:=p.y+h*eps1;r.z:=p.z+w*eps2; r.x:=p.x end;
  end;
  dist:=VecSQR(r-x);
  lp:=VecNorm(r-x);
  result:=lp;
end;

function RectClass.omega_1_pi(const l:VecRecord):real;
begin
  tempR:=l*GetNorm(l);
  if tempR<0 then tempR:=-tempR;
  result:=tempR*Area/(pi*dist);
end;

function RectClass.GetLightOMEGA(const vert:VertexRecord;x:VecRecord):real;
begin
  dist:=VecSQR(Vert.p-x);
  lp:=VecNorm(Vert.p-x);
  tempR:=lp*GetNorm(lp);if tempR<0 then tempR:=-tempR;
  result:=tempR*Area/(pi*dist);
{
  writeln('dist=',dist:8:2,' tempR=',tempR:8:2,' Area=',Area:8:2,' OMEGA=',result);
  write('r=');VecWriteln(r);
  write('x=');VecWriteln(x);
}
end;

function RectClass.GetVertexOMEGA(const vert:VertexRecord;x:VecRecord):real;
var
  r:VecRecord;
begin
  dist:=VecSQR(Vert.p-x);
  lp:=VecNorm(Vert.p-x);
  tempR:=lp*GetNorm(lp);if tempR<0 then tempR:=-tempR;
  result:=tempR*Area/(pi*dist);
end;

constructor RectAngleClass.Create(p1,p2,e_,c_:VecRecord;refl_:RefType);
begin
  inherited create(p2,e_,c_,refl_);
  (*xy*)
  RAary[0]:=RectClass.Create(XY,p1.x,p2.x,p1.y,p2.y,p1,e_,c_,refl_);
  RAary[1]:=RectClass.Create(XY,p1.x,p2.x,p1.y,p2.y,p2,e_,c_,refl_);
  (*xz*)
  RAary[2]:=RectClass.Create(XZ,p1.x,p2.x,p1.z,p2.z,p1,e_,c_,refl_);
  RAary[3]:=RectClass.Create(XZ,p1.x,p2.x,p1.z,p2.z,p2,e_,c_,refl_);
  (*YZ*)
  RAary[4]:=RectClass.Create(YZ,p1.y,p2.y,p1.z,p2.z,p1,e_,c_,refl_);
  RAary[5]:=RectClass.Create(YZ,p1.y,p2.y,p1.z,p2.z,p2,e_,c_,refl_);  
  (*NEE*)
  RACenter:=(p1+p2)/2;
  TotalArea:=RAary[0].Area+RAary[2].Area+RAary[4].Area;
  XAreaP:=RAary[0].Area/TotalArea;
  YAreaP:=RAary[2].Area/TotalArea;
  ZAreaP:=RAary[4].Area/TotalArea;
  XpYAreaP:=(RAary[0].Area+RAary[2].Area)/TotalArea;
end;

function RectAngleClass.CreateLight(id:integer;o:VecRecord):VertexRecord;
var
  eps:real;
  i:integer;
begin
  eps:=random;
  if eps<XAreaP then (*見える3面に対して確率でどの面になるかをとっている*)
    LPid:=0 
  else if eps<XpYAreaP then 
    LPid:=2
  else
    LPid:=4;
  if random>0.5 then Inc(LPid);
  result:=RAary[LPid].CreateLight(id,o);
end;

function RectAngleClass.DeepCopy:ModelClass;
begin
  result:=RectAngleClass.Create((RACenter*2)-p,p,e,c,refl);
end;

function RectAngleClass.GetLightPath(const x:VecRecord):VecRecord;
var
  eps:real;
  i:integer;
begin
  eps:=random;
  if eps<XAreaP then (*見える3面に対して確率でどの面になるかをとっている*)
    NeeID:=0 
  else if eps<XpYAreaP then 
    NeeID:=2
  else
    NeeID:=4;
  if VecDot((RACenter-x),RAary[NeeID].nl)>0 then Inc(NeeID);
//  result:=RAary[NeeID].GetLightPath(x);

  if (NeeID mod 2)=0 then i:=0 else i:=1;
  RAary[i].GetLightPath(x);RAary[i+2].GetLightPath(x);RAary[i+4].GetLightPath(x);
  result:=RAary[NeeID].lp;
end;

function RectAngleClass.omega_1_pi(const l:VecRecord):real;
var
  d1,d2,d3:integer;
  tP:real;
begin

  case RAary[NeeID].RA of
    XY:tP:=XAreaP;
    XZ:tP:=yAreaP;
    YZ:tP:=ZAreaP;
  end;
  result:=RAary[NeeID].omega_1_pi(l)/tP;

{
  //厳密手順な場合はこちら。光線を3面求める必要があるので効率は落ちるが・・・
  case RAary[NeeID].RA of
    XY:result:=RAary[NeeID  ].omega_1_pi(l)*XAreaP+RAary[NeeID+2].omega_1_pi(l)*YAreaP+RAary[NeeID+4].omega_1_pi(l)*ZAreaP;
    XZ:result:=RAary[NeeID-2].omega_1_pi(l)*XAreaP+RAary[NeeID  ].omega_1_pi(l)*YAreaP+RAary[NeeID+2].omega_1_pi(l)*ZAreaP;
    YZ:result:=RAary[NeeID-4].omega_1_pi(l)*XAreaP+RAary[NeeID-2].omega_1_pi(l)*YAreaP+RAary[NeeID  ].omega_1_pi(l)*ZAreaP;
  end;(*case*)
NeeIDのときの数値の出し方怪しい気がする
}
end;

function RectAngleClass.GetLightOMEGA(const vert:VertexRecord;x:VecRecord):real;
var
  i:integer;
  tp:Real;
begin
  {本来見える3面全部取るべきだが現状よく添字のだ仕方がわからんので}
  RAary[LPid].dist:=VecSQR(vert.p-x);
  result:=RAary[LPid].GetLightOMEGA(vert,x);
end;
  
function RectAngleClass.GetVertexOMEGA(const vert:VertexRecord;x:VecRecord):real;
begin
  result:=GetLightOMEGA(vert,x);
end;


function RectAngleClass.intersect(const r:RayRecord):real;
var
  i:integer;
  d,t:real;
begin
  t:=INF;HitID:=-1;
  for i:=0 to 5 do begin
    d:=RAary[i].intersect(r);
    if d<t then begin
      t:=d;
      HitID:=i;
    end;
  end;
  result:=t;
end;

function RectAngleClass.GetNorm(x:VecRecord):VecRecord;
begin
  result:=RAary[HitID].GetNorm(x);
end;

constructor RotateRecAngleClass.Create(Axis:VecRecord;deg:real;p1,p2,e_,c_:VecRecord;refl_:RefType);
begin
  OrgDeg:=deg;OrgAxis:=Axis;
  Quat.CreateRotate(Axis,deg);
  RevQuat:=Quat.conj;
  inherited Create(p1,p2,e_,c_,refl_);
end;

function RotateRecAngleClass.DeepCopy:ModelClass;
begin
  result:=RotateRecAngleClass.Create(OrgAxis,OrgDeg,(RACenter*2)-p,p,e,c,refl);
end;

function RotateRecAngleClass.intersect(const r:RayRecord):real;
begin
  result:=inherited intersect(CreateRay(RevQuat.rotate(r.o-RACenter)+RACenter,RevQuat.rotate(r.d)) );
end;
function RotateRecAngleClass.GetNorm(x:VecRecord):VecRecord;
begin
  result:=Quat.Rotate(inherited GetNorm(x) );
end;

procedure VertexRecord.monitor;
begin
  write('cf=');VecWriteln(cf);
  write('p=');VecWriteln(p);
  write('n=');VecWriteln(n);
  writeln(' rad2=',rad2:8:2,' id=',id);
end;

begin
end.



