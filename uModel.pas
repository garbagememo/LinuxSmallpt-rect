unit uModel;
{$MODE objfpc}
{$INLINE ON}
{$modeswitch advancedrecords}
interface
uses SysUtils,Classes,uVect,uQuat,math;

//NEEの場合、cosθｘcosφ×天球中の立法角で光源の影響を受けるとする
//cosθｘcosφ×A^2/dist^2/π 球だとcosθ×πr^2/dist^2/π またはcosθ×２ｘ(1-cos_a_max)

const
  eps            = 1e-4;
  INF            = 1e20;
  M_1_PI         = 1/pi;
  M_2PI          = 2*pi;
  DefaultSamples = 16;
type

  VertexRecord=record
    cf:VecRecord;
    p,n:VecRecord;
    rad2:real;
    id:integer;
  //omega,ts,tr,preTR,preRad2:real;//debug
  end;

  ModelClass=class
    p,e,c:VecRecord;// position. emission,color
    refl:RefType;
    isLight:boolean;
    uvw:uvwVecRecord;
    lp:VecRecord;
    constructor Create(p_,e_,c_:VecRecord;refl_:RefType);
    function intersect(const r:RayRecord):real;virtual;abstract;
    function GetNorm(x:VecRecord):VecRecord;virtual;abstract;
    function GetLightPath(x:VecRecord):VecRecord;virtual;abstract;
    function omega_1_pi(const l:VecRecord):real;virtual;abstract;//半球に占める立法角の割合
  end;

  SphereClass=class(ModelClass)
    rad:real;       //radius
    rad2,tanR:real;
    cos_a_max:real;
    constructor Create(rad_:real;p_,e_,c_:VecRecord;refl_:RefType);
    function intersect(const r:RayRecord):real;override;
    function GetNorm(x:VecRecord):VecRecord;override;
    function GetLightPath(x:VecRecord):VecRecord;override;
    function omega_1_pi(const l:VecRecord):real;override;
  end;

  RectClass=class(ModelClass)
    H1,H2,V1,V2,w,h,area,dist,tempR:Real;
    RA:RectAxisType;
    nl:VecRecord;
    constructor Create(RA_:RectAxisType;H1_,H2_,V1_,V2_:real;p_,e_,c_:VecRecord;refl_:RefType);
    function intersect(const r:RayRecord):real;override;
    function GetNorm(x:VecRecord):VecRecord;override;
    function GetLightPath(x:VecRecord):VecRecord;override;
    function omega_1_pi(const l:VecRecord):real;override;
  end;

  RectAngleClass=class(ModelClass)
    RAary:array[0..5] of RectClass;
    HitID:integer;
    NeeID:integer;    //NEE用変数
    RACenter:VecRecord;
    TotalArea,XAreaP,YAreaP,ZAreaP,XpYAreaP:real;
    constructor Create(p1,p2,e_,c_:VecRecord;refl_:RefType);
    function intersect(const r:RayRecord):real;override;
    function GetNorm(x:VecRecord):VecRecord;override;
    function GetLightPath(x:VecRecord):VecRecord;override;
    function omega_1_pi(const l:VecRecord):real;override;
  end;

  RotateRecAngleClass=Class(RectAngleClass)
    Quat,RevQuat:QuatRecord;
    constructor Create(Axis:VecRecord;deg:real;p1,p2,e_,c_:VecRecord;refl_:RefType);
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
    procedure SetSamples(sam :integer);
    function Ray(x,y,sx,sy : integer):RayRecord;
  end;

  SceneRecord=record
    SceneName : string;
    mdl       : TList;
    cam       : CameraRecord;
  end;


function Intersect(const r:RayRecord;var t:real; var id:integer):boolean;
var
  mdl:TList;
  cam:CameraRecord;
implementation
function Intersect(const r:RayRecord;var t:real; var id:integer):boolean;
var
  d:real;
  i:integer;
begin
  t:=INF;
  for i:=0 to mdl.count-1 do begin
    d:=SphereClass(mdl[i]).intersect(r);
    if d<t then begin
      t:=d;
      id:=i;
    end;
  end;
  result:=(t<inf);
end;

procedure CameraRecord.Setup(o_,d_: VecRecord;w_,h_:integer;ratio_,dist_:real);
begin
  ratio:=ratio_;dist:=dist_;w:=w_;h:=h_;
  o:=o_;d:=VecNorm(d_);
  cx:=CreateVec(ratio*w_/h_,0,0);
  cy:=VecNorm(cx/d_)*ratio;
  samples:=DefaultSamples;
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


function SphereClass.GetLightPath(x:VecRecord):VecRecord;
var
  eps1,eps2,eps2s,ss,cc:real;
  cos_a,sin_a,phi:real;
begin
  lp:=p-x;
  uvw:=uvwVecGet(lp);
  tanR:=rad2/VecSQR(lp);
  if tanR>1 then begin
    (*半球の内外=cos_aがマイナスとsin_aが＋、－で場合分け*)
    (*半球内部なら乱反射した寄与全てを取ればよい・・はず*)

    eps1:=M_2PI*random;eps2:=random;eps2s:=sqrt(eps2);
    sincos(eps1,ss,cc);
    result:=VecNorm(uvw.u*(cc*eps2s)+uvw.v*(ss*eps2s)+uvw.w*sqrt(1-eps2));
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
    result:=2*PI/PI*(1-cos_a_max);//result:=rad2/d^2
  end;
end;
          
constructor RectClass.Create(RA_:RectAxisType;H1_,H2_,V1_,V2_:real;p_,e_,c_:VecRecord;refl_:RefType);
var
  hv,wv:VecRecord;
begin
  RA:=RA_;H1:=H1_;H2:=H2_;V1:=V1_;V2:=V2_;h:=H2-H1;w:=V2-V1;
  case RA of
    XY:begin p_.x:=H1; p_.y:=V1; hv:=CreateVec(H2-H1,0,0);wv:=CreateVec(0,V2-V1,0)*(-1);end;
    XZ:begin p_.x:=H1; p_.z:=V1; hv:=CreateVec(H2-H1,0,0);wv:=CreateVec(0,0,V2-V1)*(-1);end;
    YZ:begin p_.y:=H1; p_.z:=V1; hv:=CreateVec(0,H2-H1,0);wv:=CreateVec(0,0,V2-V1)*(-1);end;
  end;
  nl:=VecNorm(VecCross(hv,wv));
  area:=w*h;
//  writeln('Area=',Area:5:0,' w:h=',w:4:0,':',h:4:0);//これが無いとエラーで落ちる
  inherited create(p_,e_,c_,refl_);
//  writeln('nl=');VecWriteln(nl);
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
function RectClass.GetLightPath(x:VecRecord):VecRecord;
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

function RectAngleClass.GetLightPath(x:VecRecord):VecRecord;
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
{
  case RAary[NeeID].RA of
    XY:tP:=XAreaP;
    XZ:tP:=yAreaP;
    YZ:tP:=XAreaP;
  end;
  result:=RAary[NeeID].omega_1_pi/tP;
}

  //厳密手順な場合はこちら。光線を3面求める必要があるので効率は落ちるが・・・
  case RAary[NeeID].RA of
    XY:result:=  RAary[NeeID].omega_1_pi(l)*XAreaP+RAary[NeeID+2].omega_1_pi(l)*YAreaP+RAary[NeeID+4].omega_1_pi(l)*ZAreaP;
    XZ:result:=RAary[NeeID-2].omega_1_pi(l)*XAreaP  +RAary[NeeID].omega_1_pi(l)*YAreaP+RAary[NeeID+2].omega_1_pi(l)*ZAreaP;
    YZ:result:=RAary[NeeID-4].omega_1_pi(l)*XAreaP+RAary[NeeID-2].omega_1_pi(l)*YAreaP  +RAary[NeeID].omega_1_pi(l)*ZAreaP;
  end;(*case*)

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
  Quat.CreateRotate(Axis,deg);
  RevQuat:=Quat.conj;
  inherited Create(p1,p2,e_,c_,refl_);
end;
function RotateRecAngleClass.intersect(const r:RayRecord):real;
begin
  result:=inherited intersect(CreateRay(RevQuat.rotate(r.o-RACenter)+RACenter,RevQuat.rotate(r.d)) );
end;
function RotateRecAngleClass.GetNorm(x:VecRecord):VecRecord;
begin
  result:=Quat.Rotate(inherited GetNorm(x) );
end;


begin
end.



