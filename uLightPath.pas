unit uLightPath;

{$MODE objfpc}{$H+}
{$INLINE ON}
{$modeswitch advancedrecords}
interface
uses SysUtils,Classes,uVect,uModel,uXML,Math;

const 
 LightPathMax=5;
type 
  LightPathRecord=record
    LPMax:integer;
    Ary:array[0..LightPathMax] of VertexRecord;
    procedure clear;
    procedure Add(V_:VertexRecord);
  end;

  LightPathList=record
    LMax:integer;
    ary:array[0..255] of LightPathRecord;
    SR: SceneRecord;
    procedure Clear;
    procedure Add(LP : LightPathRecord);
    procedure SetScene(sr_ :SceneRecord );
    procedure GetLigthPath;
  end;


implementation

procedure LightPathRecord.clear;
begin
  LPMax:=-1;
end;

procedure LightPathRecord.Add(V_:VertexRecord);
begin
  inc(LPMax);
  if LPMax>4 then begin
    writeln('Over List!');
    halt(0);
  end;
  ary[LPMax]:=v_;
end;

procedure LightPathList.SetScene(sr_:SceneRecord);
begin
  SR:=sr_;
end;

procedure LightPathList.Clear;
begin
  LMax:=-1;
end;

procedure LightPathList.Add(LP : LightPathRecord);
begin
  Inc(LMax);
  Ary[LMax]:=LP;
end;

procedure LightPathList.GetLigthPath;
var
  LP:LightPathRecord;
  tVert,cV:VertexRecord;
  i,depth,k:integer;
  r:RayRecord;
  id:integer;
  obj:SphereClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t,ss,cc,nrd,OMEGA,cos_a_max:real;
  into:boolean;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP,ts:real;
  tDir:VecRecord;
  tu,tv,sw:VecRecord;
  cl,cf:VecRecord;
begin
  for i:=0 to SR.spl.Count-1 do begin
    if SphereClass(SR.spl[i]).isLight then begin
      tVert:=SR.GenLight(i);
      LP.Clear;
      LP.Add(tVert);
      r.d:=tVert.n;//球であるからこその省略
      r.o:=tVert.p;
      depth:=1;
      repeat
        cf:=tVert.cf;
        if SR.intersect(r,t,id) =false then BREAK;
//get radiance
        obj:=SphereClass(SR.spl[id]);
        if obj.isLight then Break;
        x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
        nrd:=n*r.d;
        if nrd<0 then nl:=n else nl:=n*-1;
        cV.p:=x;cV.n:=nl;cV.id:=id;
        if (f.x>f.y)and(f.x>f.z) then 
          p:=f.x
        else if f.y>f.z then 
          p:=f.y
        else
          p:=f.z;


        cf:=VecMul(cf,f);
        case obj.refl of
          DIFF:begin
            r1:=M_2PI*random;r2:=random;r2s:=sqrt(r2);
            w:=nl;
            if abs(w.x)>0.01 then u:=VecNorm(CreateVec(0,1,0)/w) else u:=VecNorm(CreateVec(1,0,0)/w);
            v:=w/u;
            sincos(r1,ss,cc);
            u:=u*(cc*r2s);v:=v*(ss*r2s);w:=w*(sqrt(1-r2));
            d:=VecNorm( VecAdd3(u,v,w) );
            r:=CreateRay(x,d)
          end;(*DIFF*)
          SPEC:begin
//            tv:=n*2*nrd ;tv:=r.d-tv;
            r:=CreateRay(x,(r.d-(n*2*nrd) ) );
          end;(*SPEC*)
          REFR:begin
//            tv:=n*2*nrd ;tv:=r.d-tv;
            RefRay:=CreateRay(x,(r.d-(n*2*nrd) ) );
            into:= (n*nl>0);
            nc:=1;nt:=1.5; if into then nnt:=nc/nt else nnt:=nt/nc; ddn:=r.d*nl;
            cos2t:=1-nnt*nnt*(1-ddn*ddn);
            if cos2t<0 then begin   // Total internal reflection
              r:=RefRay;
              continue;
            end;
            if into then q:=1 else q:=-1;
            tdir := VecNorm(r.d*nnt - n*(q*(ddn*nnt+sqrt(cos2t))));
            if into then Q:=-ddn else Q:=tdir*n;
            a:=nt-nc; b:=nt+nc; R0:=a*a/(b*b); c := 1-Q;
            Re:=R0+(1-R0)*c*c*c*c*c;Tr:=1-Re;P:=0.25+0.5*Re;RP:=Re/P;TP:=Tr/(1-P);
            if random<p then begin// 反射
              cf:=cf*RP;
              r:=RefRay;
            end
            else begin//屈折
              cf:=cf*TP;
              r:=CreateRay(x,tdir);
            end
          end;(*REFR*)
        end;(*CASE*)

//OMEGA 算出
        if obj.REFL=DIFF then begin
          sw:=r.d;
          tr:=VecSQR(SphereClass(SR.spl[tVert.id]).p-x);
          tr:=SphereClass(SR.spl[tVert.id]).rad2/tr;
          ts:=sw*cV.n;
          if ts<0 then ts:=-ts;
          if tr>1 then begin
              (*半球内部なら乱反射した寄与全てを取ればよい・・はず*)
            OMEGA:=1;
          end
          else begin //半球外部の場合;
            cos_a_max := sqrt(1-tr );
            OMEGA := 2*PI*(1-cos_a_max)/PI;// 1/pi for brdf
            OMEGA:=OMEGA*ts;
          end;
    //OMEGA算出
          cf:=cf*OMEGA;
        end;
        cV.cf:=cf;
        LP.Add( cV);
        tVert:=cV;
        Inc(Depth)
      until Depth>=LightPathMax;
      Add(LP);
    end;(*is Light*)
  end;(*obj毎*)
end;

begin
end.


