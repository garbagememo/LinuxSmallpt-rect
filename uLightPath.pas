unit uLightPath;

{$MODE objfpc}{$H+}
{$INLINE ON}
{$modeswitch advancedrecords}
interface
uses SysUtils,Classes,uVect,uModel,uScene,uFlux,Math;

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
    procedure Clear;
    procedure Add(LP : LightPathRecord);
  end;
  TLightPathFluxClass=class(TFluxClass)
    LPList:LightPathList;
    procedure GetLightPath;
    function Radiance(r:RayRecord;depth:integer):VecRecord;OverRide;
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

procedure LightPathList.Clear;
begin
  LMax:=-1;
end;

procedure LightPathList.Add(LP : LightPathRecord);
begin
  Inc(LMax);
  Ary[LMax]:=LP;
end;

procedure TLightPathFluxClass.GetLightPath;
var
  LP:LightPathRecord;
  tVert,cV:VertexRecord;
  i,depth,k:integer;
  r:RayRecord;
  id:integer;
  obj,m:ModelClass;
  x,n,l,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t,ss,cc,nrd,OMEGA,cos_a_max:real;
  into:boolean;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP,ts:real;
  tDir:VecRecord;
  tu,tv,sw:VecRecord;
  cl,cf:VecRecord;
  uvw:uvwVecRecord;
begin
  LPList.Clear;
  for i:=0 to mdl.Count-1 do begin
    if ModelClass(mdl[i]).isLight then begin
      tVert:=ModelClass(mdl[i]).CreateLight(i,cam.o);
      LP.Clear;
      LP.Add(tVert);
      r.d:=tVert.n;//球であるからこその省略
      r.o:=tVert.p;
      depth:=1;
      repeat
        cf:=tVert.cf;
        if intersect(r,t,id) =false then BREAK;
//get radiance
        obj:=ModelClass(mdl[id]);
        if obj.isLight then Break;
        x:=r.o+r.d*t; n:=obj.GetNorm(x); f:=obj.c;
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
            d:=VecSphereRef(nl,uvw);
            r:=CreateRay(x,d)
          end;(*DIFF*)
          SPEC:begin
            r:=CreateRay(x,(r.d-(n*2*nrd) ) );
          end;(*SPEC*)
          REFR:begin
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
          m:=ModelClass(mdl[tVert.id]);
          sw:=r.d;
          ts:=sw*cV.n;if ts<0 then ts:=-ts;
          OMEGA:=m.GetLightOMEGA(tVert,x)*ts;
//writeln('ts=',ts:8:2,' OMEGA=',OMEGA:8:2);
{
          sw:=r.d;
          tr:=VecSQR(SphereClass(Scene.mdl[tVert.id]).p-x);
          tr:=SphereClass(Scene.mdl[tVert.id]).rad2/tr;
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
}
    //OMEGA算出
          cf:=cf*OMEGA;
        end;
        cV.cf:=cf;
        LP.Add( cV);
        tVert:=cV;
        Inc(Depth)
      until Depth>=LightPathMax;
      LPList.Add(LP);
    end;(*is Light*)
  end;(*obj毎*)
end;


function TLightPathFluxClass.Radiance(r:RayRecord;depth:integer):VecRecord;
var
  id,i,j,tid:integer;
  obj:ModelClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t,m1,ss,cc:real;
  into:boolean;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
  EL,sw,su,sv,l,tw,tu,tv:VecRecord;
  cos_a_max,eps1,eps2,eps2s,cos_a,sin_a,phi,OMEGA:real;
  cl,cf:VecRecord;
  E:integer;
  tVert:VertexRecord;
  uvw:uvwVecRecord;
  function GetLightPathEvent:VecRecord;
  var
    LPRec:LightPathRecord;
    i,j:integer;
    tRay:RayRecord;
    t,ts,LPOmega:real;
    s:ModelClass;
  begin
    result:=ZeroVec;tid:=id;
    for i:=0 to LPList.LMax do begin
      LPRec:=LPList.Ary[i];
      for j:=0 to LPRec.LPMax do begin
        tVert:=LPRec.Ary[j];
        if tVert.id=tid then continue;//光源だったら飛ばすに変えるべき
        s:=ModelClass(mdl[tVert.id]);
        sw:=VecNorm(tVert.p-x);
        tRay.d:=sw;tRay.o:=x;
        if sw*nl<0 then continue;//裏側につきぬけないように
        if intersect(tRay,t,id)=false then continue;
        if id<>tVert.id then CONTINUE;//影がある?
        ts:=sw*tVert.n;
        if ts<0 then ts:=-ts;//球の表裏で変わるので・・・・
        LPOmega:=s.GetVertexOMEGA(tVert,x);
        result:=result+VecMul(f,(tVert.cf*ts*LPOmega) );
        {
                 tr:=VecSQR(s.p-x);//ここが怖いところ。
                 tr:=tVert.rad2/tr;

                 if tr>1 then begin
                 result:=result+VecMul(f,tVert.cf*ts );
                 end
                 else begin

                 cos_a_max := sqrt(1-tr );
                 omega := 2*PI*(1-cos_a_max);
                 result:=result + VecMul(f,(tVert.cf*ts*omega))*M_1_PI;// 1/pi for brdf
                 end;
        }
      end;(*for*)
    end;
  end;
begin
  GetLightPath;//////LPL
//writeln(' DebugY=',DebugY,' DebugX=',DebugX);
  depth:=0;
  id:=0;cl:=ZeroVec;cf:=CreateVec(1,1,1);E:=1;
  while (TRUE) do begin
    Inc(depth);
    if intersect(r,t,id)=false then begin
       result:=cl;
       exit;
    end;
    obj:=ModelClass(mdl[id]);
    x:=r.o+r.d*t; n:=obj.GetNorm(x); f:=obj.c;
    if n*r.d<0 then nl:=n else nl:=n*-1;
    if (f.x>f.y)and(f.x>f.z) then p:=f.x else if f.y>f.z then p:=f.y else p:=f.z;
    tw:=obj.e*E;
    cl:=cl+VecMul(cf,tw);

    if (Depth > 5) or (p = 0) then
       if (random < p) then begin
         f:= f / p;
       end
       else begin
         Result := cl;
         exit;
       end;

    cf:=VecMul(cf,f);
    case obj.refl of
      DIFF:begin
        d:=VecSphereRef(nl,uvw);
{
        r1:=M_2PI*random;r2:=random;r2s:=sqrt(r2);
        w:=nl;
        if abs(w.x)>0.01 then
          u:=VecNorm(CreateVec(0,1,0)/w)
        else begin
          u:=VecNorm(CreateVec(1,0,0)/w);
        end;
        v:=w/u;

        sincos(r1,ss,cc);
        u:=u*(cc*r2s);v:=v*(ss*r2s);w:=w*(sqrt(1-r2));
        d:=VecNorm((u+v)+w);
}

        EL:=GetLightPathEvent;
        tw:=obj.e*e+EL;
        cl:= cl+VecMul(cf,tw );
        E:=0;
        r:=CreateRay(x,d)
      end;(*DIFF*)
      SPEC:begin
        tw:=obj.e*e;
        cl:=cl+VecMul(cf,tw);
        E:=1;tv:=n*2*(n*r.d) ;tv:=r.d-tv;
        r:=CreateRay(x,tv);
      end;(*SPEC*)
      REFR:begin
        tv:=n*2*(n*r.d) ;tv:=r.d-tv;
        RefRay:=CreateRay(x,tv);
        into:= (n*nl>0);
        nc:=1;nt:=1.5; if into then nnt:=nc/nt else nnt:=nt/nc; ddn:=r.d*nl;
        cos2t:=1-nnt*nnt*(1-ddn*ddn);
        if cos2t<0 then begin   // Total internal reflection
          cl:=cl+VecMul(cf,obj.e*E);
          E:=1;
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
          cl:=cl+VecMul(cf,obj.e*E);
          E:=1;
          r:=RefRay;
        end
        else begin//屈折
          cf:=cf*TP;
          cl:=cl+VecMul(cf,obj.e*E);
          E:=1;
          r:=CreateRay(x,tdir);
        end
      end;(*REFR*)
    end;(*CASE*)
  end;(*WHILE LOOP *)
end;

begin
end.


