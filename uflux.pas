{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 51 Franklin Street - Fifth Floor, Boston, MA 02110-1335, USA.   *
 *                                                                         *
 ***************************************************************************

}
unit uFlux;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}


interface

uses
  LCLIntf, LCLType,
  Classes, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,LMessages,
  SysUtils, uVect,uModel,uScene,uQuat,math;

const
  MSG_NEWLINE         = WM_USER + 0;
  MSG_DecThreadCount = WM_USER+1;
  MSG_DrawNextScene  = WM_USER+2;
  DefaultSaveFile    = 'out.png';
  PathSeparator     = '/';(*IF Windows THEN \*)

type
  { TRenderThread }

  TLineBuffer=array[0..1980] of VecRecord;
  (*TColor=r,g,b*)

  TRenderThread = class(TThread)
  private
    fStatusText: string;
    DoneCalc:boolean;
    procedure InitRend;
    procedure DoRend;
    procedure DoneRend;
  protected
    procedure Execute; override;
  public
    LineBuffer:TLineBuffer;
    wide,h,samps:integer;
    mdl:TList;
    cam:CameraRecord;
    yRender:integer;
    function Intersect(const r:RayRecord;var t:real; var id:integer):boolean;
    function Radiance(r:RayRecord;depth:integer):VecRecord;virtual;
    constructor Create(CreateSuspended: boolean);
  end;
  TNEEThread=class(TRenderThread)
    function Radiance(r : RayRecord;Depth:integer ):VecRecord;OverRide;
  end;                
  TLoopThread=class(TRenderThread)
    function Radiance(r : RayRecord;Depth:integer ):VecRecord;OverRide;
  end;                


  { TMainForm }

  TMainForm = class(TForm)
    cmdSave: TButton;
    cmdRender: TButton;
    AlgolCombo: TComboBox;
    Aloglthm: TLabel;
    Model: TLabel;
    SceneCombo: TComboBox;
    SaveDlg: TSaveDialog;
    StrWidth: TEdit;
    StrHeight: TEdit;
    StrSampleCount: TEdit;
    StrThreadCount: TEdit;
    ImgRender: TImage;
    label1: TLabel;
    label2: TLabel;
    label3: TLabel;
    label4: TLabel;
    label5: TLabel;
    lblTime: TLabel;

    procedure AlgolComboChange(Sender: TObject);
    procedure cmdRenderClick(Sender: TObject);
    procedure cmdSaveClick(Sender: TObject);
    procedure SceneComboChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SaveDlgClick(Sender: TObject);
  private
    ModelIndex:integer;
    AlgolIndex:integer;
    MinimamHeight:integer;
  public
    ThreadNum:integer;
    samps:integer;
    StartTime:Int64;
    ThreadList:TList;
    yAxis:integer;
    procedure RenderSetup;
    function isAllDone:boolean;
    function GetYAxis:integer;
  end;

    
var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }


function SecToTime(Sec : integer) : STRING;
var
  H, M, S : STRING;
  ZH, ZM, ZS : integer;
begin
  ZH := Sec DIV 3600;
  ZM := Sec DIV 60 - ZH * 60;
  ZS := Sec - (ZH * 3600 + ZM * 60);
  H := IntToStr(ZH);
  if (H = '0') then
    H := '00';
  M := IntToStr(ZM);
  if (M = '0') then
    M := '00';
  S := IntToStr(ZS);
  if (S = '0') then
    S := '00';
  Result := H + ':' + M + ':' + S;
end;

function TMainForm.isAllDone:boolean;
var
  i:integer;
begin
  isAllDone:=TRUE;
  for i:=0 to ThreadNum-1 do begin
     if TRenderThread(ThreadList[i]).DoneCalc=false then begin
        isAllDone:=false;
       exit;
     end;
  end;
end;

function TMainForm.GetYAxis:integer;
begin
   yAxis:=yAxis+1;
   result:=yAxis;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i:integer;
begin
    DoubleBuffered := TRUE;
    TOP:=10;
    Left:=10;
    SRList.InitSceneRecord(320,240);
    for i:=0 to SRList.MaxIndex do
      SceneCombo.Items.Add(SRList.SRL[i].SceneName);
    SceneCombo.ItemIndex:=1;
    ModelIndex:=1;
    AlgolCombo.Items.Add('Original');
    AlgolCombo.Items.Add('Next Event');
    AlgolCombo.Items.Add('Non Loop');
    AlgolCombo.Items.Add('LightPath');
    AlgolCombo.ItemIndex:=1;
    AlgolIndex:=1;
    MinimamHeight:=Height;
    Randomize;
end;

procedure TMainForm.SaveDlgClick(Sender: TObject);
begin
  if (SaveDlg.Execute) then
    imgRender.Picture.SaveToFile(SaveDlg.FileName);
end;

procedure TMainForm.RenderSetup;
var
  RenderThread: TRenderThread;
  i:integer;
begin
   if Assigned(ThreadList) then begin
      ThreadList.Destroy;
   end;

   imgRender.Width := strtoint(strWidth.Text);
   imgRender.Height := strtoint(strHeight.Text);
   ThreadNum:=StrToInt(StrThreadCount.Text);
   samps:=StrToInt(StrSampleCount.Text);
   //add
   imgRender.Picture.Bitmap.Width:=imgRender.Width;
   imgRender.Picture.Bitmap.Height:=imgRender.Height;
   //Orginal source
   imgRender.Canvas.Brush.Color := clBlack;
   imgRender.Canvas.FillRect(0,0, imgRender.Width, imgRender.Height);

   cmdRender.Enabled:=false;
   ClientWidth := imgRender.Left + 5 + imgRender.Width;
   if (ImgRender.Top+5+ImgRender.Height) >MinimamHeight then
      ClientHeight := imgRender.Top + 5 + imgRender.Height;
   ThreadList:=TList.Create;
   yAxis:=-1;
   for i:=0 to ThreadNum-1 do begin
      case AlgolIndex of
        0:RenderThread:=TRenderThread.Create(True);
        1:RenderThread:=TNEEThread.Create(true);
        2:RenderThread:=TLoopThread.Create(true);
        else RenderThread:=TRenderThread.Create(true);
      end;

      // True parameter it doesnt start automatically
      if Assigned(RenderThread.FatalException) then
        raise RenderThread.FatalException;
      RenderThread.wide:=imgRender.Width;
      RenderThread.h:=imgRender.Height;
      RenderThread.samps:=StrToInt(StrSampleCount.text);
      RenderThread.yRender:=GetYAxis;
      RenderThread.DoneCalc:=false;
      ThreadList.Add(RenderThread);
   end;
   StartTime:=GetTickCount64; 
end;
procedure TMainForm.cmdRenderClick(Sender: TObject);
var
  RenderThread: TRenderThread;
  i:integer;
begin
   RenderSetup;
   for i:=0 to ThreadList.Count-1 do begin
     TRenderThread(ThreadList[i]).Start;
   end;
end;

procedure TMainForm.AlgolComboChange(Sender: TObject);
begin
  AlgolIndex:=AlgolCombo.ItemIndex;
end;


procedure TMainForm.cmdSaveClick(Sender: TObject);
begin
  if SaveDlg.Execute then ImgRender.Picture.SaveToFile(SaveDlg.Filename);
end;

procedure TMainForm.SceneComboChange(Sender: TObject);
begin
  ModelIndex:=SceneCombo.ItemIndex;
end;


{ TRenderThread }

procedure TRenderThread.InitRend;
begin
   mdl:=SRList.DeepCopyModel(MainForm.ModelIndex);
   cam:=SRList.SRL[MainForm.ModelIndex].cam;
   cam.ReWidth(wide);
end;

procedure TRenderThread.DoRend;
// this method is only called by Synchronize(@ShowStatus) and therefore
// executed by the main thread
// The main thread can access GUI elements, for example MainForm.Caption.
var
  x : integer;
begin
  MainForm.Caption := fStatusText;
  if DoneCalc=false then begin
    for x:=0 to Wide-1 do begin
      MainForm.ImgRender.Canvas.Pixels[x,yRender]:=
          ColToByte(LineBuffer[x].x)+         //red
          ColToByte(LineBuffer[x].y)*256+     //green
          ColToByte(LineBuffer[x].z)*256*256; //blune
    end;
    MainForm.LblTime.Caption:=SecToTime((GetTickCount64 - MainForm.startTime) DIV 1000);
    yRender:=MainForm.GetYAxis;
  end;
end;
procedure TRenderThread.DoneRend;
var
   st : string;
begin
   if MainForm.isAllDone then begin
      MainForm.yAxis:=-1;
      MainForm.cmdRender.Enabled:=TRUE;
      MainForm.Caption:='TRenderThread Time: '+FormatDateTime('YYYY-MM-DD HH:NN:SS',Now);
      
     MainForm.imgRender.Picture.SaveToFile(DefaultSaveFile); 
  end;
end;

procedure TRenderThread.Execute;
var
  x,y,sx,sy,s:integer;
  temp       : VecRecord;
  tColor,r : VecRecord;
  StatusText1:string;
begin
  y:=yRender;
  Synchronize(@InitRend); 
  fStatusText := 'Render Running ...';
  StatusText1:=fStatusText;
  while y<h do begin
    for x:=0 to wide-1 do begin
     r:=CreateVec(0, 0, 0);
     tColor:=ZeroVec;
     for sy:=0 to 1 do begin
       for sx:=0 to 1 do begin
         for s:=0 to samps-1 do begin
           temp:=Radiance(Cam.Ray(x,y,sx,sy),0);
           temp:= temp/ samps;
           r:= r+temp;
         end;(*samps*)
         temp:= ClampVector(r)* 0.25;
         tColor:=tColor+ temp;
         r:=CreateVec(0, 0, 0);
       end;(*sx*)
     end;(*sy*)
     LineBuffer[x]:=tColor;
   end;(*x*)
   fStatusText:=StatusText1+'y='+IntToStr(y);
   Synchronize(@DoRend);
   y:=yRender;
  end;(*y*)
  DoneCalc:=TRUE;
  Synchronize(@DoneRend);
 end;


constructor TRenderThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  DoneCalc:=false;
  yRender:=0;
  inherited Create(CreateSuspended);
end;

function TRenderThread.Intersect(const r:RayRecord;var t:real; var id:integer):boolean;
var
  d:real;
  i:integer;
begin
  t:=INF;
  for i:=0 to mdl.count-1 do begin
    d:=ModelClass(mdl[i]).intersect(r);
    if d<t then begin
      t:=d;
      id:=i;
    end;
  end;
  result:=(t<inf);
end;


function TRenderThread.Radiance(r:RayRecord;depth:integer):VecRecord;
var
  id:integer;
  obj:ModelClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t:real;
  into:boolean;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
begin
  id:=0;depth:=depth+1;
  if intersect(r,t,id)=false then begin
    result:=ZeroVec;exit;
  end;
  obj:=ModelClass(mdl[id]);
  x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
  if VecDot(n,r.d)<0 then nl:=n else nl:=n*-1;
  if (f.x>f.y)and(f.x>f.z) then
    p:=f.x
  else if f.y>f.z then 
    p:=f.y
  else
    p:=f.z;
  if (Depth > 5) or (p = 0) then begin
      if (random < p) then begin
        f:= f / p;
        if (p = 1) and (f.x = 1) and (f.y = 1) and (f.z = 1) then begin
          Result := obj.e;
          exit;
        end;
      end
      else begin
        Result := obj.e;
        exit;
      end;
  end;
  case obj.refl of
    DIFF:begin
      r1:=2*PI*random;r2:=random;r2s:=sqrt(r2);
      w:=nl;
      if abs(w.x)>0.1 then
        u:=VecNorm(MidOneVec/w)
      else begin
        u:=VecNorm(TopOneVec/w);
      end;
      v:=w/u;
      d := VecNorm(u*cos(r1)*r2s + v*sin(r1)*r2s + w*sqrt(1-r2));
      result:=obj.e+VecMul(f,Radiance(CreateRay(x,d),depth) );
    end;(*DIFF*)
    SPEC:begin
      result:=obj.e+VecMul(f,(Radiance(CreateRay(x,r.d-n*2*(n*r.d) ),depth)));
    end;(*SPEC*)
    REFR:begin
      RefRay:=CreateRay(x,r.d-n*2*(n*r.d) );
      into:= (n*nl>0);
      nc:=1;nt:=1.5; if into then nnt:=nc/nt else nnt:=nt/nc; ddn:=r.d*nl; 
      cos2t:=1-nnt*nnt*(1-ddn*ddn);
      if cos2t<0 then begin   // Total internal reflection
        result:=obj.e + VecMul(f,Radiance(RefRay,depth));
        exit;
      end;
      if into then q:=1 else q:=-1;
      tdir := VecNorm(r.d*nnt - n*(q*(ddn*nnt+sqrt(cos2t))));
      if into then Q:=-ddn else Q:=tdir*n;
      a:=nt-nc; b:=nt+nc; R0:=a*a/(b*b); c := 1-Q;
      Re:=R0+(1-R0)*c*c*c*c*c;Tr:=1-Re;P:=0.25+0.5*Re;RP:=Re/P;TP:=Tr/(1-P);
      if depth>2 then begin
        if random<p then // 反射
          result:=obj.e+VecMul(f,Radiance(RefRay,depth)*RP)
        else //屈折
          result:=obj.e+VecMul(f,Radiance(CreateRay(x,tdir),depth)*TP);
      end
      else begin// 屈折と反射の両方を追跡
        result:=obj.e+VecMul(f,Radiance(RefRay,depth)*Re+Radiance(CreateRay(x,tdir),depth)*Tr);
      end;
    end;(*REFR*)
  end;(*CASE*)
end;

function TNEEThread.Radiance( r:RayRecord;depth:integer):VecRecord;
var
  id,i,tid:integer;
  obj,s:ModelClass;// Rect Implement
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t,m1,ss,cc:real;
  into:boolean;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
  EL,sw,su,sv,l,tw,tu,tv:VecRecord;
  cos_a_max,eps1,eps2,eps2s,cos_a,sin_a,phi,omega:real;
  cl,cf:VecRecord;
  E:integer;
  uvwRef:uvwVecRecord;
begin
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
      DIFF:
        begin
          d:=VecSphereRef(nl,uvwRef);
          // Loop over any lights
          EL:=ZeroVec;
          tid:=id;
          for i:=0 to mdl.count-1 do begin
            s:=ModelClass(mdl[i]);
            if (i=tid) then begin
              continue;
            end;
            if s.isLight=false  then continue; // skip non-lights
            s.uvwRef:=uvwRef;
            l:=s.GetLightPath(x);
            if intersect(CreateRay(x,l),t,id) then begin
              if id=i then begin
                tr:=l*nl;if tr<0 then tr:=0;
 //               tr:=l*nl;
 //               tw:=s.e*(l*nl)*s.omega_1_pi;
                EL:=EL+VecMul(f,(s.e*(tr)*s.omega_1_pi(l)));
              end;
            end;
          end;(*for*)
//          tw:=obj.e*e+EL;
          cl:= cl+VecMul(cf,(obj.e*e+EL) );
          E:=0;
          r:=CreateRay(x,d);
        end;(*DIFF*)
      SPEC:
        begin
          tw:=obj.e*e;
          cl:=cl+VecMul(cf,tw);
          E:=1;tv:=n*2*(n*r.d) ;tv:=r.d-tv;
          r:=CreateRay(x,tv);
        end;(*SPEC*)
      REFR:
        begin
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



function TLoopThread.Radiance( r:RayRecord;depth:integer):VecRecord;
var
  id:integer;
  obj:ModelClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t,ss,cc,nrd:real;
  into:boolean;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
  tu,tv:VecRecord;
  cl,cf:VecRecord;
  uvwRef:uvwVecRecord;
begin
//writeln(' DebugY=',DebugY,' DebugX=',DebugX);
  depth:=0;
  id:=0;cl:=ZeroVec;cf:=CreateVec(1,1,1);
  while (TRUE) do begin
    Inc(depth);
    if intersect(r,t,id)=false then begin
      result:=cl;
      exit;
    end;
    obj:=ModelClass(mdl[id]);
    x:=r.o+r.d*t; n:=obj.GetNorm(x); f:=obj.c;
    nrd:=n*r.d;
    if nrd<0 then nl:=n else nl:=n*-1;
    if (f.x>f.y)and(f.x>f.z) then
      p:=f.x
    else if f.y>f.z then
      p:=f.y
    else
      p:=f.z;
    cl:=cl+VecMul(cf,obj.e);
    if (Depth > 5) or (p = 0) then begin
       //p=0は要するに発光体に撃ちあたる場合＝発光体は色がぜろだから
      if (random < p) then begin
        f:= f / p;
      end
      else begin
        Result := cl;
        exit;
      end;
    end;
    cf:=VecMul(cf,f);
    case obj.refl of
      DIFF:begin
        d:=VecSphereRef(nl,uvwRef);
        r:=CreateRay(x,d)
      end;(*DIFF*)
      SPEC:begin
        tv:=n*2*nrd ;tv:=r.d-tv;
        r:=CreateRay(x,tv);
      end;(*SPEC*)
      REFR:begin
        tv:=n*2*nrd ;tv:=r.d-tv;
        RefRay:=CreateRay(x,tv);
        into:= (n*nl>0);
        nc:=1;nt:=1.5; if into then nnt:=nc/nt else nnt:=nt/nc; ddn:=r.d*nl;
        cos2t:=1-nnt*nnt*(1-ddn*ddn);
        if cos2t<0 then begin   // Total internal reflection
          cl:=cl+VecMul(cf,obj.e);
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
          cl:=cl+VecMul(cf,obj.e);
          r:=RefRay;
        end
        else begin//屈折
          cf:=cf*TP;
          cl:=cl+VecMul(cf,obj.e);
          r:=CreateRay(x,tdir);
        end
      end;(*REFR*)
    end;(*CASE*)
  end;(*WHILE LOOP *)
end;

begin
end.


