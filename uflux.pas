
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
UNIT uFlux;

{$mode objfpc}{$H+}

INTERFACE

USES
  LCLIntf, LCLType,
  Classes, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,LMessages,
  SysUtils, uVect,uModel,uScene,uQuat,math;

CONST
  MSG_NEWLINE         = WM_USER + 0;
  MSG_DecThreadCount = WM_USER+1;
  MSG_DrawNextScene  = WM_USER+2;
  DefaultSaveFile    = 'out.png';
  PathSeparator     = '/';(*IF Windows THEN \*)

TYPE
  { TRenderThread }

  TLineBuffer=ARRAY[0..1980] OF VecRecord;
  (*TColor=r,g,b*)

  TRenderThread = CLASS(TThread)
  PRIVATE
    fStatusText: string;
    DoneCalc:BOOLEAN;
    PROCEDURE InitRend;
    PROCEDURE DoRend;
    PROCEDURE DoneRend;
  PROTECTED
    PROCEDURE Execute; override;
  PUBLIC
    LineBuffer:TLineBuffer;
    wide,h,samps:INTEGER;
    mdl:TList;
    cam:CameraRecord;
    yRender:INTEGER;
    FUNCTION Intersect(CONST r:RayRecord;VAR t:real; VAR id:INTEGER):BOOLEAN;
    FUNCTION Radiance(r:RayRecord;depth:INTEGER):VecRecord;virtual;
    CONSTRUCTOR Create(CreateSuspended: BOOLEAN);
  END;
  TNEEThread=class(TRenderThread)
    function Radiance(r : RayRecord;Depth:integer ):VecRecord;OverRide;
  end;                
  TLoopThread=class(TRenderThread)
    function Radiance(r : RayRecord;Depth:integer ):VecRecord;OverRide;
  end;                


  { TMainForm }

  TMainForm = CLASS(TForm)
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
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    lblTime: TLabel;

    PROCEDURE AlgolComboChange(Sender: TObject);
    PROCEDURE cmdRenderClick(Sender: TObject);
    PROCEDURE cmdSaveClick(Sender: TObject);
    PROCEDURE SceneComboChange(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE SaveDlgClick(Sender: TObject);
  private
    ModelIndex:INTEGER;
    AlgolIndex:INTEGER;
    MinimamHeight:INTEGER;
  public
    ThreadNum:INTEGER;
    samps:INTEGER;
    StartTime:Int64;
    ThreadList:TList;
    yAxis:INTEGER;
    PROCEDURE RenderSetup;
    FUNCTION isAllDone:BOOLEAN;
    FUNCTION GetYAxis:INTEGER;
  end;

    
VAR
  MainForm: TMainForm;

IMPLEMENTATION

{$R *.lfm}

{ TMainForm }


FUNCTION SecToTime(Sec : INTEGER) : STRING;
VAR
  H, M, S : STRING;
  ZH, ZM, ZS : INTEGER;
BEGIN
  ZH := Sec DIV 3600;
  ZM := Sec DIV 60 - ZH * 60;
  ZS := Sec - (ZH * 3600 + ZM * 60);
  H := IntToStr(ZH);
  IF (H = '0') THEN
    H := '00';
  M := IntToStr(ZM);
  IF (M = '0') THEN
    M := '00';
  S := IntToStr(ZS);
  IF (S = '0') THEN
    S := '00';
  Result := H + ':' + M + ':' + S;
END;

FUNCTION TMainForm.isAllDone:BOOLEAN;
VAR
  i:INTEGER;
BEGIN
  isAllDone:=TRUE;
  FOR i:=0 TO ThreadNum-1 DO BEGIN
     IF TRenderThread(ThreadList[i]).DoneCalc=FALSE THEN BEGIN
        isAllDone:=FALSE;
       EXIT;
     END;
  END;
END;

FUNCTION TMainForm.GetYAxis:INTEGER;
BEGIN
   yAxis:=yAxis+1;
   result:=yAxis;
END;

PROCEDURE TMainForm.FormCreate(Sender: TObject);
VAR
  i:INTEGER;
BEGIN
    DoubleBuffered := TRUE;
    TOP:=10;
    Left:=10;
    SRList.InitSceneRecord(320,240);
    FOR i:=0 TO SRList.MaxIndex DO
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
END;

PROCEDURE TMainForm.SaveDlgClick(Sender: TObject);
BEGIN
  IF (SaveDlg.Execute) THEN
    imgRender.Picture.SaveToFile(SaveDlg.FileName);
END;

PROCEDURE TMainForm.RenderSetup;
VAR
  RenderThread: TRenderThread;
  i:INTEGER;
BEGIN
   IF Assigned(ThreadList) THEN BEGIN
      ThreadList.Destroy;
   END;

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

   cmdRender.Enabled:=FALSE;
   ClientWidth := imgRender.Left + 5 + imgRender.Width;
   IF (ImgRender.Top+5+ImgRender.Height) >MinimamHeight THEN
      ClientHeight := imgRender.Top + 5 + imgRender.Height;
   ThreadList:=TList.Create;
   yAxis:=-1;
   FOR i:=0 TO ThreadNum-1 DO BEGIN
      case AlgolIndex of
        0:RenderThread:=TRenderThread.Create(True);
        1:RenderThread:=TNEEThread.Create(true);
        2:RenderThread:=TLoopThread.Create(true);
        else RenderThread:=TRenderThread.Create(true);
      end;

      // True parameter it doesnt start automatically
      IF Assigned(RenderThread.FatalException) THEN
        raise RenderThread.FatalException;
      RenderThread.wide:=imgRender.Width;
      RenderThread.h:=imgRender.Height;
      RenderThread.samps:=StrToInt(StrSampleCount.text);
      RenderThread.yRender:=GetYAxis;
      RenderThread.DoneCalc:=FALSE;
      ThreadList.Add(RenderThread);
   END;
   StartTime:=GetTickCount64; 
END;
PROCEDURE TMainForm.cmdRenderClick(Sender: TObject);
VAR
  RenderThread: TRenderThread;
  i:INTEGER;
BEGIN
   RenderSetup;
   FOR i:=0 TO ThreadList.Count-1 DO BEGIN
     TRenderThread(ThreadList[i]).Start;
   END;
END;

PROCEDURE TMainForm.AlgolComboChange(Sender: TObject);
BEGIN
  AlgolIndex:=AlgolCombo.ItemIndex;
END;


PROCEDURE TMainForm.cmdSaveClick(Sender: TObject);
BEGIN
  IF SaveDlg.Execute THEN ImgRender.Picture.SaveToFile(SaveDlg.Filename);
END;

PROCEDURE TMainForm.SceneComboChange(Sender: TObject);
BEGIN
  ModelIndex:=SceneCombo.ItemIndex;
END;


{ TRenderThread }

PROCEDURE TRenderThread.InitRend;
BEGIN
   mdl:=SRList.DeepCopyModel(MainForm.ModelIndex);
   cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),wide,h,0.5135,140);
END;

PROCEDURE TRenderThread.DoRend;
// this method is only called by Synchronize(@ShowStatus) and therefore
// executed by the main thread
// The main thread can access GUI elements, for example MainForm.Caption.
VAR
  x : INTEGER;
BEGIN
  MainForm.Caption := fStatusText;
  if DoneCalc=FALSE THEN BEGIN
    for x:=0 TO Wide-1 DO BEGIN
      MainForm.ImgRender.Canvas.Pixels[x,yRender]:=
          ColToByte(LineBuffer[x].x)+         //red
          ColToByte(LineBuffer[x].y)*256+     //green
          ColToByte(LineBuffer[x].z)*256*256; //blune
    end;
    MainForm.LblTime.Caption:=SecToTime((GetTickCount64 - MainForm.startTime) DIV 1000);
    yRender:=MainForm.GetYAxis;
  end;
end;
PROCEDURE TRenderThread.DoneRend;
VAR
   st : string;
BEGIN
   IF MainForm.isAllDone THEN BEGIN
      MainForm.yAxis:=-1;
      MainForm.cmdRender.Enabled:=TRUE;
      MainForm.Caption:='TRenderThread Time: '+FormatDateTime('YYYY-MM-DD HH:NN:SS',Now);
      
     MainForm.imgRender.Picture.SaveToFile(DefaultSaveFile); 
  END;
END;

PROCEDURE TRenderThread.Execute;
VAR
  x,y,sx,sy,s:INTEGER;
  temp       : VecRecord;
  tColor,r : VecRecord;
  StatusText1:string;
BEGIN
  y:=yRender;
  Synchronize(@InitRend); 
  fStatusText := 'Render Running ...';
  StatusText1:=fStatusText;
  WHILE y<h DO BEGIN
    FOR x:=0 TO wide-1 DO BEGIN
     r:=CreateVec(0, 0, 0);
     tColor:=ZeroVec;
     FOR sy:=0 TO 1 DO BEGIN
       FOR sx:=0 TO 1 DO BEGIN
         FOR s:=0 TO samps-1 DO BEGIN
           temp:=Radiance(Cam.Ray(x,y,sx,sy),0);
           temp:= temp/ samps;
           r:= r+temp;
         END;(*samps*)
         temp:= ClampVector(r)* 0.25;
         tColor:=tColor+ temp;
         r:=CreateVec(0, 0, 0);
       END;(*sx*)
     END;(*sy*)
     LineBuffer[x]:=tColor;
   END;(*x*)
   fStatusText:=StatusText1+'y='+IntToStr(y);
   Synchronize(@DoRend);
   y:=yRender;
  END;(*y*)
  DoneCalc:=TRUE;
  Synchronize(@DoneRend);
 END;


CONSTRUCTOR TRenderThread.Create(CreateSuspended: BOOLEAN);
BEGIN
  FreeOnTerminate := True;
  DoneCalc:=FALSE;
  yRender:=0;
  inherited Create(CreateSuspended);
END;

FUNCTION TRenderThread.Intersect(CONST r:RayRecord;VAR t:real; VAR id:INTEGER):BOOLEAN;
VAR
  d:real;
  i:INTEGER;
BEGIN
  t:=INF;
  FOR i:=0 TO mdl.count-1 DO BEGIN
    d:=ModelClass(mdl[i]).intersect(r);
    IF d<t THEN BEGIN
      t:=d;
      id:=i;
    END;
  END;
  result:=(t<inf);
END;


FUNCTION TRenderThread.Radiance(r:RayRecord;depth:INTEGER):VecRecord;
VAR
  id:INTEGER;
  obj:ModelClass;
  x,n,f,nl,u,v,w,d:VecRecord;
  p,r1,r2,r2s,t:real;
  into:BOOLEAN;
  RefRay:RayRecord;
  nc,nt,nnt,ddn,cos2t,q,a,b,c,R0,Re,RP,Tr,TP:real;
  tDir:VecRecord;
BEGIN
  id:=0;depth:=depth+1;
  IF intersect(r,t,id)=FALSE THEN BEGIN
    result:=ZeroVec;EXIT;
  END;
  obj:=ModelClass(mdl[id]);
  x:=r.o+r.d*t; n:=VecNorm(x-obj.p); f:=obj.c;
  IF VecDot(n,r.d)<0 THEN nl:=n ELSE nl:=n*-1;
  IF (f.x>f.y)AND(f.x>f.z) THEN
    p:=f.x
  ELSE IF f.y>f.z THEN 
    p:=f.y
  ELSE
    p:=f.z;
  IF (Depth > 5) OR (p = 0) THEN BEGIN
      IF (random < p) THEN BEGIN
        f:= f / p;
        IF (p = 1) AND (f.x = 1) AND (f.y = 1) AND (f.z = 1) THEN BEGIN
          Result := obj.e;
          EXIT;
        END;
      END
      ELSE BEGIN
        Result := obj.e;
        EXIT;
      END;
  END;
  CASE obj.refl OF
    DIFF:BEGIN
      r1:=2*PI*random;r2:=random;r2s:=sqrt(r2);
      w:=nl;
      IF abs(w.x)>0.1 THEN
        u:=VecNorm(MidOneVec/w)
      ELSE BEGIN
        u:=VecNorm(TopOneVec/w);
      END;
      v:=w/u;
      d := VecNorm(u*cos(r1)*r2s + v*sin(r1)*r2s + w*sqrt(1-r2));
      result:=obj.e+VecMul(f,Radiance(CreateRay(x,d),depth) );
    END;(*DIFF*)
    SPEC:BEGIN
      result:=obj.e+VecMul(f,(Radiance(CreateRay(x,r.d-n*2*(n*r.d) ),depth)));
    END;(*SPEC*)
    REFR:BEGIN
      RefRay:=CreateRay(x,r.d-n*2*(n*r.d) );
      into:= (n*nl>0);
      nc:=1;nt:=1.5; IF into THEN nnt:=nc/nt ELSE nnt:=nt/nc; ddn:=r.d*nl; 
      cos2t:=1-nnt*nnt*(1-ddn*ddn);
      IF cos2t<0 THEN BEGIN   // Total internal reflection
        result:=obj.e + VecMul(f,Radiance(RefRay,depth));
        EXIT;
      END;
      IF into THEN q:=1 ELSE q:=-1;
      tdir := VecNorm(r.d*nnt - n*(q*(ddn*nnt+sqrt(cos2t))));
      IF into THEN Q:=-ddn ELSE Q:=tdir*n;
      a:=nt-nc; b:=nt+nc; R0:=a*a/(b*b); c := 1-Q;
      Re:=R0+(1-R0)*c*c*c*c*c;Tr:=1-Re;P:=0.25+0.5*Re;RP:=Re/P;TP:=Tr/(1-P);
      IF depth>2 THEN BEGIN
        IF random<p THEN // 反射
          result:=obj.e+VecMul(f,Radiance(RefRay,depth)*RP)
        ELSE //屈折
          result:=obj.e+VecMul(f,Radiance(CreateRay(x,tdir),depth)*TP);
      END
      ELSE BEGIN// 屈折と反射の両方を追跡
        result:=obj.e+VecMul(f,Radiance(RefRay,depth)*Re+Radiance(CreateRay(x,tdir),depth)*Tr);
      END;
    END;(*REFR*)
  END;(*CASE*)
END;

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

BEGIN
END.


