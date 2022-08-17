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
unit uEmit;

{$mode objfpc}{$H+}
{$modeswitch advancedrecords}


interface

uses
  LCLIntf, LCLType,
  Classes, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,LMessages,
  SysUtils, uVect,uModel,uScene,uQuat,uFlux,uLightPath,
  getopts;

const
  MSG_NEWLINE         = WM_USER + 0;
  MSG_DecThreadCount = WM_USER+1;
  MSG_DrawNextScene  = WM_USER+2;
  DefaultSaveFile    = 'out.png';
  PathSeparator     = '/';(*IF Windows THEN \*)

type
  FluxOptionRecord=record
    w,h,samps:integer;
    AlgolID,ModelID:integer;
    OutFN:string;
    DefaultOutFileName:string;
    procedure Setup(w_,h_,samp_,Algol,Model:integer;OFN:string);
    function OutFileName:string;
  end;

  
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
    yRender:integer;
    Flx:TFluxClass;
    constructor Create(CreateSuspended: boolean);
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
    FluxOpt:FluxOptionRecord;
    StartTime:Int64;
    ThreadList:TList;
    yAxis:integer;
    isRun:boolean;
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
  c:char;
  ArgInt:integer;
  ArgFN:string;
begin
  isRun:=false;
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
  FluxOpt.Setup(320,240,16,AlgolIndex,ModelIndex,'out.png');
  c:=#0;
  repeat
    c:=getopt('m:o:a:s:w:r');
    case c of
      'r':begin
            isRun:=true;
          end;
      'm': begin
             ArgInt:=StrToInt(OptArg);
             FluxOpt.ModelId:=ArgInt;
           end;
      'a': begin
             ArgInt:=StrToInt(OptArg);
             FluxOpt.AlgolID:=ArgInt;
             if (ArgInt>3) or (ArgInt<0) then FluxOpt.AlgolID:=0;
           end;
      'o': begin
             ArgFN:=OptArg;
             if ArgFN<>'' then FluxOpt.OutFN:=ArgFN;
           end;
      's': begin
             ArgInt:=StrToInt(OptArg);
             FluxOpt.samps:=ArgInt;
           end;
      'w': begin
             ArgInt:=StrToInt(OptArg);
             FluxOpt.w:=ArgInt;FluxOpt.h:=FluxOpt.w *3 div 4;
           end;
    end; { case }
  until c=endofoptions;
  //  height:=FluxOpt.h;
  if isRun then begin
    FluxOpt.OutFN:='';//空白にする事で出力ファイル名を自動的に決める形に
    RenderSetup;
    for i:=0 to ThreadList.Count-1 do begin
      TRenderThread(ThreadList[i]).Start;
    end;
  end;
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

   if isRun=false then 
     FluxOpt.setup(imgRender.Width,imgRender.Height,
                   StrToInt(StrSampleCount.text),
                   AlgolIndex,ModelIndex,'out.png');
   
   imgRender.Width := FluxOpt.w;
   imgRender.Height := FluxOpt.h;
   ThreadNum:=FluxOpt.samps;
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
      RenderThread:=TRenderThread.Create(true);
      case FluxOpt.AlgolID of
        0:RenderThread.Flx:=TFluxClass.Create;
        1:RenderThread.Flx:=TNEEFluxClass.Create;
        2:RenderThread.Flx:=TLoopFluxClass.Create;
        3:RenderThread.Flx:=TLightPathFluxClass.Create;
        else RenderThread.Flx:=TFluxClass.Create;
      end;

      // True parameter it doesnt start automatically
      if Assigned(RenderThread.FatalException) then
        raise RenderThread.FatalException;
      RenderThread.wide:=FluxOpt.w;
      RenderThread.h:=FluxOpt.h;
      RenderThread.samps:=FluxOpt.samps;
      RenderThread.yRender:=GetYAxis;
      RenderThread.DoneCalc:=false;
      ThreadList.Add(RenderThread);
   end;
   StartTime:=GetTickCount64;
end;
procedure TMainForm.cmdRenderClick(Sender: TObject);
var
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
   Flx.mdl:=SRList.DeepCopyModel(MainForm.FluxOpt.ModelID);
   Flx.cam:=SRList.SRL[MainForm.ModelIndex].cam;
   Flx.cam.ReWidth(MainForm.FluxOpt.w);
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
     MainForm.imgRender.Picture.SaveToFile(MainForm.FluxOpt.OutFileName);
     if MainForm.isRun then halt;
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
           temp:=Flx.Radiance(Flx.Cam.Ray(x,y,sx,sy),0);
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

procedure FluxOptionRecord.Setup(w_,h_,samp_,Algol,Model:integer;OFN:string);
begin
  DefaultOutFileName:='out.png';
  w:=w_;h:=h_;samps:=samp_;AlgolID:=Algol;ModelID:=Model;
  OutFN:=OFN;
end;
function FluxOptionRecord.OutFileName:string;
var
  AlgolStr:string;
begin
  if OutFN<>'' then begin
    result:=OutFN;
  end
  else begin
    AlgolStr:='Org';
    if AlgolID=0 then AlgolStr:='ORG';
    if AlgolID=1 then AlgolStr:='NEE';
    if AlgolID=2 then AlgolStr:='Loop';
    if AlgolID=3 then AlgolStr:='LP';
    result:='M'+IntToStr(ModelID)+AlgolStr+DefaultOutFileName;
  end;
end;


begin
end.


