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

  Abstract:
    Demo to show, how to start a thread and how synchronize with the main
    thread.
    Important: The cthread unint must be added to the uses section of the .lpr
               file. See multithreadingexample1.lpr.

TThreadクラスのプロパティのonTerminateをtrueにしておくとループから脱出すると
開放処理されるがインスタンスは残っているので不正アクセスの温床になるため
自分でMyThreadにNILを代入するべき。
今回は常に終了後に外側で常にCreateする処理にした
}
unit RenderUnit;
{$mode objfpc}{$H+}
{$modeswitch advancedrecords}



interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  uVect,uModel,uScene,uFlux;

const
  MaxThreadNum=4;
type
 
  TLineBuffer=array[0..2000] of VecRecord;

  { TMyThread }
  TMyThread = class(TThread)
  private
    fStatusText: string;
    procedure InitRend;
    procedure DoRend;
    procedure RendDone;
    procedure ShowStatus;
  protected
    procedure Execute; override;
  public
    LineBuffer:TLineBuffer;
    wide,h,samps:INTEGER;
    ModelID:integer;
    yRender:integer;
    tCam:CameraRecord;
    FLx:TFluxClass;
    constructor Create(CreateSuspended: boolean);
  end;

  {ThreadList}
  ThreadListRecord=Record
    Ary:array[0..MaxThreadNum-1] of TMyThread;
    MaxThread:integer;
    procedure ClearList;
    procedure Add(Th:TMyThread);
    function GetThread(i:integer):TMyThread;
  end;
  { TRenderForm }

  TRenderForm = class(TForm)
    AlgoCombo: TComboBox;
    ThreadEdit: TEdit;
    Thread: TLabel;
    SaveBotton: TButton;
    RenderButton: TButton;
    ComboBox1: TComboBox;
    Label4: TLabel;
    Label5: TLabel;
    SamplesEdit: TEdit;
    HeightEdit: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    WidthEdit: TEdit;
    imgRender: TImage;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure RenderButtonClick(Sender: TObject);
    procedure RenderSetup;
  private
    MinimamHeight:integer;
    ThreadList : ThreadListRecord;
  public
    yAxis:integer;
  end;

var
  RenderForm: TRenderForm;

implementation

{$R *.lfm}

procedure ThreadListRecord.ClearList;
begin
  MaxThread:=-1;
end;
procedure ThreadListRecord.Add(Th:TMyThread);
begin
  if MaxThread<MaxThreadNum-1 then inc(MaxThread) else exit;
  ary[MaxThread]:=Th;
end;
function ThreadListRecord.GetThread(i:integer):TMyThread;
begin
  if i<=MaxThread then result:=Ary[i] else result:=nil;
end;



{ TRenderForm }

procedure TRenderForm.FormCreate(Sender: TObject);
begin
  // Here the code initialises anything required before the threads starts executing
  MinimamHeight:=Height;
end;

procedure TRenderForm.RenderButtonClick(Sender: TObject);
var
  i:integer;
begin
  RenderSetup;
  for i:=0 to ThreadList.MaxThread do ThreadList.GetThread(i).Start;
end;

procedure TRenderForm.RenderSetup;
var
  i,samps:integer;
  MyThread:TMyThread;
  ThreadNum:integer;
begin
  ThreadList.ClearList;//この時点ではスレッドが常に無い状態なのでClearで問題ない　onTerminate=TRUEなので
  yAxis:=-1;
  imgRender.Width := strtoint(WidthEdit.Text);
  imgRender.Height := strtoint(HeightEdit.Text);
  ThreadNum:=StrToInt(ThreadEdit.text);
  samps:=StrToInt(SamplesEdit.text);
  //add
  imgRender.Picture.Bitmap.Width:=imgRender.Width;
  imgRender.Picture.Bitmap.Height:=imgRender.Height;
  //Orginal source
  imgRender.Canvas.Brush.Color := clBlack;
  imgRender.Canvas.FillRect(0,0, imgRender.Width, imgRender.Height);

  ClientWidth := imgRender.Left + 5 + imgRender.Width;
  IF (ImgRender.Top+5+ImgRender.Height) >MinimamHeight THEN
    ClientHeight := imgRender.Top + 5 + imgRender.Height;

  for i:=0 to ThreadNum-1 do begin
    MyThread:=TMyThread.Create(True); // With the True parameter it doesnot start automatically
    if Assigned(MyThread.FatalException) then
      raise MyThread.FatalException;
    MyThread.wide:=imgRender.Width;
    MyThread.h:=imgRender.Height;
    Inc(yAxis);
    MyThread.yRender:=yAxis;
    MyThread.FLx:=TFluxClass.Create;
    MyThread.ModelID:=1;
    MyThread.samps:=samps;
    ThreadList.add(MyThread);
  end;
  
end;

{ TMyThread }

procedure TMyThread.InitRend;
var
  i:integer;
  SceneRec:SceneRecord;
begin
  SRList.InitSceneRecord(wide,h);
  SceneRec:=SRList.GetScene(ModelID);
  mdl:=SceneRec.mdl;
  cam:=SceneRec.cam;
end;

procedure TMyThread.DoRend;
var
  x:integer;
begin
   for x:=0 to wide-1 do begin
     RenderForm.ImgRender.Canvas.Pixels[x,yRender]:=
 	     ColToByte(LineBuffer[x].x)+         //red
         ColToByte(LineBuffer[x].y)*256+     //green
         ColToByte(LineBuffer[x].z)*256*256; //blune
   end;
   Inc(RenderForm.yAxis);
   yRender:=RenderForm.yAxis;
end;

procedure TMyThread.RendDone;
begin

end;

procedure TMyThread.ShowStatus;
// this method is only called by Synchronize(@ShowStatus) and therefore
// executed by the main thread
// The main thread can access GUI elements, for example RenderForm.Caption.
begin
  RenderForm.Caption := fStatusText;
end;

procedure TMyThread.Execute;
var
  newStatus : string;
  y,x,sx,sy,s:integer;
  temp       : VecRecord;
  tColor,r : VecRecord;
begin
  fStatusText := 'TMyThread Starting ...';
  Synchronize(@Showstatus);
  Synchronize(@initRend);
  fStatusText := 'TMyThread Running ...';
  while y<h do begin
    for x:=0 to wide-1 do begin
      r:=CreateVec(0, 0, 0);
      tColor:=ZeroVec;
      for sy:=0 to 1 do begin
        for sx:=0 to 1 do begin
          for s:=0 to samps do begin
            temp:=FLx.Radiance(Cam.Ray(x,y,sx,sy),0);
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
    fStatusText:='y='+IntToStr(y);
    Synchronize(@DoRend);
    y:=yRender;
  end;(*y*)
  fStatusText:='TMyThread is End';
  Synchronize(@RendDone);
end;

constructor TMyThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

end.

