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



interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

const
  MaxThread=4;
type

  { TMyThread }

  TLineBuffer=array[0..2000] of record r,g,b:byte; end;
 

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
    yRender:integer;
    constructor Create(CreateSuspended: boolean);
  end;

  { TRenderForm }

  TRenderForm = class(TForm)
    AlgoCombo: TComboBox;
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
    RTAry : array[0..MaxThread-1] of TMyThread;
  public
    yAxis:integer;
  end;

var
  RenderForm: TRenderForm;

implementation

{$R *.lfm}

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
  for i:=0 to MaxThread-1 do RTAry[i].Start;
end;

procedure TRenderForm.RenderSetup;
var
  i:integer;
  MyThread:TMyThread;
begin
  yAxis:=-1;
  imgRender.Width := strtoint(WidthEdit.Text);
  imgRender.Height := strtoint(HeightEdit.Text);
  //add
  imgRender.Picture.Bitmap.Width:=imgRender.Width;
  imgRender.Picture.Bitmap.Height:=imgRender.Height;
  //Orginal source
  imgRender.Canvas.Brush.Color := clBlack;
  imgRender.Canvas.FillRect(0,0, imgRender.Width, imgRender.Height);

  ClientWidth := imgRender.Left + 5 + imgRender.Width;
  IF (ImgRender.Top+5+ImgRender.Height) >MinimamHeight THEN
    ClientHeight := imgRender.Top + 5 + imgRender.Height;

  for i:=0 to MaxThread-1 do begin
    MyThread:=TMyThread.Create(True); // With the True parameter it doesnot start automatically
    if Assigned(MyThread.FatalException) then
      raise MyThread.FatalException;
    RTAry[i] :=MyThread; 
    RTAry[i].wide:=imgRender.Width;
    RTAry[i].h:=imgRender.height;
    Inc(yAxis);
    RTAry[i].yRender:=yAxis;
  end;
  
end;

{ TMyThread }

procedure TMyThread.InitRend;
var
  i:integer;
begin
  for i:=0 to 2000 do begin
    LineBuffer[i].r:=0;LineBuffer[i].g:=0;LineBuffer[i].b:=0;
  end;
end;

procedure TMyThread.DoRend;
var
  x:integer;
begin
   for x:=0 to wide-1 do begin
      RenderForm.ImgRender.Canvas.Pixels[x,yRender]:=
   	     LineBuffer[x].r+         //red
         LineBuffer[x].g*256+     //green
         LineBuffer[x].b*256*256; //blune
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
  y,x:integer;
begin
  fStatusText := 'TMyThread Starting ...';
  Synchronize(@Showstatus);
  Synchronize(@initRend);
  fStatusText := 'TMyThread Running ...';
  while  yRender<h do begin
    for x:=0 to wide-1 do begin
      LineBuffer[x].r:=x mod 255;
      LineBuffer[x].g:=RenderForm.yAxis mod 255;
      LineBuffer[x].b:=128;
      newStatus:='TMyThread Time: '+FormatDateTime('YYYY-MM-DD HH:NN:SS',Now);
    end;
    if NewStatus <> fStatusText then begin
      fStatusText := newStatus;
      Synchronize(@Showstatus);
    end;
    Synchronize(@DoRend);
  end;
  fStatusText:='TMyThread is End';
  Synchronize(@RendDone);
end;

constructor TMyThread.Create(CreateSuspended: boolean);
begin
  FreeOnTerminate := True;
  inherited Create(CreateSuspended);
end;

end.

