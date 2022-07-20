unit uScene;
{$MODE objfpc}
{$INLINE ON}
{$modeswitch advancedrecords}
interface
uses SysUtils,Classes,uVect,uModel,math;


const
  eps            = 1e-4;
  INF            = 1e20;
  M_1_PI         = 1/pi;
  M_2PI          = 2*pi;
  DefaultSamples = 16;
  MaxSceneList=32;
type
  SceneRecord=record
    SceneName : string;
    mdl       : TList;
    cam       : CameraRecord;
  end;
  SceneRecordList=record
    MaxIndex:integer;
    SRL:array[0..MaxSceneList] of SceneRecord;
    procedure InitSceneRecord(w,h:integer);
    function GetScene(id:integer):SceneRecord;
    procedure AddScene(Scene:SceneRecord);
  end;
var
  SRList:SceneRecordList;
  
implementation
procedure SceneRecordList.AddScene(Scene:SceneRecord);
begin
  if MaxIndex<MaxSceneList then begin
    Inc(MaxIndex);
    SRL[MaxIndex]:=Scene;
  end;
end;

function SceneRecordList.GetScene(id:integer):SceneRecord;
begin
  if MaxIndex<id then begin writeln('Model List index over!');halt;end;
  result:=SRL[id];
end;


procedure SceneRecordList.InitSceneRecord(w,h:integer);
var
  i:integer;
  tempL:TList;
  Cen,C,TC,SCC:VecRecord;
  R,T,D,Z:real;
begin
  MaxIndex:=-1;

  //-------------Debug Scene sc1-------------
  Inc(MaxIndex);
  SRL[MaxIndex].SceneName:='0-Debug Scene';
  tempL:=TList.Create;
  tempL.add( SphereClass.Create(1e5, CreateVec( 1e5+1,40.8,81.6),  ZeroVec,CreateVec(0.75,0.25,0.25),DIFF) );//Left
  tempL.add( SphereClass.Create(1e5, CreateVec(-1e5+99,40.8,81.6), ZeroVec,CreateVec(0.25,0.25,0.75),DIFF) );//Right
  tempL.add( SphereClass.Create(1e5, CreateVec(50,40.8, 1e5),      ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Back
  tempL.add( SphereClass.Create(1e5, CreateVec(50,40.8,-1e5+170+eps),ZeroVec,CreateVec(0,0,0)       ,DIFF) );//Front
  tempL.add( SphereClass.Create(1e5, CreateVec(50, 1e5, 81.6),     ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Bottomm
  tempL.add( SphereClass.Create(1e5, CreateVec(50,-1e5+81.6,81.6), ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Top
  tempL.add( SphereClass.Create(16.5,CreateVec(27,16.5,47),        ZeroVec,CreateVec(1,1,1)*0.999,   SPEC) );//Mirror
  tempL.add( SphereClass.Create(16.5,CreateVec(73,16.5,88),        ZeroVec,CreateVec(1,1,1)*0.999,   REFR) );//Glass
  tempL.add( SphereClass.Create(600,CreateVec(50,681.6-0.27,81.6), CreateVec(4,4,4),   ZeroVec,  DIFF) );//Ligth

  SRL[MaxIndex].mdl:=tempL;
  SRL[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);



  //----------cornel box sc1-----------
  Inc(MaxIndex);
  SRL[MaxIndex].SceneName:='1-cornel Box';
  tempL:=TList.Create;
  tempL.add( SphereClass.Create(1e5, CreateVec( 1e5+1,40.8,81.6),  ZeroVec,CreateVec(0.75,0.25,0.25),DIFF) );//Left
  tempL.add( SphereClass.Create(1e5, CreateVec(-1e5+99,40.8,81.6), ZeroVec,CreateVec(0.25,0.25,0.75),DIFF) );//Right
  tempL.add( SphereClass.Create(1e5, CreateVec(50,40.8, 1e5),      ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Back
  tempL.add( SphereClass.Create(1e5, CreateVec(50,40.8,-1e5+170+eps),ZeroVec,CreateVec(0,0,0)       ,DIFF) );//Front
  tempL.add( SphereClass.Create(1e5, CreateVec(50, 1e5, 81.6),     ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Bottomm
  tempL.add( SphereClass.Create(1e5, CreateVec(50,-1e5+81.6,81.6), ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Top
  tempL.add( SphereClass.Create(16.5,CreateVec(27,16.5,47),        ZeroVec,CreateVec(1,1,1)*0.999,   SPEC) );//Mirror
  tempL.add( SphereClass.Create(16.5,CreateVec(73,16.5,88),        ZeroVec,CreateVec(1,1,1)*0.999,   REFR) );//Glass
  tempL.add( SphereClass.Create( 1.5,CreateVec(50,81.6-16.5,81.6), CreateVec(4,4,4)*100,   ZeroVec,  DIFF) );//Ligth

  SRL[MaxIndex].mdl:=tempL;
  SRL[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);


  //-----------sky sc2--------------
  Inc(MaxIndex);
  SRL[MaxIndex].SceneName:='2-Sky';
  tempL:=TList.Create;
  Cen:=CreateVec(50,40.8,-860);

  tempL.add(SphereClass.Create(1600, CreateVec(1,0,2)*3000, CreateVec(1,0.9,0.8)*1.2e1*1.56*2,ZeroVec, DIFF)); // sun
  tempL.add(SphereClass.Create(1560, CreateVec(1,0,2)*3500,CreateVec(1,0.5,0.05)*4.8e1*1.56*2, ZeroVec,  DIFF) ); // horizon sun2
  tempL.add(SphereClass.Create(10000,Cen+CreateVec(0,0,-200), CreateVec(0.00063842, 0.02001478, 0.28923243)*6e-2*8, CreateVec(0.7,0.7,1)*0.25,  DIFF)); // sky

  tempL.add(SphereClass.Create(100000, CreateVec(50, -100000, 0),ZeroVec,CreateVec(0.3,0.3,0.3),DIFF)); // grnd
  tempL.add(SphereClass.Create(110000, CreateVec(50, -110048.5, 0),CreateVec(0.9,0.5,0.05)*4,ZeroVec,DIFF));// horizon brightener
  tempL.add(SphereClass.Create(4e4, CreateVec(50, -4e4-30, -3000),ZeroVec,CreateVec(0.2,0.2,0.2),DIFF));// mountains

  tempL.add(SphereClass.Create(26.5,CreateVec(22,26.5,42),ZeroVec,CreateVec(1,1,1)*0.596, SPEC)); // white Mirr
  tempL.add(SphereClass.Create(13,CreateVec(75,13,82),ZeroVec,CreateVec(0.96,0.96,0.96)*0.96, REFR));// Glas
  tempL.add(SphereClass.Create(22,CreateVec(87,22,24),ZeroVec,CreateVec(0.6,0.6,0.6)*0.696, REFR));    // Glas2

  SRL[MaxIndex].mdl:=tempL;
  SRL[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);


  //------------nightsky sc3----
  Inc(MaxIndex);
  SRL[MaxIndex].SceneName:='3-nightsky';
  tempL:=TList.Create;
  tempL.add(SphereClass.Create(2.5e3,CreateVec(0.82,0.92,0-2)*1e4,    CreateVec(1,1,1)*0.8e2,     ZeroVec, DIFF)); // moon
  tempL.add(SphereClass.Create(2.5e4,CreateVec(50, 0, 0),  CreateVec(0.114, 0.133, 0.212)*1e-2,  CreateVec(0.216,0.384,1)*0.003, DIFF)); // sky
  tempL.add(SphereClass.Create(5e0,  CreateVec(-0.2,0.16,-1)*1e4, CreateVec(1.00, 0.843, 0.698)*1e2,   ZeroVec, DIFF));  // star
  tempL.add(SphereClass.Create(5e0,  CreateVec(0,  0.18,-1)*1e4,  CreateVec(1.00, 0.851, 0.710)*1e2,  ZeroVec, DIFF));  // star
  tempL.add(SphereClass.Create(5e0,  CreateVec(0.3, 0.15,-1)*1e4, CreateVec(0.671, 0.780, 1.00)*1e2,   ZeroVec, DIFF));  // star
  tempL.add(SphereClass.Create(3.5e4,CreateVec(600,-3.5e4+1, 300), ZeroVec,   CreateVec(0.6,0.8,1)*0.01,  REFR));   //pool
  tempL.add(SphereClass.Create(5e4,  CreateVec(-500,-5e4+0, 0),    ZeroVec,   CreateVec(1,1,1)*0.35,  DIFF));    //hill
  tempL.add(SphereClass.Create(16.5, CreateVec(27,0,47),           ZeroVec,   CreateVec(1,1,1)*0.33, DIFF)); //hut
  tempL.add(SphereClass.Create(7,    CreateVec(27+8*sqrt(2),0,47+8*sqrt(2)),ZeroVec,  CreateVec(1,1,1)*0.33,  DIFF)); //door
  tempL.add(SphereClass.Create(500,  CreateVec(-1e3,-300,-3e3), ZeroVec,  CreateVec(1,1,1)*0.351,    DIFF));  //mnt
  tempL.add(SphereClass.Create(830,  CreateVec(0,   -500,-3e3), ZeroVec,  CreateVec(1,1,1)*0.354,    DIFF));  //mnt
  tempL.add(SphereClass.Create(490,  CreateVec(1e3, -300,-3e3), ZeroVec,  CreateVec(1,1,1)*0.352,    DIFF));  //mnt

  SRL[MaxIndex].mdl:=tempL;
  SRL[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);

  //-----------island sc4-------
  Inc(MaxIndex);
  SRL[MaxIndex].SceneName:='4-island';

  tempL:=TList.Create;
  Cen:=CreateVec(50,-20,-860);

  tempL.add(SphereClass.Create(160, Cen+CreateVec(0, 600, -500),CreateVec(1,1,1)*2e2, ZeroVec,  DIFF)); // sun
  tempL.add(SphereClass.Create(800, Cen+CreateVec(0,-880,-9120),CreateVec(1,1,1)*2e1, ZeroVec,  DIFF)); // horizon
  tempL.add(SphereClass.Create(10000,Cen+CreateVec(0,0,-200), CreateVec(0.0627, 0.188, 0.569)*1e0, CreateVec(1,1,1)*0.4,  DIFF)); // sky
  tempL.add(SphereClass.Create(800, Cen+CreateVec(0,-720,-200),ZeroVec,  CreateVec(0.110, 0.898, 1.00)*0.996,  REFR)); // water
  tempL.add(SphereClass.Create(790, Cen+CreateVec(0,-720,-200),ZeroVec,  CreateVec(0.4,0.3,0.04)*0.6, DIFF)); // earth
  tempL.add(SphereClass.Create(325, Cen+CreateVec(0,-255,-50), ZeroVec,  CreateVec(0.4,0.3,0.04)*0.8, DIFF)); // island
  tempL.add(SphereClass.Create(275, Cen+CreateVec(0,-205,-33), ZeroVec,  CreateVec(0.02,0.3,0.02)*0.75,DIFF)); // grass

  SRL[MaxIndex].mdl:=tempL;
  SRL[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);

  //-------------Vista sc5------------
  Inc(MaxIndex);
  SRL[MaxIndex].SceneName:='5-Vista';

  tempL:=TList.Create;
  Cen:=CreateVec(50,-20,-860);

  tempL.add(SphereClass.Create(8000, Cen+CreateVec(0,-8000,-900),CreateVec(1,0.4,0.1)*5e-1, ZeroVec,  DIFF)); // sun
  tempL.add(SphereClass.Create(1e4,  Cen+ZeroVec, CreateVec(0.631, 0.753, 1.00)*3e-1, CreateVec(1,1,1)*0.5,  DIFF)); // sky

  tempL.add(SphereClass.Create(150,  Cen+CreateVec(-350,0, -100),ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt
  tempL.add(SphereClass.Create(200,  Cen+CreateVec(-210,0,-100), ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt
  tempL.add(SphereClass.Create(145,  Cen+CreateVec(-210,85,-100),ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF)); // snow
  tempL.add(SphereClass.Create(150,  Cen+CreateVec(-50,0,-100),  ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt
  tempL.add(SphereClass.Create(150,  Cen+CreateVec(100,0,-100),  ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt
  tempL.add(SphereClass.Create(125,  Cen+CreateVec(250,0,-100),  ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt
  tempL.add(SphereClass.Create(150,  Cen+CreateVec(375,0,-100),  ZeroVec,  CreateVec(1,1,1)*0.3,  DIFF)); // mnt

  tempL.add(SphereClass.Create(2500, Cen+CreateVec(0,-2400,-500),ZeroVec,  CreateVec(1,1,1)*0.1,  DIFF)); // mnt base

  tempL.add(SphereClass.Create(8000, Cen+CreateVec(0,-8000,200), ZeroVec,  CreateVec(0.2,0.2,1),    REFR)); // water
  tempL.add(SphereClass.Create(8000, Cen+CreateVec(0,-8000,1100),ZeroVec,  CreateVec(0,0.3,0),     DIFF)); // grass
  tempL.add(SphereClass.Create(8   , Cen+CreateVec(-75, -5, 850),ZeroVec,  CreateVec(0,0.3,0),     DIFF)); // bush
  tempL.add(SphereClass.Create(30,   Cen+CreateVec(0,   23, 825),ZeroVec,  CreateVec(1,1,1)*0.996, REFR)); // ball

  tempL.add(SphereClass.Create(30,  Cen+CreateVec(200,280,-400),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));   // clouds
  tempL.add(SphereClass.Create(37,  Cen+CreateVec(237,280,-400),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));   // clouds
  tempL.add(SphereClass.Create(28,  Cen+CreateVec(267,280,-400),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));   // clouds

  tempL.add(SphereClass.Create(40,  Cen+CreateVec(150,280,-1000),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));  // clouds
  tempL.add(SphereClass.Create(37,  Cen+CreateVec(187,280,-1000),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));  // clouds

  tempL.add(SphereClass.Create(40,  Cen+CreateVec(600,280,-1100),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));  // clouds
  tempL.add(SphereClass.Create(37,  Cen+CreateVec(637,280,-1100),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));  // clouds

  tempL.add(SphereClass.Create(37,  Cen+CreateVec(-800,280,-1400),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF)); // clouds
  tempL.add(SphereClass.Create(37,  Cen+CreateVec(0,280,-1600),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));    // clouds
  tempL.add(SphereClass.Create(37,  Cen+CreateVec(537,280,-1800),  ZeroVec,  CreateVec(1,1,1)*0.8,  DIFF));  // clouds

  SRL[MaxIndex].mdl:=tempL;
  SRL[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);

  //----------------Overlap  sc6-----------------
  Inc(MaxIndex);
  SRL[MaxIndex].SceneName:='6-Overlap';
  tempL:=TList.Create;

  D:=50;
  R:=40;
  tempL.add(SphereClass.Create(150, CreateVec(50+75,28,62), CreateVec(1,1,1)*0e-3, CreateVec(1,0.9,0.8)*0.93, REFR));
  tempL.add(SphereClass.Create(28,  CreateVec(50+5,-28,62), CreateVec(1,1,1)*1e1, ZeroVec, DIFF));
  tempL.add(SphereClass.Create(300, CreateVec(50,28,62), CreateVec(1,1,1)*0e-3, CreateVec(1,1,1)*0.93, SPEC));

  SRL[MaxIndex].mdl:=tempL;
  SRL[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);

  //----------------wada  sc7-------------
  Inc(MaxIndex);
  SRL[MaxIndex].SceneName:='7-wada';
  tempL:=TList.Create;

  R:=60;
  //double R=120;
  T:=30*PI/180.;
  D:=R/cos(T);
  Z:=60;

  tempL.add(SphereClass.Create(1e5, CreateVec(50, 100, 0),      CreateVec(1,1,1)*3e0, ZeroVec, DIFF)); // sky
  tempL.add(SphereClass.Create(1e5, CreateVec(50, -1e5-D-R, 0), ZeroVec,     CreateVec(0.1,0.1,0.1),DIFF));           //grnd

  tempL.add(SphereClass.Create(R, CreateVec(50,40.8,62)+CreateVec( cos(T),sin(T),0)*D, ZeroVec, CreateVec(1,0.3,0.3)*0.999, SPEC)); //red
  tempL.add(SphereClass.Create(R, CreateVec(50,40.8,62)+CreateVec(-cos(T),sin(T),0)*D, ZeroVec, CreateVec(0.3,1,0.3)*0.999, SPEC)); //grn
  tempL.add(SphereClass.Create(R, CreateVec(50,40.8,62)+CreateVec(0,-1,0)*D,         ZeroVec, CreateVec(0.3,0.3,1)*0.999, SPEC)); //blue
  tempL.add(SphereClass.Create(R, CreateVec(50,40.8,62)+CreateVec(0,0,-1)*D,       ZeroVec, CreateVec(0.53,0.53,0.53)*0.999, SPEC)); //back
  tempL.add(SphereClass.Create(R, CreateVec(50,40.8,62)+CreateVec(0,0,1)*D,      ZeroVec, CreateVec(1,1,1)*0.999, REFR)); //front

  SRL[MaxIndex].mdl:=tempL;
  SRL[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);


  //-----------------wada2 sc8----------
  Inc(MaxIndex);
  SRL[MaxIndex].SceneName:='8-wada2';
  tempL:=TList.Create;

  R:=120;     // radius
  T:=30*PI/180.;
  D:=R/cos(T);     //distance
  Z:=62;
  C:=CreateVec(0.275, 0.612, 0.949);

  tempL.add(SphereClass.Create(R, CreateVec(50,28,Z)+CreateVec( cos(T),sin(T),0)*D,    C*6e-2,CreateVec(1,1,1)*0.996, SPEC)); //red
  tempL.add(SphereClass.Create(R, CreateVec(50,28,Z)+CreateVec(-cos(T),sin(T),0)*D,    C*6e-2,CreateVec(1,1,1)*0.996, SPEC)); //grn
  tempL.add(SphereClass.Create(R, CreateVec(50,28,Z)+CreateVec(0,-1,0)*D,              C*6e-2,CreateVec(1,1,1)*0.996, SPEC)); //blue
  tempL.add(SphereClass.Create(R, CreateVec(50,28,Z)+CreateVec(0,0,-1)*R*2*sqrt(2/3),C*0e-2,CreateVec(1,1,1)*0.996, SPEC)); //back
  tempL.add(SphereClass.Create(2*2*R*2*sqrt(2/3)-R*2*sqrt(2/3)/3,
                                  CreateVec(50,28,Z)+CreateVec(0,0,-R*2*sqrt(2/3)/3), CreateVec(1,1,1)*0,CreateVec(1,1,1)*0.5, SPEC)); //front

  SRL[MaxIndex].mdl:=tempL;
  SRL[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);


  //---------------forest sc9-----------
  Inc(MaxIndex);
  SRL[MaxIndex].SceneName:='9-forest';

  tempL:=TList.Create;

  tc:=CreateVec(0.0588, 0.361, 0.0941);
  scc:=CreateVec(1,1,1)*0.7;
  tempL.add(SphereClass.Create(1e5, CreateVec(50, 1e5+130, 0),  CreateVec(1,1,1)*1.3,ZeroVec,DIFF)); //lite
  tempL.add(SphereClass.Create(1e2, CreateVec(50, -1e2+2, 47),  ZeroVec,CreateVec(1,1,1)*0.7,DIFF)); //grnd

  tempL.add(SphereClass.Create(1e4, CreateVec(50, -30, 300)+CreateVec(-sin(50*PI/180),0,cos(50*PI/180))*1e4, ZeroVec, CreateVec(1,1,1)*0.99,SPEC));// mirr L
  tempL.add(SphereClass.Create(1e4, CreateVec(50, -30, 300)+CreateVec(sin(50*PI/180),0,cos(50*PI/180))*1e4,  ZeroVec, CreateVec(1,1,1)*0.99,SPEC));// mirr R
  tempL.add(SphereClass.Create(1e4, CreateVec(50, -30, -50)+CreateVec(-sin(30*PI/180),0,-cos(30*PI/180))*1e4,ZeroVec, CreateVec(1,1,1)*0.99,SPEC));// mirr FL
  tempL.add(SphereClass.Create(1e4, CreateVec(50, -30, -50)+CreateVec(sin(30*PI/180),0,-cos(30*PI/180))*1e4, ZeroVec, CreateVec(1,1,1)*0.99,SPEC));// mirr


  tempL.add(SphereClass.Create(4, CreateVec(50,6*0.6,47),   ZeroVec,CreateVec(0.13,0.066,0.033), DIFF));//"tree"
  tempL.add(SphereClass.Create(16,CreateVec(50,6*2+16*0.6,47),   ZeroVec, tc,  DIFF));//"tree"
  tempL.add(SphereClass.Create(11,CreateVec(50,6*2+16*0.6*2+11*0.6,47),   ZeroVec, tc,  DIFF));//"tree"
  tempL.add(SphereClass.Create(7, CreateVec(50,6*2+16*0.6*2+11*0.6*2+7*0.6,47),   ZeroVec, tc,  DIFF));//"tree"

  tempL.add(SphereClass.Create(15.5,CreateVec(50,1.8+6*2+16*0.6,47),   ZeroVec, scc,  DIFF));//"tree"
  tempL.add(SphereClass.Create(10.5,CreateVec(50,1.8+6*2+16*0.6*2+11*0.6,47),   ZeroVec, scc,  DIFF));//"tree"
  tempL.add(SphereClass.Create(6.5, CreateVec(50,1.8+6*2+16*0.6*2+11*0.6*2+7*0.6,47),   ZeroVec, scc,  DIFF));//"tree"

  SRL[MaxIndex].mdl:=tempL;
  SRL[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);

  //-------------Debug Scene sc10-------------
  Inc(MaxIndex);
  SRL[MaxIndex].SceneName:='10-Debug Scene';
  tempL:=TList.Create;
  tempL.add( SphereClass.Create(1e5, CreateVec( 1e5+1,40.8,81.6),  ZeroVec,CreateVec(0.75,0.25,0.25),DIFF) );//Left
  tempL.add( SphereClass.Create(1e5, CreateVec(-1e5+99,40.8,81.6), ZeroVec,CreateVec(0.25,0.25,0.75),DIFF) );//Right
  tempL.add( SphereClass.Create(1e5, CreateVec(50,40.8, 1e5),      ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Back
  tempL.add( SphereClass.Create(1e5, CreateVec(50,40.8,-1e5+170+eps),ZeroVec,CreateVec(0,0,0)       ,DIFF) );//Front
  tempL.add( SphereClass.Create(1e5, CreateVec(50, 1e5, 81.6),     ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Bottomm
  tempL.add( SphereClass.Create(1e5, CreateVec(50,-1e5+81.6,81.6), ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Top
  tempL.add( SphereClass.Create(16.5,CreateVec(27,16.5,47),        ZeroVec,CreateVec(1,1,1)*0.999,   SPEC) );//Mirror
  tempL.add( SphereClass.Create(16.5,CreateVec(73,16.5,88),        ZeroVec,CreateVec(1,1,1)*0.999,   REFR) );//Glass
//  tempL.add( SphereClass.Create(600,CreateVec(50,681.6-0.27,81.6), CreateVec(4,4,4),   ZeroVec,  DIFF) );//Ligth
//  tempL.add( RectClass.Create(XZ,40,60,70,90,CreateVec(50,70,80), CreateVec(4,4,4),   ZeroVec,  DIFF) );//Ligth
  tempL.add( RectAngleClass.Create(CreateVec(48,68,78),CreateVec(52,72,82), CreateVec(4,4,4)*100,   ZeroVec,  DIFF) );//Ligth

  SRL[MaxIndex].mdl:=tempL;
  SRL[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);

  //-------------Debug Scene sc11-------------
  Inc(MaxIndex);
  SRL[MaxIndex].SceneName:='10-Debug Scene';
  tempL:=TList.Create;
  tempL.add( SphereClass.Create(1e5, CreateVec( 1e5+1,40.8,81.6),  ZeroVec,CreateVec(0.75,0.25,0.25),DIFF) );//Left
  tempL.add( SphereClass.Create(1e5, CreateVec(-1e5+99,40.8,81.6), ZeroVec,CreateVec(0.25,0.25,0.75),DIFF) );//Right
  tempL.add( SphereClass.Create(1e5, CreateVec(50,40.8, 1e5),      ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Back
  tempL.add( SphereClass.Create(1e5, CreateVec(50,40.8,-1e5+170+eps),ZeroVec,CreateVec(0,0,0)       ,DIFF) );//Front
  tempL.add( SphereClass.Create(1e5, CreateVec(50, 1e5, 81.6),     ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Bottomm
  tempL.add( SphereClass.Create(1e5, CreateVec(50,-1e5+81.6,81.6), ZeroVec,CreateVec(0.75,0.75,0.75),DIFF) );//Top
//  tempL.add( SphereClass.Create(16.5,CreateVec(27,16.5,47),        ZeroVec,CreateVec(1,1,1)*0.999,   SPEC) );//Mirror
  tempL.Add( RotateRecAngleClass.Create(CreateVec(0,1,0),30,
                                CreateVec(10,0,30),CreateVec(40,35,60),ZeroVec,CreateVec(0.4,0.8,0.6),SPEC) );//Rect Mirror
  tempL.add( SphereClass.Create(16.5,CreateVec(73,16.5,88),        ZeroVec,CreateVec(1,1,1)*0.999,   REFR) );//Glass
//  tempL.add( RectAngleClass.Create(CreateVec(60,0,70),CreateVec(90,35,100),ZeroVec,CreateVec(1,1,1)*0.999,REFR) );Glass
  tempL.add( RectAngleClass.Create(CreateVec(48,68,78),CreateVec(52,72,82), CreateVec(4,4,4)*100,   ZeroVec,  DIFF) );//Ligth

  SRL[MaxIndex].mdl:=tempL;
  SRL[MaxIndex].Cam.Setup(CreateVec(50,52,295.6),CreateVec(0,-0.042612,-1),w,h,0.5135,140);
end;


begin
end.



