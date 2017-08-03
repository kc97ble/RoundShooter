unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, DbCtrls, PairSplitter, Pipes, ThreadQueue,
  PlayerInputThread, PlayerOutputThread, GameObject, GameCanvas, Games;

type

  { TPlayerProcess }

  TPlayerProcess = class (TProcess)
  public
    InputThread: TPlayerInputThread; // create after Execute(), destroy before Execute() and on Destroy()
    OutputThread: TPlayerOutputThread; // create after Execute(), destroy before Execute() and on Destroy()
    StderrThread: TPlayerOutputThread; // create after Execute(), destroy before Execute() and on Destroy()
    procedure Execute; override;
    constructor Create;
    destructor Destroy; override;
  end;

  { TMainForm }

  TMainForm = class(TForm)
    Button3: TButton;
    Button4: TButton;
    FileName1View: TLabel;
    HP2View: TLabel;
    HP1View: TLabel;
    FileName2View: TLabel;
    GameGbox: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    PaintBox1: TPaintBox;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Process1Btn1: TButton;
    Process1Btn2: TButton;
    Process2Btn1: TButton;
    Process2Btn2: TButton;
    Process1Edit: TEdit;
    Process2Edit: TEdit;
    Process1Gbox: TGroupBox;
    PageControl1: TPageControl;
    Process2Gbox: TGroupBox;
    Splitter1: TSplitter;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Timer1: TTimer;
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure Process1Btn1Click(Sender: TObject);
    procedure Process1Btn2Click(Sender: TObject);
    procedure Process2Btn1Click(Sender: TObject);
    procedure Process2Btn2Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    Process1, Process2: TPlayerProcess; //
    procedure DoBeforePlaying;
    procedure DoPlaying;
    procedure DoAfterPlaying;
  public
    procedure FormIdle(Sender: TObject; var Done: Boolean);
  end;

var
  MainForm: TMainForm;

implementation

function IfThen(B: Boolean; X, Y: String): String;
begin
  if B then
    Result := X
  else
    Result := Y;
end;

{$R *.lfm}

{ TPlayerProcess }

procedure TPlayerProcess.Execute;
begin
  FreeAndNil(InputThread);
  FreeAndNil(OutputThread);
  FreeAndNil(StderrThread);
  inherited Execute;
  InputThread := TPlayerInputThread.Create(Input);
  OutputThread := TPlayerOutputThread.Create(Output);
  StderrThread := TPlayerOutputThread.Create(Stderr);
end;

constructor TPlayerProcess.Create;
begin
  inherited Create(nil);
  InputThread := nil;
  OutputThread := nil;
  StderrThread := nil;

  Options := [poUsePipes, poNoConsole];
end;

destructor TPlayerProcess.Destroy;
begin
  FreeAndNil(InputThread);
  FreeAndNil(OutputThread);
  FreeAndNil(StderrThread);
  inherited Destroy;
end;

{ TMainForm }

procedure TMainForm.Process1Btn1Click(Sender: TObject);
begin
  if Process1.Running then exit;
  Process1.Executable := Process1Edit.Text;
  Process1.Execute;
end;

procedure TMainForm.Process1Btn2Click(Sender: TObject);
begin
  if not Process1.Running then exit;
  Process1.Terminate(0);
end;

procedure TMainForm.Process2Btn1Click(Sender: TObject);
begin
  if Process2.Running then exit;
  Process2.Executable := Process2Edit.Text;
  Process2.Execute;
end;

procedure TMainForm.Process2Btn2Click(Sender: TObject);
begin
  if not Process2.Running then exit;
  Process2.Terminate(0);
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
begin
  case Game.State of
    1: DoBeforePlaying;
    2: DoPlaying;
    3: DoAfterPlaying;
  end;
end;

procedure TMainForm.DoBeforePlaying;
begin
  Game.State := 0;
  if not Process1.Running and (Process1Edit.Text<>'') then
    Process1Btn1.Click;
  if not Process2.Running and (Process2Edit.Text<>'') then
    Process2Btn1.Click;

  Game.Init;

  ListBox1.Clear;
  ListBox2.Clear;

  FileName1View.Caption := ExtractFileName(Process1Edit.Text);
  FileName2View.Caption := ExtractFileName(Process2Edit.Text);

  Game.State := 2;
end;

procedure TMainForm.DoPlaying;
begin
  Game.ApplyDamage;

  PaintBox1.Repaint;
  HP1View.Caption:=IntToStr(Game.Player1.HP);
  HP2View.Caption:=IntToStr(Game.Player2.HP);

  if Process1.OutputThread.CanReadLn then
  if Process1.InputThread.CanWriteLn then
  Process1.InputThread.WriteLn(Game.Player1.
    Response(Process1.OutputThread.ReadLn));

  if Process2.OutputThread.CanReadLn then
  if Process2.InputThread.CanWriteLn then
  Process2.InputThread.WriteLn(Game.Player2.
    Response(Process2.OutputThread.ReadLn));

  if Process1.StderrThread.CanReadLn then
    ListBox1.Items.Add(Process1.StderrThread.ReadLn);
  if Process2.StderrThread.CanReadLn then
    ListBox2.Items.Add(Process2.StderrThread.ReadLn);

  Game.NextFrame;
end;

procedure TMainForm.DoAfterPlaying;
begin
  Game.State := 0;
  if Process1.Running then
    Process1Btn2.Click;
  if Process2.Running then
    Process2Btn2.Click;
  case Game.Winner of
    0: ShowMessage('Draw');
    1: ShowMessage('Player 1 wins');
    2: ShowMessage('Player 2 wins');
  end;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Application.AddOnIdleHandler(@FormIdle);
  Process1 := TPlayerProcess.Create;
  Process2 := TPlayerProcess.Create;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  if Game.State=0 then
    Game.State:=1;
  PageControl1.ActivePage := TabSheet2;
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  if Game.State=2 then
    Game.State:=3;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Application.RemoveOnIdleHandler(@FormIdle);
  FreeAndNil(Process1);
  FreeAndNil(Process2);
end;

procedure TMainForm.PaintBox1Paint(Sender: TObject);
var
  Item: TGameObject;
begin
  PaintBox1.Canvas.Clear;
  if Game.State <> 2 then
  begin
    StaticText1.Caption:='';
    StaticText2.Caption:='';
    exit;
  end;

  PaintBox1.Canvas.Pen.Color:=clBlack;
  Ellipse(PaintBox1, Game);

  PaintBox1.Canvas.Pen.Color:=clPurple;
  Ellipse(PaintBox1, Game.Player1);
  for Pointer(Item) in Game.Player1.Buttons do
    Ellipse(PaintBox1, Item);

  PaintBox1.Canvas.Pen.Color:=clTeal;
  Ellipse(PaintBox1, Game.Player2);
  for Pointer(Item) in Game.Player2.Buttons do
    Ellipse(PaintBox1, Item);

  StaticText1.Caption := Game.Player1.GetSummaryText;
  StaticText2.Caption := Game.Player2.GetSummaryText;
end;

procedure TMainForm.FormIdle(Sender: TObject; var Done: Boolean);
var
  t0: String;
begin
  t0 := IfThen(Process1.Running, 'Running', 'Not running');
  Process1Gbox.Caption := Format('Process 1: %s', [t0]);

  t0 := IfThen(Process2.Running, 'Running', 'Not running');
  Process2Gbox.Caption := Format('Process 2: %s', [t0]);

  t0 := IfThen(Game.State=2, 'Running', 'Not running');
  GameGbox.Caption := Format('Game: %s (%d)', [t0, Game.State]);

  ListBox1.ItemIndex := ListBox1.Count-1;
  ListBox2.ItemIndex := ListBox2.Count-1;
end;

end.

