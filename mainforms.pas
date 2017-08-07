unit MainForms;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process, FileUtil, Forms, Controls, Graphics, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, DbCtrls, PairSplitter, Pipes, ThreadQueue,
  PlayerInputThread, PlayerOutputThread, GameObject, GameCanvas, Games,
  FPCanvas;

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
    Bevel1: TBevel;
    Button1: TButton;
    Button3: TButton;
    Button4: TButton;
    FileName1View: TLabel;
    HP2View: TLabel;
    HP1View: TLabel;
    FileName2View: TLabel;
    GameGbox: TGroupBox;
    CanvasLabel1: TLabel;
    CanvasLabel2: TLabel;
    Label3: TLabel;
    RadioLabel: TLabel;
    ListBox1: TListBox;
    ListBox2: TListBox;
    RadioView: TListBox;
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
    CanvasView1: TStaticText;
    CanvasView2: TStaticText;
    StaticText3: TLabel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Timer1: TTimer;
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure Label3Click(Sender: TObject);
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
    procedure Player1Radio(const Message: String);
    procedure Player2Radio(const Message: String);
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
  RadioView.Clear;

  FileName1View.Caption := ExtractFileName(Process1Edit.Text);
  FileName2View.Caption := ExtractFileName(Process2Edit.Text);

  CanvasLabel1.Tag := 0;
  CanvasLabel2.Tag := 0;

  Game.State := 2;
end;

procedure TMainForm.DoPlaying;
var
  i: Integer;
begin
  Game.ApplyDamage;

  PaintBox1.Repaint;
  HP1View.Caption:=IntToStr(Game.Player1.HP);
  HP2View.Caption:=IntToStr(Game.Player2.HP);

  if Process1.OutputThread.CanReadLn and Process1.InputThread.CanWriteLn then
    Process1.InputThread.WriteLn(Game.Player1.Response(Process1.OutputThread.ReadLn))
  else
    CanvasLabel1.Tag := CanvasLabel1.Tag + 1;

  if Process2.OutputThread.CanReadLn and Process2.InputThread.CanWriteLn then
    Process2.InputThread.WriteLn(Game.Player2.Response(Process2.OutputThread.ReadLn))
  else
    CanvasLabel2.Tag := CanvasLabel2.Tag + 1;

  for i := 1 to 8 do
  if Process1.StderrThread.CanReadLn then
    ListBox1.Items.Add(Process1.StderrThread.ReadLn)
  else
    break;

  for i := 1 to 8 do
  if Process2.StderrThread.CanReadLn then
    ListBox2.Items.Add(Process2.StderrThread.ReadLn)
  else
    break;

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

procedure TMainForm.Player1Radio(const Message: String);
begin
  RadioView.Items.Add('Player 1: ' + Message);
end;

procedure TMainForm.Player2Radio(const Message: String);
begin
  RadioView.Items.Add('Player 2: ' + Message);
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Process1 := TPlayerProcess.Create;
  Process2 := TPlayerProcess.Create;

  Application.AddOnIdleHandler(@FormIdle);
  Game.Player1.OnRadio := @Player1Radio;
  Game.Player2.OnRadio := @Player2Radio;
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  if Game.State=0 then
    Game.State:=1;
  PageControl1.ActivePage := TabSheet2;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  case Game.State of
    0: Button3.Click;
    2: Button4.Click;
  end;
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  if Game.State=2 then
    Game.State:=3;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  Game.Player1.OnRadio := nil;
  Game.Player2.OnRadio := nil;
  Application.RemoveOnIdleHandler(@FormIdle);

  FreeAndNil(Process1);
  FreeAndNil(Process2);
end;

procedure TMainForm.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  case Length(FileNames) of
    0: exit;
    1: begin
      case QuestionDlg('1 file dropped', 'What do you want with this file?',
        mtCustom, [mrYes, 'Use as process 1', mrNo, 'Use as process 2',
        mrCancel, 'Cancel'], 0)
      of
        mrYes: Process1Edit.Text := FileNames[0];
        mrNo: Process2Edit.Text := FileNames[0];
      end;
    end;
    2: begin
      Process1Edit.Text := FileNames[0];
      Process2Edit.Text := FileNames[1];
    end;
  otherwise
    ShowMessage('Accept one or two files only');
  end;
end;

procedure TMainForm.Label3Click(Sender: TObject);
begin
  Label3.Tag := Label3.Tag xor 1;
end;

procedure TMainForm.PaintBox1Paint(Sender: TObject);

  procedure Paint(Item: TGameObject; PenColor, BrushColor: TColor;
    BrushStyle: TFPBrushStyle=bsSolid);
  var
    OldPenColor, OldBrushColor: TColor;
    OldBrushStyle: TFPBrushStyle;
  begin
    OldPenColor := PaintBox1.Canvas.Pen.Color;
    PaintBox1.Canvas.Pen.Color := PenColor;
    OldBrushColor := PaintBox1.Canvas.Brush.Color;
    PaintBox1.Canvas.Brush.Color := BrushColor;
    OldBrushStyle := PaintBox1.Canvas.Brush.Style;
    PaintBox1.Canvas.Brush.Style := BrushStyle;
    Ellipse(PaintBox1, Item);
    PaintBox1.Canvas.Pen.Color := OldPenColor;
    PaintBox1.Canvas.Brush.Color := OldBrushColor;
    PaintBox1.Canvas.Brush.Style := OldBrushStyle;
  end;

var
  Item: TGameObject;
begin
  PaintBox1.Canvas.Clear;
  if Game.State <> 2 then
  begin
    CanvasView1.Caption:='';
    CanvasView2.Caption:='';
    exit;
  end;

  Paint(Game, clBlack, clDefault);

  for Pointer(Item) in Game.HEs do
    Paint(Item, clRed, clRed, bsCross);

  PaintBox1.Canvas.Pen.Color:=clPurple;
  Ellipse(PaintBox1, Game.Player1);
  for Pointer(Item) in Game.Player1.Buttons do
    Ellipse(PaintBox1, Item);

  PaintBox1.Canvas.Pen.Color:=clTeal;
  Ellipse(PaintBox1, Game.Player2);
  for Pointer(Item) in Game.Player2.Buttons do
    Ellipse(PaintBox1, Item);

  for Pointer(Item) in Game.SGs do
    Paint(Item, clBlue, clWhite, bsDiagCross);

  for Pointer(Item) in Game.HEs do
    if Item.T = 0 then
    Paint(Item, clRed, clRed);

  CanvasView1.Caption := Game.Player1.GetSummaryText;
  CanvasView2.Caption := Game.Player2.GetSummaryText;
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

  t0 := IfThen(Game.State=2, ' (Playing)', '');
  Caption := 'RoundShooter' + t0;

  ListBox1.ItemIndex := ListBox1.Count-1;
  ListBox2.ItemIndex := ListBox2.Count-1;
  RadioView.ItemIndex := RadioView.Count-1;

  GameGbox.Visible := Label3.Tag=1;
  Process1Btn1.Visible := Label3.Tag=1;
  Process1Btn2.Visible := Label3.Tag=1;
  Process2Btn1.Visible := Label3.Tag=1;
  Process2Btn2.Visible := Label3.Tag=1;
  Button1.Visible := Label3.Tag=0;
  Button1.Enabled := not odd(Game.State);
  Button1.Caption := IfThen(Game.State < 2, 'Start game', 'Stop game');

  CanvasLabel1.Caption := 'Player 1' + IfThen(CanvasLabel1.Tag = 0, '',
    Format(' (%d frame(s) wasted)', [CanvasLabel1.Tag]));
  CanvasLabel2.Caption := 'Player 2' + IfThen(CanvasLabel2.Tag = 0, '',
    Format(' (%d frame(s) wasted)', [CanvasLabel2.Tag]));
end;

end.

