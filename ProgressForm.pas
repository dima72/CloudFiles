unit ProgressForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TProgressFrm = class(TForm)
    ProgressBar1: TProgressBar;
    LblFileName: TLabel;
    LblComplete: TLabel;
    Cancel: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure CancelClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
  end;
  TProgressThread = class( TThread )
  public
    procedure DoProgress;
  protected
    procedure Execute; override;
  end;

  procedure StartProgress( FileName : string; Max :Int64 );
  procedure EndProgress;
var
  ProgressFrm: TProgressFrm;
  ProgressThread : TProgressThread;
implementation
uses CloudFilesDrive;
{$R *.dfm}

procedure StartProgress( FileName : string; Max :Int64 );
begin
  InitProgress;
  ProgressFrm := TProgressFrm.Create(Nil);
  ProgressFrm.Position := poMainFormCenter;
  ProgressFrm.ProgressBar1.Max := Max;
  ProgressFrm.ProgressBar1.Position := 0;
  ProgressFrm.LblFileName.Caption := 'File: ' + FileName;
  ProgressFrm.LblComplete.Caption := 'Connecting...';
  ProgressFrm.Show;
  ProgressFrm.Invalidate;
  Application.ProcessMessages;
  ProgressThread := TProgressThread.Create( true );
  ProgressThread.FreeOnTerminate := true;
  ProgressThread.Start;
end;

procedure EndProgress;
begin
  if Assigned( ProgressThread ) then
  begin
    ProgressThread.Terminate;
  end;
end;



{ TProgressThread }

procedure TProgressThread.DoProgress;
begin
  if Assigned( ProgressFrm ) then
    ProgressFrm.ProgressBar1.Position := GetProgress;
  ProgressFrm.LblComplete.Caption := IntToStr(
   ( ( (ProgressFrm.ProgressBar1.Position + 1) * 100 )
   div ( ProgressFrm.ProgressBar1.Max + 1 ) ) ) + ' % completed';
end;

procedure TProgressThread.Execute;
begin
  //inherited;
  while not Terminated do
  begin
    Application.ProcessMessages;
    Synchronize( DoProgress );
    Sleep( 10 );
  end;
  FreeAndNil( ProgressFrm );
end;

procedure TProgressFrm.CancelClick(Sender: TObject);
begin
   Close;
end;

procedure TProgressFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := false;
  if MessageBox( Handle, 'Abort operation?', 'Question', MB_YESNO ) = ID_YES then
  begin
    SetAbort;
    CanClose := true;
  end;
end;

end.
