unit UploadDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, SQLite3, SQLite3Wrap;

type
  TUploadDialogFrm = class(TForm)
    Button1: TButton;
    Panel1: TPanel;
    ctl_AccountName: TComboBox;
    Label1: TLabel;
    lblFileName: TLabel;
    Label2: TLabel;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  UploadDialogFrm: TUploadDialogFrm;

implementation
uses CloudFilesDrive;
{$R *.dfm}

procedure TUploadDialogFrm.FormShow(Sender: TObject);
var
  DB : TSQLite3Database;
  Stmt: TSQLite3Statement;
begin
  DB := TSQLite3Database.Create;
  try
    DB.Open(  RC_LocalDbName);
    Stmt := DB.Prepare('SELECT id, name FROM accounts');
    try
      while Stmt.Step = SQLITE_ROW do
      begin
        ctl_AccountName.Items.Add( Stmt.ColumnText(ColumnByName('name', Stmt )) );
      end;
    finally
      FreeAndNil(Stmt);
    end;
    ctl_AccountName.ItemIndex := 0;
  finally
    FreeAndNil( DB );
  end;
end;

end.
