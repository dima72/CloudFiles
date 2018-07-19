{
 Original author
 * Dmitry Konnov <konnov72@gmail.com>
 All rights reserved. This source code is not to be redistributed without
 prior permission from the copyright holder.
}

unit B2ConfigForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls,
  CloudProfileFrame, System.Actions, Vcl.ActnList, Vcl.Menus;

type
  TB2ConfigFrm = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    btnNewAccount: TButton;
    btnDeleteAccount: TButton;
    btnApply: TButton;
    ctl_OK: TButton;
    Panel3: TPanel;
    TV: TTreeView;
    PopupMenu1: TPopupMenu;
    acEditAccountName1: TMenuItem;
    ActionList1: TActionList;
    acEditAccountName: TAction;
    Panel4: TPanel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure btnNewAccountClick(Sender: TObject);
    procedure btnDeleteAccountClick(Sender: TObject);
    procedure TVEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure TVChange(Sender: TObject; Node: TTreeNode);
    procedure acEditAccountNameExecute(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FCurrentFrame: TCloudProfileFrm;
    FTestConnectionOnClose : boolean;
    procedure LoadAccountsIntoTree;
    procedure EditProfile( Id : Integer );
    procedure DeleteAccount( AccountId : Integer );

    { Private declarations }
  public
    { Public declarations }
    property TestConnectionOnClose : boolean read FTestConnectionOnClose write FTestConnectionOnClose;
  end;

  TAccountDesc = class( TObject )
  private
    FId : Integer;
    FName : string;
  public
    property Name : string read FName write FName;
    property Id : Integer read FId write FId;
  end;

var
  B2ConfigFrm: TB2ConfigFrm;
implementation
uses Inifiles, CloudFilesDrive, BackBlazeDrive, Winapi.ShellAPI, SQLite3, SQLite3Wrap;
{$R *.dfm}

procedure TB2ConfigFrm.EditProfile( Id : Integer );
begin
  if Assigned( FCurrentFrame ) then
     FreeAndNil( FCurrentFrame );
  FCurrentFrame := TCloudProfileFrm.Create(Self);
  FCurrentFrame.Parent := Panel1;
  FCurrentFrame.Align := alClient;
  FCurrentFrame.EditProfile(Id);
  btnApply.OnClick := FCurrentFrame.OnApplyClick;
end;

procedure TB2ConfigFrm.TVChange(Sender: TObject; Node: TTreeNode);
begin
  if not Assigned( Node ) then Exit;
  if not Assigned( Node.Data ) then Exit;
  EditProfile(TAccountDesc(Node.Data).Id);
end;

procedure TB2ConfigFrm.TVEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
var
  AccountDesc : TAccountDesc;
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;
begin
  if not Assigned( Node ) then Exit;
  if S = '' then Exit;
  if Assigned( Node.Data ) then
  begin
    AccountDesc := TAccountDesc( Node.Data );
  end
  else
  begin
    AccountDesc := TAccountDesc.Create;
    AccountDesc.Id := 0;
    Node.Data := AccountDesc;
  end;
  AccountDesc.Name := S;
  DB := TSQLite3Database.Create;
  try
    DB.Open(RC_LocalDbName);
    try
      if AccountDesc.Id = 0 then
      begin
        Stmt := DB.Prepare('INSERT INTO accounts (name) VALUES (?)');
        Stmt.BindText(1, AccountDesc.Name);
        Stmt.StepAndReset;
        AccountDesc.Id := DB.LastInsertRowID;
      end
      else
      begin
        Stmt := DB.Prepare('UPDATE accounts SET name = ? WHERE id = ?');
        Stmt.BindText(1, AccountDesc.Name);
        Stmt.BindInt(2, AccountDesc.Id);
        Stmt.StepAndReset;
      end;
    finally
      if Assigned( Stmt ) then
        FreeAndNil(Stmt);
    end;
  finally
    FreeAndNil( DB );
  end;
  EditProfile(AccountDesc.Id);
end;


procedure TB2ConfigFrm.LoadAccountsIntoTree;
var
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;
  RootNode, NewNode : TTreeNode;
begin
  RootNode := TV.Items.Item[0];
  CheckError( Assigned( RootNode ), 'RootNode = Nil' );
  RootNode.DeleteChildren;
  DB := TSQLite3Database.Create;
  try
    DB.Open(RC_LocalDbName);
    Stmt := DB.Prepare('SELECT id, name FROM accounts');
    try
      while Stmt.Step = SQLITE_ROW do
      begin
        NewNode := TV.Items.AddChild( RootNode, Stmt.ColumnText(1));
        NewNode.Data := TAccountDesc.Create;
        TAccountDesc( NewNode.Data ).Id := Stmt.ColumnInt(0);
        TAccountDesc( NewNode.Data ).Name := Stmt.ColumnText(1);
      end;
      RootNode.Expanded := true;
    finally
      FreeAndNil(Stmt);
    end;
  finally
    FreeAndNil( DB );
  end;
end;


procedure TB2ConfigFrm.acEditAccountNameExecute(Sender: TObject);
var
  RootNode, SelectedNode : TTreeNode;
begin
  RootNode := TV.Items.Item[0];
  CheckError( Assigned( RootNode ), 'RootNode = Nil' );
  SelectedNode := TV.Selected;
  if SelectedNode = RootNode then Exit;
  SelectedNode.EditText;
end;

procedure TB2ConfigFrm.btnDeleteAccountClick(Sender: TObject);
var
  RootNode, SelectedNode : TTreeNode;
begin
  RootNode := TV.Items.Item[0];
  CheckError( Assigned( RootNode ), 'RootNode = Nil' );
  SelectedNode := TV.Selected;
  if( not Assigned( SelectedNode ) or ( SelectedNode = RootNode ) or
    ( SelectedNode.Data = Nil) ) then
    Exit;

  DeleteAccount( TAccountDesc( SelectedNode.Data ).Id );
  SelectedNode.Delete;

end;

procedure TB2ConfigFrm.DeleteAccount( AccountId : Integer );
var
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;
  CountFiles : LongInt;
begin
  DB := TSQLite3Database.Create;
  try
    DB.Open(RC_LocalDbName);
    Stmt := DB.Prepare('SELECT COUNT(*) FROM files WHERE account_id = ?');
    try
      Stmt.BindInt( 1, AccountId );
      CheckError( Stmt.Step = SQLITE_ROW, 'query failed');
      CountFiles := Stmt.ColumnInt64(0);
      CheckError( CountFiles = 0, 'this profile still has uploaded files, '+#10#13+
        'please download files back first before you can delete it.' );
      Stmt.Reset;
      Stmt := DB.Prepare('DELETE FROM accounts WHERE id = ?');
      Stmt.BindInt( 1, AccountId );
      CheckError( Stmt.Step = SQLITE_DONE, 'delete failed' );
      Stmt.Reset;
    finally
      FreeAndNil(Stmt);
    end;
  finally
    FreeAndNil(DB);
  end;
end;

procedure TB2ConfigFrm.btnNewAccountClick(Sender: TObject);
var
  RootNode, NewNode : TTreeNode;
begin
  RootNode := TV.Items.Item[0];
  CheckError( Assigned( RootNode ), 'RootNode = Nil' );
  NewNode := TV.Items.AddChild( RootNode, '' );
  RootNode.Expanded := true;
  TV.Select(NewNode, []);
  NewNode.EditText;
end;

procedure TB2ConfigFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Assigned( FCurrentFrame ) then
    FCurrentFrame.Free;
  CanClose := true;
end;

procedure TB2ConfigFrm.FormCreate(Sender: TObject);
begin
 // CreateLocalDatabaseStructure;
  LoadAccountsIntoTree;
end;

end.
