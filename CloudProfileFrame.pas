{
 Original author
 * Dmitry Konnov <konnov72@gmail.com>
 All rights reserved. This source code is not to be redistributed without
 prior permission from the copyright holder.
}

unit CloudProfileFrame;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls,
  SQLite3, SQLite3Wrap, Vcl.ExtCtrls;

type
  TCloudProfileFrm = class(TFrame)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    FilesTabSheet: TTabSheet;
    PageControl2: TPageControl;
    BackblazeSheet: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    ctl_BucketName: TEdit;
    ctl_AppKey: TEdit;
    ctl_AppSecret: TEdit;
    ctl_CheckBucket: TButton;
    HubicSheet: TTabSheet;
    Label14: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    Label11: TLabel;
    ctl_HubicTestBtn: TButton;
    CloudFilesSheet: TTabSheet;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label16: TLabel;
    ctl_CloudfilesLogin: TEdit;
    ctl_CloudFilesPassword: TEdit;
    ctl_CloudFilesCheck: TButton;
    Panel1: TPanel;
    Label12: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    ctl_ActiveStorageComboBox: TComboBox;
    ctl_StorageSizeEdit: TEdit;
    ctl_StorageSize: TUpDown;
    Panel2: TPanel;
    FilesLV: TListView;
    procedure ctl_ActiveStorageComboBoxChange(Sender: TObject);
    procedure ctl_CloudFilesCheckClick(Sender: TObject);
    procedure ctl_CheckBucketClick(Sender: TObject);
    procedure Label13Click(Sender: TObject);
    procedure Label5Click(Sender: TObject);
    procedure Label9Click(Sender: TObject);
    procedure ctl_HubicTestBtnClick(Sender: TObject);
    procedure ctl_BucketNameChange(Sender: TObject);
    procedure ctl_StorageSizeChanging(Sender: TObject;
      var AllowChange: Boolean);
    procedure FilesTabSheetShow(Sender: TObject);
  private
    FModified : boolean;
    FId : Integer;
    FBucketId : string;//B2 BucketId
    { Private declarations }
  public
    procedure EditProfile( Id : Integer );
    procedure LoadFilesListView( AccountId : Integer );
    procedure SaveProfile;
    procedure SetPageControl( ind : Integer; Stmt: TSQLite3Statement = Nil );
    procedure OnApplyClick(Sender: TObject);
    destructor Destroy; override;
    { Public declarations }
  end;
implementation
uses CloudFilesDrive, BackBlazeDrive, Winapi.ShellAPI;
{$R *.dfm}


function FormatByteSize(const bytes: Longint): string;
const   B = 1; //byte
  KB = 1024 * B; //kilobyte
  MB = 1024 * KB; //megabyte
  GB = 1024 * MB; //gigabyte
begin
  if bytes > GB then
    result := FormatFloat('#.## GB', bytes / GB)
  else  if bytes > MB then
    result := FormatFloat('#.## MB', bytes / MB)
  else  if bytes > KB then
    result := FormatFloat('#.## KB', bytes / KB)
  else
    result := FormatFloat('#.## bytes', bytes) ;
end;

procedure TCloudProfileFrm.SaveProfile;
var
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;
  Provider : string;
begin
  Provider := LowerCase(ctl_ActiveStorageComboBox.Items.Strings[ctl_ActiveStorageComboBox.ItemIndex]);
  if Provider = '' then Exit;
  DB := TSQLite3Database.Create;
  try
    DB.Open(RC_LocalDbName);
    Stmt := DB.Prepare('UPDATE accounts SET cloud_type = ? WHERE id = ?');
    try
      Stmt.BindText(1, Provider);
      Stmt.BindInt( 2, FId );
      CheckError( Stmt.Step = SQLITE_DONE, 'Update db error' );
      Stmt.Reset;
      if PageControl1.ActivePage = BackblazeSheet then
      begin
        Stmt := DB.Prepare('UPDATE accounts SET AppKey = ? WHERE id = ?');
        Stmt.BindText(1, EncryptKey( Trim( ctl_AppKey.Text ), GSecretKey ));
        Stmt.BindInt( 2, FId );
        CheckError( Stmt.Step = SQLITE_DONE, 'Update db error' );
        Stmt.Reset;
        Stmt := DB.Prepare('UPDATE accounts SET AppSecret = ? WHERE id = ?');
        Stmt.BindText(1, EncryptKey( Trim( ctl_AppSecret.Text ), GSecretKey ));
        Stmt.BindInt( 2, FId );
        CheckError( Stmt.Step = SQLITE_DONE, 'Update db error' );
        Stmt.Reset;
        Stmt := DB.Prepare('UPDATE accounts SET BucketName = ? WHERE id = ?');
        Stmt.BindText(1, EncryptKey( Trim( NormalyzeToBucketName( Trim( ctl_BucketName.Text ))), GSecretKey ));
        Stmt.BindInt( 2, FId );
        CheckError( Stmt.Step = SQLITE_DONE, 'Update db error' );
        Stmt.Reset;
        Stmt := DB.Prepare('UPDATE accounts SET BucketId = ? WHERE id = ?');
        Stmt.BindText(1, EncryptKey( Trim( FBucketId ), GSecretKey ));
        Stmt.BindInt( 2, FId );
        CheckError( Stmt.Step = SQLITE_DONE, 'Update db error' );
        Stmt.Reset;
      end;
      Stmt := DB.Prepare('UPDATE accounts SET StorageSize = ? WHERE id = ?');
      Stmt.BindDouble(1, ctl_StorageSize.Position );
      Stmt.BindInt( 2, FId );
      CheckError( Stmt.Step = SQLITE_DONE, 'Update db error' );
      Stmt.Reset;
    finally
      FreeAndNil(Stmt);
    end;
  finally
    FreeAndNil(DB);
  end;
  FModified := false;
end;

procedure TCloudProfileFrm.EditProfile( Id : Integer );
var
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;
begin
  DB := TSQLite3Database.Create;
  try
    DB.Open(RC_LocalDbName);
    Stmt := DB.Prepare('SELECT id, cloud_type, AppKey, AppSecret, BucketName,'+
      ' StorageSize, BucketId  FROM accounts WHERE id = ?');
    try
      Stmt.BindInt( 1, Id );
      while Stmt.Step = SQLITE_ROW do
      begin
        ctl_ActiveStorageComboBox.ItemIndex :=
          ctl_ActiveStorageComboBox.Items.IndexOf( Stmt.ColumnText( ColumnByName('cloud_type', Stmt ) ) );
        SetPageControl( ctl_ActiveStorageComboBox.ItemIndex, Stmt );
      end;
    finally
      FreeAndNil(Stmt);
    end;
  finally
    FreeAndNil(DB);
  end;
  FId := Id;
  FModified := false;
end;

procedure TCloudProfileFrm.FilesTabSheetShow(Sender: TObject);
begin
  LoadFilesListView(FId);
end;

procedure TCloudProfileFrm.LoadFilesListView( AccountId : Integer );
var
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;
  Li : TListItem;
  UploadTime : TDateTime;
begin
  FilesLV.Items.Clear;
  DB := TSQLite3Database.Create;
  try
    Screen.Cursor := crSQLWait;
    DB.Open(RC_LocalDbName);
    Stmt := DB.Prepare('SELECT * FROM files WHERE account_id = ?');
    try
      Stmt.BindInt( 1, AccountId );
      while Stmt.Step = SQLITE_ROW do
      begin
        Li := FilesLV.Items.Add;
        Li.Caption :=  Stmt.ColumnText( ColumnByName('RemoteFileName', Stmt ));
        Li.SubItems.Add( FormatByteSize( Stmt.ColumnInt64(ColumnByName('FileSize', Stmt )) ));
        UploadTime := Stmt.ColumnDouble(ColumnByName('UploadTime', Stmt ));
        Li.SubItems.Add( DateTimeToStr( UploadTime  ) );
      end;
    finally
      FreeAndNil(Stmt);
    end;
  finally
    FreeAndNil( DB );
    Screen.Cursor := crDefault;
  end;
end;

procedure TCloudProfileFrm.OnApplyClick(Sender: TObject);
begin
  SaveProfile;
end;


procedure TCloudProfileFrm.ctl_ActiveStorageComboBoxChange(Sender: TObject);
begin
  SetPageControl(ctl_ActiveStorageComboBox.ItemIndex);
  FModified := true;
end;

procedure TCloudProfileFrm.ctl_BucketNameChange(Sender: TObject);
begin
  FModified := true;
end;

procedure TCloudProfileFrm.SetPageControl( ind : Integer; Stmt: TSQLite3Statement );
begin
  if ind = 1 then
    PageControl2.ActivePage := BackblazeSheet
  else if ind = 0 then
    PageControl2.ActivePage := HubicSheet
  else
    PageControl2.ActivePage := Nil;

  if Assigned( Stmt ) then
  begin
    if ind = 0 then
    begin
      ctl_AppKey.Text := DecryptKey( Stmt.ColumnText( ColumnByName('AppKey', Stmt ) ), GSecretKey );
      ctl_AppSecret.Text := DecryptKey( Stmt.ColumnText( ColumnByName('AppSecret', Stmt ) ) , GSecretKey );
      ctl_BucketName.Text := DecryptKey( Stmt.ColumnText( ColumnByName('BucketName', Stmt ) ), GSecretKey );
      FBucketId := DecryptKey( Stmt.ColumnText( ColumnByName('BucketId', Stmt ) ), GSecretKey );
    end
    else if ind = 1 then
    begin

    end;
    ctl_StorageSize.Position := Stmt.ColumnInt( ColumnByName('StorageSize', Stmt ) );
  end;
end;

procedure TCloudProfileFrm.ctl_CheckBucketClick(Sender: TObject);
var
  B2Drive : TB2Drive;
begin
  try
    CheckError( FId > 0, 'FId = 0' );
    CheckError( ctl_AppKey.Text > '', 'AppKey is empty' );
    CheckError( ctl_AppSecret.Text > '', 'AppSecret is empty' );
    CheckError( ctl_BucketName.Text > '', 'BucketName is empty' );
    B2Drive := TB2Drive.Create( Nil );
    try
      B2Drive.AccountId := FId;
      B2Drive.BucketName := NormalyzeToBucketName( ctl_BucketName.Text );
      B2Drive.AppKey := ctl_AppKey.Text;
      B2Drive.AppSecret := ctl_AppSecret.Text;
      B2Drive.SaveTokens;
      B2Drive.DoAuth();
      B2Drive.BucketId := B2Drive.GetBucketId( B2Drive.BucketName );
      if B2Drive.BucketId > '' then
        MessageBox( Handle, PChar('Bucket: ' + B2Drive.BucketName + ' exists.'), 'Info', MB_OK )
      else
      begin
         B2Drive.BucketId := B2Drive.CreateBucket(B2Drive.BucketName);
         CheckError( B2Drive.BucketId > '', 'GBucketId is empty' );
         MessageBox( Handle, PChar('Bucket: ' + B2Drive.BucketName + ' was created.'), 'Info', MB_OK );
         ctl_BucketName.Text := B2Drive.BucketName;
         B2Drive.SaveTokens;
         FModified := true;
      end;
    finally
      FreeAndNil( B2Drive );
    end;
 except on E : Exception do
 begin
   MessageBox( Handle, PChar(E.Message), 'Message', MB_OK );
 end;
 end;

end;

procedure TCloudProfileFrm.ctl_CloudFilesCheckClick(Sender: TObject);
begin
    CheckError( MailURLMayBeInvalid( Trim( ctl_CloudfilesLogin.Text ) ), 'Not valid email format' );
  if ( ctl_CloudFilesPassword.Text <> GCloudFilesPassword ) and ( ctl_CloudFilesPassword.Text <> '*****' )then
     GCloudFilesPassword := ctl_CloudFilesPassword.Text;
  if RequestCloudFilesAuthority( ctl_CloudfilesLogin.Text, GCloudFilesPassword ) then
    MessageBox( Handle, PChar('Authentication OK!'), 'Info', MB_OK )
  else
    MessageBox( Handle, PChar('Either login or pasword are wrong!'), 'Info', MB_OK )
end;

procedure TCloudProfileFrm.ctl_HubicTestBtnClick(Sender: TObject);
begin
{var
  HubicDrive : THubicDrive;
  try
    HubicDrive := THubicDrive.Create( Nil );
    try
      HubicDrive.AccountId := FId;
      HubicDrive.LoadTokens;
      if not HubicDrive.TestTokens then
      begin
        HubicDrive.DoAuth();
        FModified := true;
      end;
      if HubicDrive.Authenticated then
        MessageBox( Handle, 'Connection to Hubic successful.', 'Message', MB_OK );
    finally
      FreeAndNil( HubicDrive );
    end;
   except on E : Exception do
   begin
     MessageBox( Handle, PChar(E.Message), 'Message', MB_OK );
   end;
   end; }
end;

procedure TCloudProfileFrm.ctl_StorageSizeChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  FModified := true;
end;

destructor TCloudProfileFrm.Destroy;
begin
  if FModified then
  begin
    if MessageBox( Handle, 'Save profile?', 'Message', MB_YESNO ) = ID_YES then
      SaveProfile;
  end;
  inherited;
end;

procedure TCloudProfileFrm.Label13Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'http://www.hubic.com', '',  Nil, SW_SHOWNORMAL);
end;

procedure TCloudProfileFrm.Label5Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'http://www.cloudfiles365.com', '',  Nil, SW_SHOWNORMAL);
end;

procedure TCloudProfileFrm.Label9Click(Sender: TObject);
begin
  ShellExecute(handle, 'open', 'https://www.backblaze.com/', '',  Nil, SW_SHOWNORMAL);
end;

end.
