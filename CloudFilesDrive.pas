{
 Original author
 * Dmitry Konnov <konnov72@gmail.com>
 All rights reserved. This source code is not to be redistributed without
 prior permission from the copyright holder.
}
unit CloudFilesDrive;
interface
uses Classes, Winapi.Windows, System.SysUtils, Forms,
  synacode, synautil, httpsend, ssl_openssl, ssl_openssl_lib, blcksock, SyncObjs,
  Shlobj, ActiveX, ComObj, System.Variants, WinInet,
  IdCompressorZLib, OverbyteIcsMD5, SQLite3, SQLite3Wrap, SHA1Code, DKJSON;

type
  PMD5Context= ^TMD5Context;
  PMD5Digest =^TMD5Digest;

  TCoreCloudHeader = record
    header: String;
    value: String;
  end;
  TCoreCloudHeaders = array of TCoreCloudHeader;
  THTTPCommand = (cbhcPost,cbhcPut,cbhcPostMultiPart,cbhcPostMultiPartRelated,
  cbhcPutMultiPart,cbhcPutNoHeader,cbhcMove,cbhcPatch);


  TCloudFilesDrive = class( TComponent )
  protected
    FAccountId : Integer;
    FAppKey : string;
    FAppSecret : string;
  public
    procedure CheckDriveConnected(); virtual; abstract;
    procedure DoAuth(); virtual; abstract;
    procedure UploadFromStream( const DestFileNameAndPath : string;  const ASource: TStream; var FileID : string; var Filename : string);virtual; abstract;
    function DownloadToStream( const  FileID : string; const FileName : string;
       ADest: TStream ): boolean;virtual; abstract;
    procedure DeleteFile( const FileId : UnicodeString; const FileName : UnicodeString );virtual; abstract;
    property AppKey : string read FAppKey write FAppKey;
    property AppSecret : string read FAppSecret write FAppSecret;
    property AccountId : Integer read FAccountId  write FAccountId;
  end;

  TCFItem = class( TObject )
  private
    FFullpath : string;
  public
    property Fullpath : string read FFullpath write FFullpath;
  end;


  TAnsiStringStream = class(TStream)
  private
    FDataString: AnsiString;
    FPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const AString: AnsiString);
    function Read(var Buffer; Count: Longint): Longint; override;
    function ReadString(Count: Longint): AnsiString;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure WriteString(const AString: AnsiString);
    property DataString: AnsiString read FDataString;
  end;


  function GetCloudFilesDrive( CloudAccountName : string ) : TCloudFilesDrive;
  function CFUploadFile( const SourceFileName : UnicodeString; OwnerHandle : THandle ): string;
  procedure CFRegisterFileInDatabase( AccountId : Integer; RemoteFileID : string;
    RemoteFilename : string;  FileSize : Double );
  procedure CFUnRegisterFileFromDatabase( AccountId : Integer; RemoteFileID : string );

  function CFDownloadFile(  const MetaFileName : string; OwnerHandle : THandle ) : string;
  procedure CFDeleteFile(  const MetaFileName : UnicodeString; OwnerHandle : THandle );

  procedure AddHeader(var AHeaders: TCoreCloudHeaders; Header: String; Value: String);
  function HttpsPost(const ServerName, Resource: String;Headers: TCoreCloudHeaders;
      const  PostData : AnsiString;Var Response:AnsiString): Integer;

  function SynHttpsPut(AnUrl: string; customheaders: TCoreCloudHeaders;
                       customdata: ansistring; HttpCommand: THTTPCommand;
                       var ResultCode:Integer;
                       const ASource: TStream;
                       var md5Str : AnsiString; var sha1 : AnsiString;
                       AContentType:string='application/octet-stream';
                       const ImmediateRetries:Boolean=false;
                       const MultiMD5Context:PMD5Context=nil;
                       const MultiLastPart:Boolean=false;
                       RetryAfterForbidden:Boolean=false;
                       const TolerateHTTPErrorCode:Integer=0;
                       const DoNotRetryError:AnsiString='';
                       const CanDecompress:Boolean=true): ansistring;

  function HttpError(ErrorCode:Cardinal): string;
  function URLEncode(const Url: string): string;
  function UrlDecode(const S : String) : String;
  function HttpsGet(const Url: String; Headers: TCoreCloudHeaders; Dest : TStream = Nil) : AnsiString;
  procedure CheckError( ACase : Boolean; const ErrorMsg : AnsiString );
  function GetJSONProp(JSON: string; ID: string): UnicodeString; overload;
  function GetJSONProp(O: TJSONOBject; ID: string): UnicodeString; overload;
  function GetJSONValue(O: TJSONObject; ID: string): TJSONValue;
  function CleanUpJSON(Value: ansistring): ansistring;
  function FileVersion(const FileName: TFileName): String;
  function getFileSizeInBytes(const fn: string): integer;
  function GetProgress : Int64;
  procedure AddProgress( Val : Int64 );
  procedure InitProgress;
  procedure RaiseIfAbort;
  procedure SetAbort;
  function NormalyzeToBucketName( const Val : string ): string;
  function DecryptKey( Val : string; SecretVal : string ): string;
  function EncryptKey( Val : string; SecretVal : string ): string;
  function GetSpecialFolderPath(folder : integer) : string;
  function GetIniPath : string;
  function DetectMIMEType(const fn:string):string;
  function PosFromIndex( SubStr, S : UnicodeString; Index : integer ) : integer; overload;
  function ExtractBetween(const Value : UnicodeString; BeginMark : UnicodeString; EndMark : UnicodeString = '' ) : UnicodeString;
  function MailURLMayBeInvalid(const s: string): Boolean;
  function MemStreamToStr( MS : TMemoryStream ): string;
  function RequestCloudFilesAuthority( Login : string; Password : string ): boolean;
  function GetWMIstring(const WMIClass, WMIProperty:string): string;


  procedure CloudSafeInternetReadStream(const UrlHandle:HINTERNET;const memst:TStream;
                                          const ExpectedSize:Int64=-1;
                                          const ReportProgress:Boolean=false;
                                          const URL:string='');
  function CloudSafeInternetReadString(const UrlHandle:HINTERNET):RawByteString;
  function ColumnByName(const ColumnName: WideString; Stmt: TSQLite3Statement ): Integer;
  procedure CheckLocalDatabaseStructure;
  function AccountsEmpty: boolean;

 var
  GCloudFilesLogin : string;
  GCloudFilesPassword : string;
  GSecretKey : string; //used for internal encrypt
  RC_LocalDbName : string;
 const
  GAgent = 'Mozilla/5.001 (windows; U; NT4.0; en-US; rv:1.0) Gecko/25250101';
  cmdHTTPCommands:array[THTTPCommand] of string =
        ('POST','PUT','POST','POST','PUT','PUT', 'MOVE', 'PATCH');
  cMultipartBoundary='_+*_BOUNDARY_!$_{2BcBCf40-D36e-4041-9A9e-8d16E5358C32}_/(_BOUNDARY_=?_';
  cAttemptCount=3;
  //these keys are solely experiemental
  GCloudFilesAppKey = '532880bb7475';
  GCloudFilesAppSecret = '0019cf303b98bed6541d0d75bf292eaadc557380b1';
implementation
uses BackBlazeDrive, ProgressForm, Inifiles, UploadDialog, Vcl.Controls;
var
   GProgress : Int64;
   GAbort : boolean;
   GLastError : WORD;

function ColumnExists(const ColumnName: WideString; Stmt: TSQLite3Statement ): boolean;
var
  I : Integer;
begin
  for I := 0 to Stmt.ColumnCount do
  begin
    if Stmt.ColumnName(I) = ColumnName then
    begin
      Result := true;
      Exit;
    end;
  end;
  Result := false;
end;

function ColumnByName(const ColumnName: WideString; Stmt: TSQLite3Statement ): Integer;
var
  I : Integer;
begin
  for I := 0 to Stmt.ColumnCount do
  begin
    if Stmt.ColumnName(I) = ColumnName then
    begin
      Result := I;
      Exit;
    end;
  end;
  CheckError( false, 'Field ' + ColumnName + ' not found' );
end;

function AccountsEmpty: boolean;
var
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;
begin
  DB := TSQLite3Database.Create;
  try
    DB.Open(RC_LocalDbName);
    Stmt := DB.Prepare('SELECT COUNT(*) FROM accounts');
    try
      CheckError( Stmt.Step = SQLITE_ROW, 'select db error' );
      Result := Stmt.ColumnInt(0) > 0;
    finally
      FreeAndNil(Stmt);
    end;
  finally
    FreeAndNil( DB );
  end;
end;

procedure CreateColumn( const TableName: string; const ColumnSQL: string );
var
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;
begin
  DB := TSQLite3Database.Create;
  try
    DB.Open(RC_LocalDbName);
    Stmt := DB.Prepare('ALTER TABLE '+TableName+' ADD COLUMN ' + ColumnSQL);
    try
      CheckError( Stmt.Step = SQLITE_DONE,
        'alter table '+TableName+' error ADD COLUMN:' + ColumnSQL );
      Stmt.Reset;
    finally
      FreeAndNil(Stmt);
    end;
  finally
    FreeAndNil( DB );
  end;
end;



procedure CheckLocalDatabaseStructure;
var
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;
  res : integer;
begin
 // Create database and fill it with example data
  DB := TSQLite3Database.Create;
  try
    DB.Open(RC_LocalDbName);

    DB.Execute('CREATE TABLE if not exists accounts ( id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT UNIQUE )' );
    Stmt := DB.Prepare('SELECT * FROM accounts WHERE id = -1');
    res := Stmt.Step;
    CheckError( ( res = SQLITE_ROW ) or ( res = SQLITE_DONE ), 'select db error' );
    if not ColumnExists( 'cloud_type', Stmt ) then
      CreateColumn( 'accounts', 'cloud_type TEXT DEFAULT "backblaze"' );
    if not ColumnExists( 'ACCESS_TOKEN', Stmt ) then
      CreateColumn( 'accounts', 'ACCESS_TOKEN TEXT' );
    if not ColumnExists( 'AUTH_TOKEN', Stmt ) then
      CreateColumn( 'accounts', 'AUTH_TOKEN TEXT' );
    if not ColumnExists( 'ACCESS_TOKEN_SECRET', Stmt ) then
      CreateColumn( 'accounts', 'ACCESS_TOKEN_SECRET TEXT' );
    if not ColumnExists( 'AUTH_TOKEN_SECRET', Stmt ) then
      CreateColumn( 'accounts', 'AUTH_TOKEN_SECRET' );
    if not ColumnExists( 'REFRESH_TOKEN', Stmt ) then
      CreateColumn( 'accounts', 'REFRESH_TOKEN TEXT' );
    if not ColumnExists( 'ACCESS_ENDPOINT', Stmt ) then
      CreateColumn( 'accounts', 'ACCESS_ENDPOINT TEXT' );
    if not ColumnExists( 'AppKey', Stmt ) then
      CreateColumn( 'accounts', 'AppKey TEXT' );
    if not ColumnExists( 'AppSecret', Stmt ) then
      CreateColumn( 'accounts', 'AppSecret TEXT' );
    if not ColumnExists( 'BucketName', Stmt ) then
      CreateColumn( 'accounts', 'BucketName TEXT' );
    if not ColumnExists( 'BucketId', Stmt ) then
      CreateColumn( 'accounts', 'BucketId TEXT' );
    if not ColumnExists( 'Login', Stmt ) then
      CreateColumn( 'accounts', 'Login TEXT' );
    if not ColumnExists( 'Password', Stmt ) then
      CreateColumn( 'accounts', 'Password TEXT' );
    if not ColumnExists( 'StorageSize', Stmt ) then
      CreateColumn( 'accounts', 'StorageSize UNSIGNED INT DEFAULT 0' );
    if not ColumnExists( 'BasePath', Stmt ) then
      CreateColumn( 'accounts', 'BasePath TEXT' );




    DB.Execute('CREATE TABLE if not exists files ( id INTEGER PRIMARY KEY AUTOINCREMENT )' );
    Stmt := DB.Prepare('SELECT * FROM files WHERE id = -1');
    res := Stmt.Step;
    CheckError( ( res = SQLITE_ROW ) or ( res = SQLITE_DONE ), 'select db error' );
    if not ColumnExists( 'account_id', Stmt ) then
      CreateColumn( 'files', 'account_id INTEGER' );//NOT NULL
    if not ColumnExists( 'RemoteFileId', Stmt ) then
      CreateColumn( 'files', 'RemoteFileId TEXT' );//NOT NULL
    if not ColumnExists( 'RemoteFileName', Stmt ) then
      CreateColumn( 'files', 'RemoteFileName TEXT' );
    if not ColumnExists( 'FileSize', Stmt ) then
      CreateColumn( 'files', 'FileSize UNSIGNED INT DEFAULT 0' );
    if not ColumnExists( 'UploadTime', Stmt ) then
      CreateColumn( 'files', 'UploadTime DATETIME' );//NOT NULL
    if not ColumnExists( 'MD5_checksum', Stmt ) then
      CreateColumn( 'files', 'MD5_checksum CHAR(32)' );
    if not ColumnExists( 'SHA1_checksum', Stmt ) then
      CreateColumn( 'files', 'SHA1_checksum CHAR(40)' );
  finally
    FreeAndNil( DB );
  end;
end;


procedure CFRegisterFileInDatabase( AccountId : Integer; RemoteFileID : string;
 RemoteFilename : string;  FileSize : Double );
var
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;
begin
  DB := TSQLite3Database.Create;
  try
    DB.Open(RC_LocalDbName);
    Stmt := DB.Prepare('INSERT INTO files ( account_id, RemoteFileId , '+
    'RemoteFileName, FileSize, UploadTime ) VALUES ( ?, ?, ?, ?, ? )');
    try
      Stmt.BindInt( 1, AccountId );
      Stmt.BindText( 2, RemoteFileID );
      Stmt.BindText( 3, RemoteFilename );
      Stmt.BindDouble( 4, FileSize );
      Stmt.BindDouble( 5, Now() );
      CheckError( Stmt.Step = SQLITE_DONE, 'Insert db error' );
      Stmt.Reset;
    finally
      FreeAndNil(Stmt);
    end;
  finally
    FreeAndNil(DB);
  end;
end;


procedure CFUnRegisterFileFromDatabase( AccountId : Integer; RemoteFileID : string);
var
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;
begin
  DB := TSQLite3Database.Create;
  try
    DB.Open(RC_LocalDbName);
    Stmt := DB.Prepare('DELETE FROM files WHERE account_id = ? AND RemoteFileId = ? ');
    try
      Stmt.BindInt( 1, AccountId );
      Stmt.BindText( 2, RemoteFileID );
      CheckError( Stmt.Step = SQLITE_DONE, 'Delete db error' );
      Stmt.Reset;
    finally
      FreeAndNil(Stmt);
    end;
  finally
    FreeAndNil(DB);
  end;
end;


{ TAnsiStringStream }

constructor TAnsiStringStream.Create(const AString: AnsiString);
begin
  inherited Create;
  FDataString := AString;
end;

function TAnsiStringStream.Read(var Buffer; Count: Longint): Longint;
begin
  Result := Length(FDataString) - FPosition;
  if Result=0 then
     Exit;
  if Result > Count then Result := Count;
  Move(PAnsiChar(@FDataString[FPosition + SizeOf(AnsiChar)])^, Buffer, Result * SizeOf(AnsiChar));
  Inc(FPosition, Result);
end;

function TAnsiStringStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := Count;
  SetLength(FDataString, (FPosition + Result));
  Move(Buffer, PAnsiChar(@FDataString[FPosition + SizeOf(AnsiChar)])^, Result * SizeOf(AnsiChar));
  Inc(FPosition, Result);
end;

function TAnsiStringStream.Seek(Offset: Longint; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition := Offset;
    soFromCurrent: FPosition := FPosition + Offset;
    soFromEnd: FPosition := Length(FDataString) - Offset;
  end;
  if FPosition > Length(FDataString) then
    FPosition := Length(FDataString)
  else if FPosition < 0 then FPosition := 0;
  Result := FPosition;
end;

function TAnsiStringStream.ReadString(Count: Longint): AnsiString;
var
  Len: Integer;
begin
  Len := Length(FDataString) - FPosition;
  if Len=0 then
     Result:=''
  else begin
     if Len > Count then Len := Count;
     SetString(Result, PAnsiChar(@FDataString[FPosition + SizeOf(AnsiChar)]), Len);
     Inc(FPosition, Len);
     end;
end;

procedure TAnsiStringStream.WriteString(const AString: AnsiString);
begin
  Write(PAnsiChar(AString)^, Length(AString));
end;

procedure TAnsiStringStream.SetSize(NewSize: Longint);
begin
  SetLength(FDataString, NewSize);
  if FPosition > NewSize then FPosition := NewSize;
end;

function GetCloudFilesDrive( CloudAccountName : string ) : TCloudFilesDrive;
var
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;
  CloudStorage : string;
begin
  CheckError( CloudAccountName > '', 'CloudAccountName param is empty!' );
  DB := TSQLite3Database.Create;
  try
    DB.Open(RC_LocalDbName);
    Stmt := DB.Prepare('SELECT id, cloud_type FROM accounts WHERE name = ?');
    try
      Stmt.BindText( 1, CloudAccountName );
      Stmt.Step;
      CloudStorage := Stmt.ColumnText( ColumnByName('cloud_type', Stmt ) );
      if CloudStorage = 'backblaze' then
        Result := TB2Drive.Create(Nil)
//      else if CloudStorage = 'hubic' then
//        Result :=  THubicDrive.Create(Nil)
      else CheckError( false, 'unknown drive requested:' + CloudStorage );

      Result.AccountId := Stmt.ColumnInt( ColumnByName('id', Stmt ) );
    finally
      FreeAndNil(Stmt);
    end;
  finally
    FreeAndNil( DB );
  end;
end;



function CFUploadFile( const SourceFileName : UnicodeString; OwnerHandle : THandle ): string;
var
  FileID, NewFileName, Filename : UnicodeString;
  SS : TStringList;
  MS : TMemoryStream;
  CFDrive : TCloudFilesDrive;
  UploadDialogFrm : TUploadDialogFrm;
  CloudAccountName : string;
begin
  try
    UploadDialogFrm := TUploadDialogFrm.Create( Nil );
    try
      UploadDialogFrm.lblFileName.Caption := SourceFileName;
      if UploadDialogFrm.ShowModal <> mrOk then
        Exit;
      CloudAccountName := UploadDialogFrm.ctl_AccountName.Text;
    finally
      FreeAndNil( UploadDialogFrm );
    end;
    CFDrive := GetCloudFilesDrive( CloudAccountName );
    try
      MS := TMemoryStream.Create;
      try
        MS.LoadFromFile( SourceFileName );
        CheckError( MS.Size > 0, 'File is Zero length. ' + SourceFileName );
        StartProgress( SourceFileName, MS.Size );
        CFDrive.CheckDriveConnected;
        CFDrive.UploadFromStream( SourceFileName, MS, FileID, Filename );
        CheckError( FileID > '', 'FileID is empty' );

        CFRegisterFileInDatabase( CFDrive.AccountId, FileID, Filename, MS.Size );

        if DeleteFile( SourceFileName ) then
        begin
          Result := SourceFileName + '.dima';
          SS := TStringList.Create;
          try
            SS.DefaultEncoding := TEncoding.UTF8;
            SS.Add( FileID + '|' + SourceFileName + '|' + IntToStr(MS.Size) + '|' + CloudAccountName );
            SS.SaveToFile(Result);
          finally
            FreeAndNil( SS );
          end;
        end;
        EndProgress;
        MessageBox( OwnerHandle, PChar('File: '+ SourceFileName +#10#13 + ' was uploaded.'),
          'Upload', MB_OK );
      finally
        FreeAndNil( MS );
      end;
    finally
      FreeAndNil( CFDrive );
    end;
  except on E: Exception do
  begin
     MessageBox( OwnerHandle, PChar(E.Message), 'Message', MB_OK );
  end;
  end;
end;



function CFDownloadFile(  const MetaFileName : string; OwnerHandle : THandle ): string;
var
  FS : TFileStream;
  SS : TStringList;
  FileId, FileName, PrevFileName, FileSize, FileInfo, RemFileName, CloudAccountName, OrigFileName : UnicodeString;
  CFDrive : TCloudFilesDrive;
begin
  try
    PrevFileName := MetaFileName;
    SS := TStringList.Create;
    try
      SS.DefaultEncoding := TEncoding.UTF8;
      SS.LoadFromFile(PrevFileName);
      FileInfo := SS.Text;
      FileId := Trim( Fetch( FileInfo, '|' ) );
      OrigFileName := Trim( Fetch( FileInfo, '|' ) );
      FileSize := Trim( Fetch( FileInfo, '|' ) );
      CloudAccountName := Trim(FileInfo);
    finally
      FreeAndNil( SS );
    end;

    FileName := Copy( PrevFileName, 1, Length(PrevFileName) - Length('.dima') );

    FS := TFileStream.Create( FileName, fmCreate or fmOpenWrite or fmShareDenyWrite );
    try
      CFDrive := GetCloudFilesDrive(CloudAccountName);
      try
        StartProgress( FileName, StrToIntDef( FileSize, 0 ) );
        CFDrive.CheckDriveConnected;
        if CFDrive.DownloadToStream( FileId, '', FS ) then
        begin
          CFUnRegisterFileFromDatabase( CFDrive.AccountId, FileId );
          EndProgress;
          System.SysUtils.DeleteFile( MetaFileName );
          if MessageBox( OwnerHandle, PChar('File: '+ FileName +#10#13 + ' was downloaded. If you prefer to keep remote file version click Yes.'),
            'Download', MB_YESNO ) <> ID_YES then
            begin
              RemFileName := FileName;
              Fetch( RemFileName, ':\' );
              RemFileName := StringReplace( RemFileName, '\', '/', [rfReplaceAll]);
              CFDrive.DeleteFile( FileId, RemFileName );
            end;
        end;
        Result := FileName;
      finally
        FreeAndNil( CFDrive );
      end;
    finally
      FreeAndNil( FS );
    end;
  except on E: Exception do
  begin
     MessageBox( OwnerHandle, PChar(E.Message), 'Message', MB_OK );
  end;
  end;
end;

procedure CFDeleteFile(  const MetaFileName : UnicodeString; OwnerHandle : THandle );
var
 // FS : TFileStream;
  SS : TStringList;
  FileId, OrigFileName, CloudAccountName, FileName, PrevFileName, FileSize, FileInfo, RemFileName : UnicodeString;
  CFDrive : TCloudFilesDrive;
begin
  try
    PrevFileName := MetaFileName;
    SS := TStringList.Create;
    try
      SS.DefaultEncoding := TEncoding.UTF8;
      SS.LoadFromFile(PrevFileName);
      FileInfo := SS.Text;
      FileId := Trim( Fetch( FileInfo, '|' ) );
      OrigFileName := Trim( Fetch( FileInfo, '|' ) );
      FileSize := Trim( Fetch( FileInfo, '|' ) );
      CloudAccountName := Trim(FileInfo);
    finally
      FreeAndNil( SS );
    end;

    FileName := Copy( PrevFileName, 1, Length(PrevFileName) - Length('.dima') );

    CFDrive := GetCloudFilesDrive( CloudAccountName );
    try
      CFDrive.CheckDriveConnected;
      RemFileName := FileName;
      Fetch( RemFileName, ':\' );
      RemFileName := StringReplace( RemFileName, '\', '/', [rfReplaceAll]);
      CFDrive.DeleteFile( FileId, RemFileName );
    finally
      FreeAndNil( CFDrive );
    end;
  except on E: Exception do
  begin
     MessageBox( OwnerHandle, PChar(E.Message), 'Message', MB_OK );
  end;
  end;
end;


////////////////////////////////////////////////////////////////////////////////
function GetProgress : Int64;
begin
  Result := GProgress;
  Application.ProcessMessages;
end;

procedure AddProgress( Val : Int64 );
begin
  GProgress := GProgress + Val;
  Application.ProcessMessages;
end;

procedure InitProgress;
begin
  GProgress := 0;
  GAbort := false;
  Application.ProcessMessages;
end;

procedure SetAbort;
begin
  GAbort := true;
end;


procedure AddHeader(var AHeaders: TCoreCloudHeaders; Header: String; Value: String);
begin
  SetLength(AHeaders, Length(AHeaders) + 1);
  AHeaders[Length(AHeaders) - 1].header := Header;
  AHeaders[Length(AHeaders) - 1].value := Value;
end;


procedure RaiseIfAbort;
begin
  CheckError( not GAbort, 'Operation aborted!' );
end;

function RequestCloudFilesAuthority( Login : string; Password : string ) : boolean;
var
  MHTTP : THttpSend;
  SS : TStringStream;
  Response : RawByteString;
const
  url = 'http://www.livny.biz/storage-solution/index.php?event=enter';
begin
  MHTTP := THttpSend.Create;
  try
    MHTTP.MimeType:= '';
    MHTTP.UserAgent:= '';
    MHTTP.Protocol := '1.1';
    MHTTP.Sock.SSL.VerifyCert:= True;
    MHTTP.Sock.SSL.CertCAFile:= ExtractFilePath(ParamStr(0))+'cacert.pem';
    SS := TStringStream.Create( 'login='+URLEncode(Login)+'&pass=' +URLEncode(Password) );
    try
      MHTTP.Document.LoadFromStream(SS);
    finally
      FreeAndNil( SS );
    end;
      MHTTP.Document.Position := 0;
      MHTTP.Headers.Add( 'Content-Type: application/x-www-form-urlencoded' );
      MHTTP.HTTPMethod( 'POST', url );
      MHTTP.Document.Position := 0;
      SetLength( Response, MHTTP.Document.Size );
      MHTTP.Document.Read( Pointer(Response)^, MHTTP.Document.Size );
      Result := Pos( 'url for download', LowerCase(Response) ) > 0
  finally
    FreeAndNil( MHTTP );
  end;
end;

function MailURLMayBeInvalid(const s: string): Boolean;
var
  i: Integer;
  c: string;
  bad : boolean;
begin // ' ', ä, ö, ü, ß, [, ], (, ), : in EMail-Address
  Result := false;
  bad := (Trim(s) = '') or (Pos(' ', AnsiLowerCase(s)) > 0) or
    (Pos('ä', AnsiLowerCase(s)) > 0) or (Pos('ö', AnsiLowerCase(s)) > 0) or
    (Pos('ü', AnsiLowerCase(s)) > 0) or (Pos('ß', AnsiLowerCase(s)) > 0) or
    (Pos('[', AnsiLowerCase(s)) > 0) or (Pos(']', AnsiLowerCase(s)) > 0) or
    (Pos('(', AnsiLowerCase(s)) > 0) or (Pos(')', AnsiLowerCase(s)) > 0) or
    (Pos(':', AnsiLowerCase(s)) > 0) or (Pos(' ', AnsiLowerCase(s)) > 0) or
    (Pos(',', AnsiLowerCase(s)) > 0) ;

  if bad then Exit; // @ not in EMail-Address;
  i      := Pos('@', s);
  bad :=  (i = 0) or (i = 1) or (i = Length(s)) ;
  if bad then Exit;
  bad := (Pos('@', Copy(s, i + 1, Length(s) - 1)) > 0);
  if bad then Exit; // Domain <= 1
  c      := Copy(s, i + 1, Length(s));
  bad := Length(c) <= 1 ;
  if bad then Exit;
  i      := Pos('.', c);
  bad := (i = 0) or (i = 1) or (i = Length(c)) or (c[Length(c)] = '.');
  if not bad then
    Result := true;
end;


procedure SetTimeouts(const AHandle:HInternet);
var
  dwTimeout: DWORD;
begin
  dwTimeout:=60000;
  InternetSetOption(AHandle,INTERNET_OPTION_CONNECT_TIMEOUT,@dwTimeout,sizeof(dwTimeout));
  InternetSetOption(AHandle,INTERNET_OPTION_RECEIVE_TIMEOUT,@dwTimeout,sizeof(dwTimeout));
  InternetSetOption(AHandle,INTERNET_OPTION_SEND_TIMEOUT,@dwTimeout,sizeof(dwTimeout));
  end;

function HttpsGet(const Url: String; Headers: TCoreCloudHeaders; Dest : TStream = Nil) : AnsiString;
var
  NetHandle: HINTERNET;
  UrlHandle: HINTERNET;
  Buffer: array[0..1024] of AnsiChar;
  BytesRead: dWord;
  Header: string;
  I: Integer;
begin
  RaiseIfAbort;
  header := '';
  if Assigned(headers) then
  begin
    for I := 0 to Length(headers) - 1 do
      header := header + Headers[I].header + ': ' + headers[I].value;
  end;

  //Result := '';
  NetHandle := InternetOpen(PChar(GAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if Assigned(NetHandle) then
  begin
    SetTimeouts(NetHandle);
    if Header <> '' then
      UrlHandle := InternetOpenUrl(NetHandle, PChar(Url), PChar(Header), Length(Header), INTERNET_FLAG_RELOAD, 0)
    else
      UrlHandle := InternetOpenUrl(NetHandle, PChar(Url), nil, 0, INTERNET_FLAG_RELOAD, 0);

    if Assigned(UrlHandle) then
    begin
      repeat
        BytesRead := 0;
        //fillchar( buffer, sizeof( buffer ), #0 );
        InternetReadFile(UrlHandle, @Buffer, SizeOf(Buffer) - 1, BytesRead);
        buffer[BytesRead] := #0;
        if Assigned( Dest ) then
          Dest.Write( buffer, BytesRead )
        else
          Result := Result + Copy( buffer, 1, BytesRead );//Copy( buffer, 0, BytesRead -1 );
        AddProgress( BytesRead );
        RaiseIfAbort;
      until (BytesRead = 0);
      InternetCloseHandle(UrlHandle);

    end
    else
      raise Exception.CreateFmt('Cannot open URL %s', [Url]);

    InternetCloseHandle(NetHandle);
  end
  else
    raise Exception.Create('Unable to initialize Wininet');
end;


function HttpsPost(const ServerName, Resource: String;Headers: TCoreCloudHeaders;const  PostData : AnsiString;Var Response:AnsiString): Integer;
const
  BufferSize = 1024*64;
var
  hInet: HINTERNET;
  hConnect: HINTERNET;
  hRequest: HINTERNET;
  ErrorCode: Integer;
  lpdwBufferLength: DWORD;
  lpdwReserved: DWORD;
  dwBytesRead: DWORD;
  Flags: DWORD;
  Buffer: array[0..1024] of AnsiChar;
  Header: string;
  I: Integer;
begin
  header := '';
  if Assigned(headers) then
  begin
    for I := 0 to Length(headers) - 1 do
      header := header + Headers[I].header + ': ' + headers[I].value;
  end;

  Result := 0;
  Response := '';
  hInet := InternetOpen(PChar(GAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

  if hInet = nil then
  begin
    ErrorCode := GetLastError;
    raise Exception.Create(Format('InternetOpen Error %d Description %s',[ErrorCode,HttpError(ErrorCode)]));
  end;

  try
    hConnect := InternetConnect(hInet, PChar(ServerName), INTERNET_DEFAULT_HTTPS_PORT, nil, nil, INTERNET_SERVICE_HTTP, 0, 0);
    if hConnect=nil then
    begin
      ErrorCode := GetLastError;
      raise Exception.Create(Format('InternetConnect Error %d Description %s',[ErrorCode,HttpError(ErrorCode)]));
    end;

    try
      Flags := INTERNET_FLAG_SECURE;
      Flags := Flags or INTERNET_FLAG_PASSIVE;
      Flags := Flags or INTERNET_FLAG_KEEP_CONNECTION;
      hRequest := HttpOpenRequest(hConnect, 'POST', PChar(Resource), HTTP_VERSION, '', nil, Flags, 0);
      if hRequest=nil then
      begin
        ErrorCode := GetLastError;
        raise Exception.Create(Format('HttpOpenRequest Error %d Description %s',[ErrorCode,HttpError(ErrorCode)]));
      end;

      try
        //send the post request
        if not HTTPSendRequest(hRequest, PChar(Header), Length(Header), @PostData[1], Length(PostData)) then
        begin
          ErrorCode := GetLastError;
          raise Exception.Create(Format('HttpSendRequest Error %d Description %s',[ErrorCode,HttpError(ErrorCode)]));
        end;

          lpdwBufferLength := SizeOf(Result);
          lpdwReserved := 0;
          //get the response code
          if not HttpQueryInfo(hRequest, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @Result, lpdwBufferLength, lpdwReserved) then
          begin
            ErrorCode := GetLastError;
            raise Exception.Create(Format('HttpQueryInfo Error %d Description %s',[ErrorCode,HttpError(ErrorCode)]));
          end;

         //OutputDebugString(pchar(IntToStr(Result)));
         //if the response code = 2xx or 4xx then get the body
         if (Result div 100=2) or (Result div 100 = 4) then
         begin
           Response := '';
           dwBytesRead := 0;
           FillChar(Buffer, SizeOf(Buffer), 0);
           repeat
             Response := Response + Copy(Buffer, 1, dwBytesRead);
//             outputdebugstring(pchar(inttostr(Length(Response))));
             FillChar(Buffer, SizeOf(Buffer), 0);
             InternetReadFile(hrequest, @Buffer, SizeOf(Buffer), dwBytesRead);
             AddProgress( dwBytesRead );
           until dwBytesRead = 0;
         end;

      finally
        InternetCloseHandle(hRequest);
      end;
    finally
      InternetCloseHandle(hConnect);
    end;
  finally
    InternetCloseHandle(hInet);
  end;
end;

function URLEncode(const Url: string): string;
var
  i: Integer;
  UrlA: ansistring;
  res: ansistring;
begin
  res := '';
  UrlA := ansistring(UTF8Encode(Url));

  for i := 1 to Length(UrlA) do
  begin
    case UrlA[i] of
      'A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.':
        res := res + UrlA[i];
    else
        res := res + '%' + ansistring(IntToHex(Ord(UrlA[i]), 2));
    end;
  end;

  Result := string(res);
end;

function XDigit(Ch : Char) : Integer;
begin
    case Ch of
        '0'..'9' : Result := Ord(Ch) - Ord('0');
    else
        Result := (Ord(Ch) and 15) + 9;
    end;
end;


function htoi2(const S1,S2:Char):Integer;
begin
  Result:=XDigit(S1)*16+XDigit(S2);
  end;

function UrlDecode(const S : String) : String;
var
    I, J, L : Integer;
    U8Str   : AnsiString;
    Ch      : AnsiChar;
begin
    L := Length(S);
    SetLength(U8Str, L);
    I := 1;
    J := 0;
    while (I <= L) do begin
        Ch := AnsiChar(S[I]);
        if Ch = '%' then begin
            Ch := AnsiChar(htoi2(S[I + 1],S[I + 2]));
            Inc(I, 2);
        end
        else if Ch = '+' then
            Ch := ' ';
        Inc(J);
        U8Str[J] := Ch;
        Inc(I);
    end;
    SetLength(U8Str, J);
    Result := U8Str;
end;

function FileVersion(const FileName: TFileName): String;
var
  VerInfoSize: Cardinal;
  VerValueSize: Cardinal;
  Dummy: Cardinal;
  PVerInfo: Pointer;
  PVerValue: PVSFixedFileInfo;
begin
  Result := '';
  VerInfoSize := GetFileVersionInfoSize(PChar(FileName), Dummy);
  GetMem(PVerInfo, VerInfoSize);
  try
    if GetFileVersionInfo(PChar(FileName), 0, VerInfoSize, PVerInfo) then
      if VerQueryValue(PVerInfo, '\', Pointer(PVerValue), VerValueSize) then
        with PVerValue^ do
          Result := Format('v%d.%d.%d build %d', [
            HiWord(dwFileVersionMS), //Major
            LoWord(dwFileVersionMS), //Minor
            HiWord(dwFileVersionLS), //Release
            LoWord(dwFileVersionLS)]); //Build
  finally
    FreeMem(PVerInfo, VerInfoSize);
  end;
end;


function NormalyzeToBucketName( const Val : string ): string;
var
  i, len: Integer;
  S: string;
  valid : boolean;
const
  Allowed = ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '-'];
begin
  S := Val;
  if Length( S ) < 6 then
  begin
    System.Insert( '000000', S, Length( S ) +1 );
    SetLength( S, 6 );
  end;
  len := Length( S );
  valid := true;
  for I := 1 to  len do
  begin
    if not ( S[i] in Allowed ) then
    begin
      valid := false;
      break;
    end;
  end;
  if not valid then
  begin
    for I := 1 to  len do
    begin
      if not ( S[i] in Allowed ) then
        S[i] := '-';
    end;
  end;
  Result := S;
end;

function GetSpecialFolderPath(folder : integer) : string;
const   SHGFP_TYPE_CURRENT = 0;
var   path: array [0..MAX_PATH] of char;
begin
  if SUCCEEDED(SHGetFolderPath(0,folder,0,SHGFP_TYPE_CURRENT,@path[0])) then
   Result := path
   else
   Result := '';
end;

function GetIniPath : string;
var
  Dirname : string;
  ss : TStrings;
begin
  Dirname := GetSpecialFolderPath(CSIDL_LOCAL_APPDATA) + '\CloudFiles';
  if not DirectoryExists( Dirname )then
    CheckError( CreateDir( Dirname ), 'Can not create:' + Dirname );
  Result := Dirname + '\abhay.ini';
end;


function DecryptKey( Val : string; SecretVal : string ): string;
begin
  //secret implementation
   CheckError( Trim(SecretVal) > '', 'SecretVal is empty. DecryptKey' );
   Result := Val;
end;


function EncryptKey( Val : string; SecretVal : string ): string;
begin
  //secret implementation
  Result := Val;
end;

function getFileSizeInBytes(const fn: string): integer;
var
  f: File of byte;
begin
  Result := -1;
  if (FileExists(fn)) then
  begin
    try
      {$I-}
      AssignFile(f, fn);
      Reset(f);
      {$I+}
      if (IOResult = 0) then
      begin
        Result := FileSize(f);
      end
      else
      begin
        Result := 0;
      end;
    finally
      {$I-}CloseFile(f);{$I+}
    end;
  end;
end;


function DetectMIMEType(const fn:string):string;
var sExt:string;
begin
  sExt:=UpperCase(ExtractFileExt(fn));
  if (sExt='.JPG') or (sExt='.JPEG') or (sExt='.JPE') then
     Result:='image/jpeg'
  else
  if (sExt='.TIF') or (sExt='.TIFF') then
     Result:='image/tiff'
  else
  if (sExt='.GIF') then
     Result:='image/gif'
  else
  if (sExt='.BMP') then
     Result:='image/bmp'
  else
  if (sExt='.PNG') then
     Result:='image/png'
  else
  if (sExt='.SVG') then
     Result:='image/svg+xml'
  else
  if (sExt='.HTM') or (sExt='.HTML') then
     Result:='text/html'
  else
  if (sExt='.TXT') or (sExt='.TEXT') then
     Result:='text/plain'
  else
  if (sExt='.XLS') or (sExt='.XLSX') then
     Result:='application/vnd.ms-excel'
  else
  if (sExt='.PPT') or (sExt='.PPS') then
     Result:='application/vnd.ms-powerpoint'
  else
  if (sExt='.DOC') then
     Result:='application/msword'
  else
  if (sExt='.DOCX') then
     Result:='application/vnd.openxmlformats-officedocument.wordprocessingml.document'
  else
  if (sExt='.CSS') then
     Result:='text/css'
  else
  if (sExt='.FLV') then
     Result:='video/x-flv'
  else
  if (sExt='.SWF') then
     Result:='application/x-shockwave-flash'
  else
  if (sExt='.CSV') then
     Result:='text/csv'
  else
  if (sExt='.TSV') then
     Result:='text/tab-separated-values'
  else
  if (sExt='.TAB') then
     Result:='ext/tab-separated-values'
  else
  if (sExt='.ODS') then
     Result:='application/x-vnd.oasis.opendocument.spreadsheet'
  else
  if (sExt='.ODT') then
     Result:='application/vnd.oasis.opendocument.text'
  else
  if (sExt='.RTF') then
     Result:='application/rtf'
  else
  if (sExt='.SXW') then
     Result:='application/vnd.sun.xml.writer'
  else
  if (sExt='.PDF') then
     Result:='application/pdf'
  else
  if (sExt='.JS') then
     Result:='application/x-javascript'
  else
     Result := 'binary/octet-stream'
end;

function PosFromIndex( SubStr, S : UnicodeString; Index : integer ) : integer; overload;
Var
  SplitStr : UnicodeString;
begin
  SplitStr := Copy(S, Index, Length(S) - Index);
  Result := Pos(SubStr, SplitStr);
end;

function ExtractBetween(const Value : UnicodeString; BeginMark : UnicodeString; EndMark : UnicodeString = '' ) : UnicodeString;
var
  pBegin, pEnd, pStart, Count : Integer;
begin
  if BeginMark>'' then begin
     pBegin := Pos( BeginMark, Value );
     if pBegin = 0 then Exit;
	 end
  else
     pBegin:=1;
  if length( EndMark ) > 0 then
    pEnd := PosFromIndex( EndMark, Value, pBegin + length( BeginMark ) )  + pBegin + length( BeginMark ) - 1
  else
    pEnd := length( Value ) + 1;
  if pEnd = 0 then Exit;
  pStart := pBegin + length( BeginMark );
  Count := pEnd - pStart;
  Result := Copy( Value,  pStart, Count );
end;

function MemStreamToStr( MS : TMemoryStream ): string;
begin
  with TStringStream.Create('') do
  begin
    CopyFrom( MS, MS.Size );
    Result := DataString;
    Free;
  end;
end;

function GetWMIstring(const WMIClass, WMIProperty:string): string;
const
  wbemFlagForwardOnly = $00000020;
var
  FSWbemLocator : OLEVariant;
  FWMIService   : OLEVariant;
  FWbemObjectSet: OLEVariant;
  FWbemObject   : OLEVariant;
  oEnum         : IEnumvariant;
  iValue        : LongWord;
begin;
  Result:='';
  FSWbemLocator := CreateOleObject('WbemScripting.SWbemLocator');
  FWMIService   := FSWbemLocator.ConnectServer('localhost', 'root\CIMV2', '', '');
  FWbemObjectSet:= FWMIService.ExecQuery(Format('Select %s from %s',[WMIProperty, WMIClass]),'WQL',wbemFlagForwardOnly);
  oEnum         := IUnknown(FWbemObjectSet._NewEnum) as IEnumVariant;
  if oEnum.Next(1, FWbemObject, iValue) = 0 then
    if not VarIsNull(FWbemObject.Properties_.Item(WMIProperty).Value) then
       Result:=FWbemObject.Properties_.Item(WMIProperty).Value;
   FWbemObject := Unassigned;
   FWMIService := Unassigned;
   FSWbemLocator := Unassigned;
end;


procedure CheckError( ACase : Boolean; const ErrorMsg : AnsiString );
begin
  if not ACase then
     raise Exception.Create(string(ErrorMsg));
end;

function CleanUpJSON(Value: ansistring): ansistring;
const
  JSONStart: ansistring = '{';
  JSONEnd: ansistring = '}';
var
  i: integer;
  res: ansistring;

begin
  res := Value;
  i := Pos(JSONStart,res);

  // trim first chars
  if i > 1 then
    System.Delete(res,1,i - 1);

  i := length(res);

  while (i > 1) and (res[i] <> JSONEnd) do
  begin
    dec(i);
  end;

  System.Delete(res,i + 1, length(res) - i);

  Result := res;
end;


function GetJSONProp(JSON: string; ID: string): UnicodeString;
var
  StringBytes: TBytes;
  o: TJSONObject;
  p: TJSONPair;

begin
  Result := '';

  StringBytes := BytesOf(JSON);

  o := TJSONObject.Create;

  try
    o.Parse(StringBytes,0);
    p := o.Get(ID);
    if Assigned(p) then
    begin
      Result := p.JsonValue.Value;
    end;
  finally
    o.Free;
  end;
end;

function GetJSONProp(O: TJSONOBject; ID: string): UnicodeString;
var
  p: TJSONPair;
begin
  Result := '';
  p := o.Get(ID);
  if Assigned(p) then begin
     Result := p.JsonValue.Value;
     if Result='' then
        if p.JsonValue is TJSONTrue then
           Result:='true'
        else
           if p.JsonValue is TJSONfalse then
              Result:='false';
     end;
end;

function GetJSONValue(O: TJSONObject; ID: string): TJSONValue;
var
  p: TJSONPair;
begin
  Result := nil;
  p := o.Get(ID);
  if Assigned(p) then
  begin
    Result := p.JsonValue;
  end;
end;

function HttpError(ErrorCode:Cardinal): string;
const
   winetdll = 'wininet.dll';
var
  Len: Integer;
  Buffer: PChar;
begin
  Len := FormatMessage(
  FORMAT_MESSAGE_FROM_HMODULE or FORMAT_MESSAGE_FROM_SYSTEM or
  FORMAT_MESSAGE_ALLOCATE_BUFFER or FORMAT_MESSAGE_IGNORE_INSERTS or  FORMAT_MESSAGE_ARGUMENT_ARRAY,
  Pointer(GetModuleHandle(winetdll)), ErrorCode, 0, @Buffer, SizeOf(Buffer), nil);
  try
    while (Len > 0) and {$IFDEF UNICODE}(CharInSet(Buffer[Len - 1], [#0..#32, '.'])) {$ELSE}(Buffer[Len - 1] in [#0..#32, '.']) {$ENDIF} do Dec(Len);
    SetString(Result, Buffer, Len);
  finally
    LocalFree(HLOCAL(Buffer));
  end;
end;


function ExtractServer(url:string): string;
var
  atpos,slpos: integer;
begin
  if Pos('://',UpperCase(url)) > 0 then
    Delete(url,1,Pos('://',url) + 2);

  atpos := Pos('@',UpperCase(url));
  slpos := Pos('/',UpperCase(url));

  if (atpos > 0) and (atpos < slpos) then
    Delete(url,1,Pos('@',url) + 1);

  if Pos('/',url) > 0 then
    url := Copy(url,1,Pos('/',url)-1);

  Result := url;
end;

function RemoveServer(url:string): string;
begin
  if Pos('://',UpperCase(url)) > 0 then
    Delete(url,1,Pos('://',url) + 2);

  if Pos('@',UpperCase(url)) > 0 then
    Delete(url,1,Pos('@',url) + 1);

  if Pos('/',url) > 0 then
    Delete(url,1,Pos('/',url)-1);

  Result := url;
end;

procedure CloudSafeInternetReadStream(const UrlHandle:HINTERNET;const memst:TStream;
                                                 const ExpectedSize:Int64;
                                                 const ReportProgress:Boolean;
                                                 const URL:string);
const MaxBufSizeHere=1024*1024; // performance!
      DefBufSizeHere=10*1024; // performance!
var
  Buffer:array of Byte;
  BytesRead: dWord;
  bufsizehere,
  position: int64;
  inetok:Boolean;
  WaitIfErrorUntil:TDateTime;
begin
  position := 0;
  WaitIfErrorUntil:=0;
  memst.Position:=0;
  memst.Size:=0;

  if ExpectedSize>0 then
     if ExpectedSize>MaxBufSizeHere then
        bufsizehere:=MaxBufSizeHere
     else
        bufsizehere:=ExpectedSize
  else
     bufsizehere:=DefBufSizeHere;

  SetLength(Buffer,BufSizeHere);
  try
    repeat
      FillChar(Buffer[0], BufSizeHere, 0);
      inetok:=InternetReadFile(UrlHandle, @Buffer[0], BufSizeHere, BytesRead);

      if inetok then begin
         memst.Write(buffer[0], bytesread);
         position := position + bytesread;
         if ReportProgress then
          //  DoDownloadProgress(URL,position,ExpectedSize);
         end
      else begin
        // this is extremly rare, but we want to handle it properly
        GLastError:=GetLastError;
        if WaitIfErrorUntil=0 then
           WaitIfErrorUntil:=Now+1/24/60
        else
           if Now>WaitIfErrorUntil then
              raise Exception.Create('Error Reading from HTTP: '+IntToStr(GLastError));
        //if isMainThread and Assigned(CallbackProcessMessages) then
         //  CallbackProcessMessages;
        sleep(100);
        end;
      until inetok and (BytesRead = 0); // this is the correct ending condition according to the Microsoft documentation
    finally
      SetLength(Buffer,0);
    end;
  end;

function CloudSafeInternetReadString(const UrlHandle: HINTERNET): RawByteString;
var st:TAnsiStringStream;
begin
  Result:='';
  st:=TAnsiStringStream.Create('');
  try
    CloudSafeInternetReadStream(UrlHandle,st);
    Result:=st.DataString;
    finally
      FreeAndNil(st);
    end;
 {  if IndyLogging then begin
     WriteLn(IndyLog,'RESPONSE BODY:');
     if Length(Result)<10000 then
        WriteLn(IndyLog,Result)
     else begin
        WriteLn(IndyLog,'(first 10000 characters)');
        WriteLn(IndyLog,Copy(Result,1,10000))
        end;
     end;}
end;

procedure DecompressZLibString(var s: RawByteString);
var Unc:TIdCompressorZLib;
    instr,outstr:TMemoryStream; // do to: use optimized stream class that references the string rather than copying it
begin
  if Length(s)<2 then
     Exit;
  Unc:=TIdCompressorZLib.Create;
  instr:=TMemoryStream.Create;
  outstr:=TMemoryStream.Create;
  try
    instr.Write(s[1],Length(s));
    instr.Position:=0;
    Unc.DecompressGZipStream(instr,outstr);
    FreeAndNil(instr); // free memory ASAP
    SetLength(s,outstr.Size);
    outstr.Position:=0;
    outstr.Read(s[1],Length(s));
    finally
      FreeAndNil(Unc);
      FreeAndNil(instr);
      FreeAndNil(outstr);
    end;
  end;


function SynHttpsPut(AnUrl: string; customheaders: TCoreCloudHeaders;
    customdata: ansistring; HttpCommand: THTTPCommand;
    var ResultCode:Integer;
    const ASource: TStream;
    var md5Str : AnsiString; var sha1 : AnsiString;
    AContentType:string;
    const ImmediateRetries:Boolean;
    const MultiMD5Context:PMD5Context;
    const MultiLastPart:Boolean;
    RetryAfterForbidden:Boolean;
    const TolerateHTTPErrorCode:Integer;
    const DoNotRetryError:AnsiString;
    const CanDecompress:Boolean): ansistring;

var TheResult:RawByteString;

function WinPut:RawByteString;
const
  READBUFFERSIZE = 256*1024;
var
  buf:array of AnsiChar;
  bufsize:dword;
  url, fsrvr:string;
  fsize,totsize,position:int64;
  hconnect:hinternet;
  hintfile:hinternet;
  lpdword:dword;
  bufferin:INTERNET_BUFFERS;
  BytesRead: dword;
  fm: word;
  FHinternet: hinternet;
  header: string;
  hdrs: string;
  headbound: ansistring;
  tail: ansistring;
  customheader: string;
  I: Integer;
  LastError : Int64; // GetLastError is DWORD, but Int64 is really safe
  MD5Digest: TMD5Digest;
  MD5Context : TMD5Context;
  SHA1Context: TSHA1Context;//THashContext;
  SHA1Digest: TSHA1Digest;
  lpdwBufferLength: DWORD;
  lpdwReserved: DWORD;
  origbufsize:Integer;

begin
  ResultCode:=0;
  hintfile := nil; // TG: avoid warning
  hconnect := nil;
  FHinternet := nil;

  try
    customheader := '';
    if Assigned(customheaders) then
    begin
      for I := 0 to Length(customheaders) - 1 do
        customheader := customheader + customheaders[I].header + ': ' + customheaders[I].value;
    end;

    Result := '';
    tail := '';
    headbound := '';

    url:=AnURL;
    fsrvr := ExtractServer(url);
    url := RemoveServer(url);

    RaiseIfAbort();

    FHinternet := InternetOpen(PChar(GAgent), INTERNET_OPEN_TYPE_PRECONFIG, nil, nil, 0);

    CheckError( Assigned(FHInternet), 'InternetOpen failed' );

    SetTimeouts(FHInternet);//,CanDecompress);

    hconnect := InternetConnect(FHinternet,PChar(fsrvr),INTERNET_DEFAULT_HTTPS_PORT,nil,nil,INTERNET_SERVICE_HTTP,0,0);
    CheckError( Assigned(hconnect), 'InternetConnect failed' );

    hintfile := HttpOpenRequest(HConnect,PWideChar(cmdHTTPCommands[HttpCommand]),PChar(url),
                                'HTTP/1.1',nil,nil,
                                INTERNET_FLAG_SECURE or INTERNET_FLAG_NO_CACHE_WRITE,
                                0);
    //if IndyLogging then
     //  WriteLn(IndyLog,DateTimeToStr(Now),' HTTP REQUEST OPENED: ',cmdHTTPCommands[HttpCommand],' (',txtHTTPCommands[HttpCommand],'): ',Utf8Encode(url));

    CheckError( Assigned(hintfile), 'HttpOpenRequest failed (handle=nil)' );

    SetTimeouts(hintfile);//,CanDecompress);

    fm := FileMode;
    FileMode := 0; // openread mode

      if Assigned(ASource) then
         TotSize := ASource.Size
      else
         TotSize := Length(customdata); // TG 2015

      RaiseIfAbort();

      //********************* HEADER RELATED
      if (HttpCommand in [cbhcPut,cbhcPost,cbhcMove,cbhcPatch]) then begin
         hdrs := customheader;
         if hdrs <> '' then begin
            HttpAddRequestHeaders(hintfile, pchar(hdrs), length(hdrs), HTTP_ADDREQ_FLAG_ADD);
          //  if IndyLogging then
          //     WriteLn(IndyLog,DateTimeToStr(Now),' HDR: ',Utf8Encode(hdrs));
            end;

         header := 'Content-Length: '+ inttostr(totsize);
         HttpAddRequestHeaders(hintfile, pchar(header), length(header), HTTP_ADDREQ_FLAG_ADD or HTTP_ADDREQ_FLAG_REPLACE);
        // if IndyLogging then
        //    WriteLn(IndyLog,DateTimeToStr(Now),' HDR: ',Utf8Encode(header));

         header := 'Content-Type: '+AContentType;
         HttpAddRequestHeaders(hintfile, pchar(header), length(header), HTTP_ADDREQ_FLAG_ADD or HTTP_ADDREQ_FLAG_REPLACE);
        // if IndyLogging then
         //   WriteLn(IndyLog,DateTimeToStr(Now),' HDR: ',Utf8Encode(header));

         if AContentType='application/octet-stream' then begin
            header := 'Content-Transfer-Encoding: binary';
            HttpAddRequestHeaders(hintfile, pchar(header), length(header), HTTP_ADDREQ_FLAG_ADD or HTTP_ADDREQ_FLAG_REPLACE);
           // if IndyLogging then
            //   WriteLn(IndyLog,DateTimeToStr(Now),' HDR: ',Utf8Encode(header));
            end;

         header := 'Accept-Encoding: gzip,deflate';
         HttpAddRequestHeaders(hintfile, pchar(header), length(header), HTTP_ADDREQ_FLAG_ADD or HTTP_ADDREQ_FLAG_REPLACE);
         //if IndyLogging then
          //  WriteLn(IndyLog,DateTimeToStr(Now),' HDR: ',Utf8Encode(header));
         end;

      (* *)
      if (HttpCommand in [cbhcPostMultiPart, cbhcPutMultiPart]) then
      begin
        headbound := '--'+cMultipartBoundary+#13#10;
        tail := #13#10'--'+cMultipartBoundary+'--'#13#10;

        hdrs := ''
            + customheader
            +'Content-Type: multipart/form-data; boundary='+cMultipartBoundary+#13#10
            + #13#10;

        HttpAddRequestHeaders(hintfile, pchar(hdrs), length(hdrs), HTTP_ADDREQ_FLAG_ADD);
        //if IndyLogging then
           //WriteLn(IndyLog,DateTimeToStr(Now),' HDR: ',Utf8Encode(hdrs));
      end;

      //Picasa
      if (HttpCommand in [cbhcPostMultiPartRelated]) then
      begin
        headbound := ''#13#10;
        tail := #13#10'--'+cMultipartBoundary+'--'#13#10;

        hdrs := ''
            + customheader
            +'Content-Type: multipart/related; boundary='+cMultipartBoundary+#13#10
            + #13#10;

        HttpAddRequestHeaders(hintfile, pchar(hdrs), length(hdrs), HTTP_ADDREQ_FLAG_ADD);
        //if IndyLogging then
           //WriteLn(IndyLog,DateTimeToStr(Now),' HDR: ',Utf8Encode(hdrs));
      end;
      (* *)

      //********************* DATA RELATED

      FillChar(bufferin, SizeOf(bufferin),0);
      bufferin.dwStructSize := SizeOf(INTERNET_BUFFERS);
      bufferin.dwBufferTotal := length(headbound) + length(customdata) + TotSize + length(tail);

      RaiseIfAbort();

      if not HttpSendRequestEx(hintfile,@bufferin,nil,HSR_INITIATE,0) then
      begin
        LastError := GetLastError;
     //   if IndyLogging then
     //      WriteLn(IndyLog,DateTimeToStr(Now),' HttpSendRequestEx ERROR ',LastError);
        InternetCloseHandle(hintfile);
        InternetCloseHandle(hconnect);
        if LastError = 12002 then
           raise Exception.Create('HTTP TIMEOUT')
        else
           raise Exception.Create('HttpSendRequestEx ERROR '+IntToStr(LastError));
        Exit;
      end;

      if (HttpCommand in [cbhcPutMultiPart, cbhcPostMultiPart, cbhcPostMultiPartRelated,cbhcMOVE]) and (headbound <> '') then
      begin
         InternetWriteFile(hintfile,pansichar(@headbound[1]),length(headbound),lpdword);
       //  if IndyLogging then
       //     WriteLn(IndyLog,DateTimeToStr(Now),' SENT HDR: ',headbound);
      end;

      if customdata <> '' then
      begin
         InternetWriteFile(hintfile,pansichar(@customdata[1]),length(customdata),lpdword);
        // if IndyLogging then
        //    WriteLn(IndyLog,DateTimeToStr(Now),' SENT DTA: ',customdata);
      end;

      FSize := 0;
      position := 0;

      if Assigned(ASource) then begin
         bufsize := READBUFFERSIZE;
         if bufsize>ASource.Size then // TG
            bufsize:=ASource.Size;
         SetLength(buf,bufsize); // allocate memory on heap not stack
         try
           ASource.Position := 0;
        //   if IndyLogging and (ASource.Size<1000) then
        //      WriteLn(IndyLog,DateTimeToStr(Now),' HTTP BODY:'^M^J,TGGlobal.Stream2String(ASource));
        //   DoUploadProgress( '', position, totsize );
        //   if IndyLogging then
        //      WriteLn(IndyLog,DateTimeToStr(Now),' SENDING BODY: ',totsize,' Bytes.');

           if Assigned(MultiMD5Context) then
              MD5Context:=MultiMD5Context^
           else
              MD5Init( MD5Context );
           SHA1Init( SHA1Context );

           origbufsize:=bufsize;
           if bufsize>0 then
              repeat
                RaiseIfAbort();
                bufsize := ASource.Read(buf[0], bufsize);
                MD5UpdateBuffer( MD5Context, @buf[0], bufsize);
                SHA1Update( SHA1Context, buf[0], bufsize); // XL allows larger buffer
                if (bufsize>0) and
                   not InternetWriteFile(hintfile, @buf[0], bufsize, lpdword) then begin
                   LastError := GetLastError;
                   //if IndyLogging then
                      //WriteLn(IndyLog,DateTimeToStr(Now),' InternetWriteFile ERROR ',LastError);
                   raise Exception.Create('InternetWriteFile ERROR '+IntToStr(LastError));
                   end;
                FSize := FSize + bufsize;
                position := position + lpdword;
                AddProgress( lpdword );
               // DoUploadProgress('', position, totsize );
                until bufsize<origbufsize;

           finally
             SetLength(buf,0); // free memory ASAP
           end;

         if MultiLastPart or not Assigned(MultiMD5Context) then begin
            MD5Final( MD5Digest, MD5Context);
            md5Str := MD5DigestToHex( MD5Digest );
            end
         else
            if Assigned(MultiMD5Context) then
               MultiMD5Context^:=MD5Context;

         SHA1Final( SHA1Context );//, SHA1Digest );
         sha1 := '';
         for I := 0 to 19 do
           sha1 :=  sha1 + AnsiString( IntToHex( SHA1Digest[I], 2 ) );
         end;

      if (HttpCommand in [cbhcPutMultiPart, cbhcPostMultiPart, cbhcPostMultiPartRelated]) then begin
         InternetWriteFile(hintfile,pansichar(@tail[1]),length(tail),lpdword);
        // if IndyLogging then
            //WriteLn(IndyLog,DateTimeToStr(Now),' SENT TAIL: ',tail);
         end;

      if not HttpEndRequest(hintfile,nil,0,0) then
      begin
        LastError := GetLastError;
        //if IndyLogging then
           //WriteLn(IndyLog,DateTimeToStr(Now),' HttpEndRequest ERROR ',LastError);
        raise Exception.Create('HttpEndRequest ERROR '+IntToStr(LastError));
      end;

      RaiseIfAbort();

      // get response code (HTTP code)
      lpdwBufferLength := SizeOf(ResultCode);
      lpdwReserved := 0;
      //get the response code
      if not HttpQueryInfo(hintfile, HTTP_QUERY_STATUS_CODE or HTTP_QUERY_FLAG_NUMBER, @ResultCode, lpdwBufferLength, lpdwReserved) then
      begin
        ResultCode := GetLastError;
        //if IndyLogging then
           //WriteLn(IndyLog,DateTimeToStr(Now),Format('HttpQueryInfo Error %d Description %s',[ResultCode,HttpError(ResultCode)]));
        raise Exception.Create(Format('HttpQueryInfo Error %d Description %s',[ResultCode,HttpError(ResultCode)]));
      end;

     // if IndyLogging then begin
       //  WriteLn(IndyLog,DateTimeToStr(Now),' HTTP REQUEST RESULT: ', ResultCode );
       //  WriteLn(IndyLog,'HEADERS:');
       //  WriteLn(IndyLog,GetHTTPResponseHeaders(hintfile));
        // WriteLn(IndyLog,'--------------');
      //   end;

      // get the body
      if (ResultCode in [200,201]) or (ResultCode div 100 = 4) then
         Result := CloudSafeInternetReadString(hintfile)
      else
         if ResultCode>0 then
            Result:='HTTP ERROR '+IntToStr(ResultCode)
         else
            Result:='HTTP ERROR, ResultCode is 0, GetLastError is '+IntToStr(GetLastError);

      if (Result>'') and (Result[1]=#$1F) and CanDecompress then
         DecompressZLibString(Result);

      TheResult:=Result;

      if ((ResultCode<200) or
          (ResultCode>207)) and
         ((TolerateHTTPErrorCode=0) or (ResultCode<>TolerateHTTPErrorCode)) and
         (ResultCode<>308) then
         raise Exception.Create(Result);

      {if IndyLogging then begin
         WriteLn(IndyLog,DateTimeToStr(Now),' HTTP REQUEST RESULT: ', Result );
         Close(IndyLog);
         Append(IndyLog);
         end;}
      FileMode := fm;

  finally
  begin
    if Assigned( hintfile ) then
      InternetCloseHandle(hintfile);
    if Assigned( hconnect ) then
      InternetCloseHandle(hconnect);
    if Assigned( FHinternet ) then
      InternetCloseHandle(FHinternet);
   { if IndyLogging then
    begin // flush
       WriteLn(IndyLog,DateTimeToStr(Now),' HTTP REQUEST COMPLETED with ResultCode ',ResultCode);
       Close(IndyLog);
       Append(IndyLog);
    end;}
  end;
 end;
end;

label StartOver;

var OrigUrl,OrigToken:string;
    PrevLimit,I:Integer;
    isQuotaExceeded,isDuplicate,isForbidden,isUnauthorized,isRateLimit,NeedRetry:Boolean;
    AttemptCounter:Integer;
    StreamSize:Int64;

begin
  if Assigned(ASource) then
     StreamSize := ASource.Size
  else
     StreamSize := Length(customdata); // TG 2015

{  if StreamSize>3*Int64(1024)*1024*1024 then begin // WinInet cannot do over 4 GB, and there are additional headers etc. so we ues 3 GB as limit
     UseWinInet:=false;
     if not Assigned(FHTTP) then
        InitSynapse(TSynProtocol(SynProtocol).FTPSettings);
     end;
 }

  AttemptCounter:=0;
  OrigUrl:=AnUrl;

StartOver:

  AnUrl:=OrigURL;

{  if (DebugHook<>0) and (Random(3)=1) then begin
     AnUrl:='x'+AnUrl;
     RetryAfterForbidden:=true;
     end;}


  RaiseIfAbort();

  try
    NeedRetry:=false;
    {if DebugHook<>0 then
       if AttemptCounter<10 then
          raise EInternetError.Create('HTTP ERROR 504');}
     Result:=WinPut
    except
      on E:Exception do begin
        Inc(AttemptCounter);
        isForbidden:=ResultCode=403;
        isDuplicate:=ResultCode=409;
        isUnauthorized:=ResultCode=401;
        isRateLimit:=isForbidden and (Pos('RATELIMIT',UpperCase(TheResult))>0) or // Google Drive
                     (ResultCode=429); // Amazon Cloud Drive
        isQuotaExceeded:=isForbidden and
                     ((Pos('QUOTA',UpperCase(TheResult))>0) or  // Google Drive
                      (Pos('LIMIT_EXCEEDED',UpperCase(TheResult))>0) or  // BOX
                      (Pos('INSUFFICIENT',UpperCase(TheResult))>0));  // OneDrive
        if not ImmediateRetries or
           //(DebugHook<>0) and (E.Message='HTTP ERROR 504') or
           (AttemptCounter>=cAttemptCount) or
           (ResultCode=400) and (Pos('Metadata part is too large',TheResult)>0) or // Google Drive
           (ResultCode=400) and (Pos('invalid',TheResult)>0) or // OneDrive for Business
           (ResultCode=400) and (Pos('illegal',TheResult)>0) or // OneDrive for Business
           isUnauthorized or
           (ResultCode=404) or
           isDuplicate or
           isQuotaExceeded or
           isForbidden and // don't retry after Forbidden
           not RetryAfterForbidden and
           not isRateLimit // but do retry after error 403 "Rate Limit Exceeded"
           then
           if (DoNotRetryError<>'') and
              (System.Pos(DoNotRetryError,TheResult)>0) or
              isQuotaExceeded or
              isDuplicate then
               raise Exception.Create(E.Message+': '+TheResult)
           else
              if ResultCode=400 then
                 raise Exception.Create(E.Message+': '+TheResult)
              else
                 raise;
        NeedRetry:=true;
        end;
    end;
 end;


initialization
   RC_LocalDbName := ExtractFilePath( Application.ExeName) + 'cloudfiles.db';
end.


























