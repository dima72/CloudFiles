{
 Original author
 * Dmitry Konnov <konnov72@gmail.com>
 All rights reserved. This source code is not to be redistributed without
 prior permission from the copyright holder.
}

unit BackBlazeDrive;

interface
uses Classes, Winapi.Windows, Winapi.WinInet, System.SysUtils, Forms,
  synacode, synautil, httpsend, ssl_openssl, ssl_openssl_lib, blcksock, SyncObjs,
  SHA1Code, DKJSON, SQLite3, SQLite3Wrap, CloudFilesDrive;

type

  TB2Drive = class( TCloudFilesDrive )
  private
    FB2accountId : string;
    FBucketName : string;
    FBucketId : string;
    FAuthCompleted : boolean;
    FdownloadUrl : string;
    FapiUrl : string;
    Token_Auth : string;
    procedure InitHttp( MHTTP : THttpSend );
    procedure OnSocketStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
  public
    procedure SaveTokens;
    procedure LoadTokens;
    procedure DoAuth();override;
    procedure CheckDriveConnected();override;
    function CreateBucket( BucketName : string ) : string;
    function GetBucketId( BucketName : string ) : string;
    function BucketExists( const BucketId : string ) : boolean;
    procedure UploadFromStream( const DestFileNameAndPath : string;  const ASource: TStream; var FileID : string; var Filename : string);override;
    function DownloadToStream( const  FileID : string; const FileName : string;
     ADest: TStream ): boolean;override;
    procedure DeleteFileVersion( const FileId : UnicodeString; const FileName : UnicodeString );
    procedure DeleteFile( const FileId : UnicodeString; const FileName : UnicodeString );override;
    property BucketName : string read FBucketName write FBucketName;
    property BucketId : string read FBucketId write FBucketId;
  end;

implementation
uses ProgressForm;

procedure TB2Drive.CheckDriveConnected();
begin
  DoAuth();
end;


procedure TB2Drive.SaveTokens;
var
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;
begin
  try
    CheckError( FAccountId > 0, 'AccountId is not Assigned. SaveTokens' );
    DB := TSQLite3Database.Create;
    try
      DB.Open(RC_LocalDbName);
      Stmt := DB.Prepare('UPDATE accounts SET BucketName = ?, BucketId = ?, AppKey = ?,'+
        ' AppSecret = ? WHERE id = ?');
      try
        Stmt.BindText( 1, EncryptKey( Trim( FBucketName ), GSecretKey ));
        Stmt.BindText( 2, EncryptKey( Trim( FBucketId ), GSecretKey ));
        Stmt.BindText( 3, EncryptKey( Trim( AppKey ), GSecretKey ) );
        Stmt.BindText( 4, EncryptKey( Trim( AppSecret ), GSecretKey ));
        Stmt.BindInt( 5, FAccountId );
        CheckError( Stmt.Step = SQLITE_DONE, 'Update db error' );
        Stmt.Reset;
      finally
        FreeAndNil(Stmt);
      end;
    finally
      FreeAndNil(DB);
    end;
  except on
    E : Exception do
    begin
      raise;
    end;
  end;
end;


procedure TB2Drive.LoadTokens;
var
  DB: TSQLite3Database;
  Stmt: TSQLite3Statement;
begin
  try
    CheckError( FAccountId > 0, 'AccountId is not Assigned. LoadTokens' );
    DB := TSQLite3Database.Create;
    try
      DB.Open(RC_LocalDbName);
      Stmt := DB.Prepare('SELECT id, cloud_type, AppKey, AppSecret, BucketName,'+
        ' BucketId FROM accounts WHERE id = ?');
      try
        Stmt.BindInt( 1, FAccountId );
        CheckError( Stmt.Step = SQLITE_ROW, 'account ' + IntToStr( FAccountId ) +' not found in accounts table' );
        AppKey := DecryptKey( Stmt.ColumnText( ColumnByName('AppKey', Stmt ) ), GSecretKey );
        AppSecret := DecryptKey( Stmt.ColumnText( ColumnByName('AppSecret', Stmt ) ) , GSecretKey );
        BucketName := DecryptKey( Stmt.ColumnText( ColumnByName('BucketName', Stmt ) ), GSecretKey );
        BucketId := DecryptKey( Stmt.ColumnText( ColumnByName('BucketId', Stmt ) ), GSecretKey );
      finally
        FreeAndNil(Stmt);
      end;
    finally
      FreeAndNil(DB);
    end;
  except on
    E : Exception do
    begin
      raise;
    end;
  end;
end;


procedure TB2Drive.DoAuth();
var
  headers: TCoreCloudHeaders;
  o, fo: TJSONObject;
  ja : TJSONArray;
  i: integer;
  jv: TJSONValue;
  Response : AnsiString;
  HTTPResult : Integer;
  AServ : string;
  PostData : AnsiString;
const
  URL_AUTH =
    'https://api.backblaze.com/b2api/v1/b2_authorize_account';
begin
  try
    LoadTokens;
    FAuthCompleted := false;
    {CheckError( GActiveCloudStorage > '', 'GActiveCloudStorage is empty. TB2Drive.DoAuth' );
    if GActiveCloudStorage = 'cloudfiles' then
    begin
      CheckError( GCloudFilesAppSecret > '', 'GCloudFilesAppSecret is empty. TB2Drive.DoAuth' );
      CheckError( GCloudFilesAppKey > '', 'GCloudFilesAppKey is empty. TB2Drive.DoAuth' );
      CheckError( GCloudFilesLogin > '', 'GCloudFilesLogin is empty. TB2Drive.DoAuth' );
      CheckError( RequestCloudFilesAuthority( GCloudFilesLogin, GCloudFilesPassword ), 'CloudFiles authentication failed!' );
      GBucketName := NormalyzeToBucketName(GCloudFilesLogin);
      GAppKey := DecryptKey( GCloudFilesAppKey, '7FJRBM1' );
      CheckError( GAppKey > '', 'AppKey is empty. TB2Drive.DoAuth. GCloudFilesAppKey:' + GCloudFilesAppKey + ' GSecretKey:' + GSecretKey);
      GAppSecret := DecryptKey( GCloudFilesAppSecret, '7FJRBM1' );
    end;}
    CheckError( FAppKey > '', 'AppKey is empty. TB2Drive.DoAuth' );
    CheckError( FAppSecret > '', 'AppSecret is empty. TB2Drive.DoAuth' );
    CheckError( FBucketName > '', 'BucketName is empty. TB2Drive.DoAuth' );
    AddHeader( headers, 'Authorization', 'Basic ' + EncodeBase64(FAppKey + ':' + FAppSecret));
    Response := HttpsGet( URL_AUTH, Headers );
    jv := TJSONOBject.ParseJSONValue(BytesOf(Response),0);
    CheckError( Assigned( jv ), 'jv = Nil' );
    o := jv as TJSONObject;
    FdownloadUrl := GetJSONProp( o,'downloadUrl' );
    FapiUrl := GetJSONProp( o, 'apiUrl' );
    Fetch( FapiUrl, 'https://' );
    FB2accountId := GetJSONProp( o,'accountId' );
    Token_Auth := GetJSONProp( o, 'authorizationToken' );
    CheckError( Token_Auth > '', 'authorizationToken is empty:' + Response );
    FAuthCompleted := true;
  except on
    E : Exception do
    begin
      raise;
    end;
  end;
end;

function TB2Drive.CreateBucket( BucketName : string ) : string;
var
  headers: TCoreCloudHeaders;
  Response : AnsiString;
  HTTPResult : Integer;
  AServ : string;
  PostData : AnsiString;
begin
  try
    CheckError( Token_Auth > '', 'Token_Auth is empty' );
    CheckError( FapiUrl > '', 'FapiUrl is empty' );
    headers := Nil;
    AddHeader( headers, 'Authorization', Token_Auth );
    PostData := '{"accountId":"' + FB2accountId + '", "bucketName":"'+FBucketName
      + '", "bucketType":"allPrivate"}';
    HTTPResult := HttpsPost( FapiUrl, '/b2api/v1/b2_create_bucket', headers, PostData, Response );
    CheckError( HTTPResult = 200, 'Can not create Bucket:' + FBucketName + '. Server response:' +Response );
    Result := GetJSONProp( Response, 'bucketId');
  except on
    E : Exception do
    begin
      raise;
    end;
  end;
end;

function TB2Drive.GetBucketId( BucketName : string ) : string;
var
  headers: TCoreCloudHeaders;
  o, fo: TJSONObject;
  ja : TJSONArray;
  i: integer;
  jv: TJSONValue;
  Response : AnsiString;
  HTTPResult : Integer;
  AServ : string;
  PostData : AnsiString;
begin
  try
    CheckError( Token_Auth > '', 'Token_Auth is empty' );
    CheckError( FB2accountId > '', 'FaccountId is empty' );
    headers := Nil;
    AddHeader( headers, 'Authorization', Token_Auth );
    PostData := '{"accountId":"' + FB2accountId + '"}';
    HTTPResult := HttpsPost( FapiUrl, '/b2api/v1/b2_list_buckets', headers, PostData, Response );
    CheckError( HTTPResult = 200, '/b2api/v1/b2_list_buckets : ' + Response );

    o := TJSONObject.ParseJSONValue(BytesOf(CleanUpJSON(Response)),0) as TJSONObject;
    CheckError(Assigned(o),'o=Nil');
    try
      ja := GetJSONValue(o,'buckets') as TJSONArray;
      CheckError( Assigned(ja), 'ja=Nil' );
      for i := 0 to ja.Size - 1 do
      begin
         fo := ja.Get(i) as TJSONObject;
         if LowerCase( BucketName ) = LowerCase( GetJSONProp(fo,'bucketName') ) then
         begin
           Result := GetJSONProp(fo,'bucketId');
           break;
         end;
      end;
    finally
      o.Free;
    end;
  except on E : Exception do
    begin
      raise;
    end;
  end;
end;

function TB2Drive.BucketExists( const BucketId : string ) : boolean;
var
  efn, fnpath: UnicodeString;
  postdata, md5Str, sha1Str, Response: AnsiString;
  httpres: Integer;
  NeedRetry : boolean;
  Headers: TCoreCloudHeaders;
  HTTPResult : Integer;
  uploadUrl, authorizationToken, path, resdat:string;
  o, fo: TJSONObject;
  SS : TStringStream;
  MHTTP : THttpSend;
  I : Integer;
  label StartOver;
begin
  try
    Result := false;
    if BucketId = '' then
      Exit;
    CheckError( Token_Auth > '', 'Token_Auth is empty' );
    AddHeader( headers, 'Authorization', Token_Auth );
    PostData := '{ "bucketId":"'+ FBucketId +'" }';
    HTTPResult := HttpsPost( FapiUrl, '/b2api/v1/b2_get_upload_url', headers, PostData, Response );
    if HTTPResult = 200 then
    begin
      o := TJSONObject.ParseJSONValue(BytesOf(CleanUpJSON(Response)),0) as TJSONObject;
      CheckError( Assigned(o),'o=Nil' );
      try
        uploadUrl := GetJSONProp( o, 'uploadUrl' );
        authorizationToken := GetJSONProp( o, 'authorizationToken' );
        if authorizationToken > '' then
          Result := true;
      finally
        o.Free;
      end;
    end;
  except on E : Exception do
  begin
    raise;
  end;
end;
end;


procedure TB2Drive.UploadFromStream( const DestFileNameAndPath : string;  const ASource: TStream;  var FileID : string; var Filename : string);
var
  efn, fnpath: UnicodeString;
  postdata, md5Str, sha1Str, Response: AnsiString;
  httpres: Integer;
  NeedRetry : boolean;
  Headers: TCoreCloudHeaders;
  HTTPResult : Integer;
  uploadUrl, authorizationToken, path, resdat:string;
  o, fo: TJSONObject;
  SS : TStringStream;
  MHTTP : THttpSend;
  I : Integer;
  label StartOver;
begin
  try
    CheckError( FBucketName > '', 'BucketName is empty' );
    if FBucketId = '' then
    begin
      FBucketId := GetBucketId( FBucketName );
      SaveTokens;
    end;
    CheckError( Token_Auth > '', 'Token_Auth is empty' );
    path := DestFileNameAndPath;
    Fetch( path, ':\' );
    fnpath := path;
    fnpath := StringReplace( fnpath, '\', '/', [rfReplaceAll]);
    AddHeader( headers, 'Authorization', Token_Auth );
    PostData := '{ "bucketId":"'+ FBucketId +'" }';
    HTTPResult := HttpsPost( FapiUrl, '/b2api/v1/b2_get_upload_url', headers, PostData, Response );
    CheckError( HTTPResult = 200, '/b2api/v1/b2_get_upload_url : ' + Response );
    RaiseIfAbort;
    o := TJSONObject.ParseJSONValue(BytesOf(CleanUpJSON(Response)),0) as TJSONObject;
    CheckError( Assigned(o),'o=Nil' );
    try
      uploadUrl := GetJSONProp( o, 'uploadUrl' );
      authorizationToken := GetJSONProp( o, 'authorizationToken' );
    finally
      o.Free;
    end;

    headers := Nil;
    AddHeader( headers, 'Authorization', authorizationToken );
    sha1Str := StreamSHA1( ASource );
    if ASource.Size > 0 then
    begin
      MHTTP := THttpSend.Create;
      try
        InitHttp( MHTTP );
        ASource.Position := 0;
        MHTTP.Document.LoadFromStream(ASource);
        MHTTP.Document.Position := 0;
        MHTTP.Headers.Add('Authorization: ' + authorizationToken );
        MHTTP.Headers.Add('X-Bz-File-Name: ' + URLEncode( UTF8Encode( fnpath ) ) );
        MHTTP.Headers.Add('X-Bz-Content-Sha1: ' + sha1Str );
        MHTTP.Headers.Add('Content-Type: application/octet-stream' );
        MHTTP.HTTPMethod( 'POST', uploadUrl );
        MHTTP.Document.Position := 0;
        SetLength( Response, MHTTP.Document.Size );
        MHTTP.Document.Read( Pointer(Response)^, MHTTP.Document.Size );
      finally
        FreeAndNil( MHTTP );
      end;
      o := TJSONObject.ParseJSONValue(BytesOf(CleanUpJSON(Response)),0) as TJSONObject;
      CheckError( Assigned(o),'o=Nil' );
      try
        FileID := GetJSONProp( o, 'fileId' );
        FileName := GetJSONProp( o, 'fileName' );
        CheckError( FileID > '', 'upload error:' + Response );
      finally
        o.Free;
      end;
    end

  except on E : Exception do
  begin
    raise;
  end;
end;
end;

function TB2Drive.DownloadToStream( const FileID : string;
  const FileName : string; ADest: TStream ): boolean;
var
  headers: TCoreCloudHeaders;
  url : UnicodeString;
  HTTPResult : Integer;
  PostData, Response : ansistring;
  RemFileName : string;
begin
  try
    AddHeader( headers, 'Authorization', Token_Auth );
    url := FDownloadUrl + '/b2api/v1/b2_download_file_by_id?fileId=' + FileID;
    HttpsGet( url, headers, ADest );
    CheckError( ADest.Size > 0, 'Empty stream received!' );
    SetLength( Response, 100 );
    ADest.Position := 0;
    ADest.Read( Pointer(Response)^, 100 );
    ADest.Position := 0;
    Response := CleanUpJSON( Response );
    if ( Response[1] = '{' ) and (
    ( Pos( 'internal_error', Response ) > 0 ) or
    ( Pos( 'not_found', Response ) > 0 ) or
    ( Pos( 'bad_value', Response ) > 0 ) or
    ( Pos( '"status": 400', Response ) > 0 ) )  then
    begin
      CheckError( false, 'Error:' + Response );
    end;
    Result := true;
  except
    on E : Exception do
    begin
      raise;
    end;
  end;
end;


procedure TB2Drive.DeleteFile( const FileId : UnicodeString; const FileName : UnicodeString );
var
  headers: TCoreCloudHeaders;
  HTTPResult : Integer;
  res, data : AnsiString;
  o, fo : TJSONObject;
  ja : TJSONArray;
  I: Integer;
  us : UnicodeString;
begin
  try
    CheckError( FBucketName > '', 'BucketName is empty' );
    if FBucketId = '' then
    begin
      FBucketId := GetBucketId( FBucketName );
      SaveTokens;
    end;
    headers := Nil;
    AddHeader( headers, 'Authorization', Token_Auth );
    us := '{"bucketId":"'+ FBucketId +'","startFileId":"' + FileId+
      '", "startFileName":"' + FileName +'", "maxFileCount":1000}';
    data := UTF8Encode( us );
    HTTPResult := HttpsPost( FApiUrl, '/b2api/v1/b2_list_file_versions', headers, data, res );
    CheckError( HTTPResult = 200, '/b2api/v1/b2_list_file_versions : ' + res );


    o := TJSONObject.ParseJSONValue(BytesOf(CleanUpJSON(res)),0) as TJSONObject;
    CheckError( Assigned(o),'o=Nil' );

    try
      if GetJSonValue(o,'error') <> nil then
         raise Exception.Create('Backblaze error: '+o.ToString);
      ja := GetJSONValue(o,'files') as TJSONArray;
      CheckError( Assigned(ja), 'ja=Nil' );
      for i := 0 to ja.Size - 1 do
      begin
        fo := ja.Get(i) as TJSONObject;
        if GetJSONProp(fo, 'fileName') = FileName then
          DeleteFileVersion( GetJSONProp(fo, 'fileId'), FileName );//GetJSONProp(fo, 'fileName')
      end;
    finally
      FreeAndNil(o);
    end;

  except
    on E : Exception do
    begin
      raise;
    end;
  end;
end;



procedure TB2Drive.DeleteFileVersion( const FileId : UnicodeString; const FileName : UnicodeString );
var
  headers: TCoreCloudHeaders;
  HTTPResult : Integer;
  res, data : AnsiString;
  us : UnicodeString;
begin
 try
    us := '{"fileId":"'+FileId+'", "fileName":"'+FileName+'"}';
    data := UTF8Encode( us );
    AddHeader( headers, 'Authorization', Token_Auth );
    HTTPResult := HttpsPost( FapiUrl, '/b2api/v1/b2_delete_file_version', headers, data, res );
    CheckError( HTTPResult = 200, '/b2api/v1/b2_delete_file_version : ' + res );
  except
    on E : Exception do
    begin
     // raise;
    end;
  end;
end;

procedure TB2Drive.OnSocketStatus(Sender: TObject; Reason: THookSocketReason; const Value: string);
begin
  RaiseIfAbort;
  try
    case Reason of
      HR_ReadCount:
      begin
        AddProgress( StrToIntDef( Value, 0 ) );
      end;
      HR_WriteCount:
      begin
        AddProgress( StrToIntDef( Value, 0 ) );
      end;
//      HR_Wait:
//      HR_Error:
      end;

  {  if isMainThread and Assigned( CallbackProcessMessages ) then
       CallbackProcessMessages;}
  except on
    E : Exception do
    begin
      raise;
    end;
  end;
end;


procedure TB2Drive.InitHttp( MHTTP : THttpSend );
begin
  MHTTP.MimeType:= '';
  MHTTP.UserAgent:= '';
  MHTTP.Protocol := '1.1';
  MHTTP.Sock.SSL.VerifyCert:= True;
  {$ifdef MSWINDOWS}
  MHTTP.Sock.SSL.CertCAFile:= ExtractFilePath(ParamStr(0))+'cacert.pem';
  {$endif}
  MHTTP.Headers.Clear;
  MHTTP.Sock.OnStatus := OnSocketStatus;
  {
  with TSynProtocol(SynProtocol) do
    if FTPSettings.ProxyType>0 then begin
       MHTTP.ProxyHost:= stringNC(FTPSettings.pProxyHost);
       MHTTP.ProxyUser:= stringNC(FTPSettings.pProxyUser);
       MHTTP.ProxyPass:= stringNC(FTPSettings.pProxyPassword);
       MHTTP.ProxyPort:= IntToStr(FTPSettings.ProxyPort );
       end;}
  //MHTTP.Timeout:= SessionData.Timeout;
end;

initialization
 GSecretKey := GetWMIstring('Win32_BIOS','SerialNumber');
 CheckError( GSecretKey > '', 'Can not initialize GSecretKey' );
end.
