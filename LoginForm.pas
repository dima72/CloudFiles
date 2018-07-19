unit LoginForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Inifiles, synautil;

type
  TLoginFrm = class(TForm)
    Label1: TLabel;
    ctl_Login: TEdit;
    Label2: TLabel;
    ctl_Password: TEdit;
    Label3: TLabel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  LoginFrm: TLoginFrm;

implementation
uses BackBlazeDrive;
{$R *.dfm}



{ TLoginFrm }

procedure TLoginFrm.Button1Click(Sender: TObject);
var
  Ini : TIniFile;
  HttpRes, posnum : Integer;
  headers: TCoreCloudHeaders;
  PostData, Response : AnsiString;
  B2AccountName, StrResp : UnicodeString;
  SS : TStringStream;
begin
  Ini := TIniFile.Create(GetIniPath);
  try
    Ini.WriteString('Login', 'Login', ctl_Login.Text );
    AddHeader( headers, 'Content-Type', 'application/x-www-form-urlencoded' );
    PostData := 'email=' + UrlEncode(ctl_Login.Text) + '&password='+
      ctl_Password.Text+'&signIn=Sign+In';
    HttpRes := HttpsPost('secure.backblaze.com', '/user_signin.htm', headers,
      PostData, Response );
    CheckError( HttpRes = 200, 'Server currently not available. Please try later.' );
    B2AccountName := ctl_Login.Text;
    B2AccountName := Fetch( B2AccountName, '@' );
    StrResp := StringReplace( Response, #$D#$A, '', [rfReplaceAll] );
    posnum :=  Pos( 'Welcome ' + B2AccountName, StrResp );
    CheckError( posnum > 0, 'Login failed. Please check Login and password');
    GBucketName := ConvertLoginToBucketName( ctl_Login.Text );
  finally
    FreeAndNil( Ini );
  end;
   { headers := Nil;
    SS := TStringStream.Create('');
    try
      AddHeader( headers, 'Accept', 'text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8' );
      AddHeader( headers, 'Accept-Encoding', 'gzip, deflate, sdch' );
      HttpsGet( Key_url, headers, SS );
      AppSecret := DecryptKey( SS.DataString, SecretKey );
//      SS.SaveToFile( ExtractFilePath(AppliCation.ExeName) + '\resp1.txt' );
    finally
      FreeAndNil(SS);
    end;}
    ModalResult := mrOk;
end;



procedure TLoginFrm.FormCreate(Sender: TObject);
var
  Ini : TIniFile;
begin
  Ini := TIniFile.Create(GetIniPath);
  try
    ctl_Login.Text := Ini.ReadString('Login', 'Login', '' );
  finally
    FreeAndNil( Ini );
  end;
  Caption := 'Cloud Files ( '
    + BackBlazeDrive.FileVersion( Application.ExeName ) + ' )';

end;

end.
