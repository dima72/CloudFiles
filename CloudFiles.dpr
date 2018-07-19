program CloudFiles;

uses
  Forms,
  SysUtils,
  Controls,
  Inifiles,
  Winapi.Windows,
  FormMain in 'FormMain.pas' {FrmMain},
  BackBlazeDrive in 'BackBlazeDrive.pas',
  ProgressForm in 'ProgressForm.pas' {ProgressFrm},
  B2ConfigForm in 'B2ConfigForm.pas' {B2ConfigFrm},
  AboutForm in 'AboutForm.pas' {AboutFrm},
  CloudFilesDrive in 'CloudFilesDrive.pas',
  CloudProfileFrame in 'CloudProfileFrame.pas' {CloudProfileFrm: TFrame},
  UploadDialog in 'UploadDialog.pas' {UploadDialogFrm},
  DKJSON in 'Lib\DKJSON.pas',
  SHA1Code in 'Lib\SHA1Code.pas',
  SQLite3 in 'Lib\SQLite3.pas',
  SQLite3Utils in 'Lib\SQLite3Utils.pas',
  SQLite3Wrap in 'Lib\SQLite3Wrap.pas',
  OverbyteIcsMD5 in 'Lib\OverbyteIcsMD5.pas',
  OverbyteIcsTypes in 'Lib\OverbyteIcsTypes.pas';

{$R *.res}
var
  modRes : Integer;
begin
  FrmMain := Nil;
  Application.Initialize;
  try
    //Signature.CheckSignature(Application.ExeName);
    CheckError( FileExists( ExtractFilePath(ParamStr(0))+'cacert.pem' ),
      'File cacert.pem is absent in application directory' );
    CheckError( FileExists( ExtractFilePath(ParamStr(0))+'libeay32.dll' ),
      'File libeay32.dll is absent in application directory' );
    CheckError( FileExists( ExtractFilePath(ParamStr(0))+'SSLeay32.dll' ),
      'File SSLeay32.dll is absent in application directory' );
    CheckLocalDatabaseStructure;
    if (ParamStr(2) <> 'firstrun') and not AccountsEmpty then
    begin
      MessageBox( Application.Handle, PChar('CloudFiles requires configuration!'), 'Info', MB_OK );
      B2ConfigFrm := TB2ConfigFrm.Create( Nil );
      try
        if B2ConfigFrm.ShowModal <> mrOk then
          Exit;
      finally
        FreeAndNil( B2ConfigFrm );
      end;
    end;

    if ParamStr(1) > '' then
    begin
      if ParamStr(1) = 'config' then
      begin
        B2ConfigFrm := TB2ConfigFrm.Create( Nil );
        try
          if B2ConfigFrm.ShowModal = mrOk then
          begin
            if ParamStr(2) = 'firstrun' then
            begin
               MessageBox( Application.Handle, PChar('You can start Backup and Restore'+
               ' your Files by using CloudFiles MenuItem in Windows Explorer Context Menu.'), 'Info', MB_OK );
               with TAboutFrm.Create(Nil) do
               begin
                 ShowModal;
                 Free;
               end;
            end;
          end;
        finally
          FreeAndNil( B2ConfigFrm );
        end;
        Exit;
      end;

      if ( ParamStr(2) = 'upload' ) or ( ExtractFileExt( ParamStr(1) ) <> '.dima' ) then
      begin
        if ( not FileExists( ParamStr(1) ) ) and ( DirectoryExists( ParamStr(1) ) )  then
          CheckError( false, 'Backup of directoies is not implemented in this edition.' );

        CheckError( FileExists( ParamStr(1) ), 'File ' + ParamStr(1) + ' not found' );
        CFUploadFile( ParamStr(1), Application.Handle );
      end
      else if ( ParamStr(2) = 'download' ) or ( ExtractFileExt( ParamStr(1) ) = '.dima' ) then
      begin
        if ExtractFileExt( ParamStr(1) )  <> '.dima' then
        begin
          MessageBox( Application.Handle, PChar('Only files with "*.dima" extention can be restored.'), '', MB_OK );
          Exit;
        end;

        if MessageBox( Application.Handle, PChar('Do you want restore file:' + ParamStr(1) + '?'),
          'Question', MB_YESNO ) = ID_YES then
        begin
          CFDownloadFile( ParamStr(1), Application.Handle );
        end;
      end;
      Exit;
    end;
    if not Assigned( FrmMain ) then
      Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
  except on E : Exception do
  begin
    MessageBox( Application.Handle, PChar(E.Message),
      'Application start', MB_OK );
    Exit;
  end;
  end;
end.
