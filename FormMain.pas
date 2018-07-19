unit FormMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, ComCtrls,CommCtrl, ExtCtrls,ShellApi, Vcl.AppEvnts,
  Vcl.Menus, System.Actions, Vcl.ActnList, CloudFilesDrive, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ToolWin, registry, System.ImageList, SHA1Code;

type
  TFrmMain = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter: TSplitter;
    LvFileMan: TListView;
    TvFileMan: TTreeView;
    StatusBar: TStatusBar;
    ilTvFileMan: TImageList;
    ilLvFileMan: TImageList;
    PopupMenu1: TPopupMenu;
    acUpload1: TMenuItem;
    Download1: TMenuItem;
    ActionList1: TActionList;
    acUpload: TAction;
    acDownload: TAction;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    RegisterwithWindowsShell1: TMenuItem;
    UnregisterfromWindowsShell1: TMenuItem;
    ac_DeleteFileVersions: TAction;
    acDeleteFileVersions1: TMenuItem;
    SpeedButton3: TSpeedButton;
    ac_Configuration: TAction;
    ExeSha11: TMenuItem;
    Help1: TMenuItem;
    About1: TMenuItem;
    ac_About: TAction;
    procedure FormCreate(Sender: TObject);
    procedure TvFileManClick(Sender: TObject);
    procedure acUploadExecute(Sender: TObject);
    procedure acUploadUpdate(Sender: TObject);
    procedure acDownloadUpdate(Sender: TObject);
    procedure acDownloadExecute(Sender: TObject);
    procedure RegisterwithWindowsShell1Click(Sender: TObject);
    procedure UnregisterfromWindowsShell1Click(Sender: TObject);
    procedure ac_DeleteFileVersionsExecute(Sender: TObject);
    procedure ac_ConfigurationExecute(Sender: TObject);
    procedure ExeSha11Click(Sender: TObject);
    procedure ac_AboutExecute(Sender: TObject);
  private
    procedure CheckMenuItems;

    { Private declarations }
  public
    constructor Create( Owner : TComponent ); override;
    { Public declarations }
  end;

var
  FrmMain     :TFrmMain;
  IconList    :TStringList;
const
  BackupKeyNameCommand = '*\shell\Backup to Cloud Files\Command';
  RestoreKeyNameCommand = '*\shell\Restore from Cloud Files\Command';
  BackupKeyName = '*\shell\Backup to Cloud Files';
  RestoreKeyName = '*\shell\Restore from Cloud Files';

  key_shell_open_command =
  'Software\Classes\cloudfilesinc.cloudfilesapp.v1\shell\open\command';
  key_dima_auto_file =
  'Software\Classes\dima_auto_file\shell\open\command';
  key_ProgId_icon =
  'Software\Classes\cloudfilesinc.cloudfilesapp.v1\DefaultIcon';
  key_extension = 'Software\Classes\.dima';
  key_ProgId =
  'Software\Classes\cloudfilesinc.cloudfilesapp.v1';

implementation
uses synautil, ProgressForm, B2ConfigForm, System.Win.ComObj, AboutForm;
{$R *.dfm}

function ImageList_ReplaceIcon(ImageList: THandle; Index: Integer; Icon: hIcon): Integer; stdcall; external 'comctl32.dll' name 'ImageList_ReplaceIcon';

function FormatFileSize(Size: extended): string;
begin
  if Size = 0 then
  begin
    Result := '0 B';
  end
  else if Size < 1000 then
  begin
    Result := FormatFloat('0', Size) + ' B';
  end
  else
  begin
    Size := Size / 1024;
    if (Size < 1000) then
    begin
      Result := FormatFloat('0.0', Size) + ' KB';
    end
    else
    begin
      Size := Size / 1024;
      if (Size < 1000) then
      begin
        Result := FormatFloat('0.00', Size) + ' MB';
      end
      else
      begin
        Size := Size / 1024;
        if (Size < 1000) then
        begin
          Result := FormatFloat('0.00', Size) + ' GB';
        end
        else
        begin
          Size := Size / 1024;
          if (Size < 1024) then
          begin
            Result := FormatFloat('0.00', Size) + ' TB';
          end
        end
      end
    end
  end;
end;

Procedure AddDrives;
var
  cDrives       :Array[0..128] of char;
  pDrive        :PChar;
  Icon          :TIcon;
  shInfo        :TSHFileInfo;
  TreeNode      :TTreeNode;
begin
  if GetLogicalDriveStrings(SizeOf(cDrives),cDrives) = 0 then exit;
  FrmMain.TvFileMan.Items.BeginUpdate;
  pDrive := cDrives;
  Icon := TIcon.Create;
  while pDrive^ <> #0 do
  begin
    TreeNode := FrmMain.tvFileMan.Items.Add(nil,pDrive);
    SHGetFileInfo(pChar(pDrive), 0, shInfo, SizeOf(shInfo),SHGFI_ICON or SHGFI_SMALLICON or SHGFI_SYSICONINDEX);
    Icon.Handle := shInfo.hIcon;
    TreeNode.ImageIndex :=  FrmMain.ilTvFileMan.AddIcon(Icon);
    TreeNode.SelectedIndex  := TreeNode.ImageIndex;
    Inc(pDrive,4);
  end;
  Icon.Free;
  FrmMain.TvFileMan.Items.EndUpdate;
end;


function TracePath(Node : TTreeNode; DirName : string):string;
begin
  while node.Parent <> nil do
  begin
    result :=  ExcludeTrailingPathDelimiter(node.parent.Text) + '\' + result;
    node := node.Parent;
  end;
  result := result + ExcludeTrailingPathDelimiter(dirname) +'\';
  FrmMain.StatusBar.SimpleText := Result;
end;


procedure UpdateLvItem( LItem : TListItem; FullPath :string );
Var
  shFileInfo:TSHFileInfo;
  Icon : TIcon;
begin
  ShGetFileInfo(Pchar(FullPath),0,shFileInfo,
      SizeOf(SHFileInfo),SHGFI_TYPENAME or SHGFI_ICON or SHGFI_SMALLICON );
  Icon := TIcon.Create;
  try
    LItem.Caption := ExtractFileName( FullPath );
    LItem.SubItems.Strings[0] := FormatFileSize( getFileSizeInBytes( FullPath ) );
    LItem.SubItems.Strings[1] := shFileInfo.szTypeName;
    Icon.Handle := shFileInfo.hIcon;
    LItem.ImageIndex := -1;
    LItem.ImageIndex := FrmMain.illvFileMan.AddIcon(Icon);
    TCFItem(LItem.Data).Fullpath := FullPath;
  finally
    Icon.Free;
  end;
end;

procedure TFrmMain.acUploadExecute(Sender: TObject);
var
  CFItem : TCFItem;
  NewFileName : string;
begin
  try
    if not Assigned( LvFileMan.Selected ) then
      Exit;
    CFItem := TCFItem(LvFileMan.Selected.Data);
    LvFileMan.Enabled := false;
    TvFileMan.Enabled := false;
    Application.ProcessMessages;
    try
       NewFileName := CFUploadFile( CFItem.Fullpath, Handle );
       UpdateLvItem( LvFileMan.Selected, NewFileName );
    finally
      LvFileMan.Enabled := true;
      TvFileMan.Enabled := true;
    end;
  except on E: Exception do
  begin
     MessageBox( Handle, PChar(E.Message), 'Message', MB_OK );
  end;
  end;
end;

procedure TFrmMain.acDownloadExecute(Sender: TObject);
var
  CFItem : TCFItem;
  NewFileName : string;
begin
  try
    if not Assigned( LvFileMan.Selected ) then
      Exit;
    CFItem := TCFItem(LvFileMan.Selected.Data);
    LvFileMan.Enabled := false;
    TvFileMan.Enabled := false;
    Application.ProcessMessages;
    try
      NewFileName := CFDownloadFile( CFItem.Fullpath, Handle );
      UpdateLvItem( LvFileMan.Selected, NewFileName );
    finally
      LvFileMan.Enabled := true;
      TvFileMan.Enabled := true;
    end;
  except on E: Exception do
  begin
     MessageBox( Handle, PChar(E.Message), 'Message', MB_OK );
  end;
  end;
end;

procedure TFrmMain.CheckMenuItems;
var
  FileExt : string;
begin
  if not Assigned( LvFileMan.Selected ) then
    Exit;
  FileExt := ExtractFileExt( TCFItem(LvFileMan.Selected.Data).Fullpath );
  acUpload.Enabled := FileExt <> '.dima';
  acDownload.Enabled := FileExt = '.dima';
end;

constructor TFrmMain.Create(Owner: TComponent);
begin
  inherited;
  Caption := 'CloudFiles Backup manager ( '
    + CloudFilesDrive.FileVersion( Application.ExeName ) + ' )';
end;

procedure TFrmMain.ExeSha11Click(Sender: TObject);
var
  MS : TMemoryStream;
begin
  MS := TMemoryStream.Create;
  try
    MS.LoadFromFile(Application.ExeName);
    MS.Position := 0;
    ShowMessage( StreamSHA1( MS ) );
  finally
    FreeAndNil( MS );
  end;

end;

procedure TFrmMain.acUploadUpdate(Sender: TObject);
begin
  CheckMenuItems;
end;

procedure TFrmMain.ac_AboutExecute(Sender: TObject);
begin
   with TAboutFrm.Create(Nil) do
   begin
     ShowModal;
     Free;
   end;
end;

procedure TFrmMain.ac_ConfigurationExecute(Sender: TObject);
var
  B2ConfigFrm : TB2ConfigFrm;
begin
  B2ConfigFrm := TB2ConfigFrm.Create( Nil );
  try
    B2ConfigFrm.ShowModal;
  finally
    FreeAndNil( B2ConfigFrm );
  end;
end;

procedure TFrmMain.ac_DeleteFileVersionsExecute(Sender: TObject);
begin
  try
    if not Assigned( LvFileMan.Selected ) then
      Exit;
    CFDeleteFile( TCFItem(LvFileMan.Selected.Data).Fullpath, Handle );
  except on E: Exception do
  begin
     MessageBox( Handle, PChar(E.Message), 'Message', MB_OK );
  end;
  end;
end;

procedure TFrmMain.acDownloadUpdate(Sender: TObject);
begin
  CheckMenuItems;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
begin
  AddDrives;
end;

Function FormatAttr (Attr:Integer) : String;
begin
  SetLength(Result,0);
  if Bool(Attr and FILE_ATTRIBUTE_READONLY) then Result :=          'R';
  if Bool(Attr and FILE_ATTRIBUTE_ARCHIVE)  then Result := Result +' A';
  if Bool(Attr and FILE_ATTRIBUTE_SYSTEM)   then Result := Result +' S';
  if Bool(Attr and FILE_ATTRIBUTE_HIDDEN)   then Result := Result +' H';
end;

function GetNodeByText(ATree : TTreeView; AValue:String;  AVisible: Boolean): TTreeNode;
var
    Node: TTreeNode;
begin
  Result := nil;
  if ATree.Items.Count = 0 then Exit;
  Node := ATree.Items[0];
  while Node <> nil do
  begin
    if UpperCase(Node.Text) = UpperCase(AValue) then
    begin
      Result := Node;
      if AVisible then
        Result.MakeVisible;
      Break;
    end;
    Node := Node.GetNext;
  end;
end;

Procedure ListFolders(Directory:String);
Var
  SearchRec:TSearchRec;
  SHFileInfo:TSHFileInfo;
  Icon:TIcon;
  LItem:TListItem;
  NodeList:TStringList;
  nCount:Integer;
  TmpN:TTreeNode;
  CFItem:TCFItem;
  I:Integer;
Begin
  NodeList := TStringList.Create;
  Icon := TIcon.Create;
  try
    FrmMain.ilLvFileMan.Clear;
    for I := 0 to FrmMain.LvFileMan.Items.Count -1 do
    begin
       TCFItem(FrmMain.LvFileMan.Items.Item[I].Data).Free;
       FrmMain.LvFileMan.Items.Item[I].Data := Nil;
    end;
    FrmMain.LvFileMan.Items.Clear;
    frmMain.tvFileMan.Items.BeginUpdate;
    FrmMain.LvFileMan.Items.BeginUpdate;
    SetErrorMode($8007);  //Stops Errors From Removable Storage.
    TmpN := FrmMain.TvFileMan.Selected.getFirstChild;
    for nCount := 0 to FrmMain.TvFileMan.Selected.Count -1  do
    begin
      NodeList.Add(TmpN.Text);
      TmpN := TmpN.getNextSibling;
    end;
    if FindFirst (Directory + '*.*',FaAnyFile,SearchRec)  = 0 then
    Begin
      Repeat
        If SearchRec.Name =  '.' then continue;
        If SearchRec.Name =  '..' then continue;
        ShGetFileInfo(Pchar(Directory + SearchRec.name),0,SHFileInfo,SizeOf(SHFileInfo),SHGFI_TYPENAME or SHGFI_ICON or SHGFI_SMALLICON );
        if (lowercase(shFileInfo.szTypeName) = 'file folder')
           or (SearchRec.Attr = faDirectory)
           or (lowercase(shFileInfo.szTypeName) ='folder') then
        begin
          if  NodeList.IndexOf(SearchRec.Name) <> -1 then
          begin
            //  find it
            NodeList.Delete(NodeList.IndexOf(SearchRec.Name));
          end
          else
          begin
            //  dnt find it
            FrmMain.tvFileMan.Items.AddChild(frmMain.tvFileMan.Selected,searchRec.Name);
          end;
        end
        else
        begin
          LItem := FrmMain.LvFileMan.Items.Add;
          LItem.Caption := SearchRec.Name;
          LItem.SubItems.Add( FormatFileSize(SearchRec.Size));
          Litem.SubItems.Add(shFileInfo.szTypeName);
          LItem.SubItems.Add(FormatAttr(SearchRec.Attr));
          CFItem := TCFItem.Create;
          CFItem.Fullpath := Directory + SearchRec.Name;
          LItem.Data := CFItem;
          Icon.Handle := shFileInfo.hIcon;
          LItem.ImageIndex :=  FrmMain.illvFileMan.AddIcon(Icon);
        end;
      Until FindNext(SearchRec)<> 0;
      If NodeList.Count <> 0 then
      begin
        For nCount := 0  to NodeList.Count-1 do
        begin
          TmpN := GetNodeByText(FrmMain.TvFileMan,NodeList.Strings[nCount],True);
          TmpN.Delete;
        end;
      end;
    end;
  finally
    frmMain.tvFileMan.Items.EndUpdate;
    frmMain.lvFileMan.Items.EndUpdate;
    NodeList.Free;
    Icon.Free;
  end;
end;

procedure TFrmMain.TvFileManClick(Sender: TObject);
Const
  OnNode  = TVHT_ONITEMICON or TVHT_ONITEMLABEL;
Var
  HTInfo      :TTVHitTestInfo;
  Node        :TTreeNode;
begin
  Node := nil;
  GetCursorPos(HTInfo.pt);
  HTInfo.pt := TvFileMan.ScreenToClient(HTInfo.pt);
  If TreeView_HitTest(TvFileMan.Handle,HTInfo) <> Nil then
  begin
    if Bool(HTInfo.Flags and OnNode) then
      Node := TvFileMan.Items.GetNode(HTInfo.hItem);
    if Node <> Nil then
    begin
      ListFolders(TracePath(Tvfileman.Selected,tvfileman.Selected.Text));
    end;
  end;
end;


procedure TFrmMain.RegisterwithWindowsShell1Click(Sender: TObject);
var
  reg : TRegistry;
  writen : boolean;
begin
{
[HKEY_CURRENT_USER\Software\Classes\cloudfilesinc.cloudfilesapp.v1\shell\open\command]
@="c:\path\to\app.exe \"%1\""
[HKEY_CURRENT_USER\Software\Classes\.blerg]
@="blergcorp.blergapp.v1"
}

  try
    writen := false;
    reg := TRegistry.Create();
    try
      reg.LazyWrite := false;
      reg.RootKey := HKEY_CURRENT_USER;
      CheckError( reg.OpenKey(key_ProgId_icon, true), 'Unable to create key! '+ key_ProgId_icon );
      reg.WriteString( '', ExtractFilePath( Application.ExeName ) + 'CloudFiles.ico' ) ;
      reg.CloseKey();
      writen := true;

      reg.RootKey := HKEY_CURRENT_USER;
      CheckError( reg.OpenKey(key_shell_open_command, true), 'Unable to create key! '+ key_shell_open_command );
      reg.WriteString( '', Application.ExeName + ' "%1" "download"') ;
      reg.CloseKey();
      writen := true;

      reg.RootKey := HKEY_CURRENT_USER;

      CheckError( reg.OpenKey(key_dima_auto_file, true), 'Unable to create key! '+ key_dima_auto_file );
      reg.WriteString( '', '"' + Application.ExeName + '" %1 download' ) ;
      reg.CloseKey();
      writen := true;

      reg.RootKey := HKEY_CURRENT_USER;
      CheckError( reg.OpenKey( key_extension, true), 'Unable to create key! '+ key_extension );
      reg.WriteString( '', 'cloudfilesinc.cloudfilesapp.v1') ;
      reg.CloseKey();
      writen := true;
    finally
      reg.Free;
    end;

    if writen then
      MessageBox( Application.Handle, PChar('Application '+ Application.ExeName +
    ' is registered with *.dima extension for Windows Shell'), '', MB_OK )

  except on E: Exception do
  begin
     MessageBox( Handle,
     PChar('Registration with Windows failed. '+
     'Try to run application under administrative privilegies. :' + E.Message),
     'Problem', MB_OK );
  end;
  end;
end;


procedure TFrmMain.UnregisterfromWindowsShell1Click(Sender: TObject);
var
  reg : TRegistry;
  deleted : boolean;
begin
  try
    reg := TRegistry.Create();
    try
      reg.RootKey := HKEY_CURRENT_USER;
      reg.LazyWrite := false;
      deleted := false;
      if reg.KeyExists( key_ProgId ) then
      begin
        CheckError( reg.DeleteKey( key_ProgId ), 'Unable to delete key! '+ key_shell_open_command );
        reg.CloseKey();
        deleted := true;
      end;
      reg.RootKey := HKEY_CURRENT_USER;
      if reg.KeyExists( key_extension ) then
      begin
        CheckError( reg.DeleteKey( key_extension ), 'Unable to delete key! '+ key_extension );
        reg.CloseKey();
        deleted := true;
      end;
      
     if deleted then
        MessageBox( Application.Handle, PChar('*.dima extension for '+ Application.ExeName +
         ' is un-registered from Windows Shell'), '', MB_OK )
  finally
    reg.Free;
  end;
  except on E: Exception do
  begin
     MessageBox( Handle,
     PChar('Un-Register with Windows failed. '+
     'Try to run application under administrative privilegies. :' + E.Message),
     'Problem', MB_OK );
  end;
  end;
end;







end.
