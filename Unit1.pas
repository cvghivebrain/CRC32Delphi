unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CRCFunc;

type
  TForm1 = class(TForm)
    edFile: TEdit;
    edString: TEdit;
    btnFile: TButton;
    btnString: TButton;
    dlgFile: TOpenDialog;
    btnString2: TButton;
    btnFile2: TButton;
    btnFile3: TButton;
    btnString3: TButton;
    procedure btnStringClick(Sender: TObject);
    procedure btnFileClick(Sender: TObject);
    procedure btnString2Click(Sender: TObject);
    procedure btnFile2Click(Sender: TObject);
    procedure btnFile3Click(Sender: TObject);
    procedure btnString3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.btnStringClick(Sender: TObject);
begin
  ShowMessage(CRCString(edString.Text));
end;

procedure TForm1.btnString2Click(Sender: TObject);
begin
  ShowMessage(SHA1String(edString.Text));
end;

procedure TForm1.btnString3Click(Sender: TObject);
begin
  ShowMessage(MD5String(edString.Text));
end;

procedure TForm1.btnFileClick(Sender: TObject);
begin
  if dlgFile.Execute then
    begin
    edFile.Text := dlgFile.FileName;
    ShowMessage(CRCFile(dlgFile.FileName));
    end;
end;

procedure TForm1.btnFile2Click(Sender: TObject);
begin
  if dlgFile.Execute then
    begin
    edFile.Text := dlgFile.FileName;
    ShowMessage(SHA1File(dlgFile.FileName));
    end;
end;

procedure TForm1.btnFile3Click(Sender: TObject);
begin
  if dlgFile.Execute then
    begin
    edFile.Text := dlgFile.FileName;
    ShowMessage(MD5File(dlgFile.FileName));
    end;
end;

end.
