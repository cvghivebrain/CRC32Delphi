unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    edFile: TEdit;
    edString: TEdit;
    btnFile: TButton;
    btnString: TButton;
    dlgFile: TOpenDialog;
    procedure FormCreate(Sender: TObject);
    procedure btnStringClick(Sender: TObject);
    procedure btnFileClick(Sender: TObject);
  private
    { Private declarations }
    function CRCString(s: string): longint; 
    function CRCFile(f: string): longint;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  crctable: array[0..255] of longint;  
  myfile: file;
  filearray: array of byte;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var i, j: integer;
begin
  { Create CRC32 lookup table. }
  for i := 0 to 255 do
    begin
    crctable[i] := i;
    for j := 0 to 7 do if Odd(crctable[i]) then
      crctable[i] := (crctable[i] shr 1) xor $EDB88320
      else crctable[i] := crctable[i] shr 1;
    end;
end;

{ Get CRC32 of string. }
function TForm1.CRCString(s: string): longint;
var i, x: integer;
begin
  Result := -1;
  for i := 1 to Length(s) do
    begin
    x := (Ord(s[i]) xor Result) and $000000FF;
    Result := (Result shr 8) xor crctable[x];
    end;
  Result := not Result;
end;

{ Get CRC32 of file. }
function TForm1.CRCFile(f: string): longint;
var i, x: integer;
begin
  { Open file and copy to array. }
  AssignFile(myfile,f); // Get file.
  FileMode := fmOpenRead; // Read only.
  Reset(myfile,1);
  SetLength(filearray,FileSize(myfile));
  BlockRead(myfile,filearray[0],FileSize(myfile)); // Copy file to memory.
  CloseFile(myfile); // Close file.

  { Get CRC32 of file while it's in a byte array. }
  Result := -1;
  for i := 0 to Length(filearray)-1 do
    begin
    x := (filearray[i] xor Result) and $000000FF;
    Result := (Result shr 8) xor crctable[x];
    end;
  Result := not Result;
end;

procedure TForm1.btnStringClick(Sender: TObject);
begin
  ShowMessage(InttoHex(CRCString(edString.Text),8));
end;

procedure TForm1.btnFileClick(Sender: TObject);
begin
  if dlgFile.Execute then
    begin
    edFile.Text := dlgFile.FileName;
    ShowMessage(InttoHex(CRCFile(dlgFile.FileName),8));
    end;
end;

end.