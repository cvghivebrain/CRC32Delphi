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
    btnString2: TButton;
    btnFile2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnStringClick(Sender: TObject);
    procedure btnFileClick(Sender: TObject);
    procedure btnString2Click(Sender: TObject);
    procedure btnFile2Click(Sender: TObject);
  private
    { Private declarations }
    function CRCString(s: string): string;
    function CRCFile(fi: string): string;
    function CRCData(): string;
    function SHA1String(s: string): string;
    function SHA1File(fi: string): string;
    function SHA1Data(): string;
    function rol(l: longword; i: integer): longword;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  crctable: array[0..255] of longint;  
  myfile: file;
  dataarray: array of byte;

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

{ Left rotate bits in longword. }
function TForm1.rol(l: longword; i: integer): longword;
begin
  Result := (l shl i)+(l shr (32-i));
end;

{ Get CRC32 of string. }
function TForm1.CRCString(s: string): string;
var i: integer;
begin
  SetLength(dataarray,Length(s));
  for i := 0 to (Length(s)-1) do
    dataarray[i] := Ord(s[i+1]); // Copy string to array.

  Result := CRCData();
end;

{ Get CRC32 of file. }
function TForm1.CRCFile(fi: string): string;
begin
  { Open file and copy to array. }
  AssignFile(myfile,fi); // Get file.
  FileMode := fmOpenRead; // Read only.
  Reset(myfile,1);
  SetLength(dataarray,FileSize(myfile));
  BlockRead(myfile,dataarray[0],FileSize(myfile)); // Copy file to memory.
  CloseFile(myfile); // Close file.

  Result := CRCData();
end;

{ Get CRC of data in array (from string or file). }
function TForm1.CRCData: string;
var i, x: integer;
  r: longint;
begin
  r := -1;
  for i := 0 to Length(dataarray)-1 do
    begin
    x := (dataarray[i] xor r) and $FF;
    r := (r shr 8) xor crctable[x];
    end;
  Result := LowerCase(InttoHex(not r,8));
end;

procedure TForm1.btnStringClick(Sender: TObject);
begin
  ShowMessage(CRCString(edString.Text));
end;

procedure TForm1.btnString2Click(Sender: TObject);
begin
  ShowMessage(SHA1String(edString.Text));
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

{ Get SHA-1 of string. }
function TForm1.SHA1String(s: string): string;
var i: integer;
  ml: int64;
begin
  SetLength(dataarray,Length(s)+9+64-((Length(s)+9) mod 64)); // Pad data to multiple of 64.
  for i := 0 to (Length(s)-1) do
    dataarray[i] := Ord(s[i+1]); // Copy string to array.
  dataarray[Length(s)] := $80;
  ml := Length(s)*8; // String length in bits.
  for i := 0 to 7 do
    dataarray[Length(dataarray)-1-i] := (ml shr (i*8)) and $ff; // Copy ml to end of array.

  Result := SHA1Data();
end;

{ Get SHA-1 of file. }
function TForm1.SHA1File(fi: string): string;
var i: integer;
  ml: int64;
begin
  { Open file and copy to array. }
  AssignFile(myfile,fi); // Get file.
  FileMode := fmOpenRead; // Read only.
  Reset(myfile,1);
  SetLength(dataarray,FileSize(myfile)+9+64-((FileSize(myfile)+9) mod 64)); // Pad data to multiple of 64.
  BlockRead(myfile,dataarray[0],FileSize(myfile)); // Copy file to array.
  dataarray[FileSize(myfile)] := $80;
  ml := FileSize(myfile)*8; // File size in bits.
  for i := 0 to 7 do
    dataarray[Length(dataarray)-1-i] := (ml shr (i*8)) and $ff; // Copy ml to end of array.
  CloseFile(myfile); // Close file.

  Result := SHA1Data();
end;

{ Get SHA-1 of data in array (from string or file). }
function TForm1.SHA1Data: string;
var h0,h1,h2,h3,h4,a,b,c,d,e,f,k,t: longword;
  w: array[0..79] of longword;
  i, j: integer;
begin
  h0 := $67452301; // Initialise variables.
  h1 := $EFCDAB89;
  h2 := $98BADCFE;
  h3 := $10325476;
  h4 := $C3D2E1F0;
  for j := 0 to ((Length(dataarray) div 64)-1) do
    begin
    for i := 0 to 15 do // Copy chunk into array.
      w[i] := (dataarray[(j*64)+(i*4)] shl 24)+(dataarray[(j*64)+(i*4)+1] shl 16)+(dataarray[(j*64)+(i*4)+2] shl 8)+dataarray[(j*64)+(i*4)+3];
    for i := 16 to 79 do // Extend chunk data.
      w[i] := rol((w[i-3] xor w[i-8] xor w[i-14] xor w[i-16]),1);
    a := h0;
    b := h1;
    c := h2;
    d := h3;
    e := h4;
    for i := 0 to 79 do
      begin
      if i < 20 then
        begin
        f := (b and c) or ((not b) and d);
        k := $5A827999;
        end
      else if i < 40 then
        begin
        f := b xor c xor d;
        k := $6ED9EBA1;
        end
      else if i < 60 then
        begin
        f := (b and c) or (b and d) or (c and d);
        k := $8F1BBCDC;
        end
      else
        begin
        f := b xor c xor d;
        k := $CA62C1D6;
        end;
      t := rol(a,5) + f + e + k + w[i];
      e := d;
      d := c;
      c := rol(b,30);
      b := a;
      a := t;
      end;
    h0 := h0 + a; // Add chunk result.
    h1 := h1 + b;
    h2 := h2 + c;
    h3 := h3 + d;
    h4 := h4 + e;
    end;

  Result := AnsiLowerCase(InttoHex(h0)+InttoHex(h1)+InttoHex(h2)+InttoHex(h3)+InttoHex(h4));
end;

end.
