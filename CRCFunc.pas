unit CRCFunc;

interface

uses SysUtils;

function CRCString(s: string): string;
function CRCFile(fi: string): string;
function CRCData(): string;
function SHA1String(s: string): string;
function SHA1File(fi: string): string;
function SHA1Data(): string;
function MD5String(s: string): string;
function MD5File(fi: string): string;
function MD5Data(): string;
function rol(l: longword; i: integer): longword;
function swapendian(l: longword): longword;
function swapendian64(i: uint64): uint64;

var
  hashfile: file;
  hasharray: array of byte;

const
  md5table: array[0..63] of longword = ($d76aa478, $e8c7b756, $242070db, $c1bdceee,
    $f57c0faf, $4787c62a, $a8304613, $fd469501,
    $698098d8, $8b44f7af, $ffff5bb1, $895cd7be,
    $6b901122, $fd987193, $a679438e, $49b40821,
    $f61e2562, $c040b340, $265e5a51, $e9b6c7aa,
    $d62f105d, $02441453, $d8a1e681, $e7d3fbc8,
    $21e1cde6, $c33707d6, $f4d50d87, $455a14ed,
    $a9e3e905, $fcefa3f8, $676f02d9, $8d2a4c8a,
    $fffa3942, $8771f681, $6d9d6122, $fde5380c,
    $a4beea44, $4bdecfa9, $f6bb4b60, $bebfbc70,
    $289b7ec6, $eaa127fa, $d4ef3085, $04881d05,
    $d9d4d039, $e6db99e5, $1fa27cf8, $c4ac5665,
    $f4292244, $432aff97, $ab9423a7, $fc93a039,
    $655b59c3, $8f0ccc92, $ffeff47d, $85845dd1,
    $6fa87e4f, $fe2ce6e0, $a3014314, $4e0811a1,
    $f7537e82, $bd3af235, $2ad7d2bb, $eb86d391);
  crctable: array[0..255] of longword = ($00000000, $77073096, $EE0E612C, $990951BA,
    $076DC419, $706AF48F, $E963A535, $9E6495A3,
    $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
    $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
    $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
    $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
    $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
    $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
    $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
    $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
    $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
    $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
    $26D930AC, $51DE003A, $C8D75180, $BFD06116,
    $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
    $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
    $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,
    $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
    $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
    $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
    $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
    $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
    $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
    $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
    $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
    $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
    $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
    $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
    $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
    $5005713C, $270241AA, $BE0B1010, $C90C2086,
    $5768B525, $206F85B3, $B966D409, $CE61E49F,
    $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
    $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,
    $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
    $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
    $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
    $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
    $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
    $F762575D, $806567CB, $196C3671, $6E6B06E7,
    $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
    $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
    $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
    $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
    $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
    $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
    $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
    $CC0C7795, $BB0B4703, $220216B9, $5505262F,
    $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
    $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,
    $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
    $9C0906A9, $EB0E363F, $72076785, $05005713,
    $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
    $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
    $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
    $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
    $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
    $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
    $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
    $A7672661, $D06016F7, $4969474D, $3E6E77DB,
    $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
    $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
    $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
    $BAD03605, $CDD70693, $54DE5729, $23D967BF,
    $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
    $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);
  md5shift: array[0..63] of integer = (7,12,17,22,7,12,17,22,7,12,17,22,7,12,17,22,
    5,9,14,20,5,9,14,20,5,9,14,20,5,9,14,20,
    4,11,16,23,4,11,16,23,4,11,16,23,4,11,16,23,
    6,10,15,21,6,10,15,21,6,10,15,21,6,10,15,21);

implementation

{ Left rotate bits in longword. }
function rol(l: longword; i: integer): longword;
begin
  Result := (l shl i)+(l shr (32-i));
end;

{ Change endianness. }
function swapendian(l: longword): longword;
begin
  Result := (l shl 24)+(l shr 24)+((l shl 8) and $ff0000)+((l shr 8) and $ff00);
end;

function swapendian64(i: uint64): uint64;
var h, l: longword;
begin
  l := swapendian(i and $ffffffff); // Low longword.
  h := swapendian(i shr 32); // High longword.
  Result := h + (l*$100000000); // Recombine as uint64.
end;

{ Get CRC32 of string. }
function CRCString(s: string): string;
var i: integer;
begin
  SetLength(hasharray,Length(s));
  for i := 0 to (Length(s)-1) do
    hasharray[i] := Ord(s[i+1]); // Copy string to array.

  Result := CRCData();
end;

{ Get CRC32 of file. }
function CRCFile(fi: string): string;
begin
  { Open file and copy to array. }
  AssignFile(hashfile,fi); // Get file.
  FileMode := fmOpenRead; // Read only.
  Reset(hashfile,1);
  SetLength(hasharray,FileSize(hashfile));
  BlockRead(hashfile,hasharray[0],FileSize(hashfile)); // Copy file to memory.
  CloseFile(hashfile); // Close file.

  Result := CRCData();
end;

{ Get CRC of data in array (from string or file). }
function CRCData: string;
var i, x: integer;
  r: longint;
begin
  r := -1;
  for i := 0 to Length(hasharray)-1 do
    begin
    x := (hasharray[i] xor r) and $FF;
    r := (r shr 8) xor crctable[x];
    end;
  Result := LowerCase(InttoHex(not r,8));
end;

{ Get SHA-1 of string. }
function SHA1String(s: string): string;
var i: integer;
  ml: uint64;
begin
  SetLength(hasharray,Length(s)+9+64-((Length(s)+9) mod 64)); // Pad data to multiple of 64.
  for i := 0 to 63 do
    hasharray[Length(hasharray)-1-i] := 0; // Clear last 64 bytes.
  for i := 0 to (Length(s)-1) do
    hasharray[i] := Ord(s[i+1]); // Copy string to array.
  hasharray[Length(s)] := $80; // Append bit.
  ml := Length(s)*8; // String length in bits.
  for i := 0 to 7 do
    hasharray[Length(hasharray)-1-i] := (ml shr (i*8)) and $ff; // Copy ml to end of array.

  Result := SHA1Data();
end;

{ Get SHA-1 of file. }
function SHA1File(fi: string): string;
var i: integer;
  ml: uint64;
begin
  { Open file and copy to array. }
  AssignFile(hashfile,fi); // Get file.
  FileMode := fmOpenRead; // Read only.
  Reset(hashfile,1);
  SetLength(hasharray,FileSize(hashfile)+9+64-((FileSize(hashfile)+9) mod 64)); // Pad data to multiple of 64.
  for i := 0 to 63 do
    hasharray[Length(hasharray)-1-i] := 0; // Clear last 64 bytes.
  BlockRead(hashfile,hasharray[0],FileSize(hashfile)); // Copy file to array.
  hasharray[FileSize(hashfile)] := $80; // Append bit.
  ml := FileSize(hashfile)*8; // File size in bits.
  for i := 0 to 7 do
    hasharray[Length(hasharray)-1-i] := (ml shr (i*8)) and $ff; // Copy ml to end of array.
  CloseFile(hashfile); // Close file.

  Result := SHA1Data();
end;

{ Get SHA-1 of data in array (from string or file). }
function SHA1Data: string;
var h0,h1,h2,h3,h4,a,b,c,d,e,f,k,t: longword;
  w: array[0..79] of longword;
  i, j: integer;
begin
  h0 := $67452301; // Initialise variables.
  h1 := $EFCDAB89;
  h2 := $98BADCFE;
  h3 := $10325476;
  h4 := $C3D2E1F0;
  for j := 0 to ((Length(hasharray) div 64)-1) do
    begin
    for i := 0 to 15 do // Copy chunk into array.
      w[i] := (hasharray[(j*64)+(i*4)] shl 24)+(hasharray[(j*64)+(i*4)+1] shl 16)+(hasharray[(j*64)+(i*4)+2] shl 8)+hasharray[(j*64)+(i*4)+3];
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

{ Get MD5 of string. }
function MD5String(s: string): string;
var i: integer;
  ml: uint64;
begin
  SetLength(hasharray,Length(s)+9+64-((Length(s)+9) mod 64)); // Pad data to multiple of 64.
  for i := 0 to 63 do
    hasharray[Length(hasharray)-1-i] := 0; // Clear last 64 bytes.
  for i := 0 to (Length(s)-1) do
    hasharray[i] := Ord(s[i+1]); // Copy string to array.
  hasharray[Length(s)] := $80; // Append bit.
  ml := Length(s)*8; // String length in bits.
  ml := swapendian64(ml); // Make it little endian.
  for i := 0 to 7 do
    hasharray[Length(hasharray)-1-i] := (ml shr (i*8)) and $ff; // Copy ml to end of array.

  Result := MD5Data();
end;

{ Get MD5 of file. }
function MD5File(fi: string): string;
var i: integer;
  ml: uint64;
begin
  { Open file and copy to array. }
  AssignFile(hashfile,fi); // Get file.
  FileMode := fmOpenRead; // Read only.
  Reset(hashfile,1);
  SetLength(hasharray,FileSize(hashfile)+9+64-((FileSize(hashfile)+9) mod 64)); // Pad data to multiple of 64.
  for i := 0 to 63 do
    hasharray[Length(hasharray)-1-i] := 0; // Clear last 64 bytes.
  BlockRead(hashfile,hasharray[0],FileSize(hashfile)); // Copy file to array.
  hasharray[FileSize(hashfile)] := $80; // Append bit.
  ml := FileSize(hashfile)*8; // File size in bits.
  ml := swapendian64(ml); // Make it little endian.
  for i := 0 to 7 do
    hasharray[Length(hasharray)-1-i] := (ml shr (i*8)) and $ff; // Copy ml to end of array.
  CloseFile(hashfile); // Close file.

  Result := MD5Data();
end;

{ Get MD5 of data in array (from string or file). }
function MD5Data: string;
var h0,h1,h2,h3,a,b,c,d,f,g: longword;
  w: array[0..15] of longword;
  i, j: integer;
begin
  h0 := $67452301; // Initialise variables.
  h1 := $EFCDAB89;
  h2 := $98BADCFE;
  h3 := $10325476;
  for j := 0 to ((Length(hasharray) div 64)-1) do
    begin
    for i := 0 to 15 do // Copy chunk into array.
      w[i] := hasharray[(j*64)+(i*4)]+(hasharray[(j*64)+(i*4)+1] shl 8)+(hasharray[(j*64)+(i*4)+2] shl 16)+(hasharray[(j*64)+(i*4)+3] shl 24);
    a := h0;
    b := h1;
    c := h2;
    d := h3;
    for i := 0 to 63 do
      begin
      if i < 16 then
        begin
        f := (b and c) or ((not b) and d);
        g := i;
        end
      else if i < 32 then
        begin
        f := (d and b) or ((not d) and c);
        g := ((5*i) + 1) mod 16;
        end
      else if i < 48 then
        begin
        f := b xor c xor d;
        g := ((3*i) + 5) mod 16;
        end
      else
        begin
        f := c xor (b or (not d));
        g := (7*i) mod 16;
        end;
      f := f + a + md5table[i] + w[g];
      a := d;
      d := c;
      c := b;
      b := b + rol(f,md5shift[i]);
      end;
    h0 := h0 + a; // Add chunk result.
    h1 := h1 + b;
    h2 := h2 + c;
    h3 := h3 + d;
    end;

  Result := AnsiLowerCase(InttoHex(swapendian(h0))+InttoHex(swapendian(h1))+InttoHex(swapendian(h2))+InttoHex(swapendian(h3)));
end;

end.