unit DirectBitmap;

{$mode Delphi}

interface

uses
  LazLoggerBase, Graphics;

type
  TDirectBitmap = class
  private
    BytesPerPixel: Integer;
    BitmapLine: array [0..2047] of Pointer;
    function Addr(X, Y: integer): PByte; inline;
  public
    constructor Create(bitmap: TBitmap);
    function GetPixel(X, Y: Integer): Integer;
  end;

implementation

{ TDirectBitmap }

constructor TDirectBitmap.Create(bitmap: TBitmap);
  var i: integer;
begin
  BytesPerPixel := bitmap.RawImage.Description.BitsPerPixel div 8;
  //DebugLn('DirectBitmap %d x %d with %d bytes/pixel',[bitmap.Width,bitmap.Height,BytesPerPixel]);
  for i := 0 to bitmap.Height - 1 do BitmapLine[i] := bitmap.ScanLine[i];
end;
  
function TDirectBitmap.Addr(X, Y: Integer): PByte;
begin
  Result := BitmapLine[Y] + X * BytesPerPixel;
end;

function TDirectBitmap.GetPixel(X, Y: Integer): Integer;
var
  p: PByte;
begin
  p := Addr(X, Y);
  Result := p[0] shl 16 + p[1] shl 8 + p[2];
end;

end.
