unit FastBitmap;

{$mode Delphi}

interface

uses
  Types, Graphics;

type
  TFastBitmap = class
  private
    FPixelsData: PByte;
    FSize: TSize;
    BytesPerPixel: integer;
    procedure SetSize(AValue: TSize);
  public
    constructor Create(pixelSize: integer);
    destructor Destroy; override;
    property Size: TSize read FSize write SetSize;
    function GetPixelAddr(X, Y: integer): PByte; inline;
    procedure SetPixel(X, Y, rgb24: Integer);
    procedure Copy(dest: TBitmap);
    procedure CopyFrom(src: TBitmap);
  end;

implementation

{ TFastBitmap }

procedure TFastBitmap.SetSize(AValue: TSize);
begin
  if (FSize.cx = AValue.cx) and (FSize.cy = AValue.cy) then Exit;
  FSize := AValue;
  FPixelsData := ReAllocMem(FPixelsData, FSize.cx * FSize.cy * BytesPerPixel);
end;

constructor TFastBitmap.Create(pixelSize: integer);
begin
  Size := TSize.Create(0, 0);
  BytesPerPixel := pixelSize;
end;

destructor TFastBitmap.Destroy;
begin
  FreeMem(FPixelsData);
  inherited Destroy;
end;

function TFastBitmap.GetPixelAddr(X, Y: Integer): PByte;
begin
  Result := FPixelsData + Y * FSize.cx + X * BytesPerPixel;
end;

procedure TFastBitmap.SetPixel(X, Y, rgb24: Integer);
var
  p: PByte;
begin
  p := GetPixelAddr(X, Y);
  p^ := rgb24 shr 0; Inc(p);
  p^ := rgb24 shr 8; Inc(p);
  p^ := rgb24 shr 16;
end;

procedure TFastBitmap.Copy(dest: TBitmap);
var
  X, Y, pix: integer;
  sh: boolean;
  p: PByte;
begin
  sh := dest.RawImage.Description.BitsPerPixel > 24;
  try
    dest.BeginUpdate(True);
    for Y := 0 to FSize.cy - 1 do
      for X := 0 to FSize.cx - 1 do
        begin
          p := GetPixelAddr(X, Y);
          pix := p[2] shl 16 + p[1] shl 8 + p[0];
          //if sh then pix := pix shl 8;
          dest.Canvas.Pixels[X, Y] := pix;
        end;
  finally
    dest.EndUpdate(False);
  end;
end;

procedure TFastBitmap.CopyFrom(src: TBitmap);
var
  X, Y, pix: integer;
  sh: boolean;
  p: PInteger;
begin
  sh := src.RawImage.Description.BitsPerPixel > 24;
  for Y := 0 to FSize.cy - 1 do
    for X := 0 to FSize.cx - 1 do
      begin
        p := PInteger(GetPixelAddr(X, Y));
        pix := src.Canvas.Pixels[X, Y];
        //if sh then pix := pix shr 8;
        p^ := pix;
      end;
end;

end.

