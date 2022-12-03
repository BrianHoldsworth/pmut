unit SerialUnit;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  LazLoggerBase, LCLIntf, LCLType, LMessages, SysUtils, Classes, LazSynaSer,
  Forms;

type
  TSerialThread = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create; reintroduce;
  end;

  procedure LoadHardware(port: string);
  procedure GetHardwareVersion;
  procedure FindHardware;
  function  HardwareFound: boolean;
  procedure ResetHardware;

  procedure StartDebug;
  procedure OperateDebug;
  procedure ReturnRByte;

  procedure OpenComm(baud: integer);
  procedure TString(s: string);
  procedure THex(x: byte);
  procedure TBase64(x: byte);
  procedure TLong(x: integer);
  procedure TByte(x: byte);
  function  RLong: cardinal;
  function  RWord: word;
  function  RByte: byte;
  procedure CloseComm;
  procedure CommError(ErrorMsg: string);
  procedure SerialThreadStart;
  procedure SerialThreadStop;
  procedure PumpTx;
  procedure PumpRx;

const
  DefaultBaud        = 2000000;
  TxBuffSize         = $200000;         // 2 MB, must be power-of-2
  TxBuffMask         = TxBuffSize - 1;
  RxBuffSize         = $1000000;        // 16 MB, must be power-of-2
  RxBuffMask         = RxBuffSize - 1;

var
  CommOpen           : boolean;
  CommString         : string;
  Serial             : TBlockSerial;
  TxBuff             : array[0..TxBuffSize - 1] of byte;
  TxHead             : integer;
  TxTail             : integer;

  RxBuff             : array[0..RxBuffSize - 1] of byte;
  RxHead             : integer;
  RxTail             : integer;

  Version            : byte;
  VersionMode        : boolean;
  AbortMode          : boolean;

  TimeBase           : cardinal;

  SerialThreadError  : boolean;
  SerialThreadString : string;
  SerialThreadActive : boolean;
  SerialThread       : TSerialThread;

implementation

uses GlobalUnit, DebugUnit;


/////////////////////////
//  Hardware Routines  //
/////////////////////////

// Load Hardware
procedure LoadHardware(port: string);
var
  LoadLimit, i, m, n: integer;
begin
  // find hardware
  VersionMode := False;
  CommString := port;
  if not HardwareFound then CommError('P2 hardware not found');
  // check version
  if (Version < Byte('A')) or (Version > Byte('G')) then
    CommError('Invalid Hardware Version ' + IntToStr(Version));
  // update progress
  DebugLn('Load RAM of P2 on '+CommString);
  // cap load sizes by Version
  if Version = Byte('A') then LoadLimit := Smaller(P2.ObjLength, $100000);
  if Version = Byte('B') then LoadLimit := Smaller(P2.ObjLength, $040000);
  if Version = Byte('C') then LoadLimit := Smaller(P2.ObjLength, $008000);
  if Version = Byte('D') then LoadLimit := Smaller(P2.ObjLength, $020000);
  if Version = Byte('E') then LoadLimit := Smaller(P2.ObjLength, $080000);
  if Version = Byte('F') then LoadLimit := Smaller(P2.ObjLength, $100000);
  if Version = Byte('G') then LoadLimit := Smaller(P2.ObjLength, $100000);
  // send bytes in Base64 format
  TString('Prop_Txt 0 0 0 0 ');
  n := 0; m := 0;
  for i := 0 to LoadLimit - 1 do
  begin
    m := (m shl 8) + P2.Obj[i];
    n := n + 8;
    if n >= 6 then
    begin
      TBase64((m shr (n - 6)) and $3F);
      n := n - 6;
    end;
    if n >= 6 then
    begin
      TBase64((m shr (n - 6)) and $3F);
      n := n - 6;
    end;
  end;
  if n > 0 then TBase64((m shl (6 - n)) and $3F);
  TString('~');
end;

// Get hardware version
procedure GetHardwareVersion;
var
  s: string;
begin
  VersionMode := True;
  FindHardware;
  CloseComm;
  s := 'Unknown.';
  if Version = Byte('A') then s := 'FPGA - 8 cogs, 512KB hub, 48 smart pins 63..56, 39..0, 80MHz';
  if Version = Byte('B') then s := 'FPGA - 4 cogs, 256KB hub, 12 smart pins 63..60/7..0, 80MHz';
  if Version = Byte('C') then s := 'unsupported'; // 1 cog, 32KB hub, 8 smart pins 63..62/5..0, 80MHz, No CORDIC';
  if Version = Byte('D') then s := 'unsupported'; // 1 cog, 128KB hub, 7 smart pins 63..62/4..0, 80MHz, No CORDIC';
  if Version = Byte('E') then s := 'FPGA - 4 cogs, 512KB hub, 18 smart pins 63..62/15..0, 80MHz';
  if Version = Byte('F') then s := 'unsupported'; // 16 cogs, 1024KB hub, 7 smart pins 63..62/33..32/2..0, 80MHz';
  if Version = Byte('G') then s := 'P2X8C4M64P Rev B/C - 8 cogs, 512KB hub, 64 smart pins';
  DebugLn('Propeller2 found on %s\n%s', [CommString, s]);
end;

{$ifdef MSWINDOWS}
// Check for hardware on current comm port, then on com9..com1
procedure FindHardware;
var
  i: integer; Prefix: string;
begin
  CloseComm;
  // check com9..com1 for hardware
  Prefix := 'COM';
  for i := 9 downto 0 do
  begin
    CommString := Prefix + IntToStr(i);
    begin
      DebugLn('Checking ' + CommString);
      if HardwareFound then Exit;
    end;
  end;
  CommError('No hardware found for COM ports');
end;
{$else}
// Check for hardware on current port, then /dev/ttyUSB0../dev/ttyUSB9,
// then /dev/ttyS0../dev/ttyS9
procedure FindHardware;
var
  i: integer; Prefix: string;
begin
  CloseComm;
  // check ttyUSB0..ttyUSB9 for hardware
  Prefix := '/dev/ttyUSB';
  for i := 0 to 9 do
  begin
    CommString := Prefix + IntToStr(i);
    begin
      DebugLn('Checking ' + CommString);
      if HardwareFound then Exit;
    end;
  end;
  // check ttyS0..ttyS9 for hardware
  Prefix := '/dev/ttyS';
  for i := 0 to 9 do
  begin
    CommString := Prefix + IntToStr(i);
    begin
      DebugLn('Checking ' + CommString);
      if HardwareFound then Exit;
    end;
  end;
  CommError('No hardware found for standard tty devices');
end;
{$endif}

// Check for hardware on current comm port
function HardwareFound: boolean;
var
  i: integer; s: string;
begin
  try
    // in case error, result is false
    Result := False;
    // disallow abort
    AbortMode := False;
    // check hardware
    OpenComm(P2.DownloadBaud);
    ResetHardware;
    TString('> Prop_Chk 0 0 0 0 ');
    // receive version string
    s := '';
    for i := 1 to 14 do s := s + Chr(RByte);
    Version := Byte(s[12]);
    s[12] := 'X';
    if s <> String(Chr(13) + Chr(10) + 'Prop_Ver X' + Chr(13) + Chr(10)) then
      CommError('Hardware lost');
    // if find hardware mode, send shutdown command and reset hardware to reboot
    if VersionMode then
    begin
      ResetHardware;
      CloseComm;
    end;
    // allow abort
    AbortMode := True;
    // connected, result is true
    Result := True;
  except
    // error, result is false, allow abort
    AbortMode := True;
  end;
end;

// Reset hardware
procedure ResetHardware;
begin
  // stop serial thread
  SerialThreadStop;
  // generate P2 reset pulse via DTR
  Sleep(1);
  Serial.DTR := True;
  Sleep(1);
  Serial.DTR := False;
  // allow time for P2 ROM loader to start
  Sleep(15);
  // restart serial thread
  SerialThreadStart;
end;


//////////////////////
//  DEBUG Receiver  //
//////////////////////

// Start DEBUG
procedure StartDebug;
begin
  AbortMode := True;
  if P2.DebugBaud <> P2.DownloadBaud then
     OpenComm(P2.DebugBaud);
  OperateDebug;
end;

// Operate DEBUG
procedure OperateDebug;
var
  i: integer;
  b: byte;
begin
  DebugActive := True;
  DebugForm.ResetDisplays;
  while DebugActive do
  begin
    for i := 1 to 100 do
    begin
      if SerialThreadError then CommError(SerialThreadString);
      if RxHead <> RxTail then
      begin
        b := RxBuff[RxTail];
        RxTail := (RxTail + 1) and RxBuffMask;
        DebugForm.ChrIn(b);
      end
      else Break;
    end;
    Application.ProcessMessages;        // process messages when no byte ready or after 100 consecutive bytes
  end;
  CloseComm;
end;

// Back up received byte
procedure ReturnRByte;
begin
  RxTail := (RxTail - 1) and RxBuffMask;
end;


/////////////////////
//  Comm Routines  //
/////////////////////

// Open comm port
procedure OpenComm(baud: integer);
begin
  CloseComm;
  Serial := TBlockSerial.Create;
  Serial.Connect(CommString);
  Serial.Config(baud, 8, 'N', 1, False, False);
  if Serial.LastError <> 0 then
  begin
    CommError(Serial.LastErrorDesc);
    Exit;
  end;
  SerialThreadStart;
  CommOpen := True;
end;

// Transmit string
procedure TString(s: string);
var
  i: integer;
begin
  for i := 1 to Length(s) do
    TByte(Byte(s[i]));
end;

// Transmit hex byte
procedure THex(x: byte);
const
  HexChr: array [0..15] of Char = '0123456789ABCDEF';
begin
  TByte(Byte(HexChr[x shr 4 and $F]));
  TByte(Byte(HexChr[x and $F]));
  TByte($20);
end;

// Transmit base64 character
procedure TBase64(x: byte);
const
  Base64Chr: array [0..63] of Char = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';
begin
  TByte(Byte(Base64Chr[x]));
end;

// Transmit long
procedure TLong(x: integer);
var
  i: integer;
begin
  for i := 0 to 3 do TByte(x shr (i shl 3));
end;

// Transmit byte
procedure TByte(x: byte);
begin
  TxBuff[TxHead] := x;
  TxHead := (TxHead + 1) and TxBuffMask;
end;

// Receive long
function RLong: cardinal;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to 3 do Result := Result shr 8 + RByte shl 24;
end;

// Receive word
function RWord: word;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to 1 do Result := Result shr 8 + RByte shl 8;
end;

// Receive byte
function RByte: byte;
var
  Ticks: cardinal;
begin
  Ticks := GetTickCount64;
  repeat
    if SerialThreadError then CommError(SerialThreadString);
    if RxHead <> RxTail then
    begin
      Result := RxBuff[RxTail];
      RxTail := (RxTail + 1) and RxBuffMask;
      Exit;
    end;
  until GetTickCount64 - Ticks > 500;
  CommError('Hardware lost');
end;

// Close comm port
procedure CloseComm;
begin
  // close down debug in case active
  if (DebugForm<>nil) then
  begin
    DebugForm.CloseDisplays;
    DebugForm.CloseLogFile;
  end;
  DebugActive := False;
  // if port open, close it
  if CommOpen then
  begin
    SerialThreadStop;
    Serial.CloseSocket;
    Serial.Destroy;
    CommOpen := False;
  end;
end;

// Comm error
procedure CommError(ErrorMsg: string);
begin
  CloseComm;
  DebugLn(ErrorMsg+' (%s)', [CommString]);
  Abort;
end;

/////////////////////
//  Serial Thread  //
/////////////////////

procedure SerialThreadStart;
begin
  // init variables
  TxHead := 0;
  TxTail := 0;
  RxHead := 0;
  RxTail := 0;
  SerialThreadError := False;
  SerialThreadString := '';
  SerialThreadActive := True;
  // read any residual Rx data to purge deep buffers
  while Serial.RecvBufferEx(@RxBuff, RxBuffSize, 1) > 0 do
    DebugLn('Emptying Recv Buffer');
  // start thread
  SerialThread := TSerialThread.Create;
end;

procedure SerialThreadStop;
begin
  // allow Tx buffer to finish transmitting
  while not SerialThreadError and (TxHead <> TxTail) do;
  // deactivate thread
  SerialThreadActive := False;
  // wait for confirmation
  repeat until SerialThreadString <> '';
  SerialThread.Terminate;
end;

constructor TSerialThread.Create;
begin
  inherited Create(False);
  FreeOnTerminate := True;
  Priority := tpTimeCritical;
end;

procedure TSerialThread.Execute;
begin
  // keep pumping while active and no error
  while SerialThreadActive and not SerialThreadError do
  begin
    // pump data
    PumpTx;
    PumpRx;
    // yield time to the GUI thread
    Yield;
  end;
  // terminating, confirm string is not empty
  if not SerialThreadError then
    SerialThreadString := 'OK';
end;

procedure PumpTx;
var
  x: integer;
begin
  // Exit?
  if (TxHead = TxTail) or SerialThreadError then Exit;
  // Transmit any data
  if TxHead < TxTail then x := TxBuffSize else x := TxHead;
  if Serial.SendBuffer(@TxBuff[TxTail], x - TxTail) = (x-TxTail) then
    TxTail := x and TxBuffMask
  else
  begin
    SerialThreadError := True;
    SerialThreadString := 'Unable to write';
  end;
end;

procedure PumpRx;
var
  x: integer;
begin
  // Exit?
  if SerialThreadError then Exit;
  // Receive any data
  x := Serial.RecvBufferEx(@RxBuff[RxHead], RxBuffSize-RxHead, 1);
  if x > 0 then
    RxHead := (RxHead + x) and RxBuffMask
  else if Serial.LastError <> ErrTimeout then
  begin
    SerialThreadError := True;
    SerialThreadString := 'Unable to read from';
  end;
end;

end.

