unit DependencyHandler;

{$mode Delphi}

interface

uses
{$ifdef LINUX}
  baseunix, linux,
{$endif}
  Crt, LazLoggerBase, Classes, SysUtils;

type
  TCallbackProc = procedure of object;
  TSrcFile = record
    fname: string;
    watch_fd: integer;
  end;

  TDependencyHandler = class
    private
      Srcs: array of TSrcFile;
      infd: integer;
      FNotify: TCallbackProc;
      FModified: boolean;

      procedure SetNotify(cb: TCallbackProc);
      procedure SetModify(is_mod: boolean);
      procedure AddDepends(fname: string);
    public
      constructor Create;
      destructor Destroy; override;
      function Exists(filename: string): boolean;
      function Always(filename: string): boolean;
      property Notify: TCallbackProc write SetNotify;
      property Modified: boolean read FModified write SetModify;
      procedure Watch;
  end;

const
{$ifdef LINUX}
  // holds size of largest expected inotify_event size
  maxExpEventSize = sizeof(inotify_event) + name_max;
{$else}
  maxExpEventSize = 500;    //TODO for Windows?
{$endif}

implementation
{ TDependencyHandler }

constructor TDependencyHandler.Create;
begin
  Srcs := [];
  FModified := False;
end;

destructor TDependencyHandler.Destroy;
var
  i: integer;
begin
{$ifdef LINUX}
  for i := 0 to Length(Srcs)-1 do
    if Srcs[i].watch_fd <> -1 then
      inotify_rm_watch(infd, Srcs[i].watch_fd);
  fpClose(infd);
{$endif}
  inherited Destroy;
end;

procedure TDependencyHandler.SetNotify(cb: TCallbackProc);
var
  i: integer;
  inwd: integer;
begin
{$ifdef LINUX}
  infd := inotify_init1(in_nonblock);
  if infd < 0 then DebugLn('Could not init file notifications. File watching disabled.');
  // add a watch for each source file dependency
  for i := 0 to Length(Srcs)-1 do
  begin
    if Srcs[i].watch_fd <> -1 then Continue;
    inwd := inotify_add_watch(infd, PChar(Srcs[i].fname), in_modify);
    if inwd < 0 then
      DebugLn('Counld not set watcher on %s', [Srcs[i].fname])
    else begin
      Srcs[i].watch_fd:=inwd;
      DebugLn('Watching %s wd=%d', [Srcs[i].fname,inwd])
    end;
  end;
{$endif}
  FNotify := cb;
end;

{$ifdef LINUX}
procedure TDependencyHandler.Watch;
var
  inEventQueueBuffer: PChar;
  bytesReadCount, bytesProcessedCount: TsSize;
  inEvent: PInotify_event;
  poll: pollfd;
begin
  // poll for new inotify event
  poll.fd:=infd;
  poll.events:=POLLIN;
  poll.revents:=0;
  if FpPoll(@poll, 1, 0)<=0 then Exit;

  // read() returns the amount of bytes have been read.
  // read() reads at most maxExpEventSize bytes to the memory position
  // specified by inEventQueueBuffer.
  inEventQueueBuffer := getMem(maxExpEventSize);
  bytesReadCount := fpRead(infd, inEventQueueBuffer, maxExpEventSize);
  if bytesReadCount < 0 then Exit;

  // as there may be multiple inotify_events saved at inEventQueueBuffer
  // we have to keep a counter how much has been processed
  bytesProcessedCount := 0;

  // reset inEvent
  // typecast PChar to use it as a PInotify_event as such allows the
  // usage of dot-fieldname-designators
  inEvent := PInotify_event(inEventQueueBuffer);

  while bytesProcessedCount < bytesReadCount do
  begin
    with inEvent^ do
      if (mask and IN_MODIFY) > 0 then
      begin
        TextColor(Yellow);
        DebugLn('Source is modified. Rebuilding.');
        TextColor(LightGray);
        FModified := True;
      end;
    inc(bytesProcessedCount, sizeof(inotify_event)-1 + inEvent^.len);
    inEvent := PInotify_event(@inEventQueueBuffer[sizeof(inotify_event)-1 +
    	inEvent^.len]);
  end;
  // Cleanup
  freeMem(inEventQueueBuffer, maxExpEventSize);
end;
{$else}
procedure TDependencyHandler.Watch;
begin
  //TODO for Windows?
end;
{$endif}

procedure TDependencyHandler.SetModify(is_mod: boolean);
begin
  if not is_mod then FNotify;
  FModified := is_mod;
end;

procedure TDependencyHandler.AddDepends(fname: string);
var
  i: integer;
  srcfile: TSrcFile;
begin
  for i := 0 to Length(Srcs)-1 do
    if Srcs[i].fname = fname then Exit;
  setLength(Srcs, Length(Srcs)+1);
  srcfile.fname := fname;
  srcfile.watch_fd := -1;
  Srcs[Length(Srcs)-1] := srcfile;
end;

function TDependencyHandler.Exists(filename: string): boolean;
var
  objname: string;
  age: integer;
begin
  if filename.EndsWith('.spin2', True) then
  begin
    objname := StringReplace(filename, '.spin2', '.obj', [rfIgnoreCase]);
    age := FileAge(objname);
    if (age <> -1) and (age > FileAge(filename)) then
    begin
      if fileExists(filename) then AddDepends(filename);
      Result := False;
      Exit;
    end;
  end;
  Result := FileExists(filename);
  if Result then AddDepends(filename);
end;

function TDependencyHandler.Always(filename: string): boolean;
begin
  Result := FileExists(filename);
  if Result then AddDepends(filename);
end;

end.

