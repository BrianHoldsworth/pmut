program pmut;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}
{$IFDEF MSWINDOWS}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, Interfaces, LazLogger, IniFiles, GlobalUnit, SerialUnit,
  CustApp, Forms, DebugUnit;

var
  Settings: TIniFile;
  CurrentFilename: string;
  TopFilename: string;

  TopDir: string;
  CurrentDir: string;
  LibraryDir: string;

// Forward declare
procedure WriteHelp(err: string=''); forward;
procedure CompileProgram; forward;
procedure CompileDebug; forward;
procedure Compile; forward;
procedure CompileRecursively(Filename: string; Level: integer); forward;
procedure ComposeRAM(ProgramFlash, DownloadToRAM: boolean); forward;
procedure CompilerError(ErrorMsg: string); forward;
procedure LoadCompilerFile(Filename: string); forward;
procedure LoadFile(const Filename: string); forward;
procedure SaveTextFile(Filename: string; Start: PByteArray; Bytes: integer); forward;
procedure SaveFile(Filename: string; Start: Pointer; Bytes: integer); forward;
procedure LoadObj(Filename: string); forward;

procedure Run;
var
  ParseErr, ShortOpts: string;
  LongOpts, Nonopts, Commands: TStringArray;
  i: integer;
begin
  ShortOpts:='hdl';
  LongOpts:=['help','doc','list'];
  Commands:=['set','build','load','flash','debug'];

  // Read .ini file settings
  Settings := TIniFile.Create('.pmut_project');

  // Basic command parsing
  ParseErr := Application.CheckOptions(ShortOpts, LongOpts);
  if (Length(ParseErr) > 0) or Application.HasOption('h', 'help') then
  begin
    WriteHelp(ParseErr);
    Exit;
  end;

  Nonopts := Application.GetNonOptions(ShortOpts, LongOpts);
  if Length(NonOpts) > 2 then
  begin
    WriteHelp('Too many arguments');
    Exit;
  end
  else if Length(Nonopts) = 0 then
  begin
    WriteHelp('Too few arguments');
    Exit;
  end;

  // Check for allowed commands
  for i := 0 to Length(Commands) do
  begin
    if i = Length(Commands) then
    begin
      WriteHelp('Command not recognized');
      Exit;
    end;
    if Commands[i] = Nonopts[0] then Break;
  end;

  // Handle the 'set' command
  if i = 0 then
  begin
    WriteHelp('TODO set command');
    Exit;
  end;

  // Set Topfile
  if Length(Nonopts) = 2 then
    TopFilename := Nonopts[1]
  else
    TopFilename := Settings.ReadString('Source', 'Top', '');
  if Topfilename='' then
  begin
    WriteHelp('No topfile for source code specified');
    Exit;
  end;
  // And verify Topfilename exists
  if not FileExists(TopFilename) then
  begin
    if not TopFilename.EndsWith('.spin2') then TopFilename := TopFilename + '.spin2';
    if not FileExists(TopFilename) then
    begin
      DebugLn('Top file not found: "%s"', [TopFilename]);
      Exit;
    end;
  end;

  // init p2com interface
  P2 := P2InitStruct;
  P2.List := @ListBuffer;
  P2.ListLimit := ListLimit;
  P2.Doc := @DocBuffer;
  P2.DocLimit := DocLimit;
  P2.DebugMode := i=4;    //debug?

  Compile;
  if i=1 then Exit;       //build?
  // load/debug?
  if i<>3 then ComposeRAM(False, True)
  // flash?
  else ComposeRAM(True, True);
end;

procedure SetFilename(const NewFilename: string);
begin
  CurrentFilename := NewFilename;
end;

procedure WriteErrorFile(s: string);
var
  f: TextFile;
begin
  AssignFile(f, 'Error.txt');
  ReWrite(f);
  WriteLn(f, s);
  CloseFile(f);
end;

/////////////////////
//  Tool Routines  //
/////////////////////

procedure SetDirectories;
begin
  if TopFilename = '' then
    TopDir := '<none>'
  else
    TopDir := IncludeTrailingPathDelimiter(ExtractFileDir(TopFilename));
  CurrentDir := IncludeTrailingPathDelimiter(GetCurrentDir());
  LibraryDir := IncludeTrailingPathDelimiter(ExtractFileDir(Application.ExeName));
end;

procedure CompileProgram;
begin
  P2.DebugMode := False;
  Compile; // aborts if error
  ComposeRAM(False, True);
end;

procedure CompileDebug;
begin
  P2.DebugMode := True;
  Compile; // aborts if error
  ComposeRAM(False, True);
end;

procedure Compile;
var
  Level, i: integer;
begin
  // perform nested compilation
  SetDirectories;
  if TopFilename = '' then Level := 2 else Level := 1;
  if Level = 1 then TopFile := TopFilename else TopFile := CurrentFilename;
  P2.DownloadBaud := DefaultBaud;               // set default download baud
  PWordArray(@P2.DebugData)[0] := $200;         // reset debug data
  for i := 1 to 255 do PWordArray(@P2.DebugData)[i] := 0;
  P2.ObjStackPtr := 0;
  CompileRecursively(TopFile, Level);           // aborts if error
  DebugLn('Compilation Successful');
end;

procedure CompileRecursively(Filename: string; Level: integer);
var
  i, p, s: integer;
  ObjFiles, DatFiles: integer;
  ObjFilename, DatFilename: string;
  ObjFilenames: array[0..FileLimit-1] of string[32];
  ObjFilenamesStart: array[0..FileLimit-1] of integer;
  ObjFilenamesFinish: array[0..FileLimit-1] of integer;
  ObjTitle: PChar;
  f: file;
begin
  // update progress form
  DebugLn('Compiling '+ExtractFilename(Filename));
  // increment stack pointer and check for overflow (possible circular reference)
  Inc(P2.ObjStackPtr);
  if P2.ObjStackPtr > ObjStackLimit then
    CompilerError('Object nesting exceeds ' + IntToStr(ObjStackLimit) + ' levels - illegal circular reference may exist');
  // load source file and perform first pass of compilation
  LoadCompilerFile(Filename);
  P2Compile1;
  if P2.Error then CompilerError(P2.ErrorMsg+' @'+IntToStr(P2.SourceStart)); //aborts if error
  if P2.PasmMode and (P2.ObjStackPtr > 1) then CompilerError(Filename + ' is a PASM file and cannot be used as a Spin2 object'); // aborts if error
  ObjFiles := P2.ObjFiles;
  DatFiles := P2.DatFiles;
  // ensure presence of any sub-objects' .obj files
  if ObjFiles > 0 then
  begin
    // get sub-objects' filename data
    for i := 0 to ObjFiles-1 do
    begin
      ObjFilenames[i] := PChar(@P2.ObjFilenames[i shl 8]);
      ObjFilenamesStart[i] := P2.ObjFilenamesStart[i];
      ObjFilenamesFinish[i] := P2.ObjFilenamesFinish[i];
    end;
    // compile any sub-objects' .spin2 files or verify existence of .obj files
    for i := 0 to ObjFiles-1 do
      if (Level = 1) and FileExists(TopDir + ObjFilenames[i] + '.spin2') then CompileRecursively(TopDir + ObjFilenames[i] + '.spin2', 1)
      else if not ((Level = 1) and FileExists(TopDir + ObjFilenames[i] + '.obj')) then
        if (Level <> 3) and FileExists(CurrentDir + ObjFilenames[i] + '.spin2') then CompileRecursively(CurrentDir + ObjFilenames[i] + '.spin2', 2)
        else if not ((Level <> 3) and FileExists(CurrentDir + ObjFilenames[i] + '.obj')) then
          if FileExists(LibraryDir + ObjFilenames[i] + '.spin2') then CompileRecursively(LibraryDir + ObjFilenames[i] + '.spin2', 3)
          else if not FileExists(LibraryDir + ObjFilenames[i] + '.obj') then
          // error, neither .spin2 nor .obj file found
          begin
            LoadCompilerFile(Filename);
            P2.SourceStart := ObjFilenamesStart[i];
            P2.SourceFinish := ObjFilenamesFinish[i];
            CompilerError('Cannot find .spin2 or .obj file for ' + ObjFilenames[i]);
          end;
  end;
  // reload source file and reperform first pass of compilation
  LoadCompilerFile(Filename);
  P2Compile1;
  if P2.Error then CompilerError(P2.ErrorMsg+' @'+IntToStr(P2.SourceStart)); //aborts if error
  // load sub-objects' .obj files
  p := 0;
  if ObjFiles > 0 then
    for i := 0 to ObjFiles-1 do
    begin
      ObjFilename := ObjFilenames[i] + '.obj';
      if (Level = 1) and FileExists(TopDir + ObjFilename) then ObjFilename := TopDir + ObjFilename
      else if (Level <> 3) and FileExists(CurrentDir + ObjFilename) then ObjFilename := CurrentDir + ObjFilename
      else ObjFilename := LibraryDir + ObjFilename;
      AssignFile(f, ObjFilename);
      try
        try
          Reset(f, 1);
          s := FileSize(f);
          if p + s > ObjLimit then CompilerError('OBJ files exceed ' + IntToStr(ObjLimit div 1024) + 'k limit');
          BlockRead(f, P2.ObjData[p], s);
          P2.ObjOffsets[i] := p;
          P2.ObjLengths[i] := s;
          p := p + s;
        except
          CompilerError('Failure reading file ' + ObjFilename); //aborts if error
        end;
      finally
        CloseFile(f);
      end;
    end;
  // load any data files
  p := 0;
  if DatFiles > 0 then
    for i := 0 to DatFiles-1 do
    begin
      DatFilename := PChar(@P2.DatFilenames[i shl 8]);
      if (Level = 1) and FileExists(TopDir + DatFilename) then DatFilename := TopDir + DatFilename
      else if (Level <> 3) and FileExists(CurrentDir + DatFilename) then DatFilename := CurrentDir + DatFilename
      else DatFilename := LibraryDir + DatFilename;
      AssignFile(f, DatFilename);
      DebugLn('Load DAT file ' + DatFilename);
      try
        try
          Reset(f, 1);
          s := FileSize(f);
          if p + s > ObjLimit then CompilerError('DAT files exceed ' + IntToStr(ObjLimit div 1024) + 'k limit');
          BlockRead(f, P2.DatData[p], s);
          P2.DatOffsets[i] := p;
          P2.DatLengths[i] := s;
          p := p + s;
        except
          CompilerError('Failure reading file ' + DatFilename); //aborts if error
        end;
      finally
        CloseFile(f);
      end;
    end;
  // perform second pass of compilation
  ObjTitle := @(P2.ObjTitle);
  StrCopy(ObjTitle, PChar(ExtractFilename(Filename)));
  P2Compile2;
  if P2.Error then CompilerError(P2.ErrorMsg); //aborts if error
  // save obj file
  SaveFile(ExtFilename(CurrentFilename, 'obj'), @(P2.Obj), P2.ObjLength);
  // save documentation and listing files
  if Application.HasOption('d','doc') then
    SaveTextFile(ExtFilename(CurrentFilename, 'txt'), P2.Doc, P2.DocLength);
  if Application.HasOption('l','list') then
    SaveTextFile(ExtFilename(CurrentFilename, 'lst'), P2.List, P2.ListLength);
  // decrement stack pointer
  Dec(P2.ObjStackPtr);
end;

procedure CompilerError(ErrorMsg: string);
begin
  DebugLn(ErrorMsg + '.');
  Abort;
end;

procedure LoadCompilerFile(Filename: string);
begin
  LoadFile(Filename);
  P2.Source := @SourceBuffer;
end;

procedure LoadFile(const Filename: string);
var
  f: TStringList;
begin
  f := TStringList.Create;
  f.DefaultEncoding := TEncoding.ANSI;
  f.LoadFromFile(Filename, TEncoding.UTF8);
  if f.Encoding=TEncoding.UTF8 then
  begin
       f.Text := Utf8ToAnsi(f.Text);
  end;
  f.TextLineBreakStyle := tlbsCR;
  StrPCopy(@SourceBuffer, f.Text);
  SetFilename(Filename);
  f.Free;
end;

procedure SaveTextFile(Filename: string; Start: PByteArray; Bytes: integer);
var
  t: TStringList;
  i: integer;
  s: string;
begin
  t := TStringList.Create;
  t.DefaultEncoding := TEncoding.ANSI;
  s := '';
  for i:=0 to Bytes-1 do
  begin
    if Start[i]=13 then
    begin
      t.Add(s);
      s := '';
      Continue;
    end;
    s := s + AnsiChar(Start[i]);
  end;
  t.Add(s);
  t.Text := AnsiToUtf8(t.Text);
  t.SaveToFile(Filename, TEncoding.UTF8);
end;

procedure SaveFile(Filename: string; Start: Pointer; Bytes: integer);
var
  f: file;
begin
  AssignFile(f, Filename);
  try
    Rewrite(f, 1);
    BlockWrite(f, Start^, Bytes);
  finally
    CloseFile(f);
  end;
end;

procedure ComposeRAM(ProgramFlash, DownloadToRAM: boolean);
var
  s: integer;
  comport: string;
begin
  // insert interpreter?
  if not P2.PasmMode then
  begin
    P2InsertInterpreter;
    if P2.Error then CompilerError(P2.ErrorMsg);  //aborts if error
  end;
  // check to make sure program fits into hub
  s := P2.SizeObj;
  if P2.DebugMode then s := s + $4000;  // account for debugger
  if not P2.PasmMode then s := s + P2.SizeInterpreter + P2.SizeVar + $400;
  if s > HubLimit then
    CompilerError('Program requirement exceeds ' + IntToStr(HubLimit div 1024)
      + 'KB hub RAM by ' + IntToStr(s - HubLimit) + ' bytes');  //aborts if error
  // save .bin file
  SaveFile(ExtFilename(CurrentFilename, 'bin'), @(P2.Obj), P2.ObjLength);
  // insert debugger?
  if P2.DebugMode then
  begin
    P2InsertDebugger;
    if P2.Error then CompilerError(P2.ErrorMsg);  //aborts if error
  end;
  // insert clock setter?
  if not P2.DebugMode and P2.PasmMode and (P2.ClkMode <> 0) then
  begin
    P2InsertClockSetter;
    if P2.Error then CompilerError(P2.ErrorMsg);  //aborts if error
  end;
  // insert flash loader?
  if ProgramFlash then
  begin
    DebugLn('Loading binary to P2 Flash');
    if (P2.SizeFlashLoader + P2.SizeObj) > HubLimit then
      CompilerError('Need to reduce program by ' + IntToStr(P2.SizeFlashLoader
        + P2.SizeObj - HubLimit) + ' bytes, in order to fit flash loader into hub RAM download');
    P2InsertFlashLoader;
    if P2.Error then CompilerError(P2.ErrorMsg);  //aborts if error
  end;
  // download to RAM?
  if DownloadToRAM then
  begin
    DebugLn('Load binary...');
    comport := Settings.ReadString('Comm', 'Port', '');
    if comport <> '' then
       LoadHardware(comport)
    else DebugLn('Comm device not set. Try using "pmut set port"');
  end;
end;

procedure LoadObj(Filename: string);
var
  f: file;
  Size, i: Integer;
begin
  AssignFile(f, Filename);
  try
    Reset(f, 1);
    Size := Smaller(FileSize(f), ObjLimit);
    BlockRead(f, P2.Obj, Size);
    if Size < ObjLimit then for i := Size to ObjLimit-1 do P2.Obj[i] := 0;
  finally
    CloseFile(f);
  end;
end;

procedure WriteHelp(err: string='');
begin
  if Length(err) > 0 then WriteLn(err);
  Writeln('Usage: pmut [-d,--doc] [-l,--list] [-h,--help] command [topfile]');
  Writeln('where command is one of "build","load","flash", "debug", or "set"');
  Writeln('Use "pmut set" to modify/list the project settings (in ".pmut_project").');
  Writeln('Name of source code "topfile" is taken from project settings,');
  Writeln('if not included in the command line.');
end;

begin
  Application.Scaled:=True;
  Application.Initialize;
  Run;
  if (P2<>nil) and P2.DebugMode and (P2.DebugWindowsOff = 0) then
  begin
    Application.CreateForm(TDebugForm, DebugForm);
    Application.Run;
    FreeAndNil(DebugForm);
  end;
  CloseComm;
end.

