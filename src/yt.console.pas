unit yt.console;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  syncobjs;

type

  //this was put in when trying to work with object methods for tthread, but not needed now. leaving in case it's useful later
  TSyncBoi = class
  private
    procedure InternalWriteInfo();
    procedure InternalWriteError();
  end;

procedure WriteInfo(const AMessage : String);
procedure WriteError(const AMessage : String);

implementation
uses
  Generics.Collections;
var
  CRITICAL : TCriticalSection;

  //see above "not needed now" note
  INFO_QUEUE,
  ERROR_QUEUE : TQueue<String>;
  GOOD_BOI : TSyncBoi;

procedure TSyncBoi.InternalWriteInfo();
var
  LMsg : String;
begin
  CRITICAL.Enter;
  try
    INFO_QUEUE.Dequeue;
  finally
    CRITICAL.Leave;
  end;

  WriteLn('[INFO]-', LMsg);
end;

procedure TSyncBoi.InternalWriteError();
var
  LMsg : String;
begin
  CRITICAL.Enter;
  try
    INFO_QUEUE.Dequeue;
    WriteLn('[ERROR]-', LMsg);
  finally
    CRITICAL.Leave;
  end;
end;

procedure WriteInfo(const AMessage: String);
begin
  CRITICAL.Enter;
  try
    WriteLn('[INFO]-', AMessage);
  finally
    CRITICAL.Leave;
  end;
end;

procedure WriteError(const AMessage: String);
begin
  CRITICAL.Enter;
  try
    WriteLn('[ERROR]-', AMessage);
  finally
    CRITICAL.Leave;
  end;
end;

initialization
  INFO_QUEUE := TQueue<String>.Create;
  ERROR_QUEUE := TQueue<String>.Create;
  CRITICAL := TCriticalSection.Create;
  GOOD_BOI := TSyncBoi.Create;
finalization
  INFO_QUEUE.Free;
  ERROR_QUEUE.Free;
  CRITICAL.Free;
  GOOD_BOI.Free;
end.

