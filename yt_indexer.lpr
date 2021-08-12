{$mode delphi}
program yt_indexer;
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  yt.indexer,
  ezjson;

var
  LIndexer : TYTIndexer;
  LJSON, LError : String;
begin
  LIndexer := TYTIndexer.Create;

  //load settings when we have them
  if FileExists('appsettings.json') then
    with TStringStream.Create do
    begin
      try
        LoadFromFile('appsettings.json');
        LJSON := DataString;

        if not (EZDeserialize<TYTIndexer>(LJSON, LIndexer, LError)) then
          WriteLn('unable to load appsettings.json [' + LError + ']');
      finally
        Free;
      end;
    end;

  //todo... index videos

  //once finished save to the settings file
  if not (EZSerialize<TYTIndexer>(LIndexer, LJSON, LError)) then
    WriteLn('unable to serialize indexer [' + LError + ']');

  with TStringStream.Create(LJSON) do
  begin
    try
      SaveToFile('appsettings.json');
    finally
      Free;
    end;
  end;
  ReadLn;
end.

