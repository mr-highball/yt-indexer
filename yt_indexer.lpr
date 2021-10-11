{$mode delphi}
program yt_indexer;
uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils,
  yt.indexer,
  ezjson,
  yt.console;

var
  LIndexer : TYTIndexer;
  LJSON, LError : String;
begin
  WriteLn('youtube indexer - STARTED');
  WriteLn('-----');
  WriteLn('press any key to stop indexing...');
  LIndexer := TYTIndexer.Create(nil);
  try
    LIndexer.OnInfo := WriteInfo;
    LIndexer.OnError := WriteError;

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

    //start indexing videos
    LIndexer.Run;

    if LIndexer.Running then
      WriteLn('indexing videos, press any key to stop...')
    else
      WriteLn('unable to index video, see log. press any key to exit...');

    //wait for user
    ReadLn;

    WriteLn('stopping...');
    LIndexer.Stop;
    while LIndexer.Running do
      Sleep(50);

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
    WriteLn('youtube indexer - STOPPED');
    WriteLn('press any key to exit...');
    WriteLn('-----');
    ReadLn;
  finally
    LIndexer.Free;
  end;
end.

