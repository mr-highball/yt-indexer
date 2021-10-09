unit yt.indexer;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fphttpwebclient,
  fpoauth2,
  fpwebclient,
  jsonparser,
  googlebase,
  googleservice,
  googleclient,
  googleyoutube,
  opensslsockets,
  ezjson,
  ezthreads,
  ezthreads.pool,
  syncobjs,
  Generics.Collections,
  SQLite3Conn,
  SQLDB;

type

  { TYTIndexer }

  [JsonObject('yt_indexer')]
  TYTIndexer = class(TComponent)
  strict protected
    type
      TWorkEntry = TPair<TDateTime, String>;
      TWorkQueue = TQueue<TWorkEntry>;
  strict private
    FDaily: Integer;
    FKeyCSV: String;
    FKeys: TStringArray;
    FSpent: Integer;
    FDBName: String;
    FQuotaCost: Integer;
    FKey : String;
    FError : String;
    FPool : IEZThreadPool;
    FStopRequest : Boolean;
    FCritical : TCriticalSection;
    FDBCritical : TCriticalSection;
    FQueue : TWorkQueue;
    FRunning : Boolean;
    FConnection : TSQLite3Connection;
    function GetError: String;
    function GetKey: String;
    function GetRemaining: Integer;
    function GetRunning: Boolean;
    procedure SetKey(AValue: String);
    procedure SetKeyCSV(AValue: String);
    procedure SetRemaining(AValue: Integer);
    procedure Initialize(const AThread : IEZThread);
    procedure FetchResults(const AThread : IEZThread);
    procedure InitializeDB;
  strict protected
    function RandomizeKeywords : TStringArray;
    function ExecuteSQL(const ASQL: String; out Error: String): Boolean; //if we need to can rip our old work off to return json https://github.com/mr-highball/dcl-hackathon-2019/blob/master/services/common/controller.base.pas#L466
    procedure LogError(const AError : String);
    procedure SaveQuery(const AResponse : TSearchListResponse);
    class function DateTimeDiffMS(const ANow, AThen : TDateTime) : Int64; static;
  public
    procedure Run;
    procedure Stop;

    property Running : Boolean read GetRunning;
    property Error : String read GetError;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    [JsonProperty('db_file_name')]
    property DBFileName : String read FDBName write FDBName;

    [JsonProperty('quota_cost')]
    property QuotaCost : Integer read FQuotaCost write FQuotaCost;

    [JsonProperty('daily_quota')]
    property DailyQuota : Integer read FDaily write FDaily;

    [JsonProperty('remaining_quota')]
    property RemainingQuota : Integer read GetRemaining write SetRemaining;

    [JsonProperty('api_key')]
    property APIKey : String read GetKey write SetKey;

    [JsonProperty('keywords_csv')] //workaround until collection support implemented
    property KeyWordCSV : String read FKeyCSV write SetKeyCSV;

    //[JsonProperty('keywords')] //need to not be lazy and implement collection support in ezjson... 🤷
    property KeyWords : TStringArray read FKeys write FKeys;
  end;

implementation
uses
  DateUtils,
  Math,
  fphttpclient,
  fpjson,
  openssl;

{ TYTIndexer }

function TYTIndexer.GetRemaining: Integer;
begin
  Result := FDaily - FSpent;

  if Result < 0 then
    Result := 0;
end;

function TYTIndexer.GetRunning: Boolean;
begin
  Result := FRunning;
end;

function TYTIndexer.GetKey: String;
begin
  Result := FKey;
end;

function TYTIndexer.GetError: String;
begin
  Exit(FError);
end;

procedure TYTIndexer.SetKey(AValue: String);
begin
  FKey := AValue;
end;

procedure TYTIndexer.SetKeyCSV(AValue: String);
begin
  FKeyCSV := AValue;
  FKeys := FKeyCSV.Split(',');
end;

procedure TYTIndexer.SetRemaining(AValue: Integer);
begin
  if AValue >= 0 then
    FSpent := FDaily - AValue
  else
    FSpent := 0;
end;

procedure TYTIndexer.Initialize(const AThread: IEZThread);
var
  LNextQueue, LNow: TDateTime;
  LCalls, J, I: Integer;
  LRemainingMS, LMSInc: Int64;
  LKeys : TStringArray;
  LWork: TWorkEntry;
begin
  LNextQueue := Default(TDateTime);
  try
    FRunning := True;

    //we'll process until the caller has requested us to stop
    while not FStopRequest do
    begin
      LNow := LocalTimeToUniversal(Now);

      //see if we need to queue up the index calls
      if DateTimeDiffMS(LNow, LNextQueue) > 0 then
      begin
        //determine how many calls we can make for the day
        LCalls := RemainingQuota div FQuotaCost;

        if LCalls < 0 then
          LCalls := 1;

        //now determine how much time we have left in the day by UTC time
        LRemainingMS := Abs(DateTimeDiffMS(LNow, EndOfTheDay(LNow)));

        if LCalls > 2 then
          LMSInc := LRemainingMS div LCalls
        else
          LMSInc := 1;

        //shuffle the keyword list to produce a random smattering of searches
        LKeys := RandomizeKeywords;

        //produce enough calls to the pool to handle the work remaining for the day
        J := -1;
        FCritical.Enter;
        try
          for I := 0 to LCalls do
          begin
            if Length(LKeys) < 1 then
              Break;

            //divide the work by keywords
            Inc(J);
            if J > High(LKeys) then
              J := 0;

            //set the queue date to be offset by the remaining msecs and calls we have
            LWork.Key := IncMilliSecond(LNow, LMSInc);

            //set the value to be the keyword we're querying for
            LWork.Value := Trim(LKeys[J]);

            //now queue the work and schedule it to the pool
            FQueue.Enqueue(LWork);
            FPool.Queue(FetchResults, nil, nil);
          end;
        finally
          FCritical.Leave;
        end;

        //set the next 'queue date' to tomorrow
        LNextQueue := LocalTimeToUniversal(Tomorrow);
      end
      else
        Sleep(1000);
    end;
  except on E : Exception do
    FError := E.Message;
  end;
end;

procedure TYTIndexer.FetchResults(const AThread: IEZThread);
var
  LList: TSearchListResponse;
  LClient : TFPHTTPWebClient;
  LResp: TWebClientResponse;
  LReq: TWebClientRequest;
  LWork : TWorkEntry;
  LQuery: String;
  LJSON: TJSONData;
begin
  InitSSLInterface; //avoid EInOutError for openssl in non-main thread (https://forum.lazarus.freepascal.org/index.php?topic=40773.0)
  LList := TSearchListResponse.Create();
  LClient := TFPHTTPWebClient.Create(nil);
  LReq := LClient.CreateRequest;
  try
    try
      //pull the keyword we're searching and time we have to wait until
      FCritical.Enter;
      try
        LWork := FQueue.Dequeue;
      finally
        FCritical.Leave;
      end;

      //sleep in increments and until the alotted time (if any) to allow for stop requests
      while DateTimeDiffMS(LocalTimeToUniversal(Now), LWork.Key) < 0 do
      begin
        if FStopRequest then
          Exit;

        Sleep(100);
      end;

      //check once more to see if we need to stop
      if FStopRequest then
        Exit;

      //attempt to query youtube
      LQuery := EncodeURLElement(LWork.Value);

      LResp := LClient.ExecuteRequest(
        'GET',
        'https://youtube.googleapis.com/youtube/v3/search?part=snippet&maxResults=50&q=' + LQuery + '&order=date&key=' + FKey,
         LReq
      );

      if (LResp.StatusCode >= 200) and (LResp.StatusCode < 300) then
      begin
        //parse the response to a search list
        LJSON := GetJSON(LResp.GetContentAsString);

        if not Assigned(LJSON) then
          raise Exception.Create('invalid response json [' + LResp.GetContentAsString+ ']');

        if not Assigned(TJSONObject(LJSON)) then
        begin
          LJSON.Free;
          raise Exception.Create('json is not properly formatted [' + LResp.GetContentAsString+ ']');
        end;

        LList.LoadFromJSON(TJSONObject(LJSON));

        //save to the db
        SaveQuery(LList);
      end
      else
        FError := LResp.StatusText; //todo - shitty error handling, need to introduce callback or write to error stack
    finally
      LList.Free;
      LClient.Free;
    end;
  except on E : Exception do
    FError := E.Message; //todo - see above 💩
  end;
end;

procedure TYTIndexer.InitializeDB;
  procedure CreateErrorTable;
  var
    LError: String;
  begin
    if not ExecuteSQL(
      'CREATE TABLE IF NOT EXISTS error_log(' +
      ' id Integer NOT NULL PRIMARY KEY AUTOINCREMENT,' +
      ' message text NOT NULL,' +
      ' timestamp datetime NOT NULL DEFAULT (datetime(''now'')));',
      LError
    ) then
      raise Exception.Create('CreateErrorTable::Schema::' + LError);
  end;

  procedure CreateResTypeTable;
  var
    LError: String;
  begin
    //create table
    if not ExecuteSQL(
      'CREATE TABLE IF NOT EXISTS resource_type(' +
      ' id Integer NOT NULL PRIMARY KEY AUTOINCREMENT,' +
      ' type text NOT NULL,' +
      ' UNIQUE(type));',
      LError
    ) then
      raise Exception.Create('CreateResTypeTable::Schema::' + LError);

    //populate all types
    if not ExecuteSQL(
      'INSERT OR IGNORE INTO resource_type(type)' +
      ' VALUES(''search'');',
      LError
    ) then
      raise Exception.Create('CreateResTypeTable::Data::' + LError);
  end;

  procedure CreateResTable;
  var
    LError: String;
  begin
    if not ExecuteSQL(
      'CREATE TABLE IF NOT EXISTS resource(' +
      ' id Integer NOT NULL PRIMARY KEY AUTOINCREMENT,' +
      ' channel_id text NOT NULL,' +
      ' kind text NOT NULL,' +
      ' playlist_id text NOT NULL,' +
      ' video_id text NOT NULL,' +
      ' resource_type_id Integer NOT NULL,' +
      ' timestamp datetime NOT NULL DEFAULT (datetime(''now'')),' +
      ' FOREIGN KEY(resource_type_id) REFERENCES resource_type(id),' +
      ' UNIQUE(video_id));',
      LError
    ) then
      raise Exception.Create('CreateResTable::Schema::' + LError);
  end;

  procedure CreateSnippetTable;
  var
    LError: String;
  begin
    if not ExecuteSQL(
      'CREATE TABLE IF NOT EXISTS snippet(' +
      ' id Integer NOT NULL PRIMARY KEY AUTOINCREMENT,' +
      ' title text NOT NULL,' +
      ' channel_title text NOT NULL,' +
      ' description text NOT NULL,' +
      ' live_broadcast_content text NOT NULL,' +
      ' published_at datetime NOT NULL,' +
      ' resource_id Integer NOT NULL,' +
      ' FOREIGN KEY(resource_id) REFERENCES resource(id),' +
      ' UNIQUE(resource_id));',
      LError
    ) then
      raise Exception.Create('CreateSnippetTable::Schema::' + LError);
  end;

  procedure CreateThumbnailsTable;
  var
    LError: String;
  begin
    if not ExecuteSQL(
      'CREATE TABLE IF NOT EXISTS thumbnails(' +
      ' id Integer NOT NULL PRIMARY KEY AUTOINCREMENT,' +
      ' type text NOT NULL,' +
      ' height Integer NOT NULL,' +
      ' width Integer NOT NULL,' +
      ' url text NOT NULL,' +
      ' snippet_id Integer NOT NULL,' +
      ' UNIQUE(snippet_id, type) ON CONFLICT IGNORE,' +
      ' FOREIGN KEY(snippet_id) REFERENCES snippet(id));',
      LError
    ) then
      raise Exception.Create('CreateThumbnailsTable::Schema::' + LError);
  end;

  procedure CreateSchema;
  begin
    CreateErrorTable;
    CreateResTypeTable;
    CreateResTable;
    CreateSnippetTable;
    CreateThumbnailsTable;
  end;

begin
  try
    if not FConnection.Connected then
    begin
      FConnection.DatabaseName := FDBName;
      FConnection.Open;
    end;

    //setup our initial tables
    CreateSchema;
  except on E : Exception do
    raise Exception.Create('InitializeDB::' + E.Message);
  end;
end;

function TYTIndexer.RandomizeKeywords: TStringArray;
var
  I: Integer;
  LIndexes : TList<Integer>;
begin
  Result := Default(TStringArray);
  Randomize;

  LIndexes := TList<Integer>.Create;
  try
    //match our internal size
    SetLength(Result, Length(FKeys));

    //store all the indexes and we'll use to pop from
    for I := Low(Result) to High(Result) do
      LIndexes.Add(I);

    //grab a random keyword and store to result
    for I := Low(Result) to High(Result) do
      Result[I] := FKeys[LIndexes.ExtractIndex(RandomRange(0, LIndexes.Count))];
  finally
    LIndexes.Free;
  end;
end;

function TYTIndexer.ExecuteSQL(const ASQL: String; out Error: String): Boolean;
var
  LQuery : TSQLQuery;
begin
  Result := False;
  FDBCritical.Enter;
  try
    try
      //create a query
      LQuery := TSQLQuery.Create(nil);
      try
        LQuery.SQLConnection := FConnection;
        LQuery.SQL.Text := ASQL;
        LQuery.ExecSQL;
        Result := True;
      finally
        FConnection.Transaction.CommitRetaining;
        LQuery.Free;
      end;
    except on E : Exception do
      Error := E.Message;
    end;
  finally
    FDBCritical.Leave;
  end;
end;

procedure TYTIndexer.LogError(const AError: String);
var
  LError : String;
begin
  if not ExecuteSQL(
    'INSERT INTO error_log(message)' +
    ' VALUES(' + QuotedStr(AError) + ');',
    LError
  ) then
    raise Exception.Create('LogError::' + LError);
end;

procedure TYTIndexer.SaveQuery(const AResponse: TSearchListResponse);
const
  SQL_INSERT_RES     = 'INSERT OR IGNORE INTO resource(channel_id, kind, playlist_id, video_id, resource_type_id)' +
                       '  VALUES($1, $2, $3, $4, (select id from resource_type where type = ''search''));';

  //currently returning support only returns to caller, so batching isn't so easy and need to use last_insert_rowid()
  //https://www.sqlite.org/lang_returning.html
  SQL_INSERT_SNIPPET = 'INSERT OR IGNORE INTO snippet(title, channel_title, description, live_broadcast_content, published_at, resource_id)' +
                       '  VALUES($1, $2, $3, $4, $5, last_insert_rowid());';

  SQL_THUMB_OPEN     = 'with last_snip as (select last_insert_rowid() as snippet_id where last_insert_rowid() = (SELECT MAX(id)  FROM snippet)) ' + sLineBreak +
                       'INSERT OR IGNORE INTO thumbnails(type, height, width, url, snippet_id)' + sLineBreak;
  SQL_THUMB_STD      = '  SELECT ''standard'', $1, $2, $3, (select snippet_id from last_snip)' +  sLineBreak;
  SQL_THUMB_MED      = '  SELECT ''medium'', $1, $2, $3, (select snippet_id from last_snip)' + sLineBreak;
  SQL_THUMB_HIGH     = '  SELECT ''high'', $1, $2, $3, (select snippet_id from last_snip)' + sLineBreak;
  SQL_THUMB_MAX      = '  SELECT ''maxres'', $1, $2, $3, (select snippet_id from last_snip)' + sLineBreak;
  SQL_THUMB_CLOSE    = 'WHERE EXISTS (select 1 from last_snip);' + sLineBreak;
var
  I: Integer;
  LQuery , LError, LThumb: String;
  LUnion : Boolean;
begin
  //guarantee schema
  InitializeDB;


  //todo - this shouldn't be done here, but with a callback to the search list
  WriteLn('');
  WriteLn('query saving ', 'results: ', Length(AResponse.items), ' time: ', FormatDateTime('yyyy-MM-dd HH:mm:ss', Now));
  WriteLn('------------');

  for I := 0 to High(AResponse.items) do
  begin
    WriteLn('id:', AResponse.items[I].id.videoId, ' title:', AResponse.items[I].snippet.title);
    with AResponse.items[I] do
    begin
      LQuery := SQL_INSERT_RES
                  .Replace('$1', QuotedStr(id.channelId))
                  .Replace('$2', QuotedStr(id.kind))
                  .Replace('$3', QuotedStr(id.playlistId))
                  .Replace('$4', QuotedStr(id.videoId)) + sLineBreak +
                SQL_INSERT_SNIPPET
                  .Replace('$1', QuotedStr(snippet.title))
                  .Replace('$2', QuotedStr(snippet.channelTitle))
                  .Replace('$3', QuotedStr(snippet.description))
                  .Replace('$4', QuotedStr(snippet.liveBroadcastContent))
                  .Replace('$5', QuotedStr(FormatDateTime('yyyy-MM-dd HH:mm:ss', snippet.publishedAt))) + sLineBreak;

      LUnion := False;
      LThumb := '';
      if Assigned(snippet.thumbnails.standard) then
      begin
        LUnion := True;
        LThumb := SQL_THUMB_STD
                    .Replace('$1', IntToStr(snippet.thumbnails.standard.height))
                    .Replace('$2', IntToStr(snippet.thumbnails.standard.width))
                    .Replace('$3', QuotedStr(snippet.thumbnails.standard.url));
      end;

      if Assigned(snippet.thumbnails.medium) then
      begin
        if LUnion then
          LThumb := LThumb + 'UNION' + sLineBreak;

        LThumb := LThumb + SQL_THUMB_MED
                    .Replace('$1', IntToStr(snippet.thumbnails.medium.height))
                    .Replace('$2', IntToStr(snippet.thumbnails.medium.width))
                    .Replace('$3', QuotedStr(snippet.thumbnails.medium.url));
        LUnion := True;
      end;

      if Assigned(snippet.thumbnails.high) then
      begin
        if LUnion then
          LThumb := LThumb + 'UNION' + sLineBreak;

        LThumb := LThumb + SQL_THUMB_HIGH
                    .Replace('$1', IntToStr(snippet.thumbnails.high.height))
                    .Replace('$2', IntToStr(snippet.thumbnails.high.width))
                    .Replace('$3', QuotedStr(snippet.thumbnails.high.url));
        LUnion := True;
      end;

      if Assigned(snippet.thumbnails.maxres) then
      begin
        if LUnion then
          LThumb := LThumb + 'UNION' + sLineBreak;

        LThumb := LThumb + SQL_THUMB_MAX
                    .Replace('$1', IntToStr(snippet.thumbnails.maxres.height))
                    .Replace('$2', IntToStr(snippet.thumbnails.maxres.width))
                    .Replace('$3', QuotedStr(snippet.thumbnails.maxres.url));
        LUnion := True;
      end;

      if not LThumb.IsEmpty then
        LQuery := LQuery + SQL_THUMB_OPEN + LThumb + SQL_THUMB_CLOSE;
    end;

    if not ExecuteSQL(LQuery, LError) then
      LogError(LError);
  end;

  WriteLn('------------');
end;

class function TYTIndexer.DateTimeDiffMS(const ANow, AThen: TDateTime): Int64;
begin
  Result := Trunc(TimeStampToMSecs(DateTimeToTimeStamp(ANow)) - TimeStampToMSecs(DateTimeToTimeStamp(AThen)));
end;

procedure TYTIndexer.Run;
begin
  try
    InitializeDB;
    FStopRequest := False;
    FPool.Queue(Initialize, nil, nil);
    FPool.Start;
  except on E : Exception do
    FError := E.Message;
  end;
end;

procedure TYTIndexer.Stop;
begin
  FStopRequest := True;
  Sleep(1000); //allow short period for graceful termination
  FPool.Stop; //kill em'
  FRunning := False;
end;

constructor TYTIndexer.Create(AOwner: TComponent);
begin
  inherited;
  FDaily := 10000;
  FQuotaCost := 100;
  FSpent := 0;
  FDBName := 'yt_indexer.db';
  FError := '';
  FPool := NewEZThreadPool(4); //min worker count 3 (1 = observer, 2 = queueing, 3+ = worker(s))
  FPool.Settings.UpdateForceTerminate(True);
  FCritical := TCriticalSection.Create;
  FDBCritical := TCriticalSection.Create;
  FQueue := TWorkQueue.Create;
  FConnection := TSQLite3Connection.Create(Self);
  FConnection.Transaction := TSQLTransaction.Create(FConnection);
end;

destructor TYTIndexer.Destroy;
begin
  FCritical.Free;
  FDBCritical.Free;
  FPool := nil;
  FQueue.Free;
  inherited Destroy;
end;

end.

