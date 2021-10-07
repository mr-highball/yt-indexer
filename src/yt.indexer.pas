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
  Generics.Collections;

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
    FQueue : TWorkQueue;
    FRunning : Boolean;
    function GetError: String;
    function GetKey: String;
    function GetRemaining: Integer;
    function GetRunning: Boolean;
    procedure SetKey(AValue: String);
    procedure SetKeyCSV(AValue: String);
    procedure SetRemaining(AValue: Integer);
    procedure Initialize(const AThread : IEZThread);
    procedure FetchResults(const AThread : IEZThread);
  strict protected
    function RandomizeKeywords : TStringArray;
    procedure SaveQuery(const AResponse : TSearchListResponse);
    class function DateTimeDiffMS(const ANow, AThen : TDateTime) : Int64;
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

        if LCalls < 1 then
          LCalls := 1;

        //now determine how much time we have left in the day by UTC time
        LRemainingMS := DateTimeDiffMS(LNow, EndOfTheDay(LNow));

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

procedure TYTIndexer.SaveQuery(const AResponse: TSearchListResponse);
var
  I: Integer;
begin
  //todo
  WriteLn('results: ', Length(AResponse.items));
  for I := 0 to High(AResponse.items) do
  begin
    WriteLn('id:', AResponse.items[I].id.videoId, ' title:', AResponse.items[I].snippet.title);
  end;
  WriteLn('-----');
end;

class function TYTIndexer.DateTimeDiffMS(const ANow, AThen: TDateTime): Int64;
begin
  Result := Trunc(TimeStampToMSecs(DateTimeToTimeStamp(ANow)) - TimeStampToMSecs(DateTimeToTimeStamp(AThen)));
end;

procedure TYTIndexer.Run;
begin
  try
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
  FPool.Stop;
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
  FPool := NewEZThreadPool(3);
  FCritical := TCriticalSection.Create;
  FQueue := TWorkQueue.Create;
end;

destructor TYTIndexer.Destroy;
begin
  FCritical.Free;
  FPool := nil;
  FQueue.Free;
  inherited Destroy;
end;

end.

