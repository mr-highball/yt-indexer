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
  ezthreads.pool;

type

  { TYTIndexer }

  [JsonObject('yt_indexer')]
  TYTIndexer = class(TComponent)
  strict private
    FDaily: Integer;
    FKeys: TStringArray;
    FSpent: Integer;
    FDBName: String;
    FQuotaCost: Integer;
    FKey : String;
    FError : String;
    FPool : IEZThreadPool;
    FStopRequest : Boolean;
    function GetError: String;
    function GetKey: String;
    function GetRemaining: Integer;
    function GetRunning: Boolean;
    procedure SetKey(AValue: String);
    procedure SetRemaining(AValue: Integer);
    procedure Initialize(const AThread : IEZThread);
    procedure FetchResults(const AThread : IEZThread);
  strict protected
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

    [JsonProperty('keywords')]
    property KeyWords : TStringArray read FKeys write FKeys;
  end;

implementation
uses
  DateUtils;

{ TYTIndexer }

function TYTIndexer.GetRemaining: Integer;
begin
  Result := FDaily - FSpent;

  if Result < 0 then
    Result := 0;
end;

function TYTIndexer.GetRunning: Boolean;
begin
  Result := False; //todo...
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
begin
  LNextQueue := Default(TDateTime);
  try
    //we'll process until the caller has requested us to stop
    while not FStopRequest do
    begin
      LNow := LocalTimeToUniversal(Now);

      //see if we need to queue up the index calls
      if MilliSecondSpan(LNow, LNextQueue) < 0 then
        begin
        //determine how many calls we can make for the day
        //...

        //now determine how much time we have left in the day by UTC time
        //...

        //shuffle the keyword list to produce a random smattering of searches
        //...

        //produce enough calls to the pool to handle the work remaining for the day
        //...
          //divide the work by keywords
          //...

        //set the next 'queue date' to tomorrow
        //...
      end
      else
        Sleep(3000);
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
begin
  LList := TSearchListResponse.Create();
  LClient := TFPHTTPWebClient.Create(nil);
  LReq := LClient.CreateRequest;
  try
    try
      //pull the keyword we're searching and time we have to wait until
      //...

      //wait the elotted time (if any)
      //Sleep(LWait);

      //attempt to query youtube
      //...
      LResp := LClient.ExecuteRequest(
        'GET',
        'https://youtube.googleapis.com/youtube/v3/search?part=snippet&maxResults=50&q=AQUERY&order=date&key=API_KEY',
         LReq
      );

      //parse the response to a search list
      //...

      //save to the db
      //...
    finally
      LList.Free;
      LClient.Free;
    end;
  except on E : Exception do
    FError := E.Message;
  end;
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
end;

constructor TYTIndexer.Create(AOwner: TComponent);
begin
  inherited;
  FDaily := 10000;
  FQuotaCost := 100;
  FSpent := 0;
  FDBName := 'yt_indexer.db';
  FError := '';
  FPool := NewEZThreadPool();
end;

destructor TYTIndexer.Destroy;
begin
  inherited Destroy;
end;

end.

