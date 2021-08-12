unit yt.indexer;

{$mode delphi}{$H+}

interface

uses
  Classes,
  SysUtils,
  fphttpwebclient,
  jsonparser,
  googlebase,
  googleservice,
  googleclient,
  googleyoutube,
  opensslsockets,
  ezjson;

type

  { TYTIndexer }

  [JsonObject('yt_indexer')]
  TYTIndexer = class(TComponent)
  strict private
    FDaily: Integer;
    FSpent: Integer;
    FDBName: String;
    FQuotaCost: Integer;
    FClient : TGoogleClient;
    FAPI : TYoutubeAPI;
    function GetKey: String;
    function GetRemaining: Integer;
    procedure SetKey(AValue: String);
    procedure SetRemaining(AValue: Integer);
  strict protected
  public
    procedure Run;

    constructor Create; virtual;
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
  end;

implementation

{ TYTIndexer }

function TYTIndexer.GetRemaining: Integer;
begin
  Result := FDaily - FSpent;

  if Result < 0 then
    Result := 0;
end;

function TYTIndexer.GetKey: String;
begin
  Result := FClient.AuthHandler.Config.DeveloperKey;
end;

procedure TYTIndexer.SetKey(AValue: String);
begin
  FClient.AuthHandler.Config.DeveloperKey := AValue;
end;

procedure TYTIndexer.SetRemaining(AValue: Integer);
begin
  if AValue >= 0 then
    FSpent := FDaily - AValue
  else
    FSpent := 0;
end;

procedure TYTIndexer.Run;
var
  LRes: TSearchResource;
  LList: TSearchListResponse;
  LEntry: TSearchResult;
  LOptions : TSearchListOptions;
begin
  LRes := FAPI.CreateSearchResource;
  try
    try
      FClient.AuthHandler.RefreshIDToken;
      LOptions := Default(TSearchListOptions);
      LOptions.eventType := 'video';
      LOptions.q := 'doggo';

      LList := LRes.List(LOptions); //todo...this is requiring auth... but should it with dev key?

      for LEntry in LList.Items do
        WriteLn(LEntry.SaveToJSON.AsJSON); //todo - change this
    finally
      LRes.Free;
    end;
  except on E : Exception do
    WriteLn(E.Message); //todo - change this
  end;
end;

constructor TYTIndexer.Create;
begin
  FDaily := 10000;
  FQuotaCost := 100;
  FSpent := 0;
  FDBName := 'yt_indexer.db';
  FClient := TGoogleClient.Create(Self);
  FClient.WebClient:=TFPHTTPWebClient.Create(Self);
  FClient.WebClient.LogFile:='yt-indexer.log'; //todo - change this from being static
  FClient.Config.AuthMethod := amDeveloperKey;
  FClient.AuthHandler := TGoogleOAuth2Handler.Create(Self);
  FClient.AuthHandler.Config.DeveloperKey := '';
  FClient.WebClient.RequestSigner := FClient.AuthHandler;
  FClient.AuthHandler.WebClient := FClient.WebClient;
  FAPI := TYoutubeAPI.Create(Self);
  FAPI.GoogleClient := FClient;
end;

destructor TYTIndexer.Destroy;
begin
  inherited Destroy;
end;

end.

