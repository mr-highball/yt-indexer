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
  TYTIndexer = class
  strict private
    FDaily: Integer;
    FKey: String;
    FSpent: Integer;
    FDBName: String;
    FQuotaCost: Integer;
    function GetRemaining: Integer;
    procedure SetRemaining(AValue: Integer);
  strict protected
  public
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
    property APIKey : String read FKey write FKey;
  end;

implementation

{ TYTIndexer }

function TYTIndexer.GetRemaining: Integer;
begin
  Result := FDaily - FSpent;

  if Result < 0 then
    Result := 0;
end;

procedure TYTIndexer.SetRemaining(AValue: Integer);
begin
  if AValue >= 0 then
    FSpent := FDaily - AValue
  else
    FSpent := 0;
end;

constructor TYTIndexer.Create;
begin
  FDaily := 10000;
  FQuotaCost := 100;
  FSpent := 0;
  FDBName := 'yt_indexer.db';
end;

destructor TYTIndexer.Destroy;
begin
  inherited Destroy;
end;

end.

