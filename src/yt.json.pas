unit yt.json;

{$mode delphi}

interface

uses
  Classes,
  SysUtils,
  fpjson,
  jsonparser,
  db;

type
  { TDatasetJSON }
  (*
    json serialized form of a dataset where rows are
    json objects in a the result array
  *)
  TDatasetJSON = record
  public
    const
      PROP_RESULT = 'result';
  private
    FRows : TArray<String>;
    function GetCount: Integer;
    function GetRow(const AIndex : Integer): String;
  public
    //single row json object representations
    property Rows : TArray<String> read FRows write FRows;
    property Row[Const AIndex : Integer] : String read GetRow; default;
    property Count : Integer read GetCount;

    procedure AddRow(Const ARowJSON : String);
    function ToJSON() : String;
    procedure FromJSON(Const AJSON : String);
  end;

function DatasetToJSON(const ADataset : TDataSet; out Data : TDatasetJSON;
  out Error : string) : Boolean;

implementation

function DatasetToJSON(const ADataset: TDataSet; out Data: TDatasetJSON; out
  Error: string): Boolean;
var
  LObj : TJSONObject;
  I: Integer;
  LField: TField;
begin
  Result := False;

  try
    //now we can open the query for reading
    if not ADataset.Active then
      ADataset.Active := True;
    try
      //traverse dataset and build up a json object to add to the the row
      while not ADataset.EOF do
      begin
        LObj := TJSONObject.Create;
        try
          try

            //iterate fields to build the object
            for I := 0 to Pred(ADataset.FieldCount) do
            begin
              //get a reference to the field
              LField := ADataset.Fields[I];

              case LField.DataType of
                //boolean case
                ftBoolean : LObj.Add(LField.DisplayLabel, TJSONBoolean.Create(LField.AsBoolean));

                //number types
                ftInteger,
                ftFloat,
                ftCurrency,
                ftsmallint,
                ftLargeint,
                ftWord : LObj.Add(LField.DisplayLabel, LField.AsFloat);

                //default to string otherwise
                else
                  LObj.Add(LField.DisplayLabel, LField.AsString);
              end;
            end;

            //add the row
            Data.AddRow(LObj.AsJSON);
          finally
            LObj.Free;
          end;
        except on E : Exception do
        begin
          Error := E.Message;
          Exit;
        end
        end;

        //next row
        ADataset.Next;
      end;
    finally
      ADataset.Close;
    end;

    //success
    Result := True;
  except on E : Exception do
    Error := E.Message;
  end;
end;

{ TDatasetJSON }

function TDatasetJSON.GetRow(const AIndex : Integer): String;
begin
  Result := FRows[AIndex];
end;

function TDatasetJSON.GetCount: Integer;
begin
  Result := Length(FRows);
end;

procedure TDatasetJSON.AddRow(const ARowJSON: String);
begin
  SetLength(FRows, Succ(Length(FRows)));
  FRows[High(FRows)] := ARowJSON;
end;

function TDatasetJSON.ToJSON(): String;
var
  I: Integer;
  LResult : TJSONObject;
  LArr : TJSONArray;
  LObj : TJSONData;
begin
  Result := '{"' + PROP_RESULT + '" : []}';

  //init result json structure
  LResult := TJSONObject.Create;
  LArr := TJSONArray.Create;
  LResult.Add(PROP_RESULT, LArr);

  //add to result array
  try
    for I := 0 to High(FRows) do
    begin
      LObj := GetJSON(FRows[I]);

      if not Assigned(LObj) then
        continue;

      if not (Lobj.JSONType = jtObject) then
      begin
        LObj.Free;
        continue;
      end;

      LArr.Add(LObj);
    end;

    //serialize
    Result := LResult.AsJSON;
  finally
    LResult.Free;
  end;
end;

procedure TDatasetJSON.FromJSON(const AJSON: String);
var
  LObj: TJSONData;
  LArr: TJSONArray;
  I: Integer;
begin
  //initialize
  SetLength(FRows, 0);
  LObj := GetJSON(AJSON);

  if not Assigned(LObj) then
    raise Exception.Create('TDatasetResponse::FromJSON::invalid json for error');

  if not (LObj.JSONType = jtObject) then
  begin
    LObj.Free;
    raise Exception.Create('TDatasetResponse::FromJSON::json is not object');
  end;

  LArr := TJSONObject(LObj).Arrays[PROP_RESULT];

  if not Assigned(LArr) then
  begin
    LObj.Free;
    raise Exception.Create('TDatasetResponse::FromJSON::invalid json for error');
  end;

  //now traverse array and fill internal rows
  try
    SetLength(FRows, LArr.Count);

    //assumes all items are objects, otherwise will either be uneven
    //or throw an exception
    for I := 0 to Pred(LArr.Count) do
      FRows[I] := LArr.Objects[I].AsJSON;
  finally
    LObj.Free;
  end;
end;
end.

