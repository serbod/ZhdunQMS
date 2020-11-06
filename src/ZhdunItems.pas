{ Copyright: Sergey Bodrov, 2020 }
unit ZhdunItems;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IniFiles;

type

  { IZOffice }

  IZOffice = interface

  end;

  { IZTicket }

  IZTicket = interface
    property Office: IZOffice;
  end;

  PTicket = ^TTicket;

  { TTicket }

  TTicket = object
    Id: Integer;
    Num: Integer;
    Caption: string;
    IconId: Integer;

    TimeCreate: TDateTime;
    TimeStart: TDateTime;
    TimeFinish: TDateTime;

    OfficeId: Integer;
    Deleted: Boolean;

    procedure Init();
  end;
  TTicketArray = array of TTicket;

  { TTicketList }

  TTicketList = object
  private
    Items: TTicketArray;
    Count: Integer;
  public

    procedure Init();

    function Add(const AValue: TTicket): Integer;
    function Delete(AId: Integer): Boolean;
    function Get(AId: Integer): PTicket;
    function GetByIndex(AIndex: Integer): PTicket;

    function GetCount(): Integer;

    { Return maximum ticket Num for given AOfficeId or 0 if no ticket found }
    function GetMaxNum(AOfficeId: Integer): Integer;
    { Return ticket with minimum Num for given AOfficeId, or nil if not found }
    function GetTopTicket(AOfficeId: Integer): PTicket;
  end;


  TOfficeState = (osUndef, osOff, osIdle, osBusy, osPaused, osLost);

  POffice = ^TOffice;

  { TOffice }

  TOffice = object
    Id: Integer;
    Num: Integer;
    Caption: string;
    Comment: string;
    IconName: string;
    State: TOfficeState;
    Deleted: Boolean;

    TicketPrefix: string;
    GroupId: Integer;

    // state
    TicketCount: Integer;
    TicketNum: Integer;

    // server part
    HostPort: string;

    procedure Init();
    function IsVisible(): Boolean;
  end;
  TOfficeArray = array of TOffice;

  { TOfficeList }

  POfficeList = ^TOfficeList;
  TOfficeList = object
  private
    Items: TOfficeArray;
    Count: Integer;
  public
    procedure Init();

    function Add(const AValue: TOffice): Integer;
    function Delete(AId: Integer): Boolean;
    function Get(AId: Integer): POffice;
    function GetByIndex(AIndex: Integer): POffice;
    function GetByNum(ANum: Integer): POffice;
    function GetCount(): Integer;
  end;


  { TOfficeListIterator }

  TOfficeListIterator = object
  private
    pList: POfficeList;
    n: Integer;
  public
    procedure Init(const AList: TOfficeList);
    function GetNext(): POffice;
    function Next(out AValue: POffice): Boolean;
  end;

  TVisualTicket = object
    Pos: TPoint;
    Size: TPoint;
    Marked: Boolean;
    OfficeText: string;
    TicketText: string;
    IsValid: Boolean;

    TicketId: Integer;
    OfficeId: Integer;
  end;

  TVisualTicketArray = array of TVisualTicket;


  TVisualButton = object
    Rect: TRect;

    Marked: Boolean;
    Text: string;
    SubText: string;
    IsValid: Boolean;

    OfficeNum: Integer;
  end;

  TVisualButtonArray = array of TVisualButton;


implementation

{ TOfficeListIterator }

procedure TOfficeListIterator.Init(const AList: TOfficeList);
begin
  pList := Addr(AList);
  n := -1;
end;

function TOfficeListIterator.GetNext(): POffice;
begin
  Inc(n);
  Result := pList^.GetByIndex(n);
  while Assigned(Result) and Result^.Deleted do
  begin
    Inc(n);
    Result := pList^.GetByIndex(n);
  end;
end;

function TOfficeListIterator.Next(out AValue: POffice): Boolean;
begin
  AValue := GetNext();
  Result := Assigned(AValue);
end;

{ TTicket }

procedure TTicket.Init();
begin
  Id := -1;
  Caption := '';

  Num := 0;
  IconId := 0;

  TimeCreate := Now();
  TimeStart := 0;
  TimeFinish := 0;

  OfficeId := 0;
  Deleted := False;
end;

{ TTicketList }

procedure TTicketList.Init();
begin
  SetLength(Items, 0);
  Count := 0;
end;

function TTicketList.Add(const AValue: TTicket): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(Items)-1 do
  begin
    if Items[i].Deleted then
    begin
      Result := i;
      Break;
    end;
  end;

  if Result = -1 then
  begin
    Result := Length(Items);
    SetLength(Items, Result+1);
  end;

  Items[Result] := AValue;
  Items[Result].Id := Result;
  Items[Result].Deleted := False;
  Inc(Count);
end;

function TTicketList.Delete(AId: Integer): Boolean;
begin
  Result := (not Items[AId].Deleted);
  if Result then
  begin
    Items[AId].Deleted := True;
    Dec(Count);
  end;
end;

function TTicketList.Get(AId: Integer): PTicket;
begin
  Result := Addr(Items[AId]);
end;

function TTicketList.GetByIndex(AIndex: Integer): PTicket;
begin
  if AIndex < Length(Items) then
    Result := Addr(Items[AIndex])
  else
    Result := nil;
end;

function TTicketList.GetCount(): Integer;
begin
  Result := Count;
end;

function TTicketList.GetMaxNum(AOfficeId: Integer): Integer;
var
  i: Integer;
  pTicketItem: PTicket;
begin
  Result := 0;
  for i := Low(Items) to High(Items) do
  begin
    pTicketItem := Addr(Items[i]);
    if (not pTicketItem^.Deleted) and (pTicketItem^.OfficeId = AOfficeId) then
    begin
      if Result < pTicketItem^.Num then
        Result := pTicketItem^.Num;
    end;
  end;
end;

function TTicketList.GetTopTicket(AOfficeId: Integer): PTicket;
var
  i, MinNum: Integer;
  pTicketItem: PTicket;
begin
  Result := nil;
  MinNum := MaxInt;
  for i := Low(Items) to High(Items) do
  begin
    pTicketItem := Addr(Items[i]);
    if (not pTicketItem^.Deleted) and (pTicketItem^.OfficeId = AOfficeId) then
    begin
      if MinNum > pTicketItem^.Num then
      begin
        MinNum := pTicketItem^.Num;
        Result := pTicketItem;
      end;
    end;
  end;
end;

{ TOffice }

procedure TOffice.Init();
begin
  Id := -1;
  Num := 0;
  Caption := '';
  Comment := '';
  IconName := '';
  State := osUndef;
  Deleted := False;

  TicketPrefix := '';
  GroupId := -1;
end;

function TOffice.IsVisible(): Boolean;
begin
  Result := State in [osBusy, osIdle, osPaused];
end;

{ TOfficeList }

procedure TOfficeList.Init();
begin
  SetLength(Items, 0);
  Count := 0;
end;

function TOfficeList.Add(const AValue: TOffice): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Length(Items)-1 do
  begin
    if Items[i].Deleted then
    begin
      Result := i;
      Break;
    end;
  end;

  if Result = -1 then
  begin
    Result := Length(Items);
    SetLength(Items, Result+1);
  end;

  Items[Result] := AValue;
  Items[Result].Id := Result;
  Items[Result].Deleted := False;
  Inc(Count);
end;

function TOfficeList.Delete(AId: Integer): Boolean;
begin
  Result := (not Items[AId].Deleted);
  if Result then
  begin
    Items[AId].Deleted := True;
    Dec(Count);
  end;
end;

function TOfficeList.Get(AId: Integer): POffice;
begin
  Result := Addr(Items[AId]);
end;

function TOfficeList.GetByIndex(AIndex: Integer): POffice;
begin
  if AIndex < Length(Items) then
    Result := Addr(Items[AIndex])
  else
    Result := nil;
end;

function TOfficeList.GetByNum(ANum: Integer): POffice;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to Length(Items)-1 do
  begin
    if (not Items[i].Deleted) and (Items[i].Num = ANum) then
    begin
      Result := Addr(Items[i]);
      Break;
    end;
  end;
end;

function TOfficeList.GetCount(): Integer;
begin
  Result := Count;
end;

end.

