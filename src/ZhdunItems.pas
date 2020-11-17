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

  { TTicket }

  TTicket = class(TObject)
  public
    //Id: Integer;
    Num: Integer;
    Caption: string;
    IconId: Integer;

    TimeCreate: TDateTime;
    TimeStart: TDateTime;
    TimeFinish: TDateTime;

    OfficeNum: Integer;
    Deleted: Boolean;

    constructor Create();
  end;

  { TTicketList }

  TTicketList = class(TList)
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;

    function GetItem(AIndex: Integer): TTicket;
    procedure SetItem(AIndex: Integer; AValue: TTicket);
  public
    function GetItemByNum(ANum: Integer): TTicket;

    { Return maximum ticket Num for given AOfficeNum or 0 if no ticket found }
    function GetMaxNum(AOfficeNum: Integer): Integer;
    { Return ticket with minimum Num for given AOfficeNum, or nil if not found }
    function GetTopTicket(AOfficeNum: Integer): TTicket;

    property Items[AIndex: Integer]: TTicket read GetItem write SetItem;
  end;


  TOfficeState = (osUndef, osOff, osIdle, osBusy, osPaused, osLost);

  { TOffice }

  TOffice = class(TObject)
  public
    //Id: Integer;
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

    // local
    ButtonPressTimestamp: Int64;

    constructor Create();
    function IsVisible(): Boolean;
  end;

  { TOfficeList }

  TOfficeList = class(TList)
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;

    function GetItem(AIndex: Integer): TOffice;
    procedure SetItem(AIndex: Integer; AValue: TOffice);
  public
    function GetByNum(ANum: Integer): TOffice;

    property Items[AIndex: Integer]: TOffice read GetItem write SetItem;
  end;


  { TOfficeListIterator }

  TOfficeListIterator = object
  private
    List: TOfficeList;
    n: Integer;
  public
    procedure Init(const AList: TOfficeList);
    function GetNext(): TOffice;
    function Next(out AValue: TOffice): Boolean;
  end;

  TVisualOffice = object
    Pos: TPoint;
    Size: TPoint;
    Marked: Boolean;
    OfficeText: string;
    TicketText: string;
    IsValid: Boolean;

    TicketNum: Integer;
    OfficeNum: Integer;
  end;

  TVisualOfficeArray = array of TVisualOffice;


  PVisualButton = ^TVisualButton;
  TVisualButton = object
    Rect: TRect;

    Marked: Boolean;
    IsPressed: Boolean;
    Text: string;
    SubText: string;
    IsValid: Boolean;

    OfficeNum: Integer;
  end;

  TVisualButtonArray = array of TVisualButton;

  TVisualTicket = object
    Rect: TRect;
    Visible: Boolean;
    OfficeText: string;
    TicketText: string;
    TimeCreate: TDateTime;

    TicketNum: Integer;
    OfficeNum: Integer;
  end;


implementation

{ TOfficeListIterator }

procedure TOfficeListIterator.Init(const AList: TOfficeList);
begin
  List := AList;
  n := -1;
end;

function TOfficeListIterator.GetNext(): TOffice;
begin
  repeat
    Inc(n);
    if n < List.Count then
      Result := List.Items[n]
    else
      Result := nil;

    if Assigned(Result) and (not Result.Deleted) then
      Exit;

  until n >= List.Count;
end;

function TOfficeListIterator.Next(out AValue: TOffice): Boolean;
begin
  AValue := GetNext();
  Result := Assigned(AValue);
end;

{ TTicket }

constructor TTicket.Create();
begin
  inherited Create();
  Caption := '';

  Num := 0;
  IconId := 0;

  TimeCreate := Now();
  TimeStart := 0;
  TimeFinish := 0;

  OfficeNum := 0;
  Deleted := False;
end;

{ TTicketList }

procedure TTicketList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if Action = lnDeleted then
    TTicket(Ptr).Free();
end;

function TTicketList.GetItem(AIndex: Integer): TTicket;
begin
  Result := TTicket(Get(AIndex));
end;

procedure TTicketList.SetItem(AIndex: Integer; AValue: TTicket);
begin
  Put(AIndex, AValue);
end;

function TTicketList.GetItemByNum(ANum: Integer): TTicket;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := TTicket(Items[i]);
    if Result.Num = ANum then
      Exit;
  end;
  Result := nil;
end;

function TTicketList.GetMaxNum(AOfficeNum: Integer): Integer;
var
  i: Integer;
  Item: TTicket;
begin
  Result := 0;
  for i := 0 to Count-1 do
  begin
    Item := Items[i];
    if (not Item.Deleted) and (Item.OfficeNum = AOfficeNum) then
    begin
      if Result < Item.Num then
        Result := Item.Num;
    end;
  end;
end;

function TTicketList.GetTopTicket(AOfficeNum: Integer): TTicket;
var
  i, MinNum: Integer;
  Item: TTicket;
begin
  Result := nil;
  MinNum := MaxInt;
  for i := 0 to Count-1 do
  begin
    Item := Items[i];
    if (not Item.Deleted) and (Item.OfficeNum = AOfficeNum) then
    begin
      if MinNum > Item.Num then
      begin
        MinNum := Item.Num;
        Result := Item;
      end;
    end;
  end;
end;

{ TOffice }

constructor TOffice.Create();
begin
  inherited Create();
  Num := 0;
  Caption := '';
  Comment := '';
  IconName := '';
  State := osUndef;
  Deleted := False;

  TicketPrefix := '';
  GroupId := -1;

  TicketCount := 0;
  TicketNum := 0;

  HostPort := '';

  ButtonPressTimestamp := 0;
end;

function TOffice.IsVisible(): Boolean;
begin
  Result := State in [osBusy, osIdle, osPaused];
end;

{ TOfficeList }

procedure TOfficeList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited Notify(Ptr, Action);
  if Action = lnDeleted then
    TOffice(Ptr).Free();
end;

function TOfficeList.GetItem(AIndex: Integer): TOffice;
begin
  Result := TOffice(Get(AIndex));
end;

procedure TOfficeList.SetItem(AIndex: Integer; AValue: TOffice);
begin
  Put(AIndex, AValue);
end;

function TOfficeList.GetByNum(ANum: Integer): TOffice;
var
  i: Integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := TOffice(Items[i]);
    if (not Result.Deleted) and (Result.Num = ANum) then
      Exit;
  end;
  Result := nil;
end;

end.

