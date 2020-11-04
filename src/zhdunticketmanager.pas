unit ZhdunTicketManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZhdunItems, RFUtils, IniFiles;

type
  TSendCmdEvent = procedure(const ACmdText, AHostPort: string) of object;

  { TTicketManager }

  TTicketManager = class
  private
    FOnSendCmd: TSendCmdEvent;
  public
    OfficeList: TOfficeList;
    TicketList: TTicketList;

    VisualTickets: TVisualTicketArray;

    VisTicketsArea: TRect;
    TicketSize: TPoint;
    TicketBorderSize: Integer;
    MaxVisTickets: Integer;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure LoadConfig();
    procedure SaveConfig();

    function CreateTicket(AOfficeId: Integer): PTicket;
    function NextTicket(AOfficeId: Integer): PTicket;

    procedure UpdateVisualTickets();

    procedure UpdateOfficesStates();

    procedure PostCmd(AOffice: TOffice; ACmdText: string);

    procedure ProcessCmd(ACmdText, AHostPort: string);

    property OnSendCmd: TSendCmdEvent read FOnSendCmd write FOnSendCmd;
  end;


implementation

{ TTicketManager }

procedure TTicketManager.AfterConstruction;
begin
  inherited AfterConstruction;
  OfficeList.Init();
  TicketList.Init();
end;

procedure TTicketManager.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;

procedure TTicketManager.LoadConfig();
var
  ini: TMemIniFile;
  sSection: string;
  i, n: Integer;
  OfficeItem: TOffice;
begin
  ini := TMemIniFile.Create('zhdun.ini');
  try
    sSection := 'Main';
    MaxVisTickets := ini.ReadInteger(sSection, 'MaxVisTickets', MaxVisTickets);
    TicketBorderSize := ini.ReadInteger(sSection, 'TicketBorderSize', TicketBorderSize);

    // Office list
    n := ini.ReadInteger(sSection, 'OfficeCount', 0);
    for i := 1 to n do
    begin
      sSection := 'Office_' + IntToStr(i);
      if not ini.SectionExists(sSection) then
        Continue;

      OfficeItem.Init();
      OfficeItem.Caption := ini.ReadString(sSection, 'Caption', OfficeItem.Caption);
      OfficeItem.Comment := ini.ReadString(sSection, 'Comment', OfficeItem.Comment);
      OfficeItem.IconName := ini.ReadString(sSection, 'IconName', OfficeItem.IconName);
      OfficeItem.TicketPrefix := ini.ReadString(sSection, 'TicketPrefix', OfficeItem.TicketPrefix);
      OfficeItem.GroupId := ini.ReadInteger(sSection, 'GroupId', OfficeItem.GroupId);

      OfficeList.Add(OfficeItem);
    end;

  finally
    FreeAndNil(ini);
  end;
end;

procedure TTicketManager.SaveConfig();
var
  ini: TMemIniFile;
  sSection: string;
  i, n: Integer;
  pOfficeItem: POffice;
begin
  ini := TMemIniFile.Create('zhdun.ini');
  try
    sSection := 'Main';
    ini.WriteInteger(sSection, 'OfficeCount', OfficeList.GetCount());
    ini.WriteInteger(sSection, 'MaxVisTickets', MaxVisTickets);
    ini.WriteInteger(sSection, 'TicketBorderSize', TicketBorderSize);

    i := 0;
    n := 0;
    while OfficeList.GetByIndex(i) <> nil do
    begin
      pOfficeItem := OfficeList.GetByIndex(i);
      if not pOfficeItem^.Deleted then
      begin
        Inc(n);
        sSection := 'Office_' + IntToStr(n+1);
        ini.WriteString(sSection, 'Caption', pOfficeItem^.Caption);
        ini.WriteString(sSection, 'Comment', pOfficeItem^.Comment);
        ini.WriteString(sSection, 'IconName', pOfficeItem^.IconName);
        //State: TOfficeState;
        //Deleted: Boolean;

        ini.WriteString(sSection, 'TicketPrefix', pOfficeItem^.TicketPrefix);
        ini.WriteInteger(sSection, 'GroupId', pOfficeItem^.GroupId);
      end;
      Inc(i);
    end;
  finally
    FreeAndNil(ini);
  end;
end;

function TTicketManager.CreateTicket(AOfficeId: Integer): PTicket;
var
  i, n: Integer;
  //pTicketItem: PTicket;
  MaxNum: Integer;
  pTmpOffice: POffice;
  TmpTicket: TTicket;
begin
  Result := nil;
  pTmpOffice := OfficeList.Get(AOfficeId);
  if pTmpOffice = nil then
    Exit;

  MaxNum := TicketList.GetMaxNum(AOfficeId);

  Inc(MaxNum);
  TmpTicket.Init();
  TmpTicket.OfficeId := AOfficeId;
  TmpTicket.Num := MaxNum;
  TmpTicket.Caption := pTmpOffice^.TicketPrefix + IntToStr(TmpTicket.Num);

  n := TicketList.Add(TmpTicket);
  Result := TicketList.Get(n);
end;

function TTicketManager.NextTicket(AOfficeId: Integer): PTicket;
var
  pTmpTicket: PTicket;
begin
  Result := nil;
  Assert(AOfficeID >= 0);

  pTmpTicket := TicketList.GetTopTicket(AOfficeId);
  if not Assigned(pTmpTicket) then
    Exit;

  TicketList.Delete(pTmpTicket^.Id);

  Result := TicketList.GetTopTicket(AOfficeId);
end;

procedure TTicketManager.UpdateVisualTickets();
var
  TmpVisualTickets: TVisualTicketArray;
  TmpVisualTicket: TVisualTicket;
  i, ii: Integer;
  OfficeIterator: TOfficeListIterator;
  pTmpOffice: POffice;
  pTmpTicket: PTicket;
  NextPos: TPoint;
begin
  TmpVisualTickets := [];
  NextPos := VisTicketsArea.TopLeft;

  TicketSize.X := VisTicketsArea.Width;
  TicketSize.Y := (VisTicketsArea.Height div MaxVisTickets);

  OfficeIterator.Init(OfficeList);
  while OfficeIterator.Next(pTmpOffice) do
  begin
    pTmpTicket := TicketList.GetTopTicket(pTmpOffice^.Id);
    if not Assigned(pTmpTicket) then
      Continue;

    //TmpVisualTicket.IsValid := True;
    //TmpVisualTicket.Marked := ;
    TmpVisualTicket.OfficeId := pTmpOffice^.Id;
    TmpVisualTicket.TicketId := pTmpTicket^.Id;
    TmpVisualTicket.OfficeText := pTmpOffice^.Caption;
    TmpVisualTicket.TicketText := pTmpTicket^.Caption;

    TmpVisualTicket.Pos := NextPos;
    TmpVisualTicket.Size := TicketSize;

    NextPos.Y := NextPos.Y + TicketSize.Y;

    TmpVisualTickets := Concat(TmpVisualTickets, [TmpVisualTicket]);
  end;
  VisualTickets := TmpVisualTickets;
end;

procedure TTicketManager.UpdateOfficesStates();
var
  OfficeIterator: TOfficeListIterator;
  pTmpOffice: POffice;
  //pTmpTicket: PTicket;
  s: string;
begin
  OfficeIterator.Init(OfficeList);
  while OfficeIterator.Next(pTmpOffice) do
  begin
    if pTmpOffice^.State in [osUndef, osOff] then
      Continue;

    // OFFICE <office_num>: STATE <tickets_count> [ticket_num]
    s := Format('OFFICE %d: STATE %d', [pTmpOffice^.Num, pTmpOffice^.TicketCount]);
    if pTmpOffice^.TicketCount > 0 then
      s := s + ' ' + IntToStr(pTmpOffice^.TicketNum);

    PostCmd(pTmpOffice^, s);
  end;
end;

procedure TTicketManager.PostCmd(AOffice: TOffice; ACmdText: string);
var
  s: string;
begin
  s := 'OFFICE ' + IntToStr(AOffice.Num) + ': ' + ACmdText;
  if Assigned(OnSendCmd) then OnSendCmd(s, AOffice.HostPort);
end;

procedure TTicketManager.ProcessCmd(ACmdText, AHostPort: string);
var
  s, ss, sCmd: string;
  iNum: Integer;
  pTmpOffice: POffice;
begin
  ss := ACmdText;
  sCmd := ExtractFirstWord(ss);

  // OFFICE <office_num>:
  if sCmd = 'OFFICE' then
  begin
    s := ExtractFirstWord(ss, ': ');
    iNum := StrToIntDef(s, -1);
    if iNum <> -1 then
    begin
      pTmpOffice := OfficeList.GetByNum(iNum);
      if Assigned(pTmpOffice) then
      begin
        if AHostPort <> '' then
          pTmpOffice^.HostPort := AHostPort;

        // OFFICE <office_num>: CREATE_TICKET
        if sCmd = 'CREATE_TICKET' then
        begin
          CreateTicket(pTmpOffice^.Id);
          UpdateVisualTickets();
        end
        else
        // OFFICE <office_num>: NEXT_TICKET
        if sCmd = 'NEXT_TICKET' then
        begin
          NextTicket(pTmpOffice^.Id);
          UpdateVisualTickets();
        end
        else
        // OFFICE <office_num>: INFO_REQ
        if sCmd = 'INFO_REQ' then
        begin
          // OFFICE 1: INFO <num> <Caption> <TicketPrefix> <IconName> <Comment>
          s := Format('INFO %d %s %s %s', [pTmpOffice^.Caption,
            pTmpOffice^.IconName, pTmpOffice^.Comment]);

          PostCmd(pTmpOffice^, s);
        end;
      end;
    end;
  end;


end;


end.

