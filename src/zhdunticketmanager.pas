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
    VisualButtons: TVisualButtonArray;

    VisTicketsArea: TRect;
    TicketSize: TPoint;
    TicketBorderSize: Integer;
    MaxVisTickets: Integer;

    VisButtonsArea: TRect;
    VisButtonSize: TPoint;
    VisButtonBorderSize: Integer;
    MaxVisButtons: Integer;

    UDPPort: Integer;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure LoadConfig();
    procedure SaveConfig();

    function CreateTicket(AOfficeId: Integer): PTicket;
    function NextTicket(AOfficeId: Integer): PTicket;

    procedure UpdateVisualTickets();
    procedure UpdateVisualButtons();

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
    UDPPort := ini.ReadInteger(sSection, 'UDPPort', UDPPort);

    // Office list
    n := ini.ReadInteger(sSection, 'OfficeCount', 0);
    for i := 1 to n do
    begin
      sSection := 'Office_' + IntToStr(i);
      if not ini.SectionExists(sSection) then
        Continue;

      OfficeItem.Init();
      OfficeItem.Num := ini.ReadInteger(sSection, 'Num', OfficeItem.Num);
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
    ini.WriteInteger(sSection, 'MaxVisTickets', MaxVisTickets);
    ini.WriteInteger(sSection, 'TicketBorderSize', TicketBorderSize);
    ini.WriteInteger(sSection, 'UDPPort', UDPPort);

    ini.WriteInteger(sSection, 'OfficeCount', OfficeList.GetCount());
    i := 0;
    n := 0;
    while OfficeList.GetByIndex(i) <> nil do
    begin
      pOfficeItem := OfficeList.GetByIndex(i);
      if not pOfficeItem^.Deleted then
      begin
        Inc(n);
        sSection := 'Office_' + IntToStr(n+1);
        ini.WriteInteger(sSection, 'Num', pOfficeItem^.Num);
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

    if pTmpOffice^.TicketCount > 1 then
      TmpVisualTicket.TicketText := TmpVisualTicket.TicketText + Format(
        '  (+%d)', [pTmpOffice^.TicketCount]);

    TmpVisualTicket.Pos := NextPos;
    TmpVisualTicket.Size := TicketSize;

    NextPos.Y := NextPos.Y + TicketSize.Y;

    TmpVisualTickets := Concat(TmpVisualTickets, [TmpVisualTicket]);
  end;
  VisualTickets := TmpVisualTickets;
end;

procedure TTicketManager.UpdateVisualButtons();
var
  TmpVisualButtons: TVisualButtonArray;
  TmpVisualButton: TVisualButton;
  //i, ii: Integer;
  OfficeIterator: TOfficeListIterator;
  pTmpOffice: POffice;
  //pTmpTicket: PTicket;
  NextPos: TPoint;
begin
  TmpVisualButtons := [];
  NextPos := VisButtonsArea.TopLeft;

  VisButtonSize.X := VisButtonsArea.Width;
  VisButtonSize.Y := (VisButtonsArea.Height div MaxVisButtons);

  OfficeIterator.Init(OfficeList);
  while OfficeIterator.Next(pTmpOffice) do
  begin
    if not pTmpOffice^.IsVisible() then
      Continue;

    //TmpVisualTicket.IsValid := True;
    //TmpVisualTicket.Marked := ;
    TmpVisualButton.OfficeNum := pTmpOffice^.Num;
    TmpVisualButton.Text := pTmpOffice^.Caption;
    TmpVisualButton.SubText := pTmpOffice^.Comment;

    TmpVisualButton.Rect.TopLeft := NextPos;
    TmpVisualButton.Rect.Width := VisButtonSize.X;
    TmpVisualButton.Rect.Height := VisButtonSize.Y;

    NextPos.Y := NextPos.Y + VisButtonSize.Y;

    TmpVisualButtons := Concat(TmpVisualButtons, [TmpVisualButton]);
  end;
  VisualButtons := TmpVisualButtons;

end;

procedure TTicketManager.UpdateOfficesStates();
var
  OfficeIterator: TOfficeListIterator;
  pTmpOffice: POffice;
  pTmpTicket: PTicket;
  s: string;
  i, TmpTicketCount: Integer;
begin
  OfficeIterator.Init(OfficeList);
  while OfficeIterator.Next(pTmpOffice) do
  begin
    if pTmpOffice^.State in [osUndef, osOff] then
      Continue;

    // update ticket count
    TmpTicketCount := 0;
    i := 0;
    while TicketList.GetByIndex(i) <> nil do
    begin
      pTmpTicket := TicketList.GetByIndex(i);
      if (not pTmpTicket^.Deleted) and (pTmpTicket^.OfficeId = pTmpOffice^.Id) then
      begin
        Inc(TmpTicketCount);
      end;
      Inc(i);
    end;
    pTmpOffice^.TicketCount := TmpTicketCount;

    pTmpTicket := TicketList.GetTopTicket(pTmpOffice^.Id);
    if Assigned(pTmpTicket) then
      pTmpOffice^.TicketNum := pTmpTicket^.Num
    else
      pTmpOffice^.TicketNum := 0;

    // STATE <tickets_count> [ticket_num]
    s := Format('STATE %d', [pTmpOffice^.TicketCount]);
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

        sCmd := ExtractFirstWord(ss);

        // CREATE_TICKET
        if sCmd = 'CREATE_TICKET' then
        begin
          CreateTicket(pTmpOffice^.Id);
          UpdateVisualTickets();
        end
        else
        // NEXT_TICKET
        if sCmd = 'NEXT_TICKET' then
        begin
          NextTicket(pTmpOffice^.Id);
          UpdateVisualTickets();
        end
        else
        // INFO_REQ
        if sCmd = 'INFO_REQ' then
        begin
          pTmpOffice^.State := osIdle;

          // INFO <Caption> <TicketPrefix> <IconName> <Comment>
          s := Format('INFO %s %s %s %s', [pTmpOffice^.Caption, pTmpOffice^.TicketPrefix,
            pTmpOffice^.IconName, pTmpOffice^.Comment]);

          PostCmd(pTmpOffice^, s);
        end
        else
        // STATE_REQ
        if sCmd = 'STATE_REQ' then
        begin
          UpdateOfficesStates();
          {
          // STATE <Caption> <TicketPrefix> <IconName> <Comment>
          s := Format('INFO %s %s %s %s', [pTmpOffice^.Caption, pTmpOffice^.TicketPrefix,
            pTmpOffice^.IconName, pTmpOffice^.Comment]);

          PostCmd(pTmpOffice^, s);  }
        end;
      end;
    end;
  end;


end;


end.

