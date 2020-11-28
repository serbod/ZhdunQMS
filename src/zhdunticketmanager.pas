unit ZhdunTicketManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZhdunItems, RFUtils, IniFiles;

const
  ciTicketShowTimeout = 10; // seconds

type
  { TTicketManager }

  TTicketManager = class
  private
    FTicketList: TTicketList;
    FOfficeList: TOfficeList;

    FRoles: TZhdunRoles;

    FOnSendCmd: TSendCmdEvent;
    FOnUplinkSendCmd: TSendCmdEvent;

  public
    VisualOffices: TVisualOfficeArray;
    VisualButtons: TVisualButtonArray;

    VisOfficesArea: TRect;
    VisOfficeSize: TPoint;
    VisOfficeBorderSize: Integer;
    MaxVisOffices: Integer;

    VisButtonsArea: TRect;
    VisButtonSize: TPoint;
    VisButtonBorderSize: Integer;
    MaxVisButtons: Integer;

    VisTicket: TVisualTicket;

    UDPPort: Integer;

    UplinkHost: string;
    UplinkPort: Integer;
    UplinkConnected: Boolean;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure LoadConfig();
    procedure SaveConfig();

    { Create new ticket for specified office }
    function CreateTicket(AOfficeNum: Integer): TTicket;
    { Discard current ticket and set next ticket as current for specified office }
    function NextTicket(AOfficeNum: Integer): TTicket;

    procedure UpdateVisualOffices();
    procedure UpdateVisualButtons();

    procedure UpdateOfficesStates();

    procedure PostCmd(AOffice: TOffice; ACmdText: string);

    procedure ProcessCmd(ACmdText, AHostPort: string);

    property TicketList: TTicketList read FTicketList;
    property OfficeList: TOfficeList read FOfficeList;

    property Roles: TZhdunRoles read FRoles;

    property OnSendCmd: TSendCmdEvent read FOnSendCmd write FOnSendCmd;
    property OnUplinkSendCmd: TSendCmdEvent read FOnUplinkSendCmd write FOnUplinkSendCmd;
  end;


implementation

{ TTicketManager }

procedure TTicketManager.AfterConstruction;
begin
  inherited AfterConstruction;
  FTicketList := TTicketList.Create();
  FOfficeList := TOfficeList.Create();

  VisTicket.Visible := False;
  MaxVisOffices := 6;
  VisOfficeBorderSize := 5;

  VisButtonBorderSize := 5;
  MaxVisButtons := 3;

  UDPPort := 4444;
  UplinkPort := 4444;
end;

procedure TTicketManager.BeforeDestruction;
begin
  FreeAndNil(FOfficeList);
  FreeAndNil(FTicketList);
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
    sSection := 'Monitor';
    if ini.SectionExists(sSection) then
    begin
      FRoles := FRoles + [zrMonitor];

      MaxVisOffices := ini.ReadInteger(sSection, 'MaxVisOffices', MaxVisOffices);
      VisOfficeBorderSize := ini.ReadInteger(sSection, 'OfficeBorderSize', VisOfficeBorderSize);

      UplinkPort := ini.ReadInteger(sSection, 'UplinkPort', UplinkPort);
      UplinkHost := ini.ReadString(sSection, 'UplinkHost', UplinkHost);
    end;

    sSection := 'Server';
    if ini.SectionExists(sSection) then
    begin
      FRoles := FRoles + [zrServer];
      UDPPort := ini.ReadInteger(sSection, 'UDPPort', UDPPort);

      // Office list
      n := ini.ReadInteger(sSection, 'OfficeCount', 0);
      for i := 1 to n do
      begin
        sSection := 'Office_' + IntToStr(i);
        if not ini.SectionExists(sSection) then
          Continue;

        OfficeItem := TOffice.Create();
        OfficeItem.Num := ini.ReadInteger(sSection, 'Num', OfficeItem.Num);
        OfficeItem.Caption := ini.ReadString(sSection, 'Caption', OfficeItem.Caption);
        OfficeItem.Comment := ini.ReadString(sSection, 'Comment', OfficeItem.Comment);
        OfficeItem.IconName := ini.ReadString(sSection, 'IconName', OfficeItem.IconName);
        OfficeItem.TicketPrefix := ini.ReadString(sSection, 'TicketPrefix', OfficeItem.TicketPrefix);
        OfficeItem.GroupNum := ini.ReadInteger(sSection, 'GroupNum', OfficeItem.GroupNum);

        OfficeList.Add(OfficeItem);
      end;

    end;

  finally
    FreeAndNil(ini);
  end;
end;

procedure TTicketManager.SaveConfig();
var
  ini: TMemIniFile;
  sSection: string;
  n: Integer;
  OfficeIterator: TOfficeListIterator;
  OfficeItem: TOffice;
begin
  ini := TMemIniFile.Create('zhdun.ini');
  try
    if zrMonitor in FRoles then
    begin
      sSection := 'Monitor';
      ini.WriteInteger(sSection, 'MaxVisOffices', MaxVisOffices);
      ini.WriteInteger(sSection, 'OfficeBorderSize', VisOfficeBorderSize);

      ini.WriteInteger(sSection, 'UplinkPort', UplinkPort);
      ini.WriteString(sSection, 'UplinkHost', UplinkHost);
    end;

    if zrServer in FRoles then
    begin
      sSection := 'Server';
      ini.WriteInteger(sSection, 'UDPPort', UDPPort);

      n := 0;
      OfficeIterator.Init(OfficeList);
      while OfficeIterator.Next(OfficeItem) do
      begin
        Inc(n);
        sSection := 'Office_' + IntToStr(n+1);
        ini.WriteInteger(sSection, 'Num', OfficeItem.Num);
        ini.WriteString(sSection, 'Caption', OfficeItem.Caption);
        ini.WriteString(sSection, 'Comment', OfficeItem.Comment);
        ini.WriteString(sSection, 'IconName', OfficeItem.IconName);
        //State: TOfficeState;
        //Deleted: Boolean;

        ini.WriteString(sSection, 'TicketPrefix', OfficeItem.TicketPrefix);
        ini.WriteInteger(sSection, 'GroupNum', OfficeItem.GroupNum);
      end;

      ini.WriteInteger('Server', 'OfficeCount', n);
    end;

  finally
    FreeAndNil(ini);
  end;
end;

function TTicketManager.CreateTicket(AOfficeNum: Integer): TTicket;
var
  MaxNum: Integer;
  TmpOffice: TOffice;
  i: Integer;
begin
  Result := nil;
  TmpOffice := OfficeList.GetByNum(AOfficeNum);
  if not Assigned(TmpOffice) then
    Exit;

  if TmpOffice.GroupNum > 0 then
    MaxNum := TicketList.GetMaxNumForGroup(TmpOffice.GroupNum)
  else
    MaxNum := TicketList.GetMaxNum(AOfficeNum);

  Inc(MaxNum);
  // replace deleted or create new
  for i := 0 to TicketList.Count-1 do
  begin
    if TicketList.Items[i].Deleted then
      Result := TicketList.Items[i];
  end;

  if not Assigned(Result) then
  begin
    Result := TTicket.Create;
    TicketList.Add(Result);
  end;

  Result.TimeCreate := Now();
  Result.TimeStart := 0;
  Result.TimeFinish := 0;

  Result.Deleted := False;
  Result.OfficeNum := AOfficeNum;
  Result.GroupNum := TmpOffice.GroupNum;
  Result.Num := MaxNum;
  Result.IconId := 0;
  Result.Caption := TmpOffice.TicketPrefix + IntToStr(Result.Num);

end;

function TTicketManager.NextTicket(AOfficeNum: Integer): TTicket;
var
  Ticket: TTicket;
begin
  Result := nil;
  Assert(AOfficeNum >= 0);

  Ticket := TicketList.GetTopTicket(AOfficeNum);
  if not Assigned(Ticket) then
    Exit;

  Ticket.Deleted := True;
  //TicketList.Delete(Ticket.Id);

  Result := TicketList.GetTopTicket(AOfficeNum);
end;

procedure TTicketManager.UpdateVisualOffices();
var
  TmpVisualOffices: TVisualOfficeArray;
  TmpVisualOffice: TVisualOffice;
  OfficeIterator: TOfficeListIterator;
  TmpOffice: TOffice;
  TmpTicket: TTicket;
  NextPos: TPoint;
begin
  TmpVisualOffices := [];
  NextPos := VisOfficesArea.TopLeft;

  VisOfficeSize.X := VisOfficesArea.Width;
  VisOfficeSize.Y := (VisOfficesArea.Height div MaxVisOffices);

  OfficeIterator.Init(OfficeList);
  while OfficeIterator.Next(TmpOffice) do
  begin
    if not TmpOffice.IsVisible() then
      Continue;

    TmpTicket := TicketList.GetTopTicket(TmpOffice.Num);
    if Assigned(TmpTicket) then
    begin
      TmpVisualOffice.TicketNum := TmpTicket.Num;
      TmpVisualOffice.TicketText := TmpTicket.Caption;
      if TmpOffice.TicketCount > 1 then
        TmpVisualOffice.TicketText := TmpVisualOffice.TicketText + Format(
          '  (+%d)', [TmpOffice.TicketCount]);
    end
    else
    begin
      TmpVisualOffice.TicketNum := 0;
      TmpVisualOffice.TicketText := '';
    end;

    //TmpVisualOffice.IsValid := True;
    //TmpVisualOffice.Marked := ;
    TmpVisualOffice.OfficeNum := TmpOffice.Num;
    TmpVisualOffice.OfficeText := TmpOffice.Caption;
    case TmpOffice.State of
      osPaused: TmpVisualOffice.OfficeText := TmpVisualOffice.OfficeText + '  (Paused)';
      osLost: TmpVisualOffice.OfficeText := TmpVisualOffice.OfficeText + '  (Lost)';
    end;


    TmpVisualOffice.Rect.TopLeft := NextPos;
    TmpVisualOffice.Rect.Width := VisOfficeSize.X;
    TmpVisualOffice.Rect.Height := VisOfficeSize.Y;

    NextPos.Y := NextPos.Y + VisOfficeSize.Y;

    // correct size to show borders
    TmpVisualOffice.Rect.Left := TmpVisualOffice.Rect.Left + (VisOfficeBorderSize div 2);
    TmpVisualOffice.Rect.Top := TmpVisualOffice.Rect.Top + (VisOfficeBorderSize div 2);
    TmpVisualOffice.Rect.Width := TmpVisualOffice.Rect.Width - VisOfficeBorderSize;
    TmpVisualOffice.Rect.Height := TmpVisualOffice.Rect.Height - VisOfficeBorderSize;

    TmpVisualOffices := Concat(TmpVisualOffices, [TmpVisualOffice]);
  end;
  VisualOffices := TmpVisualOffices;
end;

procedure TTicketManager.UpdateVisualButtons();
var
  TmpVisualButtons: TVisualButtonArray;
  TmpVisualButton: TVisualButton;
  OfficeIterator: TOfficeListIterator;
  TmpOffice: TOffice;
  NextPos: TPoint;
begin
  TmpVisualButtons := [];
  NextPos := VisButtonsArea.TopLeft;

  if VisTicket.Visible then
  begin
    VisTicket.Rect := VisButtonsArea;
    if SecondsBetween(Now(), VisTicket.TimeCreate) > ciTicketShowTimeout then
      VisTicket.Visible := False
    else
      Exit;
  end;

  VisButtonSize.X := VisButtonsArea.Width;
  VisButtonSize.Y := (VisButtonsArea.Height div MaxVisButtons);

  OfficeIterator.Init(OfficeList);
  while OfficeIterator.Next(TmpOffice) do
  begin
    if not TmpOffice.IsVisible() then
      Continue;

    //TmpVisualTicket.IsValid := True;
    //TmpVisualTicket.Marked := ;
    TmpVisualButton.OfficeNum := TmpOffice.Num;
    TmpVisualButton.Text := TmpOffice.Caption;
    TmpVisualButton.SubText := TmpOffice.Comment;

    TmpVisualButton.IsPressed := False;

    TmpVisualButton.Rect.TopLeft := NextPos;
    TmpVisualButton.Rect.Width := VisButtonSize.X;
    TmpVisualButton.Rect.Height := VisButtonSize.Y;

    NextPos.Y := NextPos.Y + VisButtonSize.Y;

    // correct size to show borders
    TmpVisualButton.Rect.Left := TmpVisualButton.Rect.Left + (VisButtonBorderSize div 2);
    TmpVisualButton.Rect.Top := TmpVisualButton.Rect.Top + (VisButtonBorderSize div 2);
    TmpVisualButton.Rect.Width := TmpVisualButton.Rect.Width - VisButtonBorderSize;
    TmpVisualButton.Rect.Height := TmpVisualButton.Rect.Height - VisButtonBorderSize;

    TmpVisualButtons := Concat(TmpVisualButtons, [TmpVisualButton]);
  end;
  VisualButtons := TmpVisualButtons;

end;

procedure TTicketManager.UpdateOfficesStates();
var
  OfficeIterator: TOfficeListIterator;
  TmpOffice: TOffice;
  TmpTicket: TTicket;
  s: string;
  i, TmpTicketCount: Integer;
begin
  OfficeIterator.Init(OfficeList);
  while OfficeIterator.Next(TmpOffice) do
  begin
    if TmpOffice.State in [osUndef, osOff] then
      Continue;

    // update ticket count
    TmpTicketCount := 0;
    for i := 0 to TicketList.Count-1 do
    begin
      TmpTicket := TicketList.Items[i];
      if (not TmpTicket.Deleted) and (TmpTicket.OfficeNum = TmpOffice.Num) then
      begin
        Inc(TmpTicketCount);
      end;
    end;
    TmpOffice.TicketCount := TmpTicketCount;

    TmpTicket := TicketList.GetTopTicket(TmpOffice.Num);
    if Assigned(TmpTicket) then
      TmpOffice.TicketNum := TmpTicket.Num
    else
      TmpOffice.TicketNum := 0;

    // STATE <tickets_count> [ticket_num]
    s := Format('STATE %d', [TmpOffice.TicketCount]);
    if TmpOffice.TicketCount > 0 then
      s := s + ' ' + IntToStr(TmpOffice.TicketNum);

    PostCmd(TmpOffice, s);
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
  TmpOffice: TOffice;
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
      TmpOffice := OfficeList.GetByNum(iNum);
      if Assigned(TmpOffice) then
      begin
        if AHostPort <> '' then
          TmpOffice.HostPort := AHostPort;

        sCmd := ExtractFirstWord(ss);

        // CREATE_TICKET
        if sCmd = 'CREATE_TICKET' then
        begin
          CreateTicket(TmpOffice.Num);
          UpdateVisualOffices();
        end
        else
        // NEXT_TICKET
        if sCmd = 'NEXT_TICKET' then
        begin
          NextTicket(TmpOffice.Num);
          UpdateVisualOffices();
        end
        else
        // INFO_REQ
        if sCmd = 'INFO_REQ' then
        begin
          TmpOffice.State := osIdle;

          // INFO <Caption> <TicketPrefix> <IconName> <Comment>
          s := Format('INFO %s %s %s %s', [TmpOffice.Caption, TmpOffice.TicketPrefix,
            TmpOffice.IconName, TmpOffice.Comment]);

          PostCmd(TmpOffice, s);
        end
        else
        // STATE_REQ
        if sCmd = 'STATE_REQ' then
        begin
          UpdateOfficesStates();
          {
          // STATE <Caption> <TicketPrefix> <IconName> <Comment>
          s := Format('INFO %s %s %s %s', [TmpOffice.Caption, TmpOffice.TicketPrefix,
            TmpOffice.IconName, TmpOffice.Comment]);

          PostCmd(TmpOffice, s);  }
        end;
      end;
    end;
  end;


end;


end.

