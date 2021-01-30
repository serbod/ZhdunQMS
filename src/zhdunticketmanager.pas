{ ZhdunQMS (C) Sergey Bodrov, 2020 }
unit ZhdunTicketManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZhdunItems, RFUtils, IniFiles;

const
  ciTicketShowTimeout = 10; // seconds
  LINK_TIMEOUT = 30; // seconds
  BTN_NEXT_TIMEOUT = 2; // seconds

type
  { TTicketManager }

  TTicketManager = class
  private
    FTicketList: TTicketList;
    FOfficeList: TOfficeList;

    FRoles: TZhdunRoles;

    FOnSendCmd: TSendCmdEvent;
    FOnUplinkSendCmd: TSendCmdEvent;
    FOnSpeechText: TGetStrProc;

  protected
    // == for Monitor role
    FReqTimestamp: Int64;
    FStateTimestamp: Int64;

  public
    // == for Monitor role
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
    FooterText: string;

    NeedUpdateInfo: Boolean;
    VoiceEnabled: Boolean;

    UplinkHost: string;
    UplinkPort: Integer;
    IsUplinkConnected: Boolean;

    // == for Server role
    UDPPort: Integer;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure LoadConfig(AFileName: string);
    procedure SaveConfig(AFileName: string);

    { Create new ticket for specified office }
    function CreateTicket(AOfficeNum: Integer): TTicket;
    { Discard current ticket and set next ticket as current for specified office }
    function NextTicket(AOfficeNum: Integer): TTicket;
    { Audio and visual notification }
    procedure AnnounceTicket(AOfficeNum: Integer);

    procedure Tick();

    procedure UpdateVisualOffices();
    procedure UpdateVisualButtons();

    procedure UpdateOfficesStates();

    procedure PostCmdToOffice(AOffice: TOffice; ACmdText: string);
    procedure PostCmd(ACmdText, AHostPort: string);
    procedure PostUplinkCmd(ACmdText: string);

    procedure ProcessCmd(ACmdText, AHostPort: string);
    //procedure ProcessUplinkCmd(ACmdText, AHostPort: string);

    property TicketList: TTicketList read FTicketList;
    property OfficeList: TOfficeList read FOfficeList;

    property Roles: TZhdunRoles read FRoles;

    property OnSendCmd: TSendCmdEvent read FOnSendCmd write FOnSendCmd;
    property OnUplinkSendCmd: TSendCmdEvent read FOnUplinkSendCmd write FOnUplinkSendCmd;
    property OnSpeechText: TGetStrProc read FOnSpeechText write FOnSpeechText;
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

procedure TTicketManager.LoadConfig(AFileName: string);
var
  ini: TMemIniFile;
  sSection: string;
  i, n: Integer;
  OfficeItem: TOffice;
begin
  ini := TMemIniFile.Create(AFileName);
  try
    sSection := 'Monitor';
    if ini.SectionExists(sSection) then
    begin
      FRoles := FRoles + [zrMonitor];

      MaxVisOffices := ini.ReadInteger(sSection, 'MaxVisOffices', MaxVisOffices);
      VisOfficeBorderSize := ini.ReadInteger(sSection, 'OfficeBorderSize', VisOfficeBorderSize);

      UplinkPort := ini.ReadInteger(sSection, 'UplinkPort', UplinkPort);
      UplinkHost := ini.ReadString(sSection, 'UplinkHost', UplinkHost);

      VoiceEnabled := ini.ReadBool(sSection, 'VoiceEnabled', False);
    end;

    sSection := 'Kiosk';
    if ini.SectionExists(sSection) then
    begin
      FRoles := FRoles + [zrKiosk];

      MaxVisButtons := ini.ReadInteger(sSection, 'MaxVisButtons', MaxVisButtons);
      VisButtonBorderSize := ini.ReadInteger(sSection, 'VisButtonBorderSize', VisButtonBorderSize);
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

procedure TTicketManager.SaveConfig(AFileName: string);
var
  ini: TMemIniFile;
  sSection: string;
  n: Integer;
  OfficeIterator: TOfficeListIterator;
  OfficeItem: TOffice;
begin
  ini := TMemIniFile.Create(AFileName);
  try
    if zrMonitor in FRoles then
    begin
      sSection := 'Monitor';
      ini.WriteInteger(sSection, 'MaxVisOffices', MaxVisOffices);
      ini.WriteInteger(sSection, 'OfficeBorderSize', VisOfficeBorderSize);
      ini.WriteBool(sSection, 'VoiceEnabled', VoiceEnabled);

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
  Office: TOffice;
  s: string;
begin
  Result := nil;
  Assert(AOfficeNum >= 0);

  Ticket := TicketList.GetTopTicket(AOfficeNum);
  if not Assigned(Ticket) then
    Exit;

  Ticket.Deleted := True;
  //TicketList.Delete(Ticket.Id);

  Result := TicketList.GetTopTicket(AOfficeNum);

  Office := OfficeList.GetByNum(AOfficeNum);
  if Assigned(Office) then
  begin
    if Assigned(Result) then
      Office.TicketNum := Result.Num
    else
      Office.TicketNum := 0;
  end;

  if Assigned(Result) then
  begin
    AnnounceTicket(AOfficeNum);
  end;
end;

procedure TTicketManager.AnnounceTicket(AOfficeNum: Integer);
var
  Office: TOffice;
  s, ss: string;
begin
  Office := OfficeList.GetByNum(AOfficeNum);
  if Assigned(Office) then
  begin
    Office.AnnounceTimestamp := GetTickCount64();
    Office.IsMarked := True;

    s := Office.TicketPrefix + IntToStr(Office.TicketNum);

    //s := 'office: ' + Office.Caption + ', next ticket number: ' + Result.Caption;
    //s := Format('Билет номер: %s, пройдите в кабинет номер: %s', [Result.Caption, Office.Caption]);
    ss := Format('Билет номер: %s, пройдите в кабинет номер: %d', [s, Office.Num]);
    if VoiceEnabled and Assigned(OnSpeechText) then
      OnSpeechText(ss);

    FooterText := ss;
  end;
end;

procedure TTicketManager.Tick();
begin
  if (zrMonitor in Roles) and (not (zrServer in Roles)) then
  begin
    if NeedUpdateInfo then
    begin
      PostUplinkCmd('INFO_REQ');
    end
    else
    begin
      PostUplinkCmd('STATE_REQ');
    end;

    FReqTimestamp := GetTickCount64();
    if (not NeedUpdateInfo) and (SecondsBetweenTicks(FReqTimestamp, FStateTimestamp) > LINK_TIMEOUT) then
    begin
      NeedUpdateInfo := True;
      IsUplinkConnected := False;
    end;
  end;
end;

procedure TTicketManager.UpdateVisualOffices();
var
  TmpVisualOffices: TVisualOfficeArray;
  TmpVisualOffice: TVisualOffice;
  OfficeIterator: TOfficeListIterator;
  TmpOffice: TOffice;
  //TmpTicket: TTicket;
  NextPos: TPoint;
  TimeDiff: Int64;
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

    TmpVisualOffice.Marked := False;
    TmpVisualOffice.State := TmpOffice.State;
    //TmpTicket := TicketList.GetTopTicket(TmpOffice.Num);
    if TmpOffice.TicketNum > 0 then
    begin
      TmpVisualOffice.TicketNum := TmpOffice.TicketNum;
      //TmpVisualOffice.TicketText := TmpTicket.Caption;
      TmpVisualOffice.TicketText := TmpOffice.TicketPrefix + IntToStr(TmpOffice.TicketNum);
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
    if TmpOffice.TicketNum <> 0 then
      TimeDiff := TickDiff(GetTickCount, TmpOffice.AnnounceTimestamp)
    else
      TimeDiff := 0;

    if (TimeDiff > 0) and (TimeDiff < (ciTicketShowTimeout * 1000)) then
    begin
      TmpVisualOffice.Marked := ((TimeDiff mod 500) > 250);
    end;
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

    if zrServer in Roles then
    begin
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
    end;

    {
    // STATE <tickets_count> [ticket_num]
    s := Format('STATE %d', [TmpOffice.TicketCount]);
    if TmpOffice.TicketCount > 0 then
      s := s + ' ' + IntToStr(TmpOffice.TicketNum);

    PostCmdToOffice(TmpOffice, s);  }
  end;
end;

procedure TTicketManager.PostCmdToOffice(AOffice: TOffice; ACmdText: string);
var
  s: string;
begin
  s := 'OFFICE ' + IntToStr(AOffice.Num) + ': ' + ACmdText;
  if Assigned(OnSendCmd) then OnSendCmd(s, AOffice.HostPort);
end;

procedure TTicketManager.PostCmd(ACmdText, AHostPort: string);
begin
  if Assigned(OnSendCmd) then OnSendCmd(ACmdText, AHostPort);
end;

procedure TTicketManager.PostUplinkCmd(ACmdText: string);
begin
  if Assigned(OnUplinkSendCmd) then OnUplinkSendCmd(ACmdText, '');
end;

procedure TTicketManager.ProcessCmd(ACmdText, AHostPort: string);
var
  s, ss, sCmd, sOut: string;
  iNum, iStateNum, iTicketNum: Integer;
  TmpOffice: TOffice;
  OfficeIterator: TOfficeListIterator;
  IsNeedAnnounce: Boolean;
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
      if not Assigned(TmpOffice) and (zrMonitor in Roles) then
      begin
        TmpOffice := TOffice.Create();
        TmpOffice.Num := iNum;
        OfficeList.Add(TmpOffice);
      end;

      if Assigned(TmpOffice) then
      begin
        if AHostPort <> '' then
          TmpOffice.HostPort := AHostPort;

        sCmd := ExtractFirstWord(ss);

        // OFFICE 1: CREATE_TICKET
        if sCmd = 'CREATE_TICKET' then
        begin
          CreateTicket(TmpOffice.Num);
          UpdateVisualOffices();
        end
        else
        // OFFICE 1: NEXT_TICKET
        if sCmd = 'NEXT_TICKET' then
        begin
          NextTicket(TmpOffice.Num);
          UpdateVisualOffices();
        end
        else
        // OFFICE 1: INFO_REQ
        if sCmd = 'INFO_REQ' then
        begin
          TmpOffice.State := osIdle;

          // INFO <Caption> <TicketPrefix> <IconName> <Comment>
          s := Format('INFO %s %s %s %s', [TmpOffice.Caption, TmpOffice.TicketPrefix,
            TmpOffice.IconName, TmpOffice.Comment]);

          PostCmdToOffice(TmpOffice, s);
        end
        else
        // OFFICE 1: STATE_REQ
        if sCmd = 'STATE_REQ' then
        begin
          UpdateOfficesStates();
          // STATE <state_num> <tickets_count> [ticket_num]
          s := Format('STATE %d %d', [Ord(TmpOffice.State), TmpOffice.TicketCount]);
          if TmpOffice.TicketCount > 0 then
            s := s + ' ' + IntToStr(TmpOffice.TicketNum);

          PostCmdToOffice(TmpOffice, s);
        end
        else
        // === commands from uplink
        // OFFICE 1: STATE <state_num> <tickets_count> [ticket_num]
        if sCmd = 'STATE' then
        begin
          s := ExtractFirstWord(ss); // state_num
          iStateNum := StrToIntDef(s, 0);
          if (iStateNum >= Ord(Low(TOfficeState))) and (iStateNum <= Ord(High(TOfficeState))) then
            TmpOffice.State := TOfficeState(iStateNum);

          s := ExtractFirstWord(ss); // tickets_count
          TmpOffice.TicketCount := StrToIntDef(s, 0);

          s := ExtractFirstWord(ss); // ticket_num
          iTicketNum := StrToIntDef(s, 0);
          IsNeedAnnounce := (TmpOffice.TicketNum <> iTicketNum);
          TmpOffice.TicketNum := iTicketNum;

          FStateTimestamp := GetTickCount64();
          IsUplinkConnected := True;
          UpdateOfficesStates();
          if IsNeedAnnounce then
            AnnounceTicket(TmpOffice.Num);
        end
        else
        // OFFICE 1: INFO <Caption> <TicketPrefix> <IconName> <Comment>
        if sCmd = 'INFO' then
        begin
          s := ExtractFirstWord(ss); // Caption
          TmpOffice.Caption := s;

          s := ExtractFirstWord(ss); // TicketPrefix
          TmpOffice.TicketPrefix := s;

          s := ExtractFirstWord(ss); // IconName
          TmpOffice.IconName := s;

          TmpOffice.Comment := Trim(ss); // Comment

          NeedUpdateInfo := False;
          IsUplinkConnected := True;
          PostUplinkCmd('STATE_REQ');

          if zrMonitor in Roles then
            TmpOffice.State := osIdle;
        end;
      end;
    end;
  end
  else
  // INFO_REQ
  if sCmd = 'INFO_REQ' then
  begin
    // for all offices
    sOut := '';
    OfficeIterator.Init(OfficeList);
    while OfficeIterator.Next(TmpOffice) do
    begin
      // OFFICE <office_num>: INFO <Caption> <TicketPrefix> <IconName> <Comment>
      s := Format('OFFICE %d: INFO %s %s %s %s',
        [TmpOffice.Num, TmpOffice.Caption, TmpOffice.TicketPrefix,
        TmpOffice.IconName, TmpOffice.Comment]);
      sOut := sOut + s + sLineBreak;
    end;
    PostCmd(sOut, AHostPort);
  end
  else
  if sCmd = 'STATE_REQ' then
  begin
    UpdateOfficesStates();
    // for all offices
    sOut := '';
    OfficeIterator.Init(OfficeList);
    while OfficeIterator.Next(TmpOffice) do
    begin
      // OFFICE <office_num>: STATE <state_num> <tickets_count> [ticket_num]
      s := Format('OFFICE %d: STATE %d %d', [TmpOffice.Num, Ord(TmpOffice.State), TmpOffice.TicketCount]);
      if TmpOffice.TicketCount > 0 then
        s := s + ' ' + IntToStr(TmpOffice.TicketNum);
      sOut := sOut + s + sLineBreak;
    end;
    PostCmd(sOut, AHostPort);
  end;


end;


end.

