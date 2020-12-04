unit ZhdunOperatorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZhdunItems, RFUtils, IniFiles;

const
  LINK_TIMEOUT = 30; // seconds
  BTN_NEXT_TIMEOUT = 2; // seconds

type

  { TOperatorManager }

  TOperatorManager = class
  private
    FOffice: TOffice;
    FOnPostCmd: TGetStrProc;

    FReqTimestamp: Int64;
    FStateTimestamp: Int64;
  public
    UplinkHost: string;
    UplinkPort: Integer;
    NeedUpdateInfo: Boolean;
    IsConnected: Boolean;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure LoadConfig();
    procedure SaveConfig();

    procedure Tick();

    function NextTicket(): Boolean;
    procedure Pause();

    procedure PostCmd(ACmdText: string);

    procedure ProcessCmd(ACmdText: string);

    property Office: TOffice read FOffice;

    property OnPostCmd: TGetStrProc read FOnPostCmd write FOnPostCmd;
  end;

implementation

{ TOperatorManager }

procedure TOperatorManager.AfterConstruction;
begin
  inherited AfterConstruction;

  FOffice := TOffice.Create();
  NeedUpdateInfo := True;
end;

procedure TOperatorManager.BeforeDestruction;
begin
  FreeAndNil(FOffice);
  inherited BeforeDestruction;
end;

procedure TOperatorManager.LoadConfig();
var
  ini: TMemIniFile;
  sSection: string;
  i, n: Integer;
begin
  ini := TMemIniFile.Create('zhdun_operator.ini');
  try
    sSection := 'Main';
    UplinkHost := ini.ReadString(sSection, 'UplinkHost', UplinkHost);
    UplinkPort := ini.ReadInteger(sSection, 'UplinkPort', UplinkPort);

    sSection := 'Office';
    Office.Num := ini.ReadInteger(sSection, 'Num', Office.Num);
    Office.Caption := ini.ReadString(sSection, 'Caption', Office.Caption);
    Office.Comment := ini.ReadString(sSection, 'Comment', Office.Comment);
    Office.IconName := ini.ReadString(sSection, 'IconName', Office.IconName);
    //State: TOfficeState;
    //Deleted: Boolean;

    Office.TicketPrefix := ini.ReadString(sSection, 'TicketPrefix', Office.TicketPrefix);
    Office.GroupNum := ini.ReadInteger(sSection, 'GroupNum', Office.GroupNum);

  finally
    FreeAndNil(ini);
  end;
end;

procedure TOperatorManager.SaveConfig();
var
  ini: TMemIniFile;
  sSection: string;
begin
  ini := TMemIniFile.Create('zhdun_operator.ini');
  try
    sSection := 'Main';
    ini.WriteString(sSection, 'UplinkHost', UplinkHost);
    ini.WriteInteger(sSection, 'UplinkPort', UplinkPort);

    sSection := 'Office';
    ini.WriteInteger(sSection, 'Num', Office.Num);
    ini.WriteString(sSection, 'Caption', Office.Caption);
    ini.WriteString(sSection, 'Comment', Office.Comment);
    ini.WriteString(sSection, 'IconName', Office.IconName);
    //State: TOfficeState;
    //Deleted: Boolean;

    ini.WriteString(sSection, 'TicketPrefix', Office.TicketPrefix);
    ini.WriteInteger(sSection, 'GroupNum', Office.GroupNum);
  finally
    FreeAndNil(ini);
  end;
end;

procedure TOperatorManager.Tick();
begin
  if NeedUpdateInfo then
  begin
    PostCmd('INFO_REQ');
  end
  else
  begin
    PostCmd('STATE_REQ');
  end;

  FReqTimestamp := GetTickCount64();
  if (not NeedUpdateInfo) and (SecondsBetweenTicks(FReqTimestamp, FStateTimestamp) > LINK_TIMEOUT) then
  begin
    NeedUpdateInfo := True;
    IsConnected := False;
  end;

end;

function TOperatorManager.NextTicket(): Boolean;
begin
  PostCmd('NEXT_TICKET');
  Result := True;
end;

procedure TOperatorManager.Pause();
begin
  PostCmd('PAUSE');
end;

procedure TOperatorManager.PostCmd(ACmdText: string);
var
  s: string;
begin
  s := 'OFFICE ' + IntToStr(Office.Num) + ': ' + ACmdText;
  if Assigned(OnPostCmd) then OnPostCmd(s);
end;

procedure TOperatorManager.ProcessCmd(ACmdText: string);
var
  s, ss, sCmd: string;
  iNum, iStateNum: Integer;
  TmpOffice: TOffice;
begin
  ss := ACmdText;
  sCmd := ExtractFirstWord(ss);

  // OFFICE <office_num>:
  if sCmd = 'OFFICE' then
  begin
    s := ExtractFirstWord(ss, ': ');
    iNum := StrToIntDef(s, -1);
    if (iNum <> -1) and (iNum = Office.Num) then
    begin
      sCmd := ExtractFirstWord(ss);

      // OFFICE 1: STATE <tickets_count> [ticket_num]
      if sCmd = 'STATE' then
      begin
        s := ExtractFirstWord(ss); // state_num
        iStateNum := StrToIntDef(s, 0);
        if (iStateNum >= Ord(Low(TOfficeState))) and (iStateNum <= Ord(High(TOfficeState))) then
          Office.State := TOfficeState(iStateNum);

        s := ExtractFirstWord(ss); // tickets_count
        Office.TicketCount := StrToIntDef(s, 0);

        s := ExtractFirstWord(ss); // ticket_num
        Office.TicketNum := StrToIntDef(s, 0);

        FStateTimestamp := GetTickCount64();
        IsConnected := True;
      end
      else
      // OFFICE 1: INFO <Caption> <TicketPrefix> <IconName> <Comment>
      if sCmd = 'INFO' then
      begin
        s := ExtractFirstWord(ss); // Caption
        Office.Caption := s;

        s := ExtractFirstWord(ss); // TicketPrefix
        Office.TicketPrefix := s;

        s := ExtractFirstWord(ss); // IconName
        Office.IconName := s;

        Office.Comment := Trim(ss); // Comment

        NeedUpdateInfo := False;
        IsConnected := True;
        PostCmd('STATE_REQ');
      end;
    end;

  end
  else
  // CREATE_TICKET <office_num>
  if sCmd = 'CREATE_TICKET' then
  begin
    {s := ExtractFirstWord(ss);
    iNum := StrToIntDef(s, -1);
    if iNum <> -1 then
    begin
      TmpOffice := OfficeList.GetByNum(iNum);
      if Assigned(TmpOffice) then
      begin
        CreateTicket(TmpOffice.Num);
        UpdateVisualTickets();
      end;
    end; }
  end
  else
  // NEXT_TICKET <office_num>
  if sCmd = 'NEXT_TICKET' then
  begin
    {s := ExtractFirstWord(ss);
    iNum := StrToIntDef(s, -1);
    if iNum <> -1 then
    begin
      TmpOffice := OfficeList.GetByNum(iNum);
      if Assigned(TmpOffice) then
      begin
        NextTicket(TmpOffice.Num);
        UpdateVisualTickets();
      end;
    end;  }
  end;
end;

end.

