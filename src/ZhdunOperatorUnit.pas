unit ZhdunOperatorUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ZhdunItems, RFUtils, IniFiles;

type

  { TOperatorManager }

  TOperatorManager = class
  private
    FOnPostCmd: TGetStrProc;
  public
    Office: TOffice;
    MonitorHost: string;
    MonitorPort: Integer;
    NeedUpdateInfo: Boolean;

    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure LoadConfig();
    procedure SaveConfig();

    function NextTicket(): PTicket;
    procedure Pause();

    procedure PostCmd(ACmdText: string);

    procedure ProcessCmd(ACmdText: string);

    property OnPostCmd: TGetStrProc read FOnPostCmd write FOnPostCmd;
  end;

implementation

{ TOperatorManager }

procedure TOperatorManager.AfterConstruction;
begin
  inherited AfterConstruction;
  Office.Init();
  NeedUpdateInfo := True;
end;

procedure TOperatorManager.BeforeDestruction;
begin
  inherited BeforeDestruction;
end;

procedure TOperatorManager.LoadConfig();
var
  ini: TMemIniFile;
  sSection: string;
  i, n: Integer;
  OfficeItem: TOffice;
begin
  ini := TMemIniFile.Create('zhdun_operator.ini');
  try
    sSection := 'Main';
    MonitorHost := ini.ReadString(sSection, 'MonitorHost', MonitorHost);
    MonitorPort := ini.ReadInteger(sSection, 'MonitorPort', MonitorPort);

    sSection := 'Office';
    Office.Num := ini.ReadInteger(sSection, 'Num', Office.Num);
    Office.Caption := ini.ReadString(sSection, 'Caption', Office.Caption);
    Office.Comment := ini.ReadString(sSection, 'Comment', Office.Comment);
    Office.IconName := ini.ReadString(sSection, 'IconName', Office.IconName);
    //State: TOfficeState;
    //Deleted: Boolean;

    Office.TicketPrefix := ini.ReadString(sSection, 'TicketPrefix', Office.TicketPrefix);
    Office.GroupId := ini.ReadInteger(sSection, 'GroupId', Office.GroupId);

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
    ini.WriteString(sSection, 'MonitorHost', MonitorHost);
    ini.WriteInteger(sSection, 'MonitorPort', MonitorPort);

    sSection := 'Office';
    ini.WriteInteger(sSection, 'Num', Office.Num);
    ini.WriteString(sSection, 'Caption', Office.Caption);
    ini.WriteString(sSection, 'Comment', Office.Comment);
    ini.WriteString(sSection, 'IconName', Office.IconName);
    //State: TOfficeState;
    //Deleted: Boolean;

    ini.WriteString(sSection, 'TicketPrefix', Office.TicketPrefix);
    ini.WriteInteger(sSection, 'GroupId', Office.GroupId);
  finally
    FreeAndNil(ini);
  end;
end;

function TOperatorManager.NextTicket(): PTicket;
begin
  PostCmd('NEXT_TICKET');
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
    if (iNum <> -1) and (iNum = Office.Num) then
    begin
      sCmd := ExtractFirstWord(ss);

      // OFFICE 1: STATE <tickets_count> [ticket_num]
      if sCmd = 'STATE' then
      begin
        s := ExtractFirstWord(ss); // tickets_count
        Office.TicketCount := StrToIntDef(s, 0);

        s := ExtractFirstWord(ss); // ticket_num
        Office.TicketNum := StrToIntDef(s, 0);
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
      pTmpOffice := OfficeList.GetByNum(iNum);
      if Assigned(pTmpOffice) then
      begin
        CreateTicket(pTmpOffice^.Id);
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
      pTmpOffice := OfficeList.GetByNum(iNum);
      if Assigned(pTmpOffice) then
      begin
        NextTicket(pTmpOffice^.Id);
        UpdateVisualTickets();
      end;
    end;  }
  end;
end;

end.

