unit OperatorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ActnList,
  Menus, StdCtrls, lNetComponents, ZhdunItems, ZhdunOperatorUnit, lNet, RFUtils,
  logger;

type

  { TFormOperator }

  TFormOperator = class(TForm)
    actExit: TAction;
    actSettings: TAction;
    actPause: TAction;
    actNextTicket: TAction;
    alOperator: TActionList;
    Button1: TButton;
    lbTotalTickets: TLabel;
    lbTotalTicketsLabel: TLabel;
    lbCurTicket: TLabel;
    lbCurTicketLabel: TLabel;
    panNoLink: TPanel;
    Timer1s: TTimer;
    Timer100ms: TTimer;
    UDPConn: TLUDPComponent;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    pmTray: TPopupMenu;
    TrayIcon: TTrayIcon;
    procedure actExitExecute(Sender: TObject);
    procedure actNextTicketExecute(Sender: TObject);
    procedure actPauseExecute(Sender: TObject);
    procedure Timer100msTimer(Sender: TObject);
    procedure Timer1sTimer(Sender: TObject);
    procedure UDPConnError(const msg: string; aSocket: TLSocket);
    procedure UDPConnReceive(aSocket: TLSocket);
  private
    FOperatorManager: TOperatorManager;
    FBtnNextLockTC: Int64;

    procedure PostCmdHandler(const S: string);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

var
  FormOperator: TFormOperator;

implementation

{$R *.lfm}

procedure UpdateLabel(ALabel: TLabel; const AText: string);
begin
  if ALabel.Caption <> AText then
    ALabel.Caption := AText;
end;

{ TFormOperator }

procedure TFormOperator.actExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TFormOperator.actNextTicketExecute(Sender: TObject);
begin
  FOperatorManager.NextTicket();
  actNextTicket.Enabled := False;
  FBtnNextLockTC := GetTickCount64();
end;

procedure TFormOperator.actPauseExecute(Sender: TObject);
begin
  FOperatorManager.Pause();
end;

procedure TFormOperator.Timer100msTimer(Sender: TObject);
begin
  UpdateLabel(lbCurTicket, IntToStr(FOperatorManager.Office.TicketNum));
  UpdateLabel(lbTotalTickets, IntToStr(FOperatorManager.Office.TicketCount));
  if (not FOperatorManager.IsConnected) <> panNoLink.Visible then
  begin
    panNoLink.Visible := (not FOperatorManager.IsConnected);
  end;

  if (not actNextTicket.Enabled)
  and (SecondsBetweenTicks(GetTickCount64(), FBtnNextLockTC) > BTN_NEXT_TIMEOUT) then
  begin
    actNextTicket.Enabled := True;
    FBtnNextLockTC := 0;
  end;
end;

procedure TFormOperator.Timer1sTimer(Sender: TObject);
begin
  FOperatorManager.Tick();
end;

procedure TFormOperator.UDPConnError(const msg: string; aSocket: TLSocket);
begin
  _LogError(msg);
end;

procedure TFormOperator.UDPConnReceive(aSocket: TLSocket);
var
  s: string;
begin
  aSocket.GetMessage(s);
  if s <> '' then
  begin
    _LogDebug(s);
    FOperatorManager.ProcessCmd(s);
  end;
end;

procedure TFormOperator.PostCmdHandler(const S: string);
var
  res: Integer;
begin
  Assert(Length(S) <= 500);
  if Length(S) <= 500 then
  begin
    if not (FOperatorManager.IsConnected) then
    begin
      UdpConn.Disconnect(True);
      UdpConn.Connect(FOperatorManager.UplinkHost, FOperatorManager.UplinkPort);
    end;
    {if not UdpConn.Connected then
      UdpConn.Connect(FOperatorManager.UplinkHost, FOperatorManager.UplinkPort);  }
    //res := UdpConn.SendMessage(S, FOperatorManager.UplinkHost + ':' + IntToStr(FOperatorManager.UplinkPort));
    res := UdpConn.SendMessage(S);
    if res = 0 then
      _LogError('SendMessage=0');
  end;
end;

procedure TFormOperator.AfterConstruction;
begin
  inherited AfterConstruction;
  FOperatorManager := TOperatorManager.Create();
  FOperatorManager.OnPostCmd := @PostCmdHandler;
  FOperatorManager.LoadConfig();

  //UdpConn.Port := FOperatorManager.UplinkPort;
  //UdpConn.Connect(FOperatorManager.UplinkHost, FOperatorManager.UplinkPort);

  TrayIcon.Icon.Assign(Application.Icon);

  FOperatorManager.PostCmd('INFO_REQ');
end;

procedure TFormOperator.BeforeDestruction;
begin
  //FOperatorManager.SaveConfig();
  FreeAndNil(FOperatorManager);
  inherited BeforeDestruction;
end;

end.

