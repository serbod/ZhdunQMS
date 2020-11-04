unit OperatorForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ActnList,
  Menus, StdCtrls, lNetComponents, ZhdunItems, ZhdunOperatorUnit, lNet;

type

  { TFormOperator }

  TFormOperator = class(TForm)
    actExit: TAction;
    actSettings: TAction;
    actPause: TAction;
    actNextTicket: TAction;
    alOperator: TActionList;
    Button1: TButton;
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
    procedure UDPConnReceive(aSocket: TLSocket);
  private
    FOperatorManager: TOperatorManager;

    procedure PostCmdHandler(const S: string);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  end;

var
  FormOperator: TFormOperator;

implementation

{$R *.lfm}

{ TFormOperator }

procedure TFormOperator.actExitExecute(Sender: TObject);
begin
  Close();
end;

procedure TFormOperator.actNextTicketExecute(Sender: TObject);
begin
  FOperatorManager.NextTicket();
end;

procedure TFormOperator.actPauseExecute(Sender: TObject);
begin
  FOperatorManager.Pause();
end;

procedure TFormOperator.UDPConnReceive(aSocket: TLSocket);
var
  s: string;
begin
  aSocket.GetMessage(s);
  if s <> '' then
    FOperatorManager.ProcessCmd(s);
end;

procedure TFormOperator.PostCmdHandler(const S: string);
begin
  Assert(Length(S) <= 500);
  if Length(S) <= 500 then
  begin
    UdpConn.SendMessage(S, FOperatorManager.MonitorHost);
  end;
end;

procedure TFormOperator.AfterConstruction;
begin
  inherited AfterConstruction;
  FOperatorManager := TOperatorManager.Create();
  FOperatorManager.OnPostCmd := @PostCmdHandler;
  FOperatorManager.LoadConfig();

  //UdpConn.Port := FOperatorManager.MonitorPort;
  UdpConn.Connect(FOperatorManager.MonitorHost, FOperatorManager.MonitorPort);

  TrayIcon.Icon.Assign(Application.Icon);

  FOperatorManager.PostCmd(Format('OFFICE %d: INFO_REQ', [FOperatorManager.Office.Num]));
end;

procedure TFormOperator.BeforeDestruction;
begin
  //FOperatorManager.SaveConfig();
  FreeAndNil(FOperatorManager);
  inherited BeforeDestruction;
end;

end.

