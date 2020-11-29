unit MonitorForm;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef WINDOWS}windows, comobj,{$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Menus,
  Variants,
  lNetComponents, ZhdunItems, ZhdunTicketManager, FPCanvas, lNet, RFUtils,
  logger;

type

  { TFormMonitor }

  TFormMonitor = class(TForm)
    miListVoices: TMenuItem;
    pmMain: TPopupMenu;
    UDPUplink: TLUDPComponent;
    UDPListener: TLUDPComponent;
    PaintBox: TPaintBox;
    Timer1000ms: TTimer;
    procedure FormResize(Sender: TObject);
    procedure miListVoicesClick(Sender: TObject);
    procedure UDPListenerError(const msg: string; aSocket: TLSocket);
    procedure UDPListenerReceive(aSocket: TLSocket);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure Timer1000msTimer(Sender: TObject);
    procedure UDPUplinkError(const msg: string; aSocket: TLSocket);
    procedure UDPUplinkReceive(aSocket: TLSocket);
  private
    FTicketManager: TTicketManager;

    FBGFileName: string;
    FBGImage: TImage;

    FHeaderText: string;

    FNeedRestartListener: Boolean;
    FSpVoice: Variant;

    procedure AfterResize();

    procedure DrawBackground(AC: TCanvas);
    procedure DrawHeader(AC: TCanvas);
    procedure DrawOffice(AC: TCanvas; const AOffice: TVisualOffice);
    procedure DrawButton(AC: TCanvas; const AButton: TVisualButton);
    procedure DrawTicket(AC: TCanvas; const ATicket: TVisualTicket);

    procedure ProcessClick(X, Y: Integer);
    procedure StartListener();

    procedure PrepareVoice();

    procedure OnSendCmdHandler(const ACmdText, AHostPort: string);
    procedure OnUplinkSendCmdHandler(const ACmdText, AHostPort: string);

    procedure OnSpeechText(const S: string);
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    procedure UpdateView();

    procedure TestTickets();
  end;

var
  FormMonitor: TFormMonitor;

const
  csHeaderText = 'СУО "Ждун" 2020 (С) serbod.com';
  csTicketNoteText = 'Запишите номер билета';

implementation

{$R *.lfm}

{ TFormMonitor }

procedure TFormMonitor.PaintBoxPaint(Sender: TObject);
var
  c: TCanvas;
  vt: TVisualOffice;
  i: Integer;
begin
  c := PaintBox.Canvas;
  DrawBackground(c);
  DrawHeader(c);

  {vt.Pos.X := 10;
  vt.Pos.Y := 50;
  vt.OfficeText := 'Кабинет 234';
  vt.TicketText := '123';

  DrawOffice(c, vt);}

  for i := Low(FTicketManager.VisualOffices) to High(FTicketManager.VisualOffices) do
  begin
    DrawOffice(c, FTicketManager.VisualOffices[i]);
  end;

  for i := Low(FTicketManager.VisualButtons) to High(FTicketManager.VisualButtons) do
  begin
    DrawButton(c, FTicketManager.VisualButtons[i]);
  end;

  if FTicketManager.VisTicket.Visible then
  begin
    DrawTicket(c, FTicketManager.VisTicket);
  end;
end;

procedure TFormMonitor.Timer1000msTimer(Sender: TObject);
begin
  UpdateView();
  if FNeedRestartListener then
  begin
    FNeedRestartListener := False;
    StartListener();
  end;
end;

procedure TFormMonitor.UDPUplinkError(const msg: string; aSocket: TLSocket);
begin
  Log.LogError(msg, 'Uplink');
end;

procedure TFormMonitor.UDPUplinkReceive(aSocket: TLSocket);
var
  s, sHostPort: string;
begin
  aSocket.GetMessage(s);
  sHostPort := aSocket.PeerAddress + ':' + IntToStr(aSocket.PeerPort);
  if s <> '' then
  begin
    Log.LogStatus(s, 'Uplink');
    FTicketManager.ProcessCmd(s, sHostPort);
  end;
end;

procedure TFormMonitor.FormResize(Sender: TObject);
begin
  AfterResize();
end;

procedure TFormMonitor.miListVoicesClick(Sender: TObject);
var
  SpVoice, SpVoicesList, SpVoiceToken: Variant;
  //VoiceString: WideString; // WideString must be used to assign variable for speech to function, can be Global.
  i: Integer;
  sl: TStringList;
begin
  SpVoice := CreateOleObject('SAPI.SpVoice'); // Can be assigned in form.create
  //VoiceString := S;              // variable assignment
  //SpVoice.Speak(VoiceString, 0);
  sl := TStringList.Create();
  try
    SpVoicesList := SpVoice.GetVoices();
    for i := 0 to SpVoicesList.Count-1 do
    begin
      SpVoiceToken := SpVoicesList.Item(i);
      sl.Add(SpVoiceToken.GetDescription());
    end;

    ShowMessage(sl.Text);

  finally
    sl.Free();
  end;
end;

procedure TFormMonitor.UDPListenerError(const msg: string; aSocket: TLSocket);
begin
  Log.LogError(msg, 'Listener');
  FNeedRestartListener := True;
end;

procedure TFormMonitor.UDPListenerReceive(aSocket: TLSocket);
var
  s, sHostPort: string;
begin
  aSocket.GetMessage(s);
  sHostPort := aSocket.PeerAddress + ':' + IntToStr(aSocket.PeerPort);
  if s <> '' then
  begin
    Log.LogStatus(s, 'Listener');
    FTicketManager.ProcessCmd(s, sHostPort);
  end;
end;

procedure TFormMonitor.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ProcessClick(X, Y);
end;

procedure TFormMonitor.AfterResize();
begin
  // x = 10 .. (2/3) - 20
  FTicketManager.VisOfficesArea.Left := 10;
  FTicketManager.VisOfficesArea.Right := ((Width div 3) * 2) - 20;
  // y = 50 .. Max-100
  FTicketManager.VisOfficesArea.Top := 50;
  FTicketManager.VisOfficesArea.Bottom := Height - 50;


  // x = (2/3)+10 .. Max-10
  FTicketManager.VisButtonsArea.Left := ((Width div 3) * 2) + 10;
  FTicketManager.VisButtonsArea.Right := Width - 10;
  // y = 50 .. Max-100
  FTicketManager.VisButtonsArea.Top := 50;
  FTicketManager.VisButtonsArea.Bottom := Height - 50;

end;

procedure TFormMonitor.DrawBackground(AC: TCanvas);
begin
  // background
  if (FBGFileName <> '') and Assigned(FBGImage) then
  begin
    // image
    //AC.CopyRect(AC.ClipRect, FBGImage.Canvas, FBGImage.Canvas.ClipRect);
    //FBGImage.Picture.Bitmap.;
    AC.Draw(0, 0, FBGImage.Picture.Graphic);
  end
  else
  begin
    // color fill
    AC.Brush.Style := bsSolid;
    AC.Brush.Color := clMoneyGreen;
    AC.FillRect(AC.ClipRect);
  end;
end;

procedure TFormMonitor.DrawHeader(AC: TCanvas);
var
  sTime: string;
  te: TSize;
begin
  AC.Brush.Style := bsClear;
  AC.Font.Name := 'Tahoma';
  AC.Font.Size := 10;
  //AC.Font.Quality := fqCleartypeNatural;
  AC.Font.Quality := fqAntialiased;

  AC.Font.Color := clLtGray;
  AC.TextOut(5, 5, FHeaderText);

  AC.Font.Color := clBlack;
  AC.TextOut(4, 4, FHeaderText);

  // == clock
  sTime := FormatDateTime('HH:NN:SS', Now());
  AC.Font.Name := 'Terminal';
  AC.Font.Size := Height div 25;
  te := AC.TextExtent(sTime);

  AC.Font.Color := clLtGray;
  AC.TextOut(Width - te.cx - 5, 5, sTime);

  AC.Font.Color := clBlack;
  AC.TextOut(Width - te.cx - 4, 4, sTime);
end;

{Procedure SetAlpha(bmp: TBitMap; Alpha: Byte; R: TRect);
type
  pRGBQuadArray = ^TRGBQuadArray;
  TRGBQuadArray = ARRAY [0 .. $EFFFFFF] OF TRGBQuad;
var
  pscanLine32: pRGBQuadArray;
  i, j: Integer;
begin
  bmp.PixelFormat := pf32Bit;
  bmp.HandleType := bmDIB;
  //bmp.ignorepalette := true;
  //bmp.alphaformat := afDefined;
  for i := 0 to bmp.Height - 1 do
  begin
    pscanLine32 := bmp.Scanline[i];
    for j := 0 to bmp.Width - 1 do
    begin
      if (j >= R.Left) and (j <= R.Right) and (i >= R.Top) and (i <= R.Bottom) then
      begin
        pscanLine32[j].rgbReserved := 0;
        pscanLine32[j].rgbBlue := 0;
        pscanLine32[j].rgbRed := 0;
        pscanLine32[j].rgbGreen := 0;
      end
      else
      begin
        pscanLine32[j].rgbReserved := Alpha;
        pscanLine32[j].rgbBlue := Alpha;
        pscanLine32[j].rgbRed := Alpha;
        pscanLine32[j].rgbGreen := Alpha;
      end;
    end;
  end;
end; }

procedure TFormMonitor.DrawOffice(AC: TCanvas; const AOffice: TVisualOffice);
var
  r: TRect;
  s: string;
  rr: Integer;
  iYShift: Integer;
begin
  // frame
  AC.DrawingMode := dmAlphaBlend;

  //AC.Brush.Style := bsSolid;
  AC.Brush.Style := bsClear;
  AC.Brush.Color := clWhite;

  r := AOffice.Rect;
  //AC.FillRect(r);
  rr := Min((r.Width div 5), (r.Height div 5));
  AC.RoundRect(r, rr, rr);

  // == Office text
  AC.Brush.Style := bsClear;
  AC.Font.Name := 'Tahoma';
  AC.Font.Size := r.Height div 2;
  //AC.Font.Quality := fqCleartypeNatural;
  AC.Font.Quality := fqAntialiased;

  //AC.Font.Color := clLtGray;
  //AC.TextOut(5, 5, FHeaderText);

  iYShift := (AC.Font.Size div 3);

  AC.Font.Color := clBlack;
  AC.TextOut(r.Left + 10, r.Top + iYShift, AOffice.OfficeText);

  AC.Font.Color := clBlack;
  AC.TextOut(r.Left + ((r.Width div 3) * 2), r.Top + iYShift, AOffice.TicketText);
end;

procedure TFormMonitor.DrawButton(AC: TCanvas; const AButton: TVisualButton);
var
  r: TRect;
  s: string;
  rr: Integer;
  ts: TTextStyle;
begin
  // frame
  AC.DrawingMode := dmAlphaBlend;

  //AC.Brush.Style := bsSolid;
  AC.Brush.Style := bsClear;
  AC.Brush.Color := clWhite;

  r := AButton.Rect;
  //AC.FillRect(r);
  rr := Min((r.Width div 5), (r.Height div 5));
  AC.RoundRect(r, rr, rr);

  if AButton.Marked then
  begin
    // border for selected item
    AC.Pen.Style := psDash;
    AC.Pen.Color := clBlue;
    AC.Pen.Width := 4;
    AC.Rectangle(R);
  end;

  // shrink rect for text
  rr := Min((r.Width div 10), (r.Height div 10));
  r.Left := r.Left + (rr div 2);
  r.Width := r.Width - rr;
  r.Top := r.Top + (rr div 2);
  r.Height := r.Height - rr;

  // == Office text
  AC.Brush.Style := bsClear;
  AC.Font.Name := 'Tahoma';
  AC.Font.Size := r.Height div 4;
  //AC.Font.Quality := fqCleartypeNatural;
  AC.Font.Quality := fqAntialiased;

  //AC.Font.Color := clLtGray;
  //AC.TextOut(5, 5, FHeaderText);

  // text style
  ts := AC.TextStyle;
  ts.SingleLine := False;
  ts.Wordbreak := True;
  //ts.Alignment := taLeftJustify;
  ts.Layout := tlTop;
  ts.Alignment := taCenter;
  //ts.Clipping := False;
  //ts.SystemFont := True;

  AC.Font.Color := clBlack;
  //AC.TextOut(r.Left + 10, r.Top + 10, AButton.Text);
  AC.TextRect(r, r.Left, r.Top, AButton.Text, ts);

  if AButton.SubText <> '' then
  begin
    AC.Font.Color := clBlack;
    AC.Font.Size := r.Height div 8;

    //AC.TextOut(r.Left + 10, r.Top + ((r.Height div 3) * 2), AButton.SubText);

    ts.Layout := tlBottom;
    AC.TextRect(r, r.Left, r.Top, AButton.SubText, ts);
  end;
end;

procedure TFormMonitor.DrawTicket(AC: TCanvas; const ATicket: TVisualTicket);
var
  r: TRect;
  te, teo: TSize;
  x, y, nx, ny, rr: Integer;
  s: string;
  ts: TTextStyle;
begin
  // frame
  AC.DrawingMode := dmAlphaBlend;

  //AC.Brush.Style := bsSolid;
  AC.Brush.Style := bsClear;
  AC.Brush.Color := clWhite;

  r := ATicket.Rect;
  rr := Min((r.Width div 5), (r.Height div 10));
  //AC.FillRect(r);
  AC.RoundRect(r, rr, rr);

  // shrink rect for text
  rr := Min((r.Width div 10), (r.Height div 10));
  r.Left := r.Left + (rr div 2);
  r.Width := r.Width - rr;
  r.Top := r.Top + (rr div 2);
  r.Height := r.Height - rr;

  // text style
  ts := AC.TextStyle;
  ts.SingleLine := False;
  ts.Wordbreak := True;
  ts.Layout := tlTop;
  ts.Alignment := taCenter;

  // == Office text
  AC.Brush.Style := bsClear;
  AC.Font.Name := 'Tahoma';
  AC.Font.Size := r.Width div 10;
  //AC.Font.Quality := fqCleartypeNatural;
  AC.Font.Quality := fqAntialiased;

  //AC.Font.Color := clLtGray;
  //AC.TextOut(5, 5, FHeaderText);

  AC.Font.Color := clBlack;
  teo := AC.TextExtent(ATicket.OfficeText);
  //AC.TextOut(r.Left + 10, r.Top + 10, ATicket.OfficeText);
  AC.TextRect(r, r.Left + 10, r.Top + 10, ATicket.OfficeText, ts);

  // === ticket text
  AC.Brush.Style := bsClear;
  AC.Font.Name := 'Tahoma';
  AC.Font.Size := r.Width div 5;
  //AC.Font.Quality := fqCleartypeNatural;
  AC.Font.Quality := fqAntialiased;

  //AC.Font.Color := clLtGray;
  //AC.TextOut(5, 5, FHeaderText);

  AC.Font.Color := clBlack;

  te := AC.TextExtent(ATicket.TicketText);
  x := r.Left + ((ATicket.Rect.Width - te.cx) div 2);
  y := r.Top + 10 + teo.Height + 10;
  //AC.TextOut(x, y, ATicket.TicketText);
  AC.TextRect(r, x, y, ATicket.TicketText, ts);

  // === note text
  AC.Brush.Style := bsClear;
  AC.Font.Name := 'Tahoma';
  AC.Font.Size := r.Width div 10;
  //AC.Font.Quality := fqCleartypeNatural;
  AC.Font.Quality := fqAntialiased;

  //AC.Font.Color := clLtGray;
  //AC.TextOut(5, 5, FHeaderText);

  AC.Font.Color := clBlack;

  //ts := AC.TextExtent(csTicketNoteText);
  nx := r.Left + 10;
  ny := y + te.Height + 10;
  //AC.TextOut(nx, ny, csTicketNoteText);
  AC.TextRect(r, nx, ny, csTicketNoteText, ts);

end;

procedure TFormMonitor.ProcessClick(X, Y: Integer);
var
  i: Integer;
  pTmpVisButton: PVisualButton;
  OfficeItem: TOffice;
  TmpTicket: TTicket;

begin
  for i := Low(FTicketManager.VisualButtons) to High(FTicketManager.VisualButtons) do
  begin
    pTmpVisButton := Addr(FTicketManager.VisualButtons[i]);
    if pTmpVisButton^.Rect.Contains(Point(X, Y)) then
    begin
      OfficeItem := FTicketManager.OfficeList.GetByNum(pTmpVisButton^.OfficeNum);
      if Assigned(OfficeItem) then
      begin
        TmpTicket := FTicketManager.CreateTicket(OfficeItem.Num);
        if Assigned(TmpTicket) then
        begin
          FTicketManager.VisTicket.OfficeNum := TmpTicket.OfficeNum;
          FTicketManager.VisTicket.TicketNum := TmpTicket.Num;
          FTicketManager.VisTicket.TimeCreate := TmpTicket.TimeCreate;

          FTicketManager.VisTicket.OfficeText := OfficeItem.Caption;
          FTicketManager.VisTicket.TicketText := TmpTicket.Caption;
          FTicketManager.VisTicket.Visible := True;
        end;
      end;
    end;
  end;
end;

procedure TFormMonitor.StartListener();
begin
  UDPListener.Listen(FTicketManager.UDPPort);
end;

procedure TFormMonitor.PrepareVoice();
var
  SpVoicesList, SpVoiceToken: Variant;
  s: string;
  i: Integer;
begin
  FSpVoice := CreateOleObject('SAPI.SpVoice');

  {$ifdef WIN32}
  SpVoicesList := FSpVoice.GetVoices();
  for i := 0 to SpVoicesList.Count-1 do
  begin
    //SpVoiceToken := SpVoicesList.Item(i);
    s := SpVoicesList.Item(i).GetDescription();
    if Pos('Russian', s) > 0 then
    begin
      FSpVoice.Voice := SpVoicesList.Item(i);
      //FSpVoice.SetVoice(SpVoiceToken);
    end;
  end;
  {$endif}
end;

procedure TFormMonitor.OnSendCmdHandler(const ACmdText, AHostPort: string);
begin
  if (ACmdText <> '') and (AHostPort <> '') then
  begin
    UDPListener.SendMessage(ACmdText, AHostPort);
  end;
end;

procedure TFormMonitor.OnUplinkSendCmdHandler(const ACmdText,
  AHostPort: string);
begin
  Assert(Length(ACmdText) <= 500);
  if Length(ACmdText) <= 500 then
  begin
    if not (FTicketManager.UplinkConnected) then
    begin
      UDPUplink.Disconnect(True);
      UDPUplink.Connect(FTicketManager.UplinkHost, FTicketManager.UplinkPort);
    end;
    UDPUplink.SendMessage(ACmdText, FTicketManager.UplinkHost + ':' + IntToStr(FTicketManager.UplinkPort));
  end;
end;

procedure TFormMonitor.OnSpeechText(const S: string);
const
  SVSFDefault = 0;
  SVSFlagsAsync = 1;
  SVSFPurgeBeforeSpeak = 2;
  SVSFIsFilename = 4;
var
  VoiceString: WideString; // WideString must be used to assign variable for speech to function, can be Global.
begin
  if VarIsEmpty(FSpVoice) then Exit;
  VoiceString := S;              // variable assignment
  FSpVoice.Speak(VoiceString, SVSFlagsAsync);
end;

procedure TFormMonitor.AfterConstruction;
begin
  inherited AfterConstruction;

  FTicketManager := TTicketManager.Create;

  FTicketManager.MaxVisOffices := 6;
  FTicketManager.VisOfficeBorderSize := 5;

  FTicketManager.VisButtonBorderSize := 5;
  FTicketManager.MaxVisButtons := 3;

  FTicketManager.UDPPort := 4444;
  FTicketManager.OnSendCmd := @OnSendCmdHandler;
  FTicketManager.OnSpeechText := @OnSpeechText;

  FTicketManager.LoadConfig();

  FHeaderText := csHeaderText;
  if FileExists('bg.png') then
  begin
    FBGFileName := 'bg.png';
    FBGImage := TImage.Create(Self);
    try
      FBGImage.Picture.LoadFromFile(FBGFileName);
    except
      FreeAndNil(FBGImage);
      FBGFileName := '';
    end;
  end
  else
    FBGFileName := '';

  PrepareVoice();
  StartListener();

  TestTickets();
end;

procedure TFormMonitor.BeforeDestruction;
begin
  FreeAndNil(FTicketManager);
  inherited BeforeDestruction;
end;

procedure TFormMonitor.UpdateView();
begin
  FTicketManager.UpdateVisualOffices();
  FTicketManager.UpdateVisualButtons();
  Invalidate();
end;

procedure TFormMonitor.TestTickets();
var
  OfficeIterator: TOfficeListIterator;
  OfficeItem: TOffice;
begin
  OfficeIterator.Init(FTicketManager.OfficeList);
  while OfficeIterator.Next(OfficeItem) do
  begin
    FTicketManager.CreateTicket(OfficeItem.Num);
    FTicketManager.CreateTicket(OfficeItem.Num);
    FTicketManager.CreateTicket(OfficeItem.Num);

    OfficeItem.State := osIdle;
  end;

end;

end.

