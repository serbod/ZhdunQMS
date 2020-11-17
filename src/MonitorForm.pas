unit MonitorForm;

{$mode objfpc}{$H+}

interface

uses
  {$ifdef WINDOWS}windows,{$endif}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  lNetComponents, ZhdunItems, ZhdunTicketManager, FPCanvas, lNet, RFUtils;

type

  { TFormMonitor }

  TFormMonitor = class(TForm)
    LUDPComponent1: TLUDPComponent;
    PaintBox: TPaintBox;
    Timer1000ms: TTimer;
    procedure FormResize(Sender: TObject);
    procedure LUDPComponent1Receive(aSocket: TLSocket);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxPaint(Sender: TObject);
    procedure Timer1000msTimer(Sender: TObject);
  private
    FTicketManager: TTicketManager;

    FBGFileName: string;
    FBGImage: TImage;

    FHeaderText: string;

    procedure AfterResize();

    procedure DrawBackground(AC: TCanvas);
    procedure DrawHeader(AC: TCanvas);
    procedure DrawOffice(AC: TCanvas; const AOffice: TVisualOffice);
    procedure DrawButton(AC: TCanvas; const AButton: TVisualButton);
    procedure DrawTicket(AC: TCanvas; const ATicket: TVisualTicket);

    procedure ProcessClick(X, Y: Integer);

    procedure OnSendCmdHandler(const ACmdText, AHostPort: string);
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
end;

procedure TFormMonitor.FormResize(Sender: TObject);
begin
  AfterResize();
end;

procedure TFormMonitor.LUDPComponent1Receive(aSocket: TLSocket);
var
  s, sHostPort: string;
begin
  aSocket.GetMessage(s);
  sHostPort := aSocket.PeerAddress + ':' + IntToStr(aSocket.PeerPort);
  if s <> '' then
    FTicketManager.ProcessCmd(s, sHostPort);
end;

procedure TFormMonitor.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  ProcessClick(X, Y);
end;

procedure TFormMonitor.AfterResize();
begin
  // x = 10 .. (2/3) - 20
  FTicketManager.VisTicketsArea.Left := 10;
  FTicketManager.VisTicketsArea.Right := ((Width div 3) * 2) - 20;
  // y = 50 .. Max-100
  FTicketManager.VisTicketsArea.Top := 50;
  FTicketManager.VisTicketsArea.Bottom := Height - 50;


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
  iYShift: Integer;
begin
  // frame
  AC.DrawingMode := dmAlphaBlend;

  //AC.Brush.Style := bsSolid;
  AC.Brush.Style := bsClear;
  AC.Brush.Color := clWhite;

  r.TopLeft := AOffice.Pos;
  r.Width := AOffice.Size.X;
  r.Height := AOffice.Size.Y;
  AC.FillRect(r);

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
begin
  // frame
  AC.DrawingMode := dmAlphaBlend;

  //AC.Brush.Style := bsSolid;
  AC.Brush.Style := bsClear;
  AC.Brush.Color := clWhite;

  r := AButton.Rect;
  AC.FillRect(r);

  if AButton.Marked then
  begin
    // border for selected item
    AC.Pen.Style := psDash;
    AC.Pen.Color := clBlue;
    AC.Pen.Width := 4;
    AC.Rectangle(R);
  end;

  // == Office text
  AC.Brush.Style := bsClear;
  AC.Font.Name := 'Tahoma';
  AC.Font.Size := r.Height div 4;
  //AC.Font.Quality := fqCleartypeNatural;
  AC.Font.Quality := fqAntialiased;

  //AC.Font.Color := clLtGray;
  //AC.TextOut(5, 5, FHeaderText);

  AC.Font.Color := clBlack;
  AC.TextOut(r.Left + 10, r.Top + 10, AButton.Text);

  if AButton.SubText <> '' then
  begin
    AC.Font.Color := clBlack;
    AC.Font.Size := r.Height div 16;
    AC.TextOut(r.Left + 10, r.Top + ((r.Height div 3) * 2), AButton.SubText);
  end;
end;

procedure TFormMonitor.DrawTicket(AC: TCanvas; const ATicket: TVisualTicket);
var
  r: TRect;
  ts, tso: TSize;
  x, y, nx, ny: Integer;
  s: string;
begin
  // frame
  AC.DrawingMode := dmAlphaBlend;

  //AC.Brush.Style := bsSolid;
  AC.Brush.Style := bsClear;
  AC.Brush.Color := clWhite;

  r := ATicket.Rect;
  AC.FillRect(r);

  // == Office text
  AC.Brush.Style := bsClear;
  AC.Font.Name := 'Tahoma';
  AC.Font.Size := r.Width div 10;
  //AC.Font.Quality := fqCleartypeNatural;
  AC.Font.Quality := fqAntialiased;

  //AC.Font.Color := clLtGray;
  //AC.TextOut(5, 5, FHeaderText);

  AC.Font.Color := clBlack;
  tso := AC.TextExtent(ATicket.OfficeText);
  AC.TextOut(r.Left + 10, r.Top + 10, ATicket.OfficeText);

  // === ticket text
  AC.Brush.Style := bsClear;
  AC.Font.Name := 'Tahoma';
  AC.Font.Size := r.Width div 5;
  //AC.Font.Quality := fqCleartypeNatural;
  AC.Font.Quality := fqAntialiased;

  //AC.Font.Color := clLtGray;
  //AC.TextOut(5, 5, FHeaderText);

  AC.Font.Color := clBlack;

  ts := AC.TextExtent(ATicket.TicketText);
  x := r.Left + ((ATicket.Rect.Width - ts.cx) div 2);
  y := r.Top + 10 + tso.Height + 10;
  AC.TextOut(x, y, ATicket.TicketText);

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
  ny := y + ts.Height + 10;
  AC.TextOut(nx, ny, csTicketNoteText);

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

procedure TFormMonitor.OnSendCmdHandler(const ACmdText, AHostPort: string);
begin
  if (ACmdText <> '') and (AHostPort <> '') then
  begin
    LUDPComponent1.SendMessage(ACmdText, AHostPort);
  end;
end;

procedure TFormMonitor.AfterConstruction;
begin
  inherited AfterConstruction;

  FTicketManager := TTicketManager.Create;

  FTicketManager.MaxVisTickets := 6;
  FTicketManager.TicketBorderSize := 5;

  FTicketManager.VisButtonBorderSize := 5;
  FTicketManager.MaxVisButtons := 3;

  FTicketManager.UDPPort := 4444;
  FTicketManager.OnSendCmd := @OnSendCmdHandler;

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


  LUDPComponent1.Listen(FTicketManager.UDPPort);

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
  end;
end;

end.

