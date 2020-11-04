{-----------------------------------------------------------------------------
RF-Link

Очередь пакетов UDP, thread-safe

Copyright (c) 2009-2016 Новатех Системы Безопасности
-----------------------------------------------------------------------------}
unit UdpQueue;

interface

uses
  Classes, SimpleObj, SysUtils, WeakRefs
  {$ifdef DEBUG}, logger{$endif}
  ;

type
  { Безопасный размер пакета UDP - 512 байт. }
  TUdpPacketRec = record
    LocalPort: Integer;
    PeerPort: Integer;
    { Уровень приоритета, >= 16 - приоритетный. +8 - не заменяется, а добавляется в очередь }
    Priority: Byte;
    LocalIP: string[15];
    PeerIP: string[15];
    //Channel: AnsiString;
    DataSize: Integer;
    Data: array[0..1399] of Byte;
    ChanWeakRef: IWeakRef;
  end;

  TUdpPacketEvent = procedure(Sender: TObject; const UdpPacketRec: TUdpPacketRec) of object;

  TUdpPacketQueueItem = class(TObject)
  public
    UdpPacketRec: TUdpPacketRec;
    NextItem: TUdpPacketQueueItem;
    //PrevItem: TUdpPacketQueueItem;
    procedure Clear();
  end;

  // очередь пакетов, thread-safe, с постоянными элементами
  TUdpPacketQueue = class(TObject)
  private
    FLock: TSimpleLock;
    FCapacity: Integer;
    FCount: Integer;
    FSpareCount: Integer;
    FMaxCount: Integer;
    // голова очереди
    FHeadItem: TUdpPacketQueueItem;
    // хвост очереди
    FTailItem: TUdpPacketQueueItem;
    // голова очереди резерва
    FSpareHeadItem: TUdpPacketQueueItem;
    // хвост очереди резерва
    FSpareTailItem: TUdpPacketQueueItem;
    procedure SetCapacity(const Value: Integer);
    procedure SetMaxCount(const Value: Integer);
    { Поместить пакет в хвост очереди основных }
    procedure PushToMain(AItem: TUdpPacketQueueItem);
    { Достать пакет из головы очереди основных }
    function PullFromMain(): TUdpPacketQueueItem;
    { Поместить пакет в хвост очереди запасных }
    procedure PushToSpare(AItem: TUdpPacketQueueItem);
    { Достать пакет из головы очереди запасных }
    function PullFromSpare(): TUdpPacketQueueItem;
  public
    PushTimestamp: Int64;
    PullCount: Int64;
    constructor Create(ACapacity: Integer);
    destructor Destroy(); override;
    { Добавление пакета в очередь, увеличивает длину очереди.
      Если длина очереди больше размера списка, то размер списка увеличивается. }
    function Push(const APacket: TUdpPacketRec): Boolean;
    { Добавление пакета в очередь, если такого адресата нет.
      Если адресат в очереди уже есть, то происходит замена имеющегося пакета
      с таким же или более низким приоритетом.
      Если длина очереди больше размера списка, то размер списка увеличивается. }
    function PushUpdate(const APacket: TUdpPacketRec): Boolean;
    { Взятие пакета из очереди, уменьшает длину очереди.
      Возвращает False, если очередь пуста. }
    function Pull(out APacket: TUdpPacketRec): Boolean;
    { Чтение пакета из очереди, без изменения длины очереди.
      Возвращает False, если очередь пуста. }
    function Peek(out APacket: TUdpPacketRec): Boolean;
    { Выбросить пакет из очереди, уменьшает длину очереди.
      Возвращает False, если очередь пуста. }
    function Drop(): Boolean;
    { Полная очистка очереди, сбрасывает длину очереди в 0
      Размер списка при этом не меняется, содержимое элементов списка очищается }
    procedure Clear();
    { Возвращает True, если в очереди есть пакет на тот же PeerIP/PeerPort }
    function HavePeer(const APacket: TUdpPacketRec): Boolean;
    { Минимальный размер списка очереди, задает количество постоянных элементов списка.
      Реальный размер списка может превысить заданный размер, если длина очереди растет,
      но при уменьшении длины очереди размер списка возвращается к заданному значению }
    property Capacity: Integer read FCapacity write SetCapacity;
    { Максимальный размер списка очереди, при превышении новые элементы перестают добавляться }
    property MaxCount: Integer read FMaxCount write SetMaxCount;
    { Длина очереди, увеличивается при добавлении элементов }
    property Count: Integer read FCount;
  end;

  { Канал связи, использующий UDP }
  TUdpChannel = class(TObject)
  private
    FTxPktQueue: TUdpPacketQueue;
    FOnTrouble: TNotifyEvent;
    procedure SetRxTimestamp(const Value: Int64);
    procedure SetTrouble(const Value: Boolean);
    function GetChanName: string;
    function GetDecription: string;
    function GetLocalIP: string;
    procedure SetChanName(const Value: string);
    procedure SetDecription(const Value: string);
  protected
    FLock: TSimpleRWSync;

    // #NN:X
    FChanName: string;
    FDecription: string;

    FLocalIP: string;
    FLocalPort: Integer;
    FEnabled: Boolean;
    FTrouble: Boolean;
    FRxTimestamp: Int64;
    FWeakRef: IWeakRef;
    procedure SetLocalIP(const Value: string); virtual;
    procedure SetLocalPort(const Value: Integer); virtual;
    procedure SetEnabled(const Value: Boolean); virtual;
    function GetRxTimestamp(): Int64; virtual;
    function GetWeakRef(): IWeakRef; virtual;
  public
    // назначается извне
    RxPktQueue: TUdpPacketQueue;

    Connected: Boolean;
    // признак необходимости перезапуска
    NeedRestart: Boolean;

    // признак резервного канала, для NSB
    SpareChannel: Boolean;
    // интервал отправки (мс), для ограничения исходящего потока для слабых каналов
    TxInterval: Integer;
    // Кол-во отправляемых пакетов за интервал
    TxQuantity: Integer;
    // размер буфера отправки
    TxBufSize: Integer;
    // время последнего ответа на опрос, используется в модемах УСА для контроля активности
    // для протоколов без опросов (с автотестами) нужно увеличивать после каждого
    // нового автотеста
    PollReplyTimestamp: Int64;
    // время, после которого можно отправлять данные
    NextTxTime: Int64;

    // Статистика
    RxUdpPkt: Int64;   // Принято пакетов UDP
    TxUdpPkt: Int64;   // Отправлено пакетов UDP
    RxUdpErr: Int64;   // Ошибок приема UDP
    TxUdpErr: Int64;   // Ошибое отправки UDP
    RxBytes: Int64;    // успешно принятых байтов
    TxBytes: Int64;    // успешно отправленных байтов

    TxBufFree: Integer;

    // отправка пакета UDP
    function SendUdpPacket(const UdpPacketRec: TUdpPacketRec): Boolean; virtual;

    procedure AfterConstruction(); override;
    procedure BeforeDestruction(); override;
    //property UdpSockThread: TSynUdpSockThread read FUdpSockThread;
    property TxPktQueue: TUdpPacketQueue read FTxPktQueue;

    // #NN:X
    property ChanName: string read GetChanName write SetChanName;
    property Decription: string read GetDecription write SetDecription;
    property LocalIP: string read GetLocalIP write SetLocalIP;
    property LocalPort: Integer read FLocalPort write SetLocalPort;
    // признак разрешения использования
    property Enabled: Boolean read FEnabled write SetEnabled;
    // признак неисправности
    property Trouble: Boolean read FTrouble write SetTrouble;
    // время последнего приема данных
    property RxTimestamp: Int64 read GetRxTimestamp write SetRxTimestamp;
    property WeakRef: IWeakRef read GetWeakRef;
    { Срабатывает при изменении признака неисправности }
    property OnTrouble: TNotifyEvent read FOnTrouble write FOnTrouble;
  end;

{ возвращает канал по слабой ссылке }
function GetChanFromWeakRef(const AWeakRef: IWeakRef): TUdpChannel;
{ возвращает название канала из пакета }
function GetUdpPacketRecChanName(const APacket: TUdpPacketRec): string;
{ возвращает название канала по слабой ссылке }
function GetChanNameFromWeakRef(const AWeakRef: IWeakRef): string;

{ возвращает данные в виде строки из пакета }
function GetUdpPacketRecDataStr(const APacket: TUdpPacketRec): AnsiString;
procedure SetUdpPacketRecDataStr(var APacket: TUdpPacketRec; const AData: AnsiString);

procedure AssignUdpPacketRec(const ASource: TUdpPacketRec; var ADest: TUdpPacketRec);

implementation

const
  LOCK_TRY_COUNT = 1000;

{ возвращает канал по слабой ссылке }
function GetChanFromWeakRef(const AWeakRef: IWeakRef): TUdpChannel;
var
  TmpObj: TObject;
begin
  Result := nil;
  if Assigned(AWeakRef) then
  begin
    TmpObj := AWeakRef.GetOwner();
    if Assigned(TmpObj) and (TmpObj is TUdpChannel) then
      Result := (TmpObj as TUdpChannel);
  end;
end;

{ возвращает название канала по слабой ссылке }
function GetChanNameFromWeakRef(const AWeakRef: IWeakRef): string;
var
  TmpObj: TObject;
begin
  Result := '';
  if Assigned(AWeakRef) then
  begin
    TmpObj := AWeakRef.GetOwner();
    if Assigned(TmpObj) and (TmpObj is TUdpChannel) then
      Result := (TmpObj as TUdpChannel).ChanName;
  end;
end;

{ возвращает название канала из пакета }
function GetUdpPacketRecChanName(const APacket: TUdpPacketRec): string;
var
  TmpObj: TObject;
begin
  Result := '';
  if Assigned(APacket.ChanWeakRef) then
  begin
    TmpObj := APacket.ChanWeakRef.GetOwner();
    if Assigned(TmpObj) and (TmpObj is TUdpChannel) then
      Result := (TmpObj as TUdpChannel).ChanName;
  end;
end;

{ возвращает данные в виде строки из пакета }
function GetUdpPacketRecDataStr(const APacket: TUdpPacketRec): AnsiString;
begin
  SetLength(Result, APacket.DataSize);
  if APacket.DataSize > 0 then
    Move(APacket.Data[0], Result[1], APacket.DataSize);
end;

procedure SetUdpPacketRecDataStr(var APacket: TUdpPacketRec; const AData: AnsiString);
begin
  APacket.DataSize := Length(AData);
  if APacket.DataSize > SizeOf(APacket.Data) then
    APacket.DataSize := SizeOf(APacket.Data);
  if APacket.DataSize > 0 then
    Move(AData[1], APacket.Data[0], APacket.DataSize);
end;

procedure AssignUdpPacketRec(const ASource: TUdpPacketRec; var ADest: TUdpPacketRec);
var
  n: Integer;
begin
  n := ASource.DataSize;
  if n > SizeOf(ADest.Data) then
    n := SizeOf(ADest.Data);
  Move(ASource, ADest, SizeOf(ADest) - SizeOf(ADest.Data) - Sizeof(ADest.ChanWeakRef) + n);
  ADest.ChanWeakRef := ASource.ChanWeakRef;
end;

{ TUdpPacketQueue }

constructor TUdpPacketQueue.Create(ACapacity: Integer);
begin
  inherited Create();
  FLock.Init();
  FCapacity := 0;
  FCount := 0;
  FMaxCount := 0;
  PullCount := 0;
  FHeadItem := nil;
  FTailItem := nil;
  FSpareHeadItem := nil;
  FSpareTailItem := nil;
  SetCapacity(ACapacity);
end;

destructor TUdpPacketQueue.Destroy;
begin
  SetCapacity(0);
  inherited;
end;

procedure TUdpPacketQueue.Clear();
var
  Item: TUdpPacketQueueItem;
begin
  if FLock.Acquire(LOCK_TRY_COUNT) then
  try
    while FCount > 0 do
    begin
      Item := PullFromMain();
      if FCount >= FCapacity then
      begin
        // очередь больше назначенной емкости, убираем элемент
        Item.Free();
      end
      else
      begin
        // помещаем в запасные
        PushToSpare(Item);
      end;
    end;
  finally
    FLock.Release();
  end;
end;

function TUdpPacketQueue.Pull(out APacket: TUdpPacketRec): Boolean;
var
  Item: TUdpPacketQueueItem;
begin
  Result := False;
  if (FCount <= 0) then Exit;

  if FLock.Acquire(LOCK_TRY_COUNT) then
  begin
    try
      if (FCount > 0) then
      begin
        Item := PullFromMain();
        //APacket := Item.UdpPacketRec;
        AssignUdpPacketRec(Item.UdpPacketRec, APacket);
        if FCount >= FCapacity then
        begin
          // очередь больше назначенной емкости, убираем элемент
          Item.Free();
        end
        else
        begin
          // помещаем в запасные
          PushToSpare(Item);
        end;
        Inc(PullCount);
        Result := True;
      end;
    finally
      FLock.Release();
    end;
  end
  else
  begin
    {$ifdef DEBUG}_LogError('TUdpPacketQueue.Pull() lock fail');{$endif}
  end;
end;

function TUdpPacketQueue.Peek(out APacket: TUdpPacketRec): Boolean;
begin
  Result := False;
  if (FCount <= 0) then Exit;

  if FLock.Acquire() then
  begin
    try
      if Assigned(FHeadItem) then
      begin
        //APacket := FHeadItem.UdpPacketRec;
        AssignUdpPacketRec(FHeadItem.UdpPacketRec, APacket);
        Result := True;
      end;
    finally
      FLock.Release();
    end;
  end
  else
  begin
    {$ifdef DEBUG}_LogError('TUdpPacketQueue.Peek() lock fail');{$endif}
  end;
end;

function TUdpPacketQueue.Drop(): Boolean;
var
  Item: TUdpPacketQueueItem;
begin
  Result := False;
  if (FCount <= 0) then Exit;

  if FLock.Acquire(LOCK_TRY_COUNT) then
  begin
    try
      if (FCount > 0) then
      begin
        Item := PullFromMain();
        if FCount >= FCapacity then
        begin
          // очередь больше назначенной емкости, убираем элемент
          Item.Free();
        end
        else
        begin
          // помещаем в запасные
          PushToSpare(Item);
        end;
        Inc(PullCount);
        Result := True;
      end;
    finally
      FLock.Release();
    end;
  end
  else
  begin
    {$ifdef DEBUG}_LogError('TUdpPacketQueue.Drop() lock fail');{$endif}
  end;
end;

function TUdpPacketQueue.Push(const APacket: TUdpPacketRec): Boolean;
var
  Item: TUdpPacketQueueItem;
begin
  Result := False;
  if FLock.Acquire(LOCK_TRY_COUNT) then
  begin
    try
      if FCount < FCapacity then
      begin
        // количество в пределах емкости, достаем запасной элемент
        Item := PullFromSpare();
        //Item.UdpPacketRec := APacket;
        AssignUdpPacketRec(APacket, Item.UdpPacketRec);
        PushToMain(Item);
        Result := True;
      end
      else if FCount < FMaxCount then
      begin
        // больше емкости но меньше максимума, добавляем новый элемент
        Item := TUdpPacketQueueItem.Create();
        //Item.UdpPacketRec := APacket;
        AssignUdpPacketRec(APacket, Item.UdpPacketRec);
        PushToMain(Item);
        Result := True;
      end;

    finally
      FLock.Release();
    end;
  end;
end;

function TUdpPacketQueue.PushUpdate(const APacket: TUdpPacketRec): Boolean;
var
  Item: TUdpPacketQueueItem;
begin
  Result := False;
  if FLock.Acquire(LOCK_TRY_COUNT) then
  begin
    try
      Item := FHeadItem;
      while Assigned(Item) do
      begin
        if (Item.UdpPacketRec.PeerIP = APacket.PeerIP) and (Item.UdpPacketRec.PeerPort = APacket.PeerPort) then
        begin
          if Item.UdpPacketRec.Priority <= APacket.Priority then
          begin
            //Item.UdpPacketRec := APacket;
            AssignUdpPacketRec(APacket, Item.UdpPacketRec);
            Result := True;
          end;
          Break;
        end;
        Item := Item.NextItem;
      end;

      if not Result then
      begin
        if FCount < FCapacity then
        begin
          // количество в пределах емкости, достаем запасной элемент
          Item := PullFromSpare();
          //Item.UdpPacketRec := APacket;
          AssignUdpPacketRec(APacket, Item.UdpPacketRec);
          PushToMain(Item);
          Result := True;
        end
        else if FCount < FMaxCount then
        begin
          // больше емкости но меньше максимума, добавляем новый элемент
          Item := TUdpPacketQueueItem.Create();
          //Item.UdpPacketRec := APacket;
          AssignUdpPacketRec(APacket, Item.UdpPacketRec);
          PushToMain(Item);
          Result := True;
        end
        else
          Result := False;

      end;
    finally
      FLock.Release();
    end;
  end;
end;

procedure TUdpPacketQueue.SetCapacity(const Value: Integer);
var
  Item: TUdpPacketQueueItem;
begin
  if not FLock.Acquire(LOCK_TRY_COUNT) then Exit;
  try
    FCapacity := Value;
    if FMaxCount < FCapacity then
      FMaxCount := FCapacity;
    // Добавление недостающих элементов
    while (FCount + FSpareCount) < FCapacity do
    begin
      Item := TUdpPacketQueueItem.Create();
      PushToSpare(Item);
    end;

    while (FCount + FSpareCount) > FCapacity do
    begin
      // список больше назначенной емкости, убираем последний элемент
      if FSpareHeadItem <> nil then
      begin
        Item := PullFromSpare();
        Item.Free();
      end
      else if FHeadItem <> nil then
      begin
        Item := PullFromMain();
        Item.Free();
      end;
    end;
  finally
    FLock.Release();
  end;
end;

function TUdpPacketQueue.HavePeer(const APacket: TUdpPacketRec): Boolean;
var
  Item: TUdpPacketQueueItem;
begin
  Result := False;
  if FHeadItem = nil then Exit;

  if not FLock.Acquire() then Exit;
  try
    Item := FHeadItem;
    while Assigned(Item) do
    begin
      if (Item.UdpPacketRec.PeerIP = APacket.PeerIP) and (Item.UdpPacketRec.PeerPort = APacket.PeerPort) then
      begin
        Result := True;
        Break;
      end;
      Item := Item.NextItem;
    end;
  finally
    FLock.Release();
  end;
end;

procedure TUdpPacketQueue.SetMaxCount(const Value: Integer);
begin
  if Value < Capacity then
    FMaxCount := Capacity
  else if Value > 0 then
    FMaxCount := Value;
end;

procedure TUdpPacketQueue.PushToMain(AItem: TUdpPacketQueueItem);
begin
  if FTailItem <> nil then
    FTailItem.NextItem := AItem
  else if FHeadItem = nil then
    FHeadItem := AItem;
  FTailItem := AItem;
  FTailItem.NextItem := nil;
  Inc(FCount);
end;

procedure TUdpPacketQueue.PushToSpare(AItem: TUdpPacketQueueItem);
begin
  if FSpareTailItem <> nil then
    FSpareTailItem.NextItem := AItem
  else if FSpareHeadItem = nil then
    FSpareHeadItem := AItem;
  FSpareTailItem := AItem;
  FSpareTailItem.NextItem := nil;
  Inc(FSpareCount);
end;

function TUdpPacketQueue.PullFromMain(): TUdpPacketQueueItem;
begin
  if FHeadItem <> nil then
  begin
    Result := FHeadItem;
    FHeadItem := Result.NextItem;
    Result.NextItem := nil;
    if FHeadItem = nil then
      FTailItem := nil;
    Dec(FCount);
  end
  else
  begin
    {$ifdef DEBUG}_LogError('TUdpPacketQueue.PullFromMain() fail');{$endif}
    Assert(False);
    Result := nil;
  end;
end;

function TUdpPacketQueue.PullFromSpare(): TUdpPacketQueueItem;
begin
  if FSpareHeadItem <> nil then
  begin
    Result := FSpareHeadItem;
    FSpareHeadItem := Result.NextItem;
    Result.NextItem := nil;
    if FSpareHeadItem = nil then
      FSpareTailItem := nil;
    Dec(FSpareCount);
  end
  else
  begin
    {$ifdef DEBUG}_LogError('TUdpPacketQueue.PullFromSpare() fail');{$endif}
    Assert(False);
    Result := nil;
  end;
end;

{ TUdpChannel }

procedure TUdpChannel.AfterConstruction;
begin
  inherited;
  FLock := TSimpleRWSync.Create();
  FTxPktQueue := TUdpPacketQueue.Create(256);
  FTxPktQueue.MaxCount := 4096;
  NeedRestart := False;
  NextTxTime := 0;
  TxInterval := 0;
  PollReplyTimestamp := 0;

  RxUdpPkt := 0;
  TxUdpPkt := 0;
  RxUdpErr := 0;
  TxUdpErr := 0;
  RxBytes := 0;
  TxBytes := 0;
end;

procedure TUdpChannel.BeforeDestruction;
begin
  RxPktQueue := nil;
  FreeAndNil(FTxPktQueue);
  if Assigned(FWeakRef) then
    FWeakRef._Clean();
  FreeAndNil(FLock);
  inherited;
end;

function TUdpChannel.GetRxTimestamp: Int64;
begin
  Result := FRxTimestamp;
end;

procedure TUdpChannel.SetRxTimestamp(const Value: Int64);
begin
  FRxTimestamp := Value;
end;

function TUdpChannel.GetWeakRef: IWeakRef;
begin
  if FWeakRef = nil then
  begin
    FWeakRef := TWeakRef.Create(Self);
  end;
  Result := FWeakRef;
end;

procedure TUdpChannel.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;

  RxUdpPkt := 0;
  TxUdpPkt := 0;
  RxUdpErr := 0;
  TxUdpErr := 0;
  RxBytes := 0;
  TxBytes := 0;
end;

procedure TUdpChannel.SetTrouble(const Value: Boolean);
begin
  if (FTrouble <> Value) then
  begin
    FTrouble := Value;
    if Assigned(OnTrouble) then OnTrouble(Self);
  end;
end;

procedure TUdpChannel.SetLocalIP(const Value: string);
begin
  if FLock.BeginWrite() then
  begin
    try
      FLocalIP := Value;
    finally
      FLock.EndWrite();
    end;
  end;
end;

procedure TUdpChannel.SetLocalPort(const Value: Integer);
begin
  FLocalPort := Value;
end;

function TUdpChannel.SendUdpPacket(const UdpPacketRec: TUdpPacketRec): Boolean;
begin
  Result := TxPktQueue.Push(UdpPacketRec);
end;

function TUdpChannel.GetChanName: string;
begin
  FLock.BeginRead();
  try
    Result := FChanName;
  finally
    FLock.EndRead();
  end;
end;

function TUdpChannel.GetDecription: string;
begin
  FLock.BeginRead();
  try
    Result := FDecription;
  finally
    FLock.EndRead();
  end;
end;

function TUdpChannel.GetLocalIP: string;
begin
  FLock.BeginRead();
  try
    Result := FLocalIP;
  finally
    FLock.EndRead();
  end;
end;

procedure TUdpChannel.SetChanName(const Value: string);
begin
  if FLock.BeginWrite() then
  begin
    try
      FChanName := Value;
    finally
      FLock.EndWrite();
    end;
  end;
end;

procedure TUdpChannel.SetDecription(const Value: string);
begin
  if FLock.BeginWrite() then
  begin
    try
      FDecription := Value;
    finally
      FLock.EndWrite();
    end;
  end;
end;

{ TUdpPacketQueueItem }

procedure TUdpPacketQueueItem.Clear();
begin
  UdpPacketRec.LocalPort := 0;
  UdpPacketRec.PeerPort := 0;
  UdpPacketRec.Priority := 0;
  UdpPacketRec.LocalIP := '';
  UdpPacketRec.PeerIP := '';
  UdpPacketRec.ChanWeakRef := nil;
  UdpPacketRec.DataSize := 0;
end;

end.
