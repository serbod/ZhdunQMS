unit ConfigForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, ComCtrls, Controls,
  StdCtrls, Buttons, Menus, IniFiles, LCLType;

type

  { TConfigNode }

  TConfigNode = class(TTreeNode)
  public
    SectionName: string;
    ParentSectionName: string;
    ModelName: string;
    { params keys }
    Params: TStringList;
    { sub-items models names }
    Models: TStringList;
    constructor Create(AnOwner: TTreeNodes); reintroduce;
    destructor Destroy(); override;
  end;

  { TfrmOptions }

  TfrmOptions = class(TForm)
    btnApply: TBitBtn;
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    btnSet: TButton;
    cbValue: TComboBox;
    gbParams: TGroupBox;
    lbParamName: TLabel;
    lvParams: TListView;
    miDeleteSubitem: TMenuItem;
    miAddSubitem: TMenuItem;
    pmOptions: TPopupMenu;
    tvOptions: TTreeView;
    procedure btnApplyClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnSetClick(Sender: TObject);
    procedure edValueChange(Sender: TObject);
    procedure edValueKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure lvParamsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure miAddSubitemClick(Sender: TObject);
    procedure miDeleteSubitemClick(Sender: TObject);
    procedure pmOptionsPopup(Sender: TObject);
    procedure tvOptionsSelectionChanged(Sender: TObject);
  private
    { private declarations }
    FOwnedConfig: Boolean;
    FOwnedConfigModels: Boolean;
    CurConfNode: TConfigNode;
    CurParam: string;
    CurValue: string;
  public
    { public declarations }
    ConfFileName: string;
    Config: TIniFile;
    ConfigModels: TIniFile;
    procedure ReadConfig();
    procedure WriteConfig();
    function ReadModel(const ModelName: string; var ModelCaption, ParentSection: string;
      var single: Boolean; Params: TStrings): Boolean;
  end;

procedure ShowConfigForm(AConfig: TIniFile = nil; AOwner: TControl = nil;
  AModal: Boolean = True);

var
  frmOptions: TfrmOptions;
  sConfModelsIni: string = 'conf_models.ini';
  sConfIni: string = 'config.ini';

implementation

procedure ShowConfigForm(AConfig: TIniFile; AOwner: TControl; AModal: Boolean);
begin
  if not Assigned(frmOptions) then frmOptions := TfrmOptions.Create(AOwner);
  if Assigned(AConfig) then frmOptions.Config := AConfig;
  if AModal then
    frmOptions.ShowModal()
  else
    frmOptions.Show();
end;

{ TConfigNode }

constructor TConfigNode.Create(AnOwner: TTreeNodes);
begin
  inherited Create(AnOwner);
  Params := TStringList.Create();
  Models := TStringList.Create();
end;

destructor TConfigNode.Destroy;
begin
  FreeAndNil(Models);
  FreeAndNil(Params);
  inherited Destroy();
end;

{$R *.lfm}

{ TfrmOptions }

procedure TfrmOptions.tvOptionsSelectionChanged(Sender: TObject);
var
  ConfNode: TConfigNode;
  li: TListItem;
  i, n: Integer;
  ParamName, ParamCaption: string;
begin
  if not Assigned(tvOptions.Selected) then Exit;
  if not (tvOptions.Selected is TConfigNode) then Exit;
  ConfNode := (tvOptions.Selected as TConfigNode);
  CurConfNode := ConfNode;
  gbParams.Caption := ConfNode.Text;
  lvParams.BeginUpdate();
  lvParams.Clear();
  for i := 0 to ConfNode.Params.Count - 1 do
  begin
    li := lvParams.Items.Add();
    //li.Caption := ConfNode.Params[i];
    // get values list for param from description string
    // Test param {value1|value2}
    ParamName := ConfNode.Params[i];
    ParamCaption := ConfigModels.ReadString(ConfNode.ModelName, ParamName, ParamName);
    n := Pos('{', ParamCaption);
    if n > 0 then
      ParamCaption := Copy(ParamCaption, 1, n-1);
    li.Caption := ParamCaption;
    li.SubItems.Add(Config.ReadString(ConfNode.SectionName, ConfNode.Params[i], ''));
  end;
  lvParams.EndUpdate();
  lbParamName.Caption := '';
  cbValue.Items.Clear();
  cbValue.Text := '';
  if lvParams.Items.Count > 0 then lvParams.Selected := lvParams.Items[0];
end;

procedure TfrmOptions.btnApplyClick(Sender: TObject);
begin
  ReadConfig();
end;

procedure TfrmOptions.btnCancelClick(Sender: TObject);
begin
  self.Close();
end;

procedure TfrmOptions.btnOkClick(Sender: TObject);
begin
  WriteConfig();
  self.Close();
end;

procedure TfrmOptions.btnSetClick(Sender: TObject);
begin
  cbValue.Text := CurValue;
end;

procedure TfrmOptions.edValueChange(Sender: TObject);
begin
  // запись значения параметра из поля ввода в таблицу параметров и конфиг
  if not Assigned(lvParams.Selected) then Exit;
  lvParams.BeginUpdate();
  lvParams.Selected.SubItems.Clear();
  lvParams.Selected.SubItems.Add(Trim(cbValue.Text));
  lvParams.EndUpdate();
  Config.WriteString(CurConfNode.SectionName, CurParam, Trim(cbValue.Text));
end;

procedure TfrmOptions.edValueKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then btnSet.Click();
end;

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  ConfFileName := sConfIni;
end;

procedure TfrmOptions.FormDestroy(Sender: TObject);
begin
  if FOwnedConfig then FreeAndNil(Config);
  if FOwnedConfigModels then FreeAndNil(ConfigModels);
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
  self.ReadConfig();
end;

procedure TfrmOptions.lvParamsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
var
  s, ss: string;
  n, m: Integer;
begin
  if not Selected then Exit;
  //CurParam := Trim(Item.Caption);
  CurParam := CurConfNode.Params[Item.Index];
  CurValue := Trim(Item.SubItems.Text);
  // lbParamName.Caption:=Item.Caption;
  //lbParamName.Caption:=AnsiToUtf8(ConfigModels.ReadString(CurConfNode.ModelName, CurParam, CurParam));

  cbValue.Items.Clear();
  // get values list for param from description string
  // Test param {value1|value2}
  ss := ConfigModels.ReadString(CurConfNode.ModelName, CurParam, CurParam);
  n := Pos('{', ss);
  m := Pos('}', ss);
  if (n > 0) and (m > n) then
  begin
    s := Copy(ss, n + 1, m - n - 1);
    ss := Copy(ss, 1, n - 1);
    n := Pos('|', s);
    while n > 0 do
    begin
      cbValue.Items.Add(Copy(s, 1, n - 1));
      s := Copy(s, n + 1, maxint);
      n := Pos('|', s);
    end;
    cbValue.Items.Add(s);
  end;

  lbParamName.Caption := ss;
  cbValue.Text := CurValue;
end;

procedure TfrmOptions.miAddSubitemClick(Sender: TObject);
var
  ConfNode, ParentNode: TConfigNode;
  sModel, sCapt, sParentSect, sNum: string;
  single: Boolean;
  SectParams: TStringList;
begin
  // Добавление экземпляра многоразовой модели в раздел конфига
  if not Assigned(tvOptions.Selected) then Exit;
  if not (tvOptions.Selected is TConfigNode) then Exit;
  ParentNode := (tvOptions.Selected as TConfigNode);

  if ParentNode.Models.Count = 0 then Exit;
  tvOptions.BeginUpdate();
  { TODO : Model selection }
  sModel := ParentNode.Models[0];
  sNum := IntToStr(ParentNode.Count + 1);

  single := False;
  sCapt := '';
  sParentSect := '';
  SectParams := TStringList.Create();
  ReadModel(sModel, sCapt, sParentSect, single, SectParams);

  // create node
  ConfNode := TConfigNode.Create(tvOptions.Items);
  ConfNode.Text := sCapt + ' ' + sNum;
  ConfNode.ModelName := sModel;
  ConfNode.SectionName := sModel + '_' + sNum;
  ConfNode.ParentSectionName := sParentSect;
  ConfNode.Params.Assign(SectParams);
  ConfNode.Data := self;
  SectParams.Free();
  // add note to tree
  tvOptions.Items.AddNode(ConfNode, ParentNode, ConfNode.Text, nil, naAddChild);
  tvOptions.EndUpdate();
end;

procedure TfrmOptions.miDeleteSubitemClick(Sender: TObject);
var
  ConfNode: TConfigNode;
begin
  if not Assigned(tvOptions.Selected) then Exit;
  if not (tvOptions.Selected is TConfigNode) then Exit;
  ConfNode := (tvOptions.Selected as TConfigNode);

  Config.EraseSection(ConfNode.SectionName);
  tvOptions.Items.Delete(ConfNode);
end;

procedure TfrmOptions.pmOptionsPopup(Sender: TObject);
begin
  { TODO : Add models to menu }
end;

procedure TfrmOptions.ReadConfig();
var
  Sections: TStringList;
  SectParams: TStringList;
  i, n, m: Integer;
  sModel, sSectName, sCapt, sParentSect: string;
  single: Boolean;
  ConfNode, ParentNode: TConfigNode;
  tn: TTreeNode;

  procedure AddNodeToTree(ASectName, AText: string);
  begin
    // create node
    ConfNode := TConfigNode.Create(tvOptions.Items);
    ConfNode.Text := AText;
    ConfNode.ModelName := sModel;
    ConfNode.SectionName := ASectName;
    ConfNode.ParentSectionName := sParentSect;
    ConfNode.Params.Assign(SectParams);
    ConfNode.Data := self;
    //Config.ReadSection(sSectName, ConfNode.Params);
  {if Config.SectionExists(sSect) then
  begin
    Config.ReadSection(sSectName, ConfNode.Params);
  end;}
    // add note to tree
    if Assigned(ParentNode) then
    begin
      //ParentNode.Models.Add(ConfNode.ModelName);
      tvOptions.Items.AddNode(ConfNode, ParentNode, ConfNode.Text, nil, naAddChild);
    end
    else
    begin
      tvOptions.Items.AddNode(ConfNode, ParentNode, ConfNode.Text, nil, naAdd);
    end;
  end;

begin
  // Read config models
  FOwnedConfigModels := False;
  if not Assigned(ConfigModels) then
  begin
    if not FileExists(sConfModelsIni) then Exit;
    ConfigModels := TMemIniFile.Create(sConfModelsIni);
    FOwnedConfigModels := True;
  end;

  // Read config
  FOwnedConfig := False;
  if not Assigned(Config) then
  begin
    //if not FileExists(csConfIni) then Exit;
    Config := TMemIniFile.Create(ConfFileName);
    FOwnedConfig := True;
  end;

  // Build options tree
  tvOptions.BeginUpdate();
  tvOptions.Items.Clear();

  Sections := TStringList.Create();
  SectParams := TStringList.Create();
  ConfigModels.ReadSections(Sections);
  for i := 0 to Sections.Count - 1 do
  begin
    // Read section model
    sModel := Sections[i];
    single := False;
    sCapt := '';
    sParentSect := '';
    ReadModel(sModel, sCapt, sParentSect, single, SectParams);

    // try to find parent node
    ParentNode := nil;
    for m := 0 to tvOptions.Items.Count - 1 do
    begin
      tn := tvOptions.Items.Item[m];
      if not (tn is TConfigNode) then Continue;
      //if not Assigned(tn.Data) then Continue;
      //if TConfigNode(tn.Data).SectionName = sParentSect then
      if (tn as TConfigNode).SectionName = sParentSect then
      begin
        ParentNode := (tn as TConfigNode);
        Break;
      end;
    end;

    // add singleton node
    if single then
    begin
      AddNodeToTree(sModel, sCapt);
    end
    else
    begin
      // Add model to parent (if any)
      if Assigned(ParentNode) then ParentNode.Models.Add(sModel);
      // try to get corresponding sections from main config
      n := 1;
      while (n > 0) and (n < 32) do
      begin
        sSectName := sModel + '_' + IntToStr(n);
        if not Config.SectionExists(sSectName) then n := 0
        else
        begin
          AddNodeToTree(sSectName, sCapt + ' ' + IntToStr(n));
          n := n + 1;
        end;
      end;
    end;
  end;
  SectParams.Free();
  Sections.Free();
  tvOptions.FullExpand();
  tvOptions.EndUpdate();
end;

procedure TfrmOptions.WriteConfig();
begin
  if Assigned(Config) then Config.UpdateFile();
end;

function TfrmOptions.ReadModel(const ModelName: string;
  var ModelCaption, ParentSection: string; var single: Boolean;
  Params: TStrings): Boolean;
var
  sParam: string;
  n: Integer;
begin
  Result := False;
  if not ConfigModels.SectionExists(ModelName) then Exit;
  // Read section model
  //ModelCaption:=AnsiToUtf8(ConfigModels.ReadString(ModelName, 'Caption', ModelName));
  ModelCaption := ConfigModels.ReadString(ModelName, 'Caption', ModelName);
  single := ConfigModels.ReadBool(ModelName, 'Singleton', True);
  ParentSection := ConfigModels.ReadString(ModelName, 'ParentSection', '');

  ConfigModels.ReadSection(ModelName, Params);
  for n := Params.Count - 1 downto 0 do
  begin
    sParam := Params[n];
    if Pos('|' + sParam + '|', '|Caption|Singleton|ParentSection|') > 0 then Params.Delete(n);
  end;
  Result := True;
end;

end.
