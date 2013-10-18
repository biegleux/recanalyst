{*
 * $Id$
 * This file is part of the recanalyst project.
 *
 * Copyright (c) 2009-2013 biegleux <biegleux[at]gmail[dot]com>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 3 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, see <http://www.gnu.org/licenses>.
 *}
unit uRecAnalyst;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

{$IFNDEF FPC}
  {.$WEAKLINKRTTI ON}
  {.$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

{.$DEFINE EXTENDED}

interface

uses
  Classes, Windows, SysUtils, Contnrs, uRecAnalystBase, uRecAnalystStructs, MemStream;

type
  TBuildingList = class;

  { TInitialState }
  TInitialState = class(TObject)
  private
    function GetStartingAge(): AnsiString;
  public
    Food: Longint;
    Wood: Longint;
    Stone: Longint;
    Gold: Longint;
    StartingAge: TStartingAge;
    HouseCapacity: Longint;
    Population: Longint;
    CivilianPop: Longint;
    MilitaryPop: Longint;
    ExtraPop: Longint;
    Position: TPoint;
    constructor Create();
    property StartingAgeString: AnsiString read GetStartingAge;
  end;

  { TMilitaryStats }
  TMilitaryStats = class(TObject)
  public
    MilitaryScore: Word;
    UnitsKilled: Word;
    UnitsLost: Word;
    BuildingsRazed: Word;
    BuildingsLost: Word;
    UnitsConverted: Word;
    constructor Create();
  end;

  { TEconomyStats }
  TEconomyStats = class(TObject)
  public
    EconomyScore: Word;
    FoodCollected: LongWord;
    WoodCollected: LongWord;
    StoneCollected: LongWord;
    GoldCollected: LongWord;
    TributeSent: Word;
    TributeRcvd: Word;
    TradeProfit: Word;
    RelicGold: Word;
    constructor Create();
  end;

  { TTechnologyStats }
  TTechnologyStats = class(TObject)
  public
    TechnologyScore: Word;
    FeudalAge: LongWord;
    CastleAge: LongWord;
    ImperialAge: LongWord;
    MapExplored: Byte;
    ResearchCount: Byte;
    ResearchPercent: Byte;
    constructor Create();
  end;

  { TSocietyStats }
  TSocietyStats = class(TObject)
  public
    SocietyScore: Word;
    TotalWonders: Byte;
    TotalCastles: Byte;
    RelicsCaptured: Byte;
    VillagerHigh: Word;
    constructor Create();
  end;

  { TAchievement }
  TAchievement = class(TObject)
  public
    Victory: Boolean;
    Medal: Boolean;
    Result: TGameResult;
    TotalScore: LongWord;
    MilitaryStats: TMilitaryStats;
    EconomyStats: TEconomyStats;
    TechnologyStats: TTechnologyStats;
    SocietyStats: TSocietyStats;
    constructor Create();
    destructor Destroy(); override;
  end;

  { TPlayer }
  TPlayer = class(TObject)
  private
    function GetCiv(): AnsiString;
  public
    Name: AnsiString;
    Index: Integer;
    Human: Boolean;
    Team: Integer;
    Owner: Boolean;
    CivId: TCivilization;
    Color: Byte;
    IsCooping: Boolean;
    FeudalTime: Integer;
    CastleTime: Integer;
    ImperialTime: Integer;
    ResignTime: Integer;
    DisconnectTime: Integer;
    Buildings: TBuildingList;
    InitialState: TInitialState;
    Achievement: TAchievement;
    constructor Create();
    destructor Destroy(); override;
    property Civ: AnsiString read GetCiv;
  end;

  { TPlayerList }
  TPlayerList = class(TObject)
  private
    fList: TObjectList;
    function GetCount(): Integer;
    function GetPlayer(Index: Integer): TPlayer;
  public
    constructor Create();
    destructor Destroy(); override;
    function AddPlayer(Player: TPlayer): Integer;
    function GetPlayerByIndex(Index: Integer): TPlayer;
    procedure Clear();
    property Items[Index: Integer]: TPlayer read GetPlayer; default;
    property Count: Integer read GetCount;
  end;

  { TTeam }
  TTeam = class(TPlayerList)
  private
    fIndex: Integer;
  public
    constructor Create();
    function AddPlayer(Player: TPlayer): Integer;
    property Index: Integer read fIndex;
  end;

  { TTeamList }
  TTeamList = class(TObject)
  private
    fList: TObjectList;
    function GetCount(): Integer;
    function GetTeam(Index: Integer): TTeam;
  public
    constructor Create();
    destructor Destroy(); override;
    function AddTeam(Team: TTeam): Integer;
    function GetTeamByIndex(Index: Integer): TTeam;
    procedure Clear();
    property Items[Index: Integer]: TTeam read GetTeam; default;
    property Count: Integer read GetCount;
  end;

  { TVictory }
  TVictory = class(TObject)
  private
    function GetVictoryString(): AnsiString;
  public
    TimeLimit: Longint;
    ScoreLimit: Longint;
    VictoryCondition: TVictoryCondition;
    constructor Create();
    procedure Clear();
    property VictoryString: AnsiString read GetVictoryString;
  end;

  { TExtraGameData }
  TExtraGameData = class(TObject)
  public
    HasData: Boolean;  { true, if we have extra data (incl. achievements)
                         (ver >= UP 1.4 (RC2), mode Multiplayer) }
    AllTechs: Boolean;
    AllowCheats: Boolean;
    TeamTogether: Boolean;
    LockSpeed: Boolean;
    Complete: Boolean;  { true, if achievements are complete }
    constructor Create();
    procedure Clear();
  end;

  { TGameSettings }
  TGameSettings = class(TObject)
  private
    function GetGameType(): AnsiString;
    function GetMapStyle(): AnsiString;
    function GetDifficultyLevel(): AnsiString;
    function GetGameSpeed(): AnsiString;
    function GetRevealMap(): AnsiString;
    function GetMapSize(): AnsiString;
    function GetGameVersion(): AnsiString;
    function GetIsScenario(): Boolean;
  public
    GameType: TGameType;
    MapStyle: TMapStyle;
    DifficultyLevel: TDifficultyLevel;
    GameSpeed: TGameSpeed;
    RevealMap: TRevealMap;
    MapSize: TMapSize;
    Map: AnsiString;
    PlayersType: AnsiString;
    POV: AnsiString;
    POVEx: AnsiString;
    ObjectivesString: AnsiString;
    MapId: Byte;
    PopLimit: Integer;
    LockDiplomacy: Boolean; { LockTeams }
    PlayTime: Integer;
    InGameCoop: Boolean;
    IsFFA: Boolean;
    Owner: TPlayer;
    ScFileName: AnsiString;
    GameVersion: TGameVersion;
    Victory: TVictory;
    GameMode: TGameMode;
    Extra: TExtraGameData;
    sGameSubVersion: AnsiString;
    constructor Create();
    destructor Destroy(); override;
    procedure Clear();
    property sGameType: AnsiString read GetGameType;
    property sMapStyle: AnsiString read GetMapStyle;
    property sDifficultyLevel: AnsiString read GetDifficultyLevel;
    property sGameSpeed: AnsiString read GetGameSpeed;
    property sRevealMap: AnsiString read GetRevealMap;
    property sMapSize: AnsiString read GetMapSize;
    property sGameVersion: AnsiString read GetGameVersion;
    property IsScenario: Boolean read GetIsScenario;
  end;

  { TBaseObject }
  TBaseObject = class(TObject)
  private
    fItemId: Integer;
    function GetName(): AnsiString; virtual; abstract;
  public
    property Name: AnsiString read GetName;
    constructor Create();
  end;

  { TResearch }
  TResearch = class(TBaseObject)
  private
    function GetName(): AnsiString; override;
  public
    Id: Integer;
    Time: Longint;
    Player: TPlayer;
    constructor Create();
  end;

  { TTribute }
  TTribute = class(TObject)
    Time: Integer;
    PlayerFrom: TPlayer;
    PlayerTo: TPlayer;
    ResourceId: TResourceId;
    Amount: Integer;
    Fee: Single;
    function GetResourceName(): AnsiString;
  end;

  { TTrainedUnit }
  TTrainedUnit = class(TBaseObject)
  private
    function GetName(): AnsiString; override;
  public
    Id: Integer;
    Count: Integer;
    constructor Create();
  end;

  { TTrainedUnitList }
  TTrainedUnitList = class(TObjectList)
  private
    function GetItem(Index: Integer): TTrainedUnit;
    procedure SetItem(Index: Integer; ATrainedUnit: TTrainedUnit);
  public
    function GetUnit(const Id: Integer): TTrainedUnit;
    property Items[Index: Integer]: TTrainedUnit read GetItem write SetItem; default;
  end;

  { TBuilding }
  TBuilding = class(TTrainedUnit)
  private
    function GetName(): AnsiString; override;
  end;

  { TBuildingList }
  TBuildingList = class(TObjectList)
  private
    function GetItem(Index: Integer): TBuilding;
    procedure SetItem(Index: Integer; ATrainedUnit: TBuilding);
  public
    function GetBuilding(const Id: Integer): TBuilding;
    property Items[Index: Integer]: TBuilding read GetItem write SetItem; default;
  end;

  { TChatMessage }
  TChatMessage = class(TObject)
    Time: Integer;
    Color: Byte;
    Msg: AnsiString;
    constructor Create();
  end;

  { TUnitObject }
  TUnitObject = class(TObject)
    Owner: TPlayer;
    Id: Integer;
    Position: TPoint;
  end;

  TGaiaObject = class(TUnitObject);

  { TRecAnalyst }
  ERecAnalystException = class(Exception)
  private
    fCode: Integer;
  public
    constructor Create(Code: Integer);
    property Code: Integer read fCode write fCode;
  end;

  TRecAnalyst = class(TObject)
  private
    fFileName: String;
    fIsMgl: Boolean;
    fIsMgx: Boolean;
    fIsMgz: Boolean;
    fIsUserPatch: Boolean;
    fHeaderStream: TMemStream;
    fBodyStream: TMemStream;
    fMapData: array of array of Integer;
    fMapWidth: Longint;
    fMapHeight: Longint;
    fAnalyzeTime: LongWord;

    GaiaObjects: TObjectList;
    PlayerObjects: TObjectList;
    fAnalyzed: Boolean;
    {$IFDEF EXTENDED}fKeepStreams: Boolean;{$ENDIF}
    fMapImage: TMemoryStream;
    fMapImageSize: TSize;

    header_len: Int32;
    next_pos: Int32;
    {$IFDEF EXTENDED}objectives_pos: Int64;{$ENDIF}

    procedure Reset();
    procedure ExtractStreams();
    procedure AnalyzeHeader();
    procedure AnalyzeBody();
    procedure BuildTeams();
    procedure PostAnalyze();
    procedure ReadPlayerInfoBlockEx(const num_player: Byte);
    procedure ReadPlayerInfoBlock(const num_player: Byte);
  protected
    procedure ProcessPreGameChat(const buff: array of Char);
    procedure ProcessInGameChat(const buff: array of Char; time_cnt: Int32);
    procedure ProcessResignCommand(const resign_cmd: TResignCmdDataStruct; time_cnt: Int32);
    procedure ProcessResearchCommand(const research_cmd: TResearchCmdDataStruct; time_cnt: Int32);
    procedure ProcessHumanTrainUnitCommand(human_train_unit_cmd: THumanTrainUnitCmdDataStruct);
    procedure ProcessPCTrainUnitCommand(unit_type_id: Word);
    procedure ProcessConstructionCommand(player_id, building_type_id: Word);
    procedure ProcessTributeCommand(const tribute_cmd: TTributeCmdDataStruct; time_cnt: Int32);
    procedure ProcessGameDataCommand(const game_data_cmd: TGameDataStruct);
  public
    GameSettings: TGameSettings;
    Players: TPlayerList;
    Teams: TTeamList;
    Tributes: TObjectList;
    Units: TTrainedUnitList;
    Researches: TObjectList;
    PreGameChatMessages: TObjectList;
    InGameChatMessages: TObjectList;
    {$IFDEF EXTENDED}CommentString: AnsiString;{$ENDIF}
    constructor Create();
    destructor Destroy(); override;
    class function GameTimeToString(Time: Integer): AnsiString;
    procedure Analyze();
    function GenerateMap(const Width: Integer; const Height: Integer): TMemoryStream;
    {$IFDEF EXTENDED}
    procedure Build(const FileName: String);
    function AddComment(const Comment: AnsiString = ''): Boolean;
    {$ENDIF}
    class function ErrorCodeToString(const ErrorCode: Integer): String;
    property FileName: String read fFileName write fFileName;
    property AnalyzeTime: LongWord read fAnalyzeTime;
    property Analyzed: Boolean read fAnalyzed;
    {$IFDEF EXTENDED}
    property KeepStreams: Boolean read fKeepStreams write fKeepStreams;
    {$ENDIF}
  end;

{ RecAnalyst Error Codes }
const
  RECANALYST_OK          = 0;
  RECANALYST_NOFILE      = -1;
  RECANALYST_FILEEXT     = -2;
  RECANALYST_EMPTYHEADER = -3;
  RECANALYST_DECOMP      = -4;
  RECANALYST_FILEREAD    = -5;
  RECANALYST_FILEOPEN    = -6;
  RECANALYST_UNKNOWN     = -7;
  RECANALYST_HEADLENREAD = -8;
  RECANALYST_NOTRIGG     = -9;
  RECANALYST_NOGAMESETS  = -10;
  RECANALYST_READPLAYER  = -11;
  {$IFDEF EXTENDED}
  RECANALYST_FILECREATE  = -12;
  RECANALYST_COMP        = -13;
  {$ENDIF}

implementation

uses
  {$IFDEF FPC}paszlib{$ELSE}ZlibEx{$ENDIF}, Math, Types, GDIPAPI, GDIPOBJ,
  GDIPUTIL, ActiveX;

resourcestring
  c_filenotspecified = 'No file has been specified for analyzing.';
  c_cannotopenfile = 'Cannot open file.';
  c_cannotreadsection = 'Cannot read sections.';
  c_cannotdecompress = 'Cannot decompress header section.';
  c_unknown = 'Unknown error.';
  c_wrongfileext = 'Wrong file extension, file format is not supported.';
  c_headerlenreaderror = 'Unable to read the header length.';
  c_headerlenempty = 'Header length is zero.';
  c_triggerinfonotfound = '"Trigger Info" block has not been found.';
  c_gamesettingsnotfound = '"Game Settings" block has not been found.';
  c_playerinforeaderror =  'Error reading "PlayerInfo" block.';
  {$IFDEF EXTENDED}
  c_cannotcreatefile = 'Cannot create file.';
  c_cannotcompress = 'Cannot compress header section.';
  {$ENDIF}

  c_victory_timelimit = '%s (%d years)';
  c_victory_scorelimit = '%s (%d)';
  c_feudal_age_advance = '%s advanced to Feudal Age';
  c_castle_age_advance = '%s advanced to Castle Age';
  c_imperial_age_advance = '%s advanced to Imperial Age';
  c_resigned = '%s resigned';
  c_disconnected = '%s disconnected';

const
  ErrorMessages: array[{$IFDEF EXTENDED}RECANALYST_COMP{$ELSE}
    RECANALYST_READPLAYER{$ENDIF}..RECANALYST_OK] of String = (
    {$IFDEF EXTENDED}
    c_cannotcompress,
    c_cannotcreatefile,
    {$ENDIF}
    c_playerinforeaderror,
    c_gamesettingsnotfound,
    c_triggerinfonotfound,
    c_headerlenreaderror,
    c_unknown,
    c_cannotopenfile,
    c_cannotreadsection,
    c_cannotdecompress,
    c_headerlenempty,
    c_wrongfileext,
    c_filenotspecified,
    ''
  );

{ TBaseObject }
constructor TBaseObject.Create();
begin
  inherited Create();
  fItemId := -1;
end;

{ TResearch }
constructor TResearch.Create();
begin
  inherited Create();
  Id := 0;
  Player := nil;
end;

function TResearch.GetName(): AnsiString;
begin
  if (fItemId <> -1) then
  begin
    Result := RESEARCHES[fItemId].Name;
    Exit;
  end;

  fItemId := ResearchById(Id);
  if (fItemId <> -1) then
    Result := RESEARCHES[fItemId].Name
  else
    Result := '';
end;

{ TTribute }
function TTribute.GetResourceName(): AnsiString;
begin
  Result := RESOURCES[ResourceId];
end;

{ TTrainedUnit }
constructor TTrainedUnit.Create();
begin
  inherited Create();
  Id := 0;
  Count := 0;
end;

function TTrainedUnit.GetName(): AnsiString;
begin
  if (fItemId <> -1) then
  begin
    Result := UNITS[fItemId].Name;
    Exit;
  end;

  fItemId := UnitById(Id);
  if (fItemId <> -1) then
    Result := UNITS[fItemId].Name
  else
    Result := '';
end;

{ TTrainedUnitList }
function TTrainedUnitList.GetItem(Index: Integer): TTrainedUnit;
begin
  Result := inherited GetItem(Index) as TTrainedUnit;
end;

procedure TTrainedUnitList.SetItem(Index: Integer; ATrainedUnit: TTrainedUnit);
begin
  inherited SetItem(Index, ATrainedUnit);
end;

function TTrainedUnitList.GetUnit(const Id: Integer): TTrainedUnit;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Id = Id then
    begin
      Result := Items[i];
      Exit;
    end;
  end;
  Result := nil;
end;

{ TBuilding }
function TBuilding.GetName(): AnsiString;
begin
  if (fItemId <> -1) then
  begin
    Result := BUILDINGS[fItemId].Name;
    Exit;
  end;

  fItemId := BuildingById(Id);
  if (fItemId <> -1) then
    Result := BUILDINGS[fItemId].Name
  else
    Result := '';
end;

{ TBuildingList }
function TBuildingList.GetItem(Index: Integer): TBuilding;
begin
  Result := inherited GetItem(Index) as TBuilding;
end;

procedure TBuildingList.SetItem(Index: Integer; ATrainedUnit: TBuilding);
begin
  inherited SetItem(Index, ATrainedUnit);
end;

function TBuildingList.GetBuilding(const Id: Integer): TBuilding;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if Items[i].Id = Id then
    begin
      Result := Items[i];
      Exit;
    end;
  end;
  Result := nil;
end;

{ TChatMessage }
constructor TChatMessage.Create();
begin
  inherited Create();
  Time := 0;
  Color := 0;
  Msg := '';
end;

{ TInitialState }
constructor TInitialState.Create();
begin
  inherited Create();
  Food := 0;
  Wood := 0;
  Stone := 0;
  Gold := 0;
  StartingAge := Low(TStartingAge);
  HouseCapacity := 0;
  Population := 0;
  CivilianPop := 0;
  MilitaryPop := 0;
  ExtraPop := 0;
  FillChar(Position, SizeOf(Position), 0);
end;

function TInitialState.GetStartingAge(): AnsiString;
begin
  Result := '';
  if (StartingAge >= Low(STARTING_AGES)) and (StartingAge <= High(STARTING_AGES)) then
    Result := STARTING_AGES[StartingAge];
end;

{ TMilitaryStats }
constructor TMilitaryStats.Create();
begin
  inherited Create();
  MilitaryScore := 0;
  UnitsKilled := 0;
  UnitsLost := 0;
  BuildingsRazed := 0;
  BuildingsLost := 0;
  UnitsConverted := 0;
end;

{ TEconomyStats }
constructor TEconomyStats.Create();
begin
  inherited Create();
  EconomyScore := 0;
  FoodCollected := 0;
  WoodCollected := 0;
  StoneCollected := 0;
  GoldCollected := 0;
  TributeSent := 0;
  TributeRcvd := 0;
  TradeProfit := 0;
  RelicGold := 0;
end;

{ TTechnologyStats }
constructor TTechnologyStats.Create();
begin
  inherited Create();
  TechnologyScore := 0;
  FeudalAge := 0;
  CastleAge := 0;
  ImperialAge := 0;
  MapExplored := 0;
  ResearchCount := 0;
  ResearchPercent := 0;
end;

{ TSocietyStats }
constructor TSocietyStats.Create();
begin
  inherited Create();
  SocietyScore := 0;
  TotalWonders := 0;
  TotalCastles := 0;
  RelicsCaptured := 0;
  VillagerHigh := 0;
end;

{ TAchievement }
constructor TAchievement.Create();
begin
  inherited Create();
  Victory := False;
  Medal := False;
  Result := Low(TGameResult);
  TotalScore := 0;
  MilitaryStats := TMilitaryStats.Create();
  EconomyStats := TEconomyStats.Create();
  TechnologyStats := TTechnologyStats.Create();
  SocietyStats := TSocietyStats.Create();
end;

destructor TAchievement.Destroy();
begin
  MilitaryStats.Free();
  EconomyStats.Free();
  TechnologyStats.Free();
  SocietyStats.Free();
  inherited Destroy();
end;

{ TPlayer }
constructor TPlayer.Create();
begin
  inherited Create();
  Name := '';
  Index := -1;
  Human := False;
  Team := -1;
  Owner := False;
  CivId := cNone;
  Color := 0;
  IsCooping := False;
  FeudalTime := 0;
  CastleTime := 0;
  ImperialTime := 0;
  ResignTime := 0;
  DisconnectTime := 0;
  Buildings := TBuildingList.Create();
  InitialState := TInitialState.Create();
  Achievement := TAchievement.Create();
end;

destructor TPlayer.Destroy();
begin
  Buildings.Free();
  InitialState.Free();
  Achievement.Free();
  inherited Destroy();
end;

function TPlayer.GetCiv(): AnsiString;
begin
  if (CivId in [Low(CIVS)..High(CIVS)]) then
    Result := CIVS[CivId]
  else Result := CIVS[cNone];
end;

{ TPlayerList }
constructor TPlayerList.Create();
begin
  inherited Create();
  fList := TObjectList.Create();
end;

destructor TPlayerList.Destroy();
begin
  fList.Free();
  inherited Destroy();
end;

function TPlayerList.AddPlayer(Player: TPlayer): Integer;
begin
  Result := fList.Add(Player);
end;

function TPlayerList.GetPlayer(Index: Integer): TPlayer;
begin
  if (Index < 0) or (Index >= fList.Count) then
    Result := nil
  else
    Result := fList[Index] as TPlayer;
end;

function TPlayerList.GetPlayerByIndex(Index: Integer): TPlayer;
var
  i: Integer;
begin
  for i := 0 to fList.Count - 1 do
  begin
    if (TPlayer(fList[i]).Index = Index) then
    begin
      Result := TPlayer(fList[i]);
      Exit;
    end;
  end;
  Result := nil;
end;

function TPlayerList.GetCount(): Integer;
begin
  Result := fList.Count;
end;

procedure TPlayerList.Clear();
begin
  fList.Clear();
end;

{ TTeam }
constructor TTeam.Create();
begin
  inherited Create();
  fList := TObjectList.Create(False);
  fIndex := -1;
end;

function TTeam.AddPlayer(Player: TPlayer): Integer;
begin
  Result := inherited AddPlayer(Player);
  if (fIndex = -1) then
    fIndex := Player.Team;
end;

{ TTeamList }
constructor TTeamList.Create();
begin
  inherited Create();
  fList := TObjectList.Create();
end;

destructor TTeamList.Destroy();
begin
  fList.Free();
  inherited Destroy();
end;

function TTeamList.AddTeam(Team: TTeam): Integer;
begin
  Result := fList.Add(Team);
end;

function TTeamList.GetTeam(Index: Integer): TTeam;
begin
  if (Index < 0) or (Index >= fList.Count) then
    Result := nil
  else
    Result := fList[Index] as TTeam;
end;

function TTeamList.GetTeamByIndex(Index: Integer): TTeam;
var
  i: Integer;
begin
  for i := 0 to fList.Count - 1 do
  begin
    if ((fList[i] as TTeam).Index = Index) then
    begin
      Result := fList[i] as TTeam;
      Exit;
    end;
  end;
  Result := nil;
end;

function TTeamList.GetCount(): Integer;
begin
  Result := fList.Count;
end;

procedure TTeamList.Clear();
begin
  fList.Clear();
end;

{ TVictory }
constructor TVictory.Create();
begin
  inherited Create();
  Clear();
end;

procedure TVictory.Clear();
begin
  VictoryCondition := Low(TVictoryCondition);
  TimeLimit := 0;
  ScoreLimit := 0;
end;

function TVictory.GetVictoryString(): AnsiString;
begin
  Result := '';
  if not (VictoryCondition in [Low(VICTORY_CONDITIONS)..High(VICTORY_CONDITIONS)]) then
    Exit;
  Result := VICTORY_CONDITIONS[VictoryCondition];

  case VictoryCondition of
    vcTimeLimit:
      begin
        if (TimeLimit <> 0) then
          Result := AnsiString(Format(c_victory_timelimit, [Result, TimeLimit]));
      end;
    vcScoreLimit:
      begin
        if (ScoreLimit <> 0) then
          Result := AnsiString(Format(c_victory_scorelimit, [Result, ScoreLimit]));
      end;
  end;
end;

{ TExtraGameData }
constructor TExtraGameData.Create();
begin
  inherited Create();
  Clear();
end;

procedure TExtraGameData.Clear();
begin
  HasData := False;
  AllTechs := False;
  AllowCheats := False;
  TeamTogether := False;
  LockSpeed := False;
  Complete := False;
end;

{ TGameSettings }
constructor TGameSettings.Create();
begin
  inherited Create();
  Victory := TVictory.Create();
  Extra := TExtraGameData.Create();
  Clear();
end;

destructor TGameSettings.Destroy();
begin
  Owner := nil;
  Victory.Free();
  Extra.Free();
  inherited Destroy();
end;

procedure TGameSettings.Clear();
begin
  Map := '';
  PlayersType := '';
  POV := '';
  POVEx := '';
  ObjectivesString := '';

  GameType := Low(TGameType);
  MapStyle := Low(TMapStyle);
  DifficultyLevel := Low(TDifficultyLevel);
  GameSpeed := Low(TGameSpeed);
  RevealMap := Low(TRevealMap);
  MapSize := Low(TMapSize);
  GameVersion := Low(TGameVersion);

  MapId := 0;
  PopLimit := 0;
  PlayTime := 0;
  LockDiplomacy := False;
  InGameCoop := False;
  IsFFA := False;
  Owner := nil;
  ScFileName := '';
  sGameSubVersion := '';
  Victory.Clear();
end;

function TGameSettings.GetGameType(): AnsiString;
begin
  Result := '';
  if (GameType in [Low(GAME_TYPES)..High(GAME_TYPES)]) then
    Result := GAME_TYPES[GameType];
end;

function TGameSettings.GetMapStyle(): AnsiString;
begin
  Result := '';
  if (MapStyle in [Low(MAP_STYLES)..High(MAP_STYLES)]) then
    Result := MAP_STYLES[MapStyle];
end;

function TGameSettings.GetDifficultyLevel(): AnsiString;
begin
  Result := '';
  if (GameVersion < gvAOC) then
  begin
    if (DifficultyLevel in [Low(AOK_DIFFICULTY_LEVELS)..High(AOK_DIFFICULTY_LEVELS)]) then
      Result := AOK_DIFFICULTY_LEVELS[DifficultyLevel];
  end else
  begin
    if (DifficultyLevel in [Low(AOC_DIFFICULTY_LEVELS)..High(AOC_DIFFICULTY_LEVELS)]) then
      Result := AOC_DIFFICULTY_LEVELS[DifficultyLevel];
  end;
end;

function TGameSettings.GetGameSpeed(): AnsiString;
begin
  Result := '';
  if (Ord(GameSpeed) in [100, 150, 200]) then
    Result := GAME_SPEEDS[(Ord(GameSpeed) - 100) div 50];
end;

function TGameSettings.GetRevealMap(): AnsiString;
begin
  Result := '';
  if (RevealMap in [Low(REVEAL_SETTINGS)..High(REVEAL_SETTINGS)]) then
    Result := REVEAL_SETTINGS[RevealMap];
end;

function TGameSettings.GetMapSize(): AnsiString;
begin
  Result := '';
  if (MapSize in [Low(MAP_SIZES)..High(MAP_SIZES)]) then
    Result := MAP_SIZES[MapSize];
end;

function TGameSettings.GetGameVersion(): AnsiString;
begin
  Result := '';
  if not (GameVersion in [Low(GAME_VERSIONS)..High(GAME_VERSIONS)]) then
    Exit;
  Result := GAME_VERSIONS[GameVersion];
end;

function TGameSettings.GetIsScenario(): Boolean;
begin
  Result := (GameType = gtScenario);
end;

{ TRecAnalyst }

constructor ERecAnalystException.Create(Code: Integer);
begin
  inherited Create(ErrorMessages[Code]);
  fCode := Code;
end;

constructor TRecAnalyst.Create();
begin
  inherited Create();
  fHeaderStream := TMemStream.Create();
  fBodyStream := TMemStream.Create();
  GameSettings := TGameSettings.Create();
  Players := TPlayerList.Create();
  Teams := TTeamList.Create();
  PreGameChatMessages := TObjectList.Create();
  InGameChatMessages := TObjectList.Create();
  Tributes := TObjectList.Create();
  Researches := TObjectList.Create();
  Units := TTrainedUnitList.Create();
  GaiaObjects := TObjectList.Create();
  PlayerObjects := TObjectList.Create();
  fMapImage := TMemoryStream.Create();
  fFileName := '';
  Reset();
end;

destructor TRecAnalyst.Destroy();
begin
  fHeaderStream.Free();
  fBodyStream.Free();
  GameSettings.Free();
  Players.Free();
  Teams.Free();
  PreGameChatMessages.Free();
  InGameChatMessages.Free();
  Tributes.Free();
  Units.Free();
  Researches.Free();
  GaiaObjects.Free();
  PlayerObjects.Free();
  SetLength(fMapData, 0);
  fMapImage.Free();
  inherited Destroy();
end;

procedure TRecAnalyst.Reset();
begin
  fIsMgl := False;
  fIsMgx := False;
  fIsMgz := False;
  fIsUserPatch := False;
  fHeaderStream.Clear();
  fBodyStream.Clear();
  fMapWidth := 0;
  fMapHeight := 0;
  SetLength(fMapData, 0);
  fMapData := nil;
  fAnalyzeTime := 0;
  header_len := 0;
  next_pos := 0;
  GameSettings.Clear();
  Players.Clear();
  Teams.Clear();
  PreGameChatMessages.Clear();
  InGameChatMessages.Clear();
  Tributes.Clear();
  Researches.Clear();
  Units.Clear();
  GaiaObjects.Clear();
  PlayerObjects.Clear();
  fMapImage.Clear();
  fMapImageSize.cx := 0;
  fMapImageSize.cy := 0;
  {$IFDEF EXTENDED}
  fKeepStreams := False;
  objectives_pos := 0;
  CommentString := '';
  {$ENDIF}
  fAnalyzed := False;
end;

class function TRecAnalyst.GameTimeToString(Time: Integer): AnsiString;
var
  Hours, Minutes, Seconds: Integer;
begin
  if (Time < 0) then Time := 0;
  Hours   := Time div 1000 div 3600;
  Minutes := Time div 1000 div 60 mod 60;
  Seconds := Time div 1000 mod 60;
  Result := AnsiString(Format('%.2d:%.2d:%.2d', [Hours, Minutes, Seconds]));
end;

procedure TRecAnalyst.ExtractStreams();
var
  ms, inStream: TMemoryStream;
  count: Int64;
const
  MGL_EXT = '.mgl';
  MGX_EXT = '.mgx';
  MGZ_EXT = '.mgz';
  NO_HEADER = -15; // raw inflate
begin
  if (fFileName = '') then
    raise ERecAnalystException.Create(RECANALYST_NOFILE);

  if (LowerCase(ExtractFileExt(fFileName)) = MGL_EXT) then
    fIsMgl := True
  else if (LowerCase(ExtractFileExt(fFileName)) = MGX_EXT) then
    fIsMgx := True
  else if (LowerCase(ExtractFileExt(fFilename)) = MGZ_EXT) then
  begin
    fIsMgx := True; fIsMgz := True;
  end else
    raise ERecAnalystException.Create(RECANALYST_FILEEXT);

  ms := TMemoryStream.Create();
  inStream := TMemoryStream.Create();
  try
    try
      ms.LoadFromFile(fFileName);
      ms.Seek(0, soFromBeginning);

      if (ms.Read(header_len, SizeOf(header_len)) < SizeOf(header_len)) then
        raise ERecAnalystException.Create(RECANALYST_HEADLENREAD);

      if (header_len = 0) then
        raise ERecAnalystException.Create(RECANALYST_EMPTYHEADER);

      if fIsMgx then
        ms.Read(next_pos, SizeOf(next_pos));

      if fIsMgx then
        Dec(header_len, SizeOf(next_pos) + SizeOf(header_len))
      else
        Dec(header_len, SizeOf(header_len));

      inStream.CopyFrom(ms, header_len);
      instream.Seek(0, soFromBeginning);

      {$IFDEF FPC}
      if (ZDecompressStream2(inStream, fHeaderStream, NO_HEADER) < 0) then
        raise ERecAnalystException.Create(RECANALYST_DECOMP);
      // zError (code)
      {$ELSE}
      ZDecompressStream2(inStream, fHeaderStream, NO_HEADER);
      {$ENDIF}

      count := ms.Size - header_len - SizeOf(header_len);
      if fIsMgx then count := count - SizeOf(next_pos);
      fBodyStream.CopyFrom(ms, count);
    except
      on ERecAnalystException do
        raise;
      on EReadError do
        raise ERecAnalystException.Create(RECANALYST_FILEREAD);
      on EFOpenError do
        raise ERecAnalystException.Create(RECANALYST_FILEOPEN);
      {$IFNDEF FPC}
      on EZDecompressionError do
        raise ERecAnalystException.Create(RECANALYST_DECOMP);
      {$ENDIF}
      else
        raise ERecAnalystException.Create(RECANALYST_UNKNOWN);
    end;
  finally
    FreeAndNil(ms);
    FreeAndNil(inStream);
  end;
end;

procedure TRecAnalyst.AnalyzeHeader();
const
  constant2: array[0..7] of AnsiChar = (#$9A, #$99, #$99, #$99, #$99, #$99, #$F9, #$3F);
  separator: array[0..3] of AnsiChar = (#$9D, #$FF, #$FF, #$FF);
  scenario_constant: array[0..3] of AnsiChar = (#$F6, #$28, #$9C, #$3F);
  aok_separator: array[0..3] of AnsiChar = (#$9A, #$99, #$99, #$3F);
  aoc_subversion: Double = 11.76;
var
  buff: array[0..7] of Byte;
  version: array[0..7] of AnsiChar;
  trigger_info_pos: Longint;
  game_settings_pos: Longint;
  scenario_header_pos: Longint;
  map_id: Int32;
  difficulty: Int32;
  i, j, x, y: Integer;
  player_data_index, human, name_len: Int32;
  buff256: array[0..MAXBYTE] of AnsiChar;
  buff65536: array[0..MAXWORD] of AnsiChar;
  Player: TPlayer;
  num_trigger: Int32;
  reveal_map, map_size, pop_limit: Int32;
  game_type, lock_diplomacy: Byte;
  lock_teams: Boolean;
  num_chat: Int32;
  include_ai: Boolean;
  string_length: Int32;
  num_string, num_rule: Word;
  game_speed: Int32;
  rec_player_ref: Word;
  num_player: Byte;
  map_size_x, map_size_y: Int32;
  num_unknown_data, num_float: Int32;
  terrain_id, elevation: Byte;
  desc_len, num_effect, num_selected_object, text_len, sound_len: Int32;
  num_condition: Int32;
  team_indexes: array[0..7] of Byte;
  unknown25, victory_condition: Int32;
  is_timelimit: Byte;
  time_limit: Single;
  num_data, num_couples, map_size_x2, map_size_y2, num_unknown_data2: Int32;
  subversion: Single;
  rounded_subversion: Double;
  game_mode: Word;
begin
  FillChar(buff, SizeOf(buff), 0);
  FillChar(buff256, SizeOf(buff256), #0);
  with fHeaderStream do
  begin
    Seek(0, soFromBeginning);
    { getting version }
    FillChar(version, SizeOf(version), #0);
    ReadBuffer(version, SizeOf(version));
    ReadFloat(subversion);
    rounded_subversion := RoundTo(subversion, -2);
    // AOE2HD: 2.0 = 11.80, 2.3 = 11.90, 2.5 = 11.91, 2.6 = 11.91, 2.8 = 11.93
    if (version = VER_94) then
    begin
      if fIsMgz then
        GameSettings.GameVersion := gvAOCUP11
      else if (rounded_subversion > aoc_subversion) then
        GameSettings.GameVersion := gvAOE2HD
      else
        GameSettings.GameVersion := gvAOC;
    end else if (version = VER_93) then
      GameSettings.GameVersion := gvAOK
    else if (version = TRL_93) and fIsMgx then
      GameSettings.GameVersion := gvAOCTrial
    else if (version = TRL_93) and fIsMgl then
      GameSettings.GameVersion := gvAOKTrial
    else if (version = VER_95) then
      GameSettings.GameVersion := gvAOFE21
    else if (version = VER_98) then
      GameSettings.GameVersion := gvAOCUP12
    else if (version = VER_99) then
      GameSettings.GameVersion := gvAOCUP13
    else if (version = VER_9A) then
      GameSettings.GameVersion := gvAOCUP14 { 1.4 (RC1) }
    else if (version = VER_9B) then
      GameSettings.GameVersion := gvAOCUP14 { 1.4 (RC2) }
    else
      GameSettings.GameVersion := gvUnknown;

    case GameSettings.GameVersion of
      gvAOK, gvAOKTrial:
        begin
          fIsMgl := True; fIsMgx := False; fIsMgz := False;
        end;
      gvAOC, gvAOCTrial, gvAOE2HD:
        begin
          fIsMgl := False; fIsMgx := True; fIsMgz := False;
        end;
      gvAOFE21, gvAOCUP11..gvAOCUP14:
        begin
          fIsMgl := False; fIsMgx := True; fIsMgz := True;
          fIsUserPatch := True;
        end;
    end;

    if (GameSettings.GameVersion = gvAOE2HD) then
    begin
      if (rounded_subversion = 11.80) then
        GameSettings.sGameSubversion := '2.0'
      else if (rounded_subversion = 11.90) then
        GameSettings.sGameSubversion := '2.3'
      else if (rounded_subversion = 11.91) then
        GameSettings.sGameSubversion := '2.6' { or 2.5, using higher one }
      else if (rounded_subversion = 11.93) then
        GameSettings.sGameSubversion := '2.8'
    end else if (GameSettings.GameVersion = gvAOCUP14) then
    begin
      if (version = VER_9A) then
        GameSettings.sGameSubversion := 'RC1'
      else if (version = VER_9B) then
        GameSettings.sGameSubversion := 'RC2';
    end;

    { getting Trigger_info position }
    Seek(-SizeOf(constant2), soFromEnd);
    trigger_info_pos := FindReverse(constant2);
    if (trigger_info_pos = -1) then
      raise ERecAnalystException.Create(RECANALYST_NOTRIGG);
    { getting Game_settings position }
    game_settings_pos := FindReverse(separator);
    if (game_settings_pos = -1) then
      raise ERecAnalystException.Create(RECANALYST_NOGAMESETS);
    { getting Scenario_header position }
    if fIsMgx then
      scenario_header_pos := FindReverse(scenario_constant)
    else
      scenario_header_pos := FindReverse(aok_separator);
    if (scenario_header_pos <> -1) then
      scenario_header_pos := scenario_header_pos - SizeOf(scenario_constant) - SizeOf(Int32) {next_unit_id};
    { getting Game_Settings data }
    { skip negative[2] }
    Seek(game_settings_pos + 8, soFromBeginning);
    if fIsMgx then ReadInt32(map_id);
    ReadInt32(difficulty);
    ReadBool(lock_teams); { duplicated data, see lock_diplomacy }
    if fIsMgx then
    begin
      GameSettings.MapId := map_id;
      if (map_id in miRealWorldMaps) then
        GameSettings.MapStyle := msRealWorld
      else if (map_id = miCustom) then
        GameSettings.MapStyle := msCustom
      else
        GameSettings.MapStyle := msStandard;
      GameSettings.Map := MapIdToName(map_id);
    end;
    GameSettings.DifficultyLevel := TDifficultyLevel(difficulty);
    GameSettings.LockDiplomacy := lock_teams;
    { getting Player_info data }
    for i := 0 to 8 do
    begin
      ReadInt32(player_data_index);
      ReadInt32(human);
      ReadString(buff256);
      { sometimes very rarely index is 1 }
      if (human = 0) or (human = 1) then Continue;
      if (i <> 0) then
      begin
        Player := TPlayer.Create();
        Player.Name := buff256;
        Player.Index := player_data_index;
        Player.Human := (human = $02);
        Players.AddPlayer(Player);
      end;
    end;
    { getting game type for aok }
    if fIsMgl then
    begin
      Seek(trigger_info_pos - SizeOf(constant2), soFromBeginning);
      Seek(-6);
      { unknown25 }
      ReadInt32(unknown25);
      case unknown25 of
          1: GameSettings.GameType := gtDeathMatch;
        256: GameSettings.GameType := gtRegicide;
      end;
    end;
    { getting victory }
    Seek(trigger_info_pos - SizeOf(constant2), soFromBeginning);
    SeekIf(fIsMgx, -7);
    Seek(-110);
    ReadInt32(victory_condition);
    Seek(8);
    ReadChar(is_timelimit);
    if (is_timelimit <> 0) then
      ReadFloat(time_limit);

    with GameSettings.Victory do
    begin
      VictoryCondition := TVictoryCondition(victory_condition);
      if (is_timelimit <> 0) then
        TimeLimit := Round(time_limit) div 10;
    end;
    { Trigger_info }
    Seek(trigger_info_pos + 1, soFromBeginning);
    { always zero in mgl? or not a really trigger_info here for aok }
    ReadInt32(num_trigger);
    if (num_trigger <> 0) then
    begin
      { skip Trigger_info data }
      for i := 0 to num_trigger - 1 do
      begin
        Seek(18);
        ReadInt32(desc_len);
        Seek(desc_len);
        ReadInt32(name_len);
        Seek(name_len);
        ReadInt32(num_effect);
        for j := 0 to num_effect - 1 do
        begin
          Seek(24);
          ReadInt32(num_selected_object);
          if (num_selected_object = -1) then
            num_selected_object := 0;
          Seek(72);
          ReadInt32(text_len);
          Seek(text_len);
          ReadInt32(sound_len);
          Seek(sound_len);
          Seek(num_selected_object * 4);
        end;
        Seek(num_effect * 4);
        ReadInt32(num_condition);
        Seek(76 * num_condition);
      end;
      Seek(num_trigger * 4);
      GameSettings.Map := '';
      GameSettings.GameType := gtScenario;  { obsolete? }
    end;
    { Other_data }
    ReadBuffer(team_indexes, SizeOf(team_indexes));

    for i := 0 to Players.Count - 1 do
      Players[i].Team := team_indexes[i] - 1;

    Seek(1);  { always 1? }
    ReadInt32(reveal_map);
    Seek(4);  { always 1? }
    ReadInt32(map_size);
    ReadInt32(pop_limit);
    if fIsMgx then
    begin
      ReadChar(game_type);
      ReadChar(lock_diplomacy);
    end;

    with GameSettings do
    begin
      RevealMap := TRevealMap(reveal_map);
      MapSize := TMapSize(map_size);
      PopLimit := pop_limit;
      if fIsMgx then
      begin
        LockDiplomacy := (lock_diplomacy = 1);
        GameType := TGameType(game_type);
      end;
    end;
    { here comes pre-game chat (mgl doesn't contain this information }
    if fIsMgx then
    begin
      ReadInt32(num_chat);
      for i := 0 to num_chat - 1 do
      begin
        ReadString(buff65536);
        ProcessPreGameChat(buff65536);
      end;
    end;
    { skip AI_info if exists }
    Seek(12, soFromBeginning);
    ReadBool(include_ai);
    if (include_ai) then
    begin
      Seek(2);
      ReadWord(num_string);
      Seek(4);
      for i := 0 to num_string - 1 do
      begin
        ReadInt32(string_length);
        Seek(string_length);
      end;
      Seek(6);
      for i := 0 to 7 do
      begin
        Seek(10);
        ReadWord(num_rule);
        Seek(4 + 400 * num_rule);
      end;
      Seek(5544);
    end;
    { getting data }
    Seek(4);
    ReadInt32(game_speed);
    Seek(37);
    ReadWord(rec_player_ref);
    ReadChar(num_player);
    Dec(num_player);
    if fIsMgx then Seek(2);
    ReadWord(game_mode);

    GameSettings.GameSpeed := TGameSpeed(game_speed);
    if (game_mode = 1) then
      GameSettings.GameMode := gmSinglePlayer
    else
      GameSettings.GameMode := gmMultiPlayer;

    Player := Players.GetPlayerByIndex(rec_player_ref);
    if Assigned(Player) then
    begin
      Player.Owner := True;
      with GameSettings do
      begin
        Owner := Player;
        POV := Player.Name;
        POVEx := Player.Name;
      end;
    end;
    GameSettings.InGameCoop := (num_player < Players.Count);

    Inc(num_player);

    { getting map }
    Seek(58);
    SeekIf(fIsMgl, -2);
    ReadInt32(map_size_x);
    ReadInt32(map_size_y);
    fMapWidth := map_size_x;
    fMapHeight := map_size_y;

    ReadInt32(num_unknown_data);
    { unknown data }
    for i := 0 to num_unknown_data - 1 do
    begin
      Seek(1275 + map_size_x * map_size_y);
      ReadInt32(num_float);
      Seek((num_float * 4) + 4);
    end;
    Seek(2);

    SetLength(fMapData, map_size_x, map_size_y);
    { map data }
    for y := 0 to map_size_y - 1 do
      for x := 0 to map_size_x - 1 do
      begin
        ReadChar(terrain_id);
        ReadChar(elevation);
        //fMapData[x, y] := terrain_id + 1000 * (elevation + 1);
        fMapData[x, y] := terrain_id;
      end;

    ReadInt32(num_data);
    Seek(4 + 4 * num_data);
    for i := 0 to num_data - 1 do
    begin
      ReadInt32(num_couples);
      Seek(num_couples * 8);
    end;
    ReadInt32(map_size_x2);
    ReadInt32(map_size_y2);
    Seek((map_size_x2 * map_size_y2 * 4) + 4);
    ReadInt32(num_unknown_data2);
    Seek(27 * num_unknown_data2 + 4);
    { getting Player_info }
    if (GameSettings.GameVersion <> gvAOE2HD) then
      ReadPlayerInfoBlockEx(num_player)
    else
      ReadPlayerInfoBlock(num_player);
    { getting objectives or instructions }
    if (scenario_header_pos > -1) then
    begin
      Seek(scenario_header_pos + 4433, soFromBeginning);
      { original scenario file name }
      ReadString(buff65536, 2);
      if (buff65536[0] <> #0) then
      begin
        GameSettings.ScFileName := buff65536;
        if fIsMgl then
          GameSettings.GameType := gtScenario; { this way we detect scenarios in mgl, is there any other way? }
      end;
      SeekIfElse(fIsMgx, 24, 20);
      { scenario instruction or Objectives string, depends on game type }
      {$IFDEF EXTENDED}objectives_pos := Position;{$ENDIF}
      ReadString(buff65536, 2);
      if (buff65536[0] <> #0) and not GameSettings.IsScenario then
        GameSettings.ObjectivesString := buff65536;
    end;
  end;
end;

procedure TRecAnalyst.AnalyzeBody();
var
  time_cnt: Int32;
  m_body_len: Integer;
  od_type, command, time: Int32;
  unknown, length: Int32;
  cmd, ver: Byte;
  buff256: array[0..MAXBYTE] of AnsiChar;
  player_id: Word;
  unit_type_id, building_type_id: Word;
  next_command_block: Int32;
  human_train_unit_cmd: THumanTrainUnitCmdDataStruct;
  tribute_cmd: TTributeCmdDataStruct;
  resign_cmd: TResignCmdDataStruct;
  research_cmd: TResearchCmdDataStruct;
  game_data_cmd: TGameDataStruct;
begin
  time_cnt := Ord(GameSettings.GameSpeed);
  m_body_len := fBodyStream.Size;
  FillChar(buff256, SizeOf(buff256), 0);
  with fBodyStream do
  begin
    Seek(0, soFromBeginning);
    while (Position < m_body_len - 3) do
    begin
      if (Position = 0) and fIsMgl then
        od_type := $04
      else
        ReadInt32(od_type);
      { ope_data types: 4(Game_start or Chat), 2(Sync), or 1(Command) }
      case od_type of
        $04, $03:
          begin
            ReadInt32(command);
            if (command = $01F4) then
            begin
              { Game_start }
              if fIsMgl then
              begin
                Seek(28);
                ReadChar(ver);
                case ver of
                  0: if (GameSettings.GameVersion <> gvAOKTrial) then
                    GameSettings.GameVersion := gvAOK20;
                  1: GameSettings.GameVersion := gvAOK20a;
                end;
                Seek(3);
              end else begin
                case od_type of
                  $03: if (GameSettings.GameVersion <> gvAOCTrial) then
                    GameSettings.GameVersion := gvAOC10;
                  $04: if (GameSettings.GameVersion = gvAOC) then
                    GameSettings.GameVersion := gvAOC10c;
                end;
                Seek(20);
              end;
            end
            else if (command = -1) then
            begin
              { Chat }
              ReadString(buff256);
              ProcessInGameChat(buff256, time_cnt);
            end;
          end;
        $02:
          begin
            { Sync }
            ReadInt32(time);
            Inc(time_cnt, time); { time_cnt is in miliseconds }
            ReadInt32(unknown);
            SeekIf(unknown = 0, 28);
            Seek(12);
          end;
        $01:
          begin
            { Command }
            ReadInt32(length);
            ReadChar(cmd);
            Seek(-1);
            case cmd of
              $0B: { player resign }
                begin
                  Seek(1);
                  ReadBuffer(resign_cmd, SizeOf(resign_cmd));
                  ProcessResignCommand(resign_cmd, time_cnt);
                  Seek(length - 4);
                end;
              $65: { research }
                begin
                  Seek(4);
                  ReadBuffer(research_cmd, SizeOf(research_cmd));
                  ProcessResearchCommand(research_cmd, time_cnt);
                  Seek(length - 12);
                end;
              $77: { training unit }
                begin
                  Seek(4);
                  ReadBuffer(human_train_unit_cmd, SizeOf(human_train_unit_cmd));
                  ProcessHumanTrainUnitCommand(human_train_unit_cmd);
                  Seek(length - 12);
                end;
              $64: { pc trains unit }
                begin
                  Seek(10);
                  ReadWord(unit_type_id);
                  ProcessPCTrainUnitCommand(unit_type_id);
                  Seek(length - 12);
                end;
              $66: { construction }
                begin
                  Seek(2);
                  { player_id }
                  ReadWord(player_id);
                  Seek(8);
                  { building_type_id unit_type_id }
                  ReadWord(building_type_id);
                  ProcessConstructionCommand(player_id, building_type_id);
                  Seek(length - 14);
                end;
              $6C: { tribute }
                begin
                  Seek(1);
                  ReadBuffer(tribute_cmd, SizeOf(tribute_cmd));
                  ProcessTributeCommand(tribute_cmd, time_cnt);
                  Seek(length - 12);
                end;
              $03, 78, $00, $75, $6F, $10: Seek(length);
              $20: Seek(length); { save chapter }
              $FF: { exists only in UP 1.4 RC2 multiplayer mode }
                begin
                  Seek(1);
                  ReadBuffer(game_data_cmd, SizeOf(game_data_cmd));
                  ProcessGameDataCommand(game_data_cmd);
                end;
              else Seek(length);
            end;
            Seek(4);
          end;
        else begin
          { detect if this is a header of saved chapter }
          { sometimes header of the saved chapter is in $03 command, instead of $20 as it should be,
            when this happens the length of $20 command is $0E, otherwise it is $02 (always?, rule?),
            we do not rely on it, that's why we are skipping saved chapter data here and not in $20 command }
          if (Position = next_pos - header_len - 4) then
          begin
            { this is a header of saved chapter data, we have already read next_command_block,
              that's why -4 in the if-statement }
            { next_pos - header_len = offset of compressed chapter data }
            next_command_block := od_type;
            ReadInt32(next_pos); // next_chapter_pos
            Seek(next_command_block - header_len - 8, soFromBeginning);
            { or Seek(next_command_block - old_next_pos - 8); old_next_pos := next_pos; }
          end else
            { shouldn't occure, just to prevent unexpected endless cycling }
            Seek(1);
        end;
      end;
    end;  { endwhile }
    GameSettings.PlayTime := time_cnt;
  end;
end;

procedure TRecAnalyst.Analyze();
var
  StartTime: LongWord;
begin
  try
    Reset();
    StartTime := GetTickCount();
    ExtractStreams();
    AnalyzeHeader();
    AnalyzeBody();
    PostAnalyze();
    fAnalyzed := True;
    fAnalyzeTime := GetTickCount() - StartTime;
  finally
    {$IFDEF EXTENDED}if not fKeepStreams then{$ENDIF}
    begin
      fHeaderStream.Clear();
      fBodyStream.Clear();
    end;
  end;
end;

procedure TRecAnalyst.BuildTeams();
var
  Player, Player_: TPlayer;
  Team: TTeam;
  i, j, k: Integer;
  found: Boolean;
begin
  if (Teams.Count > 0) then Exit;

  for i := 0 to Players.Count - 1 do
  begin
    Player := Players[i];
    if not Assigned(Player) then Continue;
    if (Player.Team = 0) then
    begin
      found := False;
      for j := 0 to Teams.Count - 1 do
      begin
        Team := Teams[j];
        if not Assigned(Team) then Continue;
        if (Team.Index <> Player.Team) then Continue;
        for k := 0 to Team.Count - 1 do
        begin
          Player_ := Team[k];
          if (Player_.Index = Player.Index) then
          begin
            Team.AddPlayer(Player);
            found := True;
            Break;
          end;
        end;
        if found then Break;
      end;
      if not found then
      begin
        Team := TTeam.Create();
        Team.AddPlayer(Player);
        Teams.AddTeam(Team);
      end;
    end else begin
      Team := Teams.GetTeamByIndex(Player.Team);
      if Assigned(Team) then
        Team.AddPlayer(Player)
      else begin
        Team := TTeam.Create();
        Team.AddPlayer(Player);
        Teams.AddTeam(Team);
      end;
    end;
  end;
end;

function TRecAnalyst.GenerateMap(const Width: Integer;
  const Height: Integer): TMemoryStream;
var
  x, y, i, terrain_id{, elevation}: Integer;
  Player: TPlayer;
  UO: TUnitObject;

  Bmp: TGPBitmap;
  Graphics, PngGraphics: TGPGraphics;

  GoldBrush, StoneBrush, CliffBrush, RelicBrush, FoodBrush: TGPSolidBrush;
  PlayerColor: COLORREF;
  PlayerBrush: TGPSolidBrush;
  PlayerPen: TGPPen;

  ns: TSize;
  sx, sy: Single;
  encoderClsid: TGUID;
  Png: TGPBitmap;
  Stream: IStream;
  NomadLikeMap: Boolean;
  gdiplusToken: ULONG;
begin
  Result := fMapImage;
  fMapImage.Position := 0;
  if (fMapImage.Size > 0) and (Width = fMapImageSize.cx) and (Height = fMapImageSize.cy) then
    Exit;
  if not fAnalyzed or not Assigned(fMapData) then
    Exit;

  InitializeGdiplus(gdiplusToken);

  Bmp := TGPBitmap.Create(fMapWidth, fMapHeight, PixelFormat24bppRGB);
  Graphics := TGPGraphics.Create(Bmp);

  try
    for x := 0 to fMapWidth - 1 do
    begin
      for y := 0 to fMapHeight - 1 do
      begin
        terrain_id := fMapData[x, y];
        //terrain_id := fMapData[x, y] mod 1000;
        //elevation := (fMapData[x, y] - terrain_id) div 1000;
        //Dec(elevation);

        if (terrain_id in [Low(TERRAIN_COLORS)..High(TERRAIN_COLORS)]) then
          Bmp.SetPixel(x, y, ColorRefToARGB(TERRAIN_COLORS[terrain_id]))
        else
          Bmp.SetPixel(x, y, ColorRefToARGB(UNKNOWN_TERRAIN_COLOR));
      end;
    end;

    { draw gaia objects }
    GoldBrush := TGPSolidBrush.Create(ColorRefToARGB(GOLD_COLOR));
    StoneBrush := TGPSolidBrush.Create(ColorRefToARGB(STONE_COLOR));
    CliffBrush := TGPSolidBrush.Create(ColorRefToARGB(CLIFF_COLOR));
    RelicBrush := TGPSolidBrush.Create(ColorRefToARGB(RELIC_COLOR));
    FoodBrush := TGPSolidBrush.Create(ColorRefToARGB(FOOD_COLOR));
    try
      for i := 0 to GaiaObjects.Count - 1 do
      begin
        UO := GaiaObjects[i] as TGaiaObject;
        case UO.Id of
          uiGoldMine:
            begin
              Graphics.FillRectangle(GoldBrush, MakeRect(Rect(UO.Position.X - 1,
                UO.Position.Y - 1, UO.Position.X + 2, UO.Position.Y + 2)));
            end;
          uiStoneMine:
            begin
              Graphics.FillRectangle(StoneBrush, MakeRect(Rect(UO.Position.X - 1,
                UO.Position.Y - 1, UO.Position.X + 2, UO.Position.Y + 2)));
            end;
          uiCliff1..uiCliff10:
            begin
              Graphics.FillRectangle(CliffBrush, MakeRect(Rect(UO.Position.X - 2,
                UO.Position.Y - 1, UO.Position.X + 2, UO.Position.Y + 2)));
            end;
          uiRelic:
            begin
              Graphics.FillRectangle(RelicBrush, MakeRect(Rect(UO.Position.X - 1,
                UO.Position.Y - 1, UO.Position.X + 2, UO.Position.Y + 2)));
            end;
          uiForageBush, uiDeer, uiBoar, uiJavelina, uiTurkey, uiSheep:
            begin
              Graphics.FillRectangle(FoodBrush, MakeRect(Rect(UO.Position.X - 1,
                UO.Position.Y - 1, UO.Position.X + 2, UO.Position.Y + 2)));
            end;
        end;
      end;
    finally
      GoldBrush.Free();
      StoneBrush.Free();
      CliffBrush.Free();
      RelicBrush.Free();
      FoodBrush.Free();
    end;

    { draw positions }
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    NomadLikeMap := GameSettings.MapId = miNomad;
    if (not NomadLikeMap and (GameSettings.MapId = miCustom) and (Players.Count > 0)) then
    begin
      Player := nil;
      for i := 0 to Players.Count - 1 do
        if (Players[i].CivId <> cHuns) then
        begin
          Player := Players[i];
          Break;
        end;
      if Assigned(Player) then
        NomadLikeMap := (Player.InitialState.HouseCapacity = 0)
      else
        // usually no scout in nomad maps
        NomadLikeMap := (Players[0].InitialState.MilitaryPop = 0);
    end;

    if not GameSettings.IsScenario and not NomadLikeMap then
    begin
      { we do not draw positions in scenarios as they may be set anywhere }
      for i := 0 to Players.Count - 1 do
      begin
        Player := Players[i];
        if Player.IsCooping then Continue;
        with Player.InitialState do
        begin
          if (Player.Color - 1 in [Low(PLAYER_COLORS)..High(PLAYER_COLORS)]) then
            PlayerColor := PLAYER_COLORS[Player.Color - 1]
          else
            PlayerColor := 0; { black }
          PlayerPen := TGPPen.Create(ColorRefToARGB(PlayerColor));
          PlayerBrush := TGPSolidBrush.Create(ColorRefToARGB(PlayerColor));
          try
            Graphics.DrawEllipse(PlayerPen, MakeRect(Rect(Position.X - 9,
              Position.Y - 9, Position.X + 9, Position.Y + 9)));

            Graphics.FillEllipse(PlayerBrush, MakeRect(Rect(Position.X - 4,
              Position.Y - 4, Position.X + 4, Position.Y + 4)));
          finally
            PlayerPen.Free();
            PlayerBrush.Free();
          end;
        end;
      end;
    end;

    Graphics.SetSmoothingMode(SmoothingModeNone);

    { draw player objects }
    for i := 0 to PlayerObjects.Count - 1 do
    begin
      UO := PlayerObjects[i] as TUnitObject;
      if not Assigned(UO.Owner) then
        Continue;
      if (UO.Owner.Color - 1 in [Low(PLAYER_COLORS)..High(PLAYER_COLORS)]) then
        PlayerColor := PLAYER_COLORS[UO.Owner.Color - 1]
      else
        PlayerColor := 0; { black }
      PlayerBrush := TGPSolidBrush.Create(ColorRefToARGB(PlayerColor));
      try
        Graphics.FillRectangle(PlayerBrush, MakeRect(Rect(UO.Position.X - 1,
          UO.Position.Y - 1, UO.Position.X + 1, UO.Position.Y + 1)));
      finally
        PlayerBrush.Free();
      end;
    end;

    ns := SizeAfterRotation(-45, fMapWidth, fMapHeight);
    sx := Width / ns.cx;
    sy := Height / ns.cy;

    GetEncoderClsid('image/png', encoderClsid);

    Png := TGPBitmap.Create(Width, Height, PixelFormat32bppARGB);
    PngGraphics := TGPGraphics.Create(Png);
    try
      PngGraphics.TranslateTransform(Png.GetWidth / 2.0, Png.GetHeight / 2.0);
      PngGraphics.ScaleTransform(sx, sy);
      PngGraphics.RotateTransform(-45);
      PngGraphics.DrawImage(Bmp, -(fMapWidth / 2), -(fMapHeight / 2));

      fMapImage.Clear();
      Stream := TStreamAdapter.Create(fMapImage, soReference);
      Png.Save(Stream, encoderClsid, nil);
      fMapImage.Position := 0;
    finally
      Png.Free();
      PngGraphics.Free();
    end;
  finally
    Bmp.Free();
    Graphics.Free();
    FinalizeGdiplus(gdiplusToken);
  end;
end;

procedure TRecAnalyst.PostAnalyze();
var
  Player: TPlayer;
  Team: TTeam;
  i, j, idx, EndTime: Integer;
  team_ary: array[0..7] of Integer;
  Lines, CoopList: TStringList;
  MapFound: Boolean;
  CP: String;
  ChatMessage: TChatMessage;
begin
  with GameSettings do
    if not IsScenario then
    begin
      Lines := TStringList.Create();
      try
        Lines.Text := String(ObjectivesString);
        { get map }
        if fIsMgl or (MapId = miCustom) then
        begin
          if (Lines.Count > 2) then
          begin
            idx := Pos(': ', Lines[2]);
            if (idx <> 0) then
              Map := AnsiString(Copy(AnsiString(Lines[2]), idx + Length(': '), Length(Lines[2])));

            if fIsMgl then
            begin
              MapFound := False;
              for i := 0 to LANGUAGES_NUM - 1 do
              begin
                for j := 0 to MAPS_NUM - 1 do
                  if (LOC_MAP_NAMES[i][j] = Map) then
                  begin
                    Map := MAPS[j].Name;
                    MapFound := True;
                    Break;
                  end;
                if MapFound then Break;
              end;
            end;
          end;
        end;
        {$IFDEF EXTENDED}
        { extract comment }
        idx := Lines.IndexOf(''); { #$0A#$0A separator }
        if (idx <> -1) then
        begin
          { comment exists }
          if (idx < Lines.Count) then
          begin
            ObjectivesString := '';
            for i := 0 to idx - 1 do
              ObjectivesString := ObjectivesString + AnsiString(Lines[i]) + #$0A;
            ObjectivesString := Copy(ObjectivesString, 1, Length(ObjectivesString) - 1);

            CommentString := '';
            for i := idx + 1 to Lines.Count - 1 do
              CommentString := CommentString + AnsiString(Lines[i]) + #$0A;
            CommentString := Copy(CommentString, 1, Length(CommentString) - 1);
          end;
        end;
        {$ENDIF}
      finally
        Lines.Free();
      end;
    end;

  BuildTeams();

  { PlayersType }
  idx := 0;
  FillChar(team_ary, SizeOf(team_ary), 0);
  for i := 0 to Teams.Count - 1 do
  begin
    Team := Teams[i];
    { tmp_ary[idx] = Team.Count - Cooping Players }
    for j := 0 to Team.Count - 1 do
    begin
      Player := Team[j];
      if not Player.IsCooping then
        Inc(team_ary[idx]);
    end;
    Inc(idx);
  end;
  idx := 0; { ArraySum }
  for i := Low(team_ary) to High(team_ary) do
    Inc(idx, team_ary[i]);
  if (idx = Teams.Count) and (Teams.Count > 2) then
  begin
    GameSettings.IsFFA := True;
    GameSettings.PlayersType := 'FFA';
  end else
  begin
    GameSettings.PlayersType := '';
    for i := Low(team_ary) to High(team_ary) do
    begin
      if (team_ary[i] = 0) then Continue;
      GameSettings.PlayersType := GameSettings.PlayersType + 'v' + AnsiString(IntToStr(team_ary[i]));
    end;
    Delete(GameSettings.PlayersType, 1, Length('v'));
  end;

  { PovEx }
  Player := GameSettings.Owner;
  if not Assigned(Player) then Exit;

  CoopList := TStringList.Create();
  try
    for i := 0 to Players.Count - 1 do
    begin
      Player := Players[i];
      if Player = GameSettings.Owner then Continue;
      if (Player.Index <> GameSettings.Owner.Index) then Continue;
      CoopList.Add(String(Player.Name));
    end;
    if (CoopList.Count > 0) then
    begin
      CP := StringReplace(CoopList.Text, sLineBreak, ', ', [rfReplaceAll]);
      CP := Copy(CP, 1, Length(CP) - Length(', '));
      GameSettings.POVEx := GameSettings.POVEx + AnsiString(Format(' (%s)', [CP]));
    end;
  finally
    CoopList.Free();
  end;

  { fix: player could click age advance, but game finished before reaching specific age or player has resigned }
  for i := 0 to Players.Count - 1 do
  begin
    with Players[i] do
    begin
      if (ResignTime < DisconnectTime) then EndTime := ResignTime else EndTime := DisconnectTime;
      if (FeudalTime > GameSettings.PlayTime) or ((EndTime > 0) and (FeudalTime > EndTime)) then
        Player.FeudalTime := 0;
      if (CastleTime > GameSettings.PlayTime) or ((EndTime > 0) and (CastleTime > EndTime)) then
        Player.CastleTime := 0;
      if (ImperialTime > GameSettings.PlayTime) or ((EndTime > 0) and (ImperialTime > EndTime)) then
        Player.ImperialTime := 0;
    end;
  end;

  { add age advance messages }
  for i := 0 to Players.Count - 1 do
  begin
    Player := Players[i];
    if not Assigned(Player) then Continue;

    if (Player.FeudalTime <> 0) then
    begin
      ChatMessage := TChatMessage.Create();
      ChatMessage.Time := Player.FeudalTime;
      ChatMessage.Msg := AnsiString(Format(c_feudal_age_advance, [Player.Name]));
      InGameChatMessages.Add(ChatMessage);
    end;
    if (Player.CastleTime <> 0) then
    begin
      ChatMessage := TChatMessage.Create();
      ChatMessage.Time := Player.CastleTime;
      ChatMessage.Msg := AnsiString(Format(c_castle_age_advance, [Player.Name]));
      InGameChatMessages.Add(ChatMessage);
    end;
    if (Player.ImperialTime <> 0) then
    begin
      ChatMessage := TChatMessage.Create();
      ChatMessage.Time := Player.ImperialTime;
      ChatMessage.Msg := AnsiString(Format(c_imperial_age_advance, [Player.Name]));
      InGameChatMessages.Add(ChatMessage);
    end;
  end;

  if (Units.Count > 0) then
    Units.Sort(@UnitsCompare);

  if (Researches.Count > 0) then
    Researches.Sort(@ResearchesCompare);

  { sort in-game chat }
  if (InGameChatMessages.Count > 0) then
    InGameChatMessages.Sort(@ChatCompare);

  if (GaiaObjects.Count > 0) then
    GaiaObjects.Sort(@GaiaObjectsCompare);

  { fix pop limit for UserPatch }
  if (fIsUserPatch) then
    GameSettings.PopLimit := 25 * GameSettings.PopLimit;

  { fix color id's for pre-game chat messages, see ProcessPreGameChat }
  for i := 0 to PreGameChatMessages.Count - 1 do
  begin
    ChatMessage := PreGameChatMessages[i] as TChatMessage;
    Player := Players.GetPlayerByIndex(ChatMessage.Color);
    if Assigned(Player) then
      ChatMessage.Color := Player.Color
    else
      ChatMessage.Color := 0;
  end;
end;
{$IFDEF EXTENDED}
procedure TRecAnalyst.Build(const FileName: String);
var
  outStream: TMemoryStream;
  header_len: Longint;
  hs: TMemoryStream;
const
  NO_HEADER = -15; // raw inflate
begin
  if not fKeepStreams or not fAnalyzed then Exit;

  hs := TMemoryStream.Create();
  outStream := TMemoryStream.Create();
  try
    try
      fHeaderStream.Seek(0, soFromBeginning);

      {$IFDEF FPC}
      if (ZCompressStream2(fHeaderStream, hs, Z_DEFAULT_COMPRESSION, NO_HEADER, 9, Z_DEFAULT_STRATEGY) < 0) then
        raise ERecAnalystException.Create(RECANALYST_COMP);
      // zError(code)
      {$ELSE}
      ZCompressStream2(fHeaderStream, hs, zcDefault, NO_HEADER, 9, zsDefault);
      {$ENDIF}
      header_len := hs.Size + SizeOf(header_len);
      if fIsMgx then Inc(header_len, SizeOf(next_pos));
      hs.Seek(0, soFromBeginning);
      fBodyStream.Seek(0, soFromBeginning);
      outStream.Write(header_len, SizeOf(header_len));
      if fIsMgx then outStream.Write(next_pos, SizeOf(next_pos));
      outStream.CopyFrom(hs, hs.Size);
      outStream.CopyFrom(fBodyStream, fBodyStream.Size);
      outStream.SaveToFile(FileName);
    except
      on ERecAnalystException do
        raise;
      on EReadError do
        raise ERecAnalystException.Create(RECANALYST_FILEREAD);
      on EFCreateError do
        raise ERecAnalystException.Create(RECANALYST_FILECREATE);
      {$IFNDEF FPC}
      on EZCompressionError do
        raise ERecAnalystException.Create(RECANALYST_COMP);
      {$ENDIF}
      else
        raise ERecAnalystException.Create(RECANALYST_UNKNOWN);
    end;
  finally
    hs.Free();
    outStream.Free();
  end;
end;

function TRecAnalyst.AddComment(const Comment: AnsiString = ''): Boolean;
var
  objectives_len: Word;
  buff65536: array[0..MAXWORD] of AnsiChar;
  ObjectivesString: PAnsiChar;
  temp: TMemoryStream;
const
  CommentSeparator = #$0A#$0A;
begin
  Result := False;
  if not fAnalyzed or not fKeepStreams or (objectives_pos = 0) then Exit;
  { scenarios are not supported for now }
  if GameSettings.IsScenario then Exit;
  if (CommentString = Comment) then Exit;

  temp := TMemoryStream.Create();
  try
    fHeaderStream.Seek(0, soFromBeginning);
    if (temp.CopyFrom(fHeaderStream, objectives_pos) <> objectives_pos) then
      Exit;

    fHeaderStream.Read(objectives_len, SizeOf(objectives_len));
    if (objectives_len > 0) then
    begin
      FillChar(buff65536, SizeOf(buff65536), #0);
      fHeaderStream.Read(buff65536[0], objectives_len);
      ObjectivesString := PAnsiChar(GameSettings.ObjectivesString + CommentSeparator + Comment);
    end else
      ObjectivesString := PAnsiChar(CommentSeparator + Comment);

    if (Comment = '') then
      ObjectivesString := PAnsiChar(GameSettings.ObjectivesString);

    objectives_len := Length(ObjectivesString) + 1;  { null-terminator character }

    temp.Write(objectives_len, SizeOf(objectives_len));
    temp.Write(ObjectivesString[0], objectives_len);
    temp.CopyFrom(fHeaderStream, fHeaderStream.Size - fHeaderStream.Position);

    temp.Seek(0, soFromBeginning);
    fHeaderStream.Clear();
    fHeaderStream.CopyFrom(temp, temp.Size);

    Result := True;
  finally
    temp.Free();
  end;
end;
{$ENDIF}
procedure TRecAnalyst.ReadPlayerInfoBlockEx(const num_player: Byte);
const
  exist_object_separator: array[0..8] of AnsiChar = (
    #$0B, #$00, #$08, #$00, #$00, #$00, #$02, #$00, #$00);
  object_end_separator: array[0..29] of Byte = (
    $FF, $FF, $FF, $FF, $00, $00, $80, $BF, $00, $00, $80, $BF, $FF, $FF, $FF,
    $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $00, $00, $00, $00, $00, $00);
  aok_object_end_separator: array[0..16] of Byte = (
   $FF, $FF, $FF, $FF, $00, $00, $80, $BF, $00, $00, $80, $BF, $00, $00, $00,
   $00, $00);
  player_info_end_separator: array[0..11] of AnsiChar = (
    #$00, #$0B, #$00, #$02, #$00, #$00, #$00, #$02, #$00, #$00, #$00, #$0B);
  objects_mid_separator_gaia: array[0..9] of AnsiChar = (
    #$00, #$0B, #$00, #$40, #$00, #$00, #$00, #$20, #$00, #$00);
  aofe_num_research = 555;
var
  i: Integer;
  exist_object_pos: Int32;
  buff256: array[0..MAXBYTE] of AnsiChar;
  object_type: Byte;
  unit_id: Word;
  owner, b: Byte;
  pos_x, pos_y: Single;
  Player, P: TPlayer;
  player_name_len: Word;
  food, wood, stone, gold, headroom, population, civilian_pop, military_pop, data6: Single;
  init_camera_pos_x, init_camera_pos_y: Single;
  civilization, player_color: Byte;
  GO: TGaiaObject;
  UO: TUnitObject;
  separator_pos: Longint;
  map_size_x, map_size_y: Int32;
  num_resources, num_last_locations: Int32;
  num_research: Word;
begin
  map_size_x := fMapWidth;
  map_size_y := fMapHeight;
  try
    with fHeaderStream do
    begin
      for i := 0 to Players.Count do { first is GAIA }
      begin
        if (i <> 0) then
        begin
          { skip GAIA player }
          Player := Players[i - 1];
          { skip cooping player, she/he has no data in Player_info }
          P := Players.GetPlayerByIndex(Player.Index);
          if (Assigned(P)) and (P <> Player) and (P.CivId <> cNone) then
          begin
            Player.CivId := P.CivId;
            Player.Color := P.Color;
            Player.Team := P.Team; { required }
            Player.IsCooping := True;
            Continue;
          end;
          if (GameSettings.GameVersion = gvAOKTrial)
            or (GameSettings.GameVersion = gvAOCTrial) then Seek(4);
          Seek(num_player + 43);
          { skip player name }
          ReadWord(player_name_len);
          Seek(player_name_len + 6);
          { Civ_header }
          ReadFloat(food);
          ReadFloat(wood);
          ReadFloat(stone);
          ReadFloat(gold);
          { headroom = (house capacity - population) }
          ReadFloat(headroom);
          Seek(4);
          { Starting Age, note: PostImperial Age = Imperial Age here }
          ReadFloat(data6);
          Seek(16);
          ReadFloat(population);
          Seek(100);
          ReadFloat(civilian_pop);
          Seek(8);
          ReadFloat(military_pop);
          SeekIfElse(fIsMgx, 629, 593);
          ReadFloat(init_camera_pos_x);
          ReadFloat(init_camera_pos_y);
          SeekIfElse(fIsMgx, 9, 5);
          ReadChar(civilization);
          { sometimes(?) civilization is zero in scenarios when the first player is briton (only? always? rule?) }
          if (civilization = 0) then Inc(civilization);
          { skip unknown9[3] }
          Seek(3);
          ReadChar(player_color);
          with Player do
          begin
            CivId := TCivilization(civilization);
            Color := player_color + 1;
            InitialState.Position.X := Round(init_camera_pos_x);
            InitialState.Position.Y := Round(init_camera_pos_y);
            InitialState.Food := Round(food);
            InitialState.Wood := Round(wood);
            InitialState.Stone := Round(stone);
            InitialState.Gold := Round(gold);
            InitialState.StartingAge := TStartingAge(Round(data6));
            // TODO: Huns, Goths, Nomad etc. var...
            InitialState.HouseCapacity := Round(headroom) + Round(population);
            InitialState.Population := Round(population);
            InitialState.CivilianPop := Round(civilian_pop);
            InitialState.MilitaryPop := Round(military_pop);
            InitialState.ExtraPop := InitialState.Population - (InitialState.CivilianPop + InitialState.MilitaryPop);
          end;
        end else
        begin
          { GAIA }
          if (GameSettings.GameVersion = gvAOKTrial)
            or (GameSettings.GameVersion = gvAOCTrial) then Seek(4);
          Seek(num_player + 43);
          ReadWord(player_name_len);
          Seek(player_name_len + 1);
          ReadInt32(num_resources);
          Seek(1 + 4 * num_resources + 9);
          if fIsMgx then
          begin
            ReadInt32(num_last_locations);
            SeekIf(num_last_locations > 0, 8 * num_last_locations);
          end;
          Seek(11);
          SeekIfElse(fIsMgx, 4182, 3578);
          SeekIf(fIsUserPatch, 8176);
          Seek(4);
          ReadWord(num_research);
          if (fIsUserPatch) then
          begin
            if (num_research = aofe_num_research) then
            begin
              if (GameSettings.GameVersion = gvAOCUP12) then
                GameSettings.GameVersion := gvAOFE22;  // AOFE 2.2 is using UP 1.2
            end;
          end;
          SeekIfElse(fIsMgx, -4182, -3578);
        end;

        SeekIfElse(fIsMgx, 41249, 34277);
        Seek(map_size_x * map_size_y);

        { Getting exist_object_pos }
        exist_object_pos := Find(exist_object_separator);
        if (exist_object_pos = -1) then
          raise ERecAnalystException.Create(RECANALYST_READPLAYER);

        while True do
        begin
          ReadChar(object_type);
          ReadChar(owner);
          ReadWord(unit_id);
          case object_type of
            10:
              begin
                case unit_id of
                  uiGoldMine, uiStoneMine, uiCliff1..uiCliff10, uiForageBush:
                    begin
                      Seek(19);
                      ReadFloat(pos_x);
                      ReadFloat(pos_y);
                      GO := TGaiaObject.Create();
                      GO.Id := unit_id;
                      GO.Position.X := Round(pos_x);
                      GO.Position.Y := Round(pos_y);
                      GaiaObjects.Add(GO);
                      Seek(- 19 - SizeOf(pos_x) - SizeOf(pos_y));
                    end;
                end;
                Seek(63 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id));
                SeekIf(fIsMgl, 1);
              end;
            20:
              begin
                // not guaranteed
                if fIsMgx then
                begin
                  Seek(59);
                  ReadChar(b);
                  Seek(-59 - SizeOf(b));
                  Seek(68 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id));
                  SeekIf(b = 2, 34);
                end else
                  Seek(103 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id))
              end;
            30:
              begin
                if fIsMgx then
                begin
                  Seek(59);
                  ReadChar(b);
                  Seek(-59 - SizeOf(b));
                  Seek(204 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id));
                  SeekIf(b = 2, 17);
                end else
                begin
                  Seek(60);
                  ReadChar(b);
                  Seek(-60 - SizeOf(b));
                  Seek(205 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id));
                  SeekIf(b = 2, 17);
                end;
             end;
            60:
              begin
                // not guaranteed
                Seek(204);
                ReadChar(b);
                Seek(-204 - SizeOf(b));
                Seek(233 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id));
                SeekIf(b <> 0, 67);
              end;
            70:
              begin
                case unit_id of
                  uiRelic, uiDeer, uiBoar, uiJavelina, uiTurkey, uiSheep:
                    begin
                      Seek(19);
                      ReadFloat(pos_x);
                      ReadFloat(pos_y);
                      GO := TGaiaObject.Create();
                      GO.Id := unit_id;
                      GO.Position.X := Round(pos_x);
                      GO.Position.Y := Round(pos_y);
                      GaiaObjects.Add(GO);
                    end;
                end;
                if (owner <> 0) and (unit_id <> uiTurkey) and (unit_id <> uiSheep) then
                begin
                  { exclude convertable objects }
                  Seek(19);
                  ReadFloat(pos_x);
                  ReadFloat(pos_y);
                  UO := TUnitObject.Create();
                  UO.Id := unit_id;
                  UO.Owner := Players.GetPlayerByIndex(owner);
                  UO.Position.X := Round(pos_x);
                  UO.Position.Y := Round(pos_y);
                  PlayerObjects.Add(UO);
                end;
                if fIsMgx then
                  separator_pos := Find(object_end_separator)
                else
                  separator_pos := Find(aok_object_end_separator);
                if (separator_pos = -1) then Exit;
              end;
            80:
              begin
                if (owner <> 0) then
                begin
                  Seek(19);
                  ReadFloat(pos_x);
                  ReadFloat(pos_y);
                  UO := TUnitObject.Create();
                  UO.Id := unit_id;
                  UO.Owner := Players.GetPlayerByIndex(owner);
                  UO.Position.X := Round(pos_x);
                  UO.Position.Y := Round(pos_y);
                  PlayerObjects.Add(UO);
                end;
                if fIsMgx then
                  separator_pos := Find(object_end_separator)
                else
                  separator_pos := Find(aok_object_end_separator);
                if (separator_pos = -1) then Exit;
                Seek(126);
                SeekIf(fIsMgx, 1);
              end;
            00:
              begin
                Seek(-(SizeOf(object_type) + SizeOf(owner) + SizeOf(unit_id)));

                ReadBuffer(buff256, SizeOf(player_info_end_separator));
                Seek(-SizeOf(player_info_end_separator));

                if CompareMem(@buff256, @player_info_end_separator,
                  SizeOf(player_info_end_separator)) then
                begin
                  Seek(SizeOf(player_info_end_separator));
                  Break;
                end;
                if CompareMem(@buff256, @objects_mid_separator_gaia, 2) then
                  Seek(SizeOf(objects_mid_separator_gaia))
                else
                  raise ERecAnalystException.Create(RECANALYST_READPLAYER);
              end;
            else
              raise ERecAnalystException.Create(RECANALYST_READPLAYER);
          end;
        end;
      end;
    end;
  except
    on E: Exception do
      raise ERecAnalystException.Create(RECANALYST_READPLAYER);
  end;
end;

procedure TRecAnalyst.ReadPlayerInfoBlock(const num_player: Byte);
const
  player_info_end_separator: array[0..11] of AnsiChar = (
    #$00, #$0B, #$00, #$02, #$00, #$00, #$00, #$02, #$00, #$00, #$00, #$0B);
var
  map_size_x, map_size_y: Int32;
  i: Integer;
  Player, P: TPlayer;
  food, wood, stone, gold, headroom, population, civilian_pop, military_pop, data6: Single;
  init_camera_pos_x, init_camera_pos_y: Single;
  civilization, player_color: Byte;
begin
  map_size_x := fMapWidth;
  map_size_y := fMapHeight;
  with fHeaderStream do
  begin
    { first is GAIA, skip some useless bytes }
    if (GameSettings.GameVersion = gvAOKTrial)
      or (GameSettings.GameVersion = gvAOCTrial) then Seek(4);
    Seek(num_player + 70);  // + 2 len of playerlen
    SeekIfElse(fIsMgx, 792, 756);
    SeekIfElse(fIsMgx, 41249, 34277);
    Seek(map_size_x * map_size_y);
    // Explored GAIA Objects
    // Units Data II

    for i := 0 to Players.Count - 1 do
    begin
      Player := Players[i];
      { skip cooping player, she/he has no data in Player_info }
      P := Players.GetPlayerByIndex(Player.Index);
      if (Assigned(P)) and (P <> Player) and (P.CivId <> cNone) then
      begin
        Player.CivId := P.CivId;
        Player.Color := P.Color;
        Player.Team := P.Team; { required }
        Player.IsCooping := True;
        Continue;
      end;

      if (Find(player_info_end_separator) = -1) then
        raise ERecAnalystException.Create(RECANALYST_READPLAYER);

      if (GameSettings.GameVersion = gvAOKTrial)
        or (GameSettings.GameVersion = gvAOCTrial) then Seek(4);
      Seek(num_player + 52 + Length(Player.Name)); // + null-terminator

      { Civ_header }
      ReadFloat(food);
      ReadFloat(wood);
      ReadFloat(stone);
      ReadFloat(gold);
      { headroom = (house capacity - population) }
      ReadFloat(headroom);
      Seek(4);
      { Starting Age, note: PostImperial Age = Imperial Age here }
      ReadFloat(data6);
      Seek(16);
      ReadFloat(population);
      Seek(100);
      ReadFloat(civilian_pop);
      Seek(8);
      ReadFloat(military_pop);
      SeekIfElse(fIsMgx, 629, 593);
      ReadFloat(init_camera_pos_x);
      ReadFloat(init_camera_pos_y);
      SeekIfElse(fIsMgx, 9, 5);
      ReadChar(civilization);
      { sometimes(?) civilization is zero in scenarios when the first player is briton (only? always? rule?) }
      if (civilization = 0) then Inc(civilization);
      { skip unknown9[3] }
      Seek(3);
      ReadChar(player_color);
      with Player do
      begin
        CivId := TCivilization(civilization);
        Color := player_color + 1;
        InitialState.Position.X := Round(init_camera_pos_x);
        InitialState.Position.Y := Round(init_camera_pos_y);
        InitialState.Food := Round(food);
        InitialState.Wood := Round(wood);
        InitialState.Stone := Round(stone);
        InitialState.Gold := Round(gold);
        InitialState.StartingAge := TStartingAge(Round(data6));
        // TODO: Huns, Goths, Nomad etc. var...
        InitialState.HouseCapacity := Round(headroom) + Round(population);
        InitialState.Population := Round(population);
        InitialState.CivilianPop := Round(civilian_pop);
        InitialState.MilitaryPop := Round(military_pop);
        InitialState.ExtraPop := InitialState.Population - (InitialState.CivilianPop + InitialState.MilitaryPop);
      end;

      SeekIfElse(fIsMgx, 41249, 34277);
      Seek(map_size_x * map_size_y);
      // Explored GAIA Objects
      // Units Data II
    end;
  end;
end;

class function TRecAnalyst.ErrorCodeToString(const ErrorCode: Integer): String;
begin
  try
    Result := ErrorMessages[ErrorCode];
  except
    Result := '';
  end;
end;

procedure TRecAnalyst.ProcessPreGameChat(const buff: array of AnsiChar);
var
  ChatMessage: TChatMessage;
  //Player: TPlayer;
begin
  { NOTE: while we are reading pre-game chat, we do not know players color yet,
    that's why we store player's index in ColorId and we assign color ids in PostAnalyze. }

  { zero-length chat exists }
  if (buff[0] = #0) then Exit;
  if (buff[0] = '@') and (buff[1] = '#') {and (buff[2] >= '1') and (buff[2] <= '8')} then
  begin
    ChatMessage := TChatMessage.Create();
    // buff65536[2] is not really player index
    // TODO may be wrong if someone enters/leaves game, or coop, no workaround
    // exists, as players may have same names
    ChatMessage.Color := StrToIntDef(String(buff[2]), -1);
    {Player := Players.GetPlayerByIndex(StrToIntDef(String(buff[2]), 0));
    if Assigned(Player) then
      ChatMessage.ColorId := Player.Index;}
    ChatMessage.Msg := Copy(buff, 4, Length(buff));
    PreGameChatMessages.Add(ChatMessage);
  end;
end;

procedure TRecAnalyst.ProcessInGameChat(const buff: array of AnsiChar; time_cnt: Int32);
var
  ChatMessage: TChatMessage;
  Player: TPlayer;
  ChatLen: Integer;
begin
  { "@#0<Messenger> nick: msg" chat messages exist, UP spectators? }
  if (buff[0] = '@') and (buff[1] = '#') and (buff[2] >= '0') and (buff[2] <= '8') then
  begin
    // skip messages like "--Warning: You are being attacked by... --"
    ChatLen := Length(AnsiString(buff));
    if (buff[2] <> '0') and (buff[3] = '-') and (buff[4] = '-') and
       (buff[ChatLen - 2] = '-') and (buff[ChatLen - 1] = '-') then
      Exit;
    ChatMessage := TChatMessage.Create();
    ChatMessage.Time := time_cnt;
    Player := Players.GetPlayer(StrToIntDef(String(buff[2]), 0) - 1);
    if Assigned(Player) then
      ChatMessage.Color := Player.Color;
    ChatMessage.Msg := Copy(buff, 4, Length(buff));
    InGameChatMessages.Add(ChatMessage);
  end;
end;

procedure TRecAnalyst.ProcessResignCommand(const resign_cmd: TResignCmdDataStruct; time_cnt: Int32);
var
  Player: TPlayer;
  ChatMessage: TChatMessage;
begin
  if (resign_cmd.player_number in [1..Players.Count]) then
  begin
    Player := Players[resign_cmd.player_number - 1];
    if Assigned(Player) then
    begin
      if (resign_cmd.disconnected = 1) and (Player.DisconnectTime = 0) then
      begin
        ChatMessage := TChatMessage.Create();
        ChatMessage.Time := time_cnt;
        ChatMessage.Msg := AnsiString(Format(c_disconnected, [Player.Name]));
        Player.DisconnectTime := time_cnt;
        InGameChatMessages.Add(ChatMessage);
      end else if (resign_cmd.disconnected <> 1) and (Player.ResignTime = 0) then
      begin
        ChatMessage := TChatMessage.Create();
        ChatMessage.Time := time_cnt;
        ChatMessage.Msg := AnsiString(Format(c_resigned, [Player.Name]));
        Player.ResignTime := time_cnt;
        InGameChatMessages.Add(ChatMessage);
      end;
    end;
  end;
end;

procedure TRecAnalyst.ProcessResearchCommand(const research_cmd: TResearchCmdDataStruct; time_cnt: Int32);
var
  Player: TPlayer;
  idx, i: Integer;
  Research: TResearch;
begin
  Player := Players.GetPlayerByIndex(research_cmd.player_id);
  if Assigned(Player) then
  begin
    case research_cmd.research_id of
      riFeudalAge:
        begin
          Player.FeudalTime := time_cnt + 130000; { + research time (2:10) }
        end;
      riCastleAge:
        begin
          if (Player.CivId = cPersians) then
            { about 10% less, but calculated as 160s / 1.10 despite of -10% = 144s }
            Player.CastleTime := time_cnt + Round(160000 / 1.10)
          else
            Player.CastleTime := time_cnt + 160000;
        end;
      riImperialAge:
        begin
          if (Player.CivId = cPersians) then
            { about 15% less, but calculated as 190s / 1.15 despite of -15% = 161,5s }
            Player.ImperialTime := time_cnt + Round(190000 / 1.15)
          else
            Player.ImperialTime := time_cnt + 190000;
        end;
    end;
    { remember this is the time player has just started to research
      the particular technology, repetitious researching may occure,
      here we are asking about it }
    idx := -1;
    for i := Researches.Count - 1 downto 0 do
    begin
      Research := Researches[i] as TResearch;
      if (Research.Id = research_cmd.research_id) and (Research.Player.Index = research_cmd.player_id) then
      begin
        idx := i;
        Break;
      end;
    end;
    if (idx = -1) then
    begin
      { just add this research }
      Research := TResearch.Create();
      Research.Id := research_cmd.research_id;
      Research.Time := time_cnt;
      Research.Player := Players.GetPlayerByIndex(research_cmd.player_id);
      Researches.Add(Research);
    end else
    begin
      Research := Researches[idx] as TResearch;
      { remember data in body section are not necessarily time-sorted
        (rarely, but may occure) that's why we are comparing times }
      if (Research.Time < time_cnt) then
        Research.Time := time_cnt;
    end;
  end;
end;

procedure TRecAnalyst.ProcessHumanTrainUnitCommand(human_train_unit_cmd: THumanTrainUnitCmdDataStruct);
var
  TrainedUnit: TTrainedUnit;
begin
  FixUnitTypeId(human_train_unit_cmd.unit_type_id);
  TrainedUnit := Units.GetUnit(human_train_unit_cmd.unit_type_id);
  if Assigned(TrainedUnit) then
    Inc(TrainedUnit.Count, human_train_unit_cmd.unit_num)
  else begin
    TrainedUnit := TTrainedUnit.Create();
    TrainedUnit.Id := human_train_unit_cmd.unit_type_id;
    TrainedUnit.Count := human_train_unit_cmd.unit_num;
    Units.Add(TrainedUnit);
  end;
end;

procedure TRecAnalyst.ProcessPCTrainUnitCommand(unit_type_id: Word);
var
  TrainedUnit: TTrainedUnit;
begin
  FixUnitTypeId(unit_type_id);
  TrainedUnit := Units.GetUnit(unit_type_id);
  if Assigned(TrainedUnit) then
    Inc(TrainedUnit.Count)
  else begin
    TrainedUnit := TTrainedUnit.Create();
    TrainedUnit.Id := unit_type_id;
    TrainedUnit.Count := 1;
    Units.Add(TrainedUnit);
  end;
end;

procedure TRecAnalyst.ProcessConstructionCommand(player_id, building_type_id: Word);
var
  Player: TPlayer;
  Building: TBuilding;
begin
  FixBuildingTypeId(building_type_id);
  Player := Players.GetPlayerByIndex(player_id);
  if Assigned(Player) then
  begin
    Building := Player.Buildings.GetBuilding(building_type_id);
    if Assigned(Building) then
      Inc(Building.Count)
    else begin
      Building := TBuilding.Create();
      Building.Id := building_type_id;
      Building.Count := 1;
      Player.Buildings.Add(Building);
    end;
  end;
end;

procedure TRecAnalyst.ProcessTributeCommand(const tribute_cmd: TTributeCmdDataStruct; time_cnt: Int32);
var
  PlayerFrom, PlayerTo: TPlayer;
  Tribute: TTribute;
begin
  PlayerFrom := Players.GetPlayerByIndex(tribute_cmd.player_id_from);
  PlayerTo := Players.GetPlayerByIndex(tribute_cmd.player_id_to);
  if Assigned(PlayerFrom) and Assigned(PlayerTo) then
  begin
    Tribute := TTribute.Create();
    Tribute.Time       := time_cnt;
    Tribute.PlayerFrom := PlayerFrom;
    Tribute.PlayerTo   := PlayerTo;
    Tribute.ResourceId := TResourceId(tribute_cmd.resource_id);
    Tribute.Amount     := Floor(tribute_cmd.amount_tributed);
    Tribute.Fee        := tribute_cmd.market_fee;
    Tributes.Add(Tribute);
  end;
end;

procedure TRecAnalyst.ProcessGameDataCommand(const game_data_cmd: TGameDataStruct);
var
  i: Integer;
  PlayerStats: TPlayerStatsStruct;
  Player: TPlayer;
begin
  with GameSettings.Extra do
  begin
    HasData := True;
    AllTechs := game_data_cmd.AllTechs = 1;
    AllowCheats := game_data_cmd.AllowCheats = 1;
    TeamTogether := game_data_cmd.TeamTogether = 1;
    LockSpeed := game_data_cmd.LockSpeed = 1;
    Complete := game_data_cmd.Complete = 1;
  end;

  for i := 0 to Players.Count - 1 do
  begin
    PlayerStats := game_data_cmd.PlayerStats[i];
    Player := Players[i];
    // TODO otestovat ked je coop: if Player.isCooping then skip?
    with Player.Achievement do
    begin
      Victory := PlayerStats.Victory = 1;
      Medal := PlayerStats.Medal = 1;
      Result := TGameResult(PlayerStats.Result);
      TotalScore := PlayerStats.TotalScore;
      with MilitaryStats do
      begin
        MilitaryScore := PlayerStats.MilitaryStats.MilitaryScore;
        UnitsKilled := PlayerStats.MilitaryStats.UnitsKilled;
        UnitsLost := PlayerStats.MilitaryStats.UnitsLost;
        BuildingsRazed := PlayerStats.MilitaryStats.BuildingsRazed;
        BuildingsLost := PlayerStats.MilitaryStats.BuildingsLost;
        UnitsConverted := PlayerStats.MilitaryStats.UnitsConverted;
      end;
      with EconomyStats do
      begin
        EconomyScore := PlayerStats.EconomyStats.EconomyScore;
        FoodCollected := PlayerStats.EconomyStats.FoodCollected;
        WoodCollected := PlayerStats.EconomyStats.WoodCollected;
        StoneCollected := PlayerStats.EconomyStats.StoneCollected;
        GoldCollected := PlayerStats.EconomyStats.GoldCollected;
        TributeSent := PlayerStats.EconomyStats.TributeSent;
        TributeRcvd := PlayerStats.EconomyStats.TributeRcvd;
        TradeProfit := PlayerStats.EconomyStats.TradeProfit;
        RelicGold := PlayerStats.EconomyStats.RelicGold;
      end;
      with TechnologyStats do
      begin
        TechnologyScore := PlayerStats.TechnologyStats.TechnologyScore;
        FeudalAge := PlayerStats.TechnologyStats.FeudalAge;
        CastleAge := PlayerStats.TechnologyStats.CastleAge;
        ImperialAge := PlayerStats.TechnologyStats.ImperialAge;
        MapExplored := PlayerStats.TechnologyStats.MapExplored;
        ResearchCount := PlayerStats.TechnologyStats.ResearchCount;
        ResearchPercent := PlayerStats.TechnologyStats.ResearchPercent;
      end;
      with SocietyStats do
      begin
        SocietyScore := PlayerStats.SocietyStats.SocietyScore;
        TotalWonders := PlayerStats.SocietyStats.TotalWonders;
        TotalCastles := PlayerStats.SocietyStats.TotalCastles;
        RelicsCaptured := PlayerStats.SocietyStats.RelicsCaptured;
        VillagerHigh := PlayerStats.SocietyStats.VillagerHigh;
      end;
    end;
  end;
end;

end.

