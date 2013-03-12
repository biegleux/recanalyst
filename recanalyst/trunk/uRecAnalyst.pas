{*
 * $Id$
 * This file is part of the recanalyst project.
 *
 * Copyright (c) 2009-2013, biegleux <biegleux[at]gmail[dot]com>
 *
 * This program is free; software; you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published
 * by the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses>.
 *}
unit uRecAnalyst;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

{$IFNDEF FPC}
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

{.$DEFINE EXTENDED}

interface

uses
  Classes, Windows, SysUtils, Contnrs, uRecAnalystBase, uRecAnalystConsts,
  MemStream;

type
  TBuildingList = class;

  { TInitialState }
  TInitialState = class(TObject)
  private
    function GetStartingAge: AnsiString;
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
    constructor Create;
    property StartingAgeString: AnsiString read GetStartingAge;
  end;

  { TPlayer }
  TPlayer = class(TObject)
  private
    function GetCiv: AnsiString;
  public
    Name: AnsiString;
    Index: Integer;
    Human: Boolean;
    Team: Integer;
    Owner: Boolean;
    CivId: TCivilization;
    ColorId: Integer;
    Color: Cardinal;
    IsCooping: Boolean;
    FeudalTime: Integer;
    CastleTime: Integer;
    ImperialTime: Integer;
    ResignTime: Integer;
    Buildings: TBuildingList;
    InitialState: TInitialState;
    constructor Create;
    destructor Destroy; override;
    procedure SetColor(const AIndex: Integer);
    property Civ: AnsiString read GetCiv;
  end;

  { TPlayerList }
  TPlayerList = class(TObject)
  private
    FList: TObjectList;
    function GetCount: Integer;
    function GetPlayer(Index: Integer): TPlayer;
  public
    constructor Create;
    destructor Destroy; override;
    function AddPlayer(Player: TPlayer): Integer;
    function GetPlayerByIndex(Index: Integer): TPlayer;
    procedure Clear;
    property Items[Index: Integer]: TPlayer read GetPlayer; default;
    property Count: Integer read GetCount;
  end;

  { TTeam }
  TTeam = class(TPlayerList)
  private
    FIndex: Integer;
  public
    constructor Create;
    function AddPlayer(Player: TPlayer): Integer;
    property Index: Integer read FIndex;
  end;

  { TTeamList }
  TTeamList = class(TObject)
  private
    FList: TObjectList;
    function GetCount: Integer;
    function GetTeam(Index: Integer): TTeam;
  public
    constructor Create;
    destructor Destroy; override;
    function AddTeam(Team: TTeam): Integer;
    function GetTeamByIndex(Index: Integer): TTeam;
    procedure Clear;
    property Items[Index: Integer]: TTeam read GetTeam; default;
    property Count: Integer read GetCount;
  end;

  { TVictory }
  TVictory = class(TObject)
  private
    function GetVictoryString: AnsiString;
  public
    TimeLimit: Longint;
    ScoreLimit: Longint;
    VictoryCondition: TVictoryCondition;
    constructor Create;
    property VictoryString: AnsiString read GetVictoryString;
  end;

  { TGameSettings }
  TGameSettings = class(TObject)
  private
    FIsMgl: Boolean; { see GetDifficultyLevel }
    function GetGameType: AnsiString;
    function GetMapStyle: AnsiString;
    function GetDifficultyLevel: AnsiString;
    function GetGameSpeed: AnsiString;
    function GetRevealMap: AnsiString;
    function GetMapSize: AnsiString;
    function GetGameVersion: AnsiString;
    function GetIsScenario: Boolean;
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
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
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
    FItemId: Integer;
    function GetName: AnsiString; virtual; abstract;
  public
    property Name: AnsiString read GetName;
    constructor Create;
  end;

  { TResearch }
  TResearch = class(TBaseObject)
  private
    function GetName: AnsiString; override;
  public
    Id: Integer;
    Time: Longint;
    PlayerId: Integer;
    constructor Create;
  end;

  { TTribute }
  TTribute = class(TObject)
    Time: Integer;
    PlayerFrom: TPlayer;
    PlayerTo: TPlayer;
    ResourceId: TResourceId;
    Amount: Integer;
    Fee: Single;
  end;

  { TTrainedUnit }
  TTrainedUnit = class(TBaseObject)
  private
    function GetName: AnsiString; override;
  public
    Id: Integer;
    Count: Integer;
    constructor Create;
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
    function GetName: AnsiString; override;
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
    Player: TPlayer;
    Msg: AnsiString;
    constructor Create;
  end;
(*
  { TImageMapRes }
  TImageMapItem = class(TObject)
    Coordinates: TRect;
    Hint: AnsiString;
  end;
*)
  TUnitObject = class(TObject)
    Owner: Integer;
    Id: Integer;
    Position: TPoint;
  end;

  TGaiaObject = class(TUnitObject);

  { TRecAnalyst }
  ERecAnalystException = class(Exception)
  private
    FCode: Integer;
  public
    constructor Create(const Msg: String; Code: Integer);
    property Code: Integer read FCode write FCode;
  end;

  TRecAnalyst = class(TObject)
  private
    FIsMgl: Boolean;
    FIsMgx: Boolean;
    FIsMgz: Boolean;
    FHeaderStream: TMemStream;
    FBodyStream: TMemStream;
    FMapData: array of array of Integer;
    FMapWidth: Longint;
    FMapHeight: Longint;
    FAnalyzeTime: Integer;

    GaiaObjects: TObjectList;
    PlayerObjects: TObjectList;
    FAnalyzed: Boolean;
    {$IFDEF EXTENDED}FKeepStreams: Boolean;{$ENDIF}
    FMapImage: TMemoryStream;
    FMapImageSize: TSize;

    header_len: Int32;
    next_pos: Int32;
    {$IFDEF EXTENDED}objectives_pos: Int64;{$ENDIF}

    function ExtractStreams: Boolean;
    function AnalyzeHeader: Boolean;
    function AnalyzeBody: Boolean;
    procedure PostAnalyze;
    procedure ReadPlayerInfoBlock(const num_player: Byte);
  public
    FileName: String;
    GameSettings: TGameSettings;
    Players: TPlayerList;
    Teams: TTeamList;
    Tributes: TObjectList;
    Units: TTrainedUnitList;
    Researches: TObjectList;
    PreGameChatMessages: TObjectList;
    InGameChatMessages: TObjectList;
    {$IFDEF EXTENDED}CommentString: AnsiString;{$ENDIF}
    constructor Create;
    destructor Destroy; override;
    class function GameTimeToString(const Time: Integer): AnsiString;
    function Analyze: Boolean;
    procedure BuildTeams;
    function GenerateMap(const Width: Integer; const Height: Integer): TMemoryStream;
    procedure Reset;
    {$IFDEF EXTENDED}
    procedure Build(const FileName: String);
    function AddComment(const Comment: AnsiString = ''): Boolean;
    {$ENDIF}
    class function ErrorCodeToString(const ErrorCode: Integer): String;
    property AnalyzeTime: Integer read FAnalyzeTime;
    property Analyzed: Boolean read FAnalyzed;
    {$IFDEF EXTENDED}
    property KeepStreams: Boolean read FKeepStreams write FKeepStreams;
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

function UnitsCompare(Item1, Item2: Pointer): Integer; forward;
function ResearchesCompare(Item1, Item2: Pointer): Integer; forward;
function ChatCompare(Item1, Item2: Pointer): Integer; forward;
function GaiaObjectsCompare(Item1, Item2: Pointer): Integer; forward;
function ResearchById(const Id: Integer): Integer; forward;
function UnitById(const Id: Integer): Integer; forward;
function BuildingById(const Id: Integer): Integer; forward;
function MapById(const Id: Integer): Integer; forward;
function InArray(const Ary: array of Integer; Value: Integer): Integer; forward;

{ TBaseObject }
constructor TBaseObject.Create;
begin
  FItemId := -1;
end;

{ TResearch }
constructor TResearch.Create;
begin
  inherited Create;
  Id := 0;
  PlayerId := 0;
end;

function TResearch.GetName: AnsiString;
begin
  if (FItemId <> -1) then
  begin
    Result := RESEARCHES[FItemId].Name;
    Exit;
  end;

  FItemId := ResearchById(Id);
  if (FItemId <> -1) then
    Result := RESEARCHES[FItemId].Name
  else
    Result := '';
end;

{ TTrainedUnit }
constructor TTrainedUnit.Create;
begin
  inherited Create;
  Id := 0;
  Count := 0;
end;

function TTrainedUnit.GetName: AnsiString;
begin
  if (FItemId <> -1) then
  begin
    Result := UNITS[FItemId].Name;
    Exit;
  end;

  FItemId := UnitById(Id);
  if (FItemId <> -1) then
    Result := UNITS[FItemId].Name
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
function TBuilding.GetName: AnsiString;
begin
  if (FItemId <> -1) then
  begin
    Result := BUILDINGS[FItemId].Name;
    Exit;
  end;

  FItemId := BuildingById(Id);
  if (FItemId <> -1) then
    Result := BUILDINGS[FItemId].Name
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
constructor TChatMessage.Create;
begin
  Time := 0;
  Player := nil;
  Msg := '';
end;

{ TInitialState }
constructor TInitialState.Create;
begin
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

function TInitialState.GetStartingAge: AnsiString;
begin
  Result := '';
  if (StartingAge >= Low(STARTING_AGES)) and (StartingAge <= High(STARTING_AGES)) then
    Result := STARTING_AGES[StartingAge];
end;

{ TPlayer }
constructor TPlayer.Create;
begin
  Name := '';
  Index := -1;
  Human := False;
  Team := -1;
  Owner := False;
  CivId := cNone;
  ColorId := -1;
  Color := $00FFFFFF;
  IsCooping := False;
  FeudalTime := 0;
  CastleTime := 0;
  ImperialTime := 0;
  ResignTime := 0;
  Buildings := TBuildingList.Create;
  InitialState := TInitialState.Create;
end;

destructor TPlayer.Destroy;
begin
  Buildings.Free;
  InitialState.Free;
end;

function TPlayer.GetCiv: AnsiString;
begin
  if (CivId in [Low(CIVS)..High(CIVS)]) then
    Result := CIVS[CivId]
  else Result := CIVS[cNone];
end;

procedure TPlayer.SetColor(const AIndex: Integer);
begin
  if (AIndex in [Low(COLORS)..High(COLORS)]) then
    Color := COLORS[AIndex];
end;

{ TPlayerList }
constructor TPlayerList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TPlayerList.Destroy;
begin
  FList.Free;
end;

function TPlayerList.AddPlayer(Player: TPlayer): Integer;
begin
  Result := FList.Add(Player);
end;

function TPlayerList.GetPlayer(Index: Integer): TPlayer;
begin
  if (Index < 0) or (Index >= FList.Count) then
    Result := nil
  else
    Result := FList[Index] as TPlayer;
end;

function TPlayerList.GetPlayerByIndex(Index: Integer): TPlayer;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    if (TPlayer(FList[i]).Index = Index) then
    begin
      Result := TPlayer(FList[i]);
      Exit;
    end;
  end;
  Result := nil;
end;

function TPlayerList.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TPlayerList.Clear;
begin
  FList.Clear;
end;

{ TTeam }
constructor TTeam.Create;
begin
  FList := TObjectList.Create(False);
  FIndex := -1;
end;

function TTeam.AddPlayer(Player: TPlayer): Integer;
begin
  Result := inherited AddPlayer(Player);
  if (FIndex = -1) then
    FIndex := Player.Team;
end;

{ TTeamList }
constructor TTeamList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TTeamList.Destroy;
begin
  FList.Free;
end;

function TTeamList.AddTeam(Team: TTeam): Integer;
begin
  Result := FList.Add(Team);
end;

function TTeamList.GetTeam(Index: Integer): TTeam;
begin
  if (Index < 0) or (Index >= FList.Count) then
    Result := nil
  else
    Result := FList[Index] as TTeam;
end;

function TTeamList.GetTeamByIndex(Index: Integer): TTeam;
var
  i: Integer;
begin
  for i := 0 to FList.Count - 1 do
  begin
    if ((FList[i] as TTeam).Index = Index) then
    begin
      Result := FList[i] as TTeam;
      Exit;
    end;
  end;
  Result := nil;
end;

function TTeamList.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TTeamList.Clear;
begin
  FList.Clear;
end;

{ TVictory }
constructor TVictory.Create;
begin
  VictoryCondition := Low(TVictoryCondition);
  TimeLimit := 0;
  ScoreLimit := 0;
end;

function TVictory.GetVictoryString: AnsiString;
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

{ TGameSettings }
constructor TGameSettings.Create;
begin
  Clear;
  Victory := TVictory.Create;
end;

destructor TGameSettings.Destroy;
begin
  Owner := nil;
  Victory.Free;
end;

procedure TGameSettings.Clear;
begin
  FIsMgl := False;
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
end;

function TGameSettings.GetGameType: AnsiString;
begin
  Result := '';
  if (GameType in [Low(GAME_TYPES)..High(GAME_TYPES)]) then
    Result := GAME_TYPES[GameType];
end;

function TGameSettings.GetMapStyle: AnsiString;
begin
  Result := '';
  if (MapStyle in [Low(MAP_STYLES)..High(MAP_STYLES)]) then
    Result := MAP_STYLES[MapStyle];
end;

function TGameSettings.GetDifficultyLevel: AnsiString;
begin
  Result := '';
  if FIsMgl then
  begin
    if (DifficultyLevel in [Low(AOK_DIFFICULTY_LEVELS)..High(AOK_DIFFICULTY_LEVELS)]) then
      Result := AOK_DIFFICULTY_LEVELS[DifficultyLevel];
  end else
  begin
    if (DifficultyLevel in [Low(AOC_DIFFICULTY_LEVELS)..High(AOC_DIFFICULTY_LEVELS)]) then
      Result := AOC_DIFFICULTY_LEVELS[DifficultyLevel];
  end;
end;

function TGameSettings.GetGameSpeed: AnsiString;
begin
  Result := '';
  if (Ord(GameSpeed) in [100, 150, 200]) then
    Result := GAME_SPEEDS[(Ord(GameSpeed) - 100) div 50];
end;

function TGameSettings.GetRevealMap: AnsiString;
begin
  Result := '';
  if (RevealMap in [Low(REVEAL_SETTINGS)..High(REVEAL_SETTINGS)]) then
    Result := REVEAL_SETTINGS[RevealMap];
end;

function TGameSettings.GetMapSize: AnsiString;
begin
  Result := '';
  if (MapSize in [Low(MAP_SIZES)..High(MAP_SIZES)]) then
    Result := MAP_SIZES[MapSize];
end;

function TGameSettings.GetGameVersion: AnsiString;
begin
  Result := '';
  if not (GameVersion in [Low(GAME_VERSIONS)..High(GAME_VERSIONS)]) then
    Exit;
  Result := GAME_VERSIONS[GameVersion];
end;

function TGameSettings.GetIsScenario: Boolean;
begin
  Result := (GameType = gtScenario);
end;

{ TRecAnalyst }

constructor ERecAnalystException.Create(const Msg: String; Code: Integer);
begin
  inherited Create(Msg);
  FCode := Code;
end;

constructor TRecAnalyst.Create;
begin
  FHeaderStream := TMemStream.Create;
  FBodyStream := TMemStream.Create;
  GameSettings := TGameSettings.Create;
  Players := TPlayerList.Create;
  Teams := TTeamList.Create;
  PreGameChatMessages := TObjectList.Create;
  InGameChatMessages := TObjectList.Create;
  Tributes := TObjectList.Create;
  Researches := TObjectList.Create;
  Units := TTrainedUnitList.Create;
  GaiaObjects := TObjectList.Create;
  PlayerObjects := TObjectList.Create;
  FMapImage := TMemoryStream.Create;
  Reset;
end;

destructor TRecAnalyst.Destroy;
begin
  FHeaderStream.Free;
  FBodyStream.Free;
  GameSettings.Free;
  Players.Free;
  Teams.Free;
  PreGameChatMessages.Free;
  InGameChatMessages.Free;
  Tributes.Free;
  Units.Free;
  Researches.Free;
  GaiaObjects.Free;
  PlayerObjects.Free;
  SetLength(FMapData, 0);
  FMapImage.Free;
end;

class function TRecAnalyst.GameTimeToString(const Time: Integer): AnsiString;
var
  hour, minute, second: Integer;
  s: String;
begin
  if (Time = 0) then
  begin
    Result := '';
    Exit;
  end;
  hour := Time div 1000 div 3600;
  minute := Time div 1000 div 60 mod 60;
  second := Time div 1000 mod 60;

  Result := '';
  s := IntToStr(hour);
  if (hour < 10) then s := '0' + s;
  Result := Result + AnsiString(s) + ':';
  s := IntToStr(minute);
  if (minute < 10) then s := '0' + s;
  Result := Result + AnsiString(s) + ':';
  s := IntToStr(second);
  if (second < 10) then s := '0' + s;
  Result := Result + AnsiString(s);
end;

function TRecAnalyst.ExtractStreams: Boolean;
var
  ms, inStream: TMemoryStream;
  count: Int64;
const
  MGL_EXT = '.mgl';
  MGX_EXT = '.mgx';
  MGZ_EXT = '.mgz';
begin
  Result := False;

  if (FileName = '') then
    raise ERecAnalystException.Create(c_filenotspecified, RECANALYST_NOFILE);

  if (LowerCase(ExtractFileExt(FileName)) = MGL_EXT) then
    FIsMgl := True
  else if (LowerCase(ExtractFileExt(FileName)) = MGX_EXT) then
    FIsMgx := True
  else if (LowerCase(ExtractFileExt(Filename)) = MGZ_EXT) then
  begin
    FIsMgx := True; FIsMgz := True;
  end else
    raise ERecAnalystException.Create(c_wrongfileext, RECANALYST_FILEEXT);

  ms := TMemoryStream.Create;
  inStream := TMemoryStream.Create;
  try
    try
      ms.LoadFromFile(FileName);
      ms.Seek(0, soFromBeginning);

      if (ms.Read(header_len, SizeOf(header_len)) < SizeOf(header_len)) then
        raise ERecAnalystException.Create(c_headerlenreaderror, RECANALYST_HEADLENREAD);

      if (header_len = 0) then
        raise ERecAnalystException.Create(c_headerlenempty, RECANALYST_EMPTYHEADER);

      if FIsMgx then
        ms.Read(next_pos, SizeOf(next_pos));

      if FIsMgx then
        Dec(header_len, SizeOf(next_pos) + SizeOf(header_len))
      else
        Dec(header_len, SizeOf(header_len));

      inStream.CopyFrom(ms, header_len);
      instream.Seek(0, soFromBeginning);

      {$IFDEF FPC}
      if (ZDecompressStream2(inStream, FHeaderStream, -15) < 0) then // raw inflate
        raise ERecAnalystException.Create(c_cannotdecompress, RECANALYST_DECOMP);
      // zError (code)
      {$ELSE}
      ZDecompressStream2(inStream, FHeaderStream, -15); // raw inflate
      {$ENDIF}

      count := ms.Size - header_len - SizeOf(header_len);
      if FIsMgx then count := count - SizeOf(next_pos);
      FBodyStream.CopyFrom(ms, count);

      Result := True;
    except
      on ERecAnalystException do
        raise;
      on EReadError do
        raise ERecAnalystException.Create(c_cannotreadsection, RECANALYST_FILEREAD);
      on EFOpenError do
        raise ERecAnalystException.Create(c_cannotopenfile, RECANALYST_FILEOPEN);
      {$IFNDEF FPC}
      on EZDecompressionError do
        raise ERecAnalystException.Create(c_cannotdecompress, RECANALYST_DECOMP);
      {$ENDIF}
      else
        raise ERecAnalystException.Create(c_unknown, RECANALYST_UNKNOWN);
    end;
  finally
    FreeAndNil(ms);
    FreeAndNil(inStream);
  end;
end;

function TRecAnalyst.AnalyzeHeader: Boolean;
const
  constant2: array[0..7] of AnsiChar = (#$9A, #$99, #$99, #$99, #$99, #$99, #$F9, #$3F);
  separator: array[0..3] of AnsiChar = (#$9D, #$FF, #$FF, #$FF);
  scenario_constant: array[0..3] of AnsiChar = (#$F6, #$28, #$9C, #$3F);
  aok_separator: array[0..3] of AnsiChar = (#$9A, #$99, #$99, #$3F);
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
  num_chat, chat_len: Int32;
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
  ChatMessage: TChatMessage;
  num_condition: Int32;
  team_indexes: array[0..7] of Byte;
  original_sc_filename_len, instruction_len: Word;
  separator_ptr: Pointer;
  unknown25, victory_condition: Int32;
  is_timelimit: Byte;
  time_limit: Single;
  num_data, num_couples, map_size_x2, map_size_y2, num_unknown_data2: Int32;
begin
  FillChar(buff, SizeOf(buff), $00);
  FillChar(buff256, SizeOf(buff256), #0);

  with FHeaderStream do
  begin
    Seek(0, soFromBeginning);

    { getting version }
    FillChar(version, SizeOf(version), #0);
    ReadBuffer(version, SizeOf(version));
    if (version = VER_94) then
    begin
      if FIsMgz then
        GameSettings.GameVersion := gvAOC11
      else
        GameSettings.GameVersion := gvAOC;
    end else if (version = VER_93) then
      GameSettings.GameVersion := gvAOK
    else if (version = TRL_93) and FIsMgx then
      GameSettings.GameVersion := gvAOCTrial
    else if (version = TRL_93) and FIsMgl then
      GameSettings.GameVersion := gvAOKTrial
    else if (version = VER_95) then
      GameSettings.GameVersion := gvAOC21
    else
      GameSettings.GameVersion := gvUnknown;

    case GameSettings.GameVersion of
      gvAOK, gvAOKTrial:
        begin
          FIsMgl := True; FIsMgx := False; FIsMgz := False;
        end;
      gvAOC, gvAOCTrial:
        begin
          FIsMgl := False; FIsMgx := True; FIsMgz := False;
        end;
      gvAOC11, gvAOC21:
        begin
          FIsMgl := False; FIsMgx := True; FIsMgz := True;
        end;
    end;
    GameSettings.FIsMgl := FIsMgl;

    { getting Trigger_info position }
    Seek(-SizeOf(constant2), soFromEnd);
    trigger_info_pos := 0;
    repeat
      ReadBuffer(buff, SizeOf(constant2));
      if CompareMem(@buff, @constant2, SizeOf(constant2)) then
      begin
        trigger_info_pos := Position;
        Break;
      end;
      Seek(-(SizeOf(constant2) + 1));
    until (Position < 0);

    if (trigger_info_pos = 0) then
      raise ERecAnalystException.Create(c_triggerinfonotfound, RECANALYST_NOTRIGG);

    { getting Game_settings position }
    game_settings_pos := 0;
    repeat
      ReadBuffer(buff, SizeOf(separator));
      if CompareMem(@buff, @separator, SizeOf(separator)) then
      begin
        game_settings_pos := Position;
        Break;
      end;
      Seek(-(SizeOf(separator) + 1));
    until (Position < 0);

    if (game_settings_pos = 0) then
      raise ERecAnalystException.Create(c_gamesettingsnotfound, RECANALYST_NOGAMESETS);

    { getting Scenario_header position }
    scenario_header_pos := 0;
    if FIsMgx then
      separator_ptr := @scenario_constant
    else
      separator_ptr := @aok_separator;
    { note: SizeOf(scenario_constant) = SizeOf(aok_separator) }

    repeat
      ReadBuffer(buff, SizeOf(scenario_constant));
      if CompareMem(@buff, separator_ptr, SizeOf(scenario_constant)) then
      begin
        scenario_header_pos := Position - SizeOf(scenario_constant) - SizeOf(Int32) {next_unit_id};
        Break;
      end;
      Seek(-(SizeOf(scenario_constant) + 1));
    until (Position < 0);

    { getting Game_Settings data }
    { skip negative[2] }
    Seek(game_settings_pos + 8, soFromBeginning);
    if FIsMgx then ReadInt32(map_id);

    ReadInt32(difficulty);
    ReadBool(lock_teams); { duplicated data, see lock_diplomacy }

    if FIsMgx then
    begin
      i := MapById(map_id);
      if (i <> -1) then
      begin
        GameSettings.Map := MAPS[i].Name;
        if (map_id in miRealWorldMaps) then
          GameSettings.MapStyle := msRealWorld
        else if (map_id = miCustom) then
          GameSettings.MapStyle := msCustom
        else
          GameSettings.MapStyle := msStandard;
        GameSettings.MapId := map_id;
      end;
    end;

    GameSettings.DifficultyLevel := TDifficultyLevel(difficulty);
    GameSettings.LockDiplomacy := lock_teams;

    { getting Player_info data }
    for i := 0 to 8 do
    begin
      ReadInt32(player_data_index);
      ReadInt32(human);
      Read(name_len, SizeOf(name_len));
      Read(buff256, name_len);
      buff256[name_len] := #0;

      { sometimes very rarely index is 1 }
      if (human = 0) or (human = 1) then Continue;
      if (i <> 0) then
      begin
        Player := TPlayer.Create;
        Player.Name := buff256;
        Player.Index := player_data_index;
        Player.Human := (human = $02);
        Players.AddPlayer(Player);
      end;
    end;

    { getting game type for aok }
    if FIsMgl then
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
    if FIsMgx then Seek(-7);
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
          Seek(num_selected_object shl 2);
        end;
        Seek(num_effect shl 2);
        ReadInt32(num_condition);
        Seek(76 * num_condition);
      end;
      Seek(num_trigger shl 2);

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
    if FIsMgx then
    begin
      ReadChar(game_type);
      ReadChar(lock_diplomacy);
    end;

    with GameSettings do
    begin
      RevealMap := TRevealMap(reveal_map);
      MapSize := TMapSize(map_size);
      PopLimit := pop_limit;
      if FIsMgx then
      begin
        LockDiplomacy := (lock_diplomacy = $01);
        GameType := TGameType(game_type);
      end;
    end;

    { here comes pre-game chat (mgl doesn't hold this information }
    if FIsMgx then
    begin
      ReadInt32(num_chat);
      for i := 0 to num_chat - 1 do
      begin
        ReadInt32(chat_len);

        { zero-length chat exists }
        if (chat_len = 0) then Continue;
        ReadBuffer(buff65536, chat_len);
        if (buff65536[0] = '@') and (buff65536[1] = '#') and (buff65536[2] >= '1') and (buff65536[2] <= '8') then
        begin
          buff65536[chat_len] := #0;

          ChatMessage := TChatMessage.Create;
          // buff65536[2] is not really player index
          // TODO may be wrong if someone enter/leave game, or coop, no workaround exists (players may have same names)
          ChatMessage.Player := Players.GetPlayerByIndex(StrToIntDef(String(buff65536[2]), 0));
          ChatMessage.Msg := Copy(buff65536, 4, Length(buff65536));
          PreGameChatMessages.Add(ChatMessage);
        end;
      end;
    end;

    { skip AI_info if exists }
    Seek($0C, soFromBeginning);
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

    GameSettings.GameSpeed := TGameSpeed(game_speed);

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
    Seek(62);
    if FIsMgl then Seek(-2);
    ReadInt32(map_size_x);
    ReadInt32(map_size_y);
    FMapWidth := map_size_x;
    FMapHeight := map_size_y;

    ReadInt32(num_unknown_data);
    { unknown data }
    for i := 0 to num_unknown_data - 1 do
    begin
      Seek(1275 + map_size_x * map_size_y);
      ReadInt32(num_float);
      Seek((num_float shl 2) + 4);
    end;
    Seek(2);

    SetLength(FMapData, map_size_x, map_size_y);
    { map data }
    for y := 0 to map_size_y - 1 do
      for x := 0 to map_size_x - 1 do
      begin
        ReadChar(terrain_id);
        ReadChar(elevation);
        FMapData[x, y] := terrain_id + 1000 * (elevation + 1);
      end;

    ReadInt32(num_data);
    Seek(4 + (num_data shl 2));
    for i := 0 to num_data - 1 do
    begin
      ReadInt32(num_couples);
      Seek(num_couples shl 2);
    end;
    ReadInt32(map_size_x2);
    ReadInt32(map_size_y2);
    Seek((map_size_x2 * map_size_y2 shl 2) + 4);
    ReadInt32(num_unknown_data2);
    Seek(27 * num_unknown_data2 + 4);

    { getting Player_info }
    ReadPlayerInfoBlock(num_player);

    { getting objectives or instructions }
    if (scenario_header_pos > 0) then
    begin
      Seek(scenario_header_pos + 4433, soFromBeginning);
      { original scenario file name }
      ReadWord(original_sc_filename_len);
      if (original_sc_filename_len > 0) then
      begin
        FillChar(buff65536, SizeOf(buff65536), #0);
        ReadBuffer(buff65536, original_sc_filename_len);
        GameSettings.ScFileName := buff65536;
        if FIsMgl then
          GameSettings.GameType := gtScenario; { this way we detect scenarios in mgl, is there any other way? }
      end;
      if FIsMgx then Seek(24) else Seek(20);

      { scenario instruction or Objectives string, depends on game type }
      {$IFDEF EXTENDED}objectives_pos := Position;{$ENDIF}
      ReadWord(instruction_len);
      if (instruction_len > 0) then
      begin
        FillChar(buff65536, SizeOf(buff65536), #0);
        ReadBuffer(buff65536, instruction_len);
        if not GameSettings.IsScenario then
          GameSettings.ObjectivesString := buff65536;
      end;
    end;

    Result := True;
  end;
end;

function TRecAnalyst.AnalyzeBody: Boolean;
var
  time_cnt: Int32;
  m_body_len, i, idx: Integer;
  od_type, command, chat_len, time: Int32;
  unknown, length: Int32;
  cmd, player_number, player_index, ver: Byte;
  buff256: array[0..MAXBYTE] of AnsiChar;
  Player, PlayerFrom, PlayerTo: TPlayer;
  player_id_from, player_id_to, resource_id: Byte;
  amount_tributed, market_fee: Single;
  player_id, research_id: Word;
  object_id: Int32;
  unit_type_id, unit_num, building_type_id: Word;
  next_command_block: Int32;
  Res: TResearch;
  Tribute: TTribute;
  TrainedUnit: TTrainedUnit;
  Building: TBuilding;
  ChatMessage: TChatMessage;
begin
  time_cnt := Ord(GameSettings.GameSpeed);

  m_body_len := FBodyStream.Size;
  FillChar(buff256, SizeOf(buff256), 0);

  with FBodyStream do
  begin
    Seek(0, soFromBeginning);
    while (Position < m_body_len - 3) do
    begin
      if (Position = 0) and FIsMgl then
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
              if FIsMgl then
              begin
                Seek(28);
                ReadChar(ver);
                case ver of
                  0: if (GameSettings.GameVersion <> gvAOKTrial) then
                    GameSettings.GameVersion := gvAOK20;
                  1: GameSettings.GameVersion := gvAOK20a;
                end;
                Seek(3);
              end else
              begin
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
              ReadInt32(chat_len);
              ReadBuffer(buff256, chat_len);

              if (buff256[0] = '@') and (buff256[1] = '#') and (buff256[2] >= '1') and (buff256[2] <= '8') then
              begin
                buff256[chat_len] := #0;
                if (buff256[3] = '-') and (buff256[4] = '-') and
                   (buff256[chat_len - 3] = '-') and (buff256[chat_len - 2] = '-') then
                begin
                  // skip
                end else
                begin
                  ChatMessage := TChatMessage.Create;
                  ChatMessage.Time := time_cnt;
                  ChatMessage.Player := Players.GetPlayer(StrToIntDef(String(buff256[2]), 0) - 1);
                  ChatMessage.Msg := Copy(buff256, 4, System.Length(buff256));
                  InGameChatMessages.Add(ChatMessage);
                end;
              end;
            end;
          end;
        $02:
          begin
            { Sync }
            ReadInt32(time);
            Inc(time_cnt, time); { time_cnt is in miliseconds }
            ReadInt32(unknown);
            if (unknown = 0) then Seek(28);
            Seek(12);
          end;
        $01:
          begin
            { Command }
            ReadInt32(length);
            ReadChar(cmd);
            Seek(-1);
            case cmd of
              $0B:
                begin
                  { player resign }
                  Seek(1);
                  ReadChar(player_index);
                  ReadChar(player_number);
                  if (player_number in [1..Players.Count]) then
                  begin
                    Player := Players[player_number - 1];
                    if Assigned(Player) and (Player.ResignTime = 0) then
                    begin
                      Player.ResignTime := time_cnt;
                      ChatMessage := TChatMessage.Create;
                      ChatMessage.Time := Player.ResignTime;
                      ChatMessage.Msg := AnsiString(Format(c_resigned, [Player.Name]));
                      InGameChatMessages.Add(ChatMessage);
                    end;
                  end;
                  Seek(length - 3);
                end;
              $65:
                begin
                  { researches }
                  Seek(8);
                  ReadWord(player_id);
                  ReadWord(research_id);
                  Player := Players.GetPlayerByIndex(player_id);

                  case research_id of
                    101:
                      begin
                        { feudal time }
                        if Assigned(Player) then
                          Player.FeudalTime := time_cnt + 130000; { + research time (2:10) }
                      end;
                    102:
                      begin
                        { castle time }
                        if Assigned(Player) then
                        begin
                          if (Player.CivId = cPersians) then
                            { about 10% less, but calculated as 160s / 1.10 despite of -10% = 144s }
                            Player.CastleTime := time_cnt + Round(160000 / 1.10)
                          else
                            Player.CastleTime := time_cnt + 160000;
                        end;
                      end;
                    103:
                      begin
                        { imperial time }
                        if Assigned(Player) then
                        begin
                          if (Player.CivId = cPersians) then
                            { about 15% less, but calculated as 190s / 1.15 despite of -15% = 161,5s }
                            Player.ImperialTime := time_cnt + Round(190000 / 1.15)
                          else
                            Player.ImperialTime := time_cnt + 190000;
                        end;
                      end;
                  end;

                  if Assigned(Player) then
                  begin
                    { remember this is the time player has just started to research the particular technology,
                      repetitious researching may occure, here we are asking about it }
                    idx := -1;
                    for i := Researches.Count - 1 downto 0 do
                    begin
                      Res := Researches[i] as TResearch;
                      if (Res.Id = research_id) and (Res.PlayerId = player_id) then
                      begin
                        idx := i;
                        Break;
                      end;
                    end;

                    if (idx = -1) then
                    begin
                      { just add this research }
                      Res := TResearch.Create;
                      Res.Id := research_id;
                      Res.Time := time_cnt;
                      Res.PlayerId := player_id;
                      Researches.Add(Res);
                    end else
                    begin
                      Res := Researches[idx] as TResearch;
                      { remember data in body section are not necessarily time-sorted
                        (rarely, but may occure) that's why we are comparing times }
                      if (Res.Time < time_cnt) then
                        Res.Time := time_cnt;
                    end;
                  end;
                  Seek(length - 12);
                end;
              $77:
                begin
                  { training unit }
                  Seek(4);
                  ReadInt32(object_id);
                  ReadWord(unit_type_id);
                  ReadWord(unit_num);

                  TrainedUnit := Units.GetUnit(unit_type_id);
                  if Assigned(TrainedUnit) then
                    Inc(TrainedUnit.Count, unit_num)
                  else
                  begin
                    TrainedUnit := TTrainedUnit.Create;
                    TrainedUnit.Id := unit_type_id;
                    TrainedUnit.Count := unit_num;
                    Units.Add(TrainedUnit);
                  end;
                  Seek(length - 12);
                end;
              $64:
                begin
                  { pc trains unit }
                  Seek(10);
                  ReadWord(unit_type_id);

                  TrainedUnit := Units.GetUnit(unit_type_id);
                  if Assigned(TrainedUnit) then
                    Inc(TrainedUnit.Count)
                  else
                  begin
                    TrainedUnit := TTrainedUnit.Create;
                    TrainedUnit.Id := unit_type_id;
                    TrainedUnit.Count := 1;
                    Units.Add(TrainedUnit);
                  end;
                  Seek(length - 12);
                end;
              $66:
                begin
                  Seek(2);
                  { player_id }
                  ReadWord(player_id);
                  Seek(8);
                  { building_type_id unit_type_id }
                  ReadWord(building_type_id);

                  if (InArray(biGates, building_type_id) <> -1) then
                    building_type_id := biGate
                  else if (InArray(biPalisadeGates, building_type_id) <> -1) then
                    building_type_id := biPalisadeGate;

                  Player := Players.GetPlayerByIndex(player_id);
                  if Assigned(Player) then
                  begin
                    Building := Player.Buildings.GetBuilding(building_type_id);
                    if Assigned(Building) then
                      Inc(Building.Count)
                    else begin
                      Building := TBuilding.Create;
                      Building.Id := building_type_id;
                      Building.Count := 1;
                      Player.Buildings.Add(Building);
                    end;
                  end;
                  Seek(length - 14);
                end;
              $6C:
                begin
                  { tributing }
                  Seek(1);
                  ReadChar(player_id_from);
                  ReadChar(player_id_to);
                  ReadChar(resource_id);
                  ReadFloat(amount_tributed);
                  ReadFloat(market_fee);
                  PlayerFrom := Players.GetPlayerByIndex(player_id_from);
                  PlayerTo   := Players.GetPlayerByIndex(player_id_to);
                  if Assigned(PlayerFrom) and Assigned(PlayerTo) then
                  begin
                    Tribute := TTribute.Create;
                    Tribute.Time       := time_cnt;
                    Tribute.PlayerFrom := PlayerFrom;
                    Tribute.PlayerTo   := PlayerTo;
                    Tribute.ResourceId := TResourceId(resource_id);
                    Tribute.Amount     := Floor(amount_tributed);
                    Tribute.Fee        := market_fee;
                    Tributes.Add(Tribute);
                  end;
                  Seek(length - 12);
                end;
              $03, 78, $00, $75, $6F, $10: Seek(length);
              $20: Seek(length); { save chapter }
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
  Result := True;
end;

function TRecAnalyst.Analyze: Boolean;
var
  StartTime: DWORD;
begin
  Result := False;
  StartTime := GetTickCount;
  try
    try
      if not ExtractStreams then Exit;
      if not AnalyzeHeader then Exit;
      if not AnalyzeBody then Exit;
      PostAnalyze;
      FAnalyzed := True;
    except
      FHeaderStream.Clear;
      FBodyStream.Clear;
      raise;
    end;
  finally
    {$IFDEF EXTENDED}if not FKeepStreams then{$ENDIF}
    begin
      FHeaderStream.Clear;
      FBodyStream.Clear;
    end;
  end;

  FAnalyzeTime := GetTickCount - StartTime;
  Result := True;
end;

procedure TRecAnalyst.BuildTeams;
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
        Team := TTeam.Create;
        Team.AddPlayer(Player);
        Teams.AddTeam(Team);
      end;
    end else begin
      Team := Teams.GetTeamByIndex(Player.Team);
      if Assigned(Team) then
        Team.AddPlayer(Player)
      else begin
        Team := TTeam.Create;
        Team.AddPlayer(Player);
        Teams.AddTeam(Team);
      end;
    end;
  end;
end;

function TRecAnalyst.GenerateMap(const Width: Integer; const Height: Integer): TMemoryStream;
var
  x, y, i, terrain_id, elevation: Integer;
  Player: TPlayer;
  UO: TUnitObject;

  Bmp: TGPBitmap;
  Graphics, PngGraphics: TGPGraphics;

  GoldBrush, StoneBrush, CliffBrush, RelicBrush, FoodBrush: TGPSolidBrush;
  PlayerBrush: TGPSolidBrush;
  PlayerPen: TGPPen;

  ns: TSize;
  sx, sy: Single;
  encoderClsid: TGUID;
  Png: TGPBitmap;
  Stream: IStream;
begin
  Result := FMapImage;
  FMapImage.Position := 0;
  if (FMapImage.Size > 0) and (Width = FMapImageSize.cx) and (Height = FMapImageSize.cy) then
    Exit;
  if not FAnalyzed or not Assigned(FMapData) then
    Exit;

  InitializeGdiplus;

  Bmp := TGPBitmap.Create(FMapWidth, FMapHeight, PixelFormat24bppRGB);
  Graphics := TGPGraphics.Create(Bmp);

  try
    for x := 0 to FMapWidth - 1 do
    begin
      for y := 0 to FMapHeight - 1 do
      begin
        terrain_id := FMapData[x, y] mod 1000;
        elevation := (FMapData[x, y] - terrain_id) div 1000;
        Dec(elevation);

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
      GoldBrush.Free;
      StoneBrush.Free;
      CliffBrush.Free;
      RelicBrush.Free;
      FoodBrush.Free;
    end;

    { draw positions }
    Graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    if not GameSettings.IsScenario and (GameSettings.MapId <> miNomad) then
    begin
      { we do not draw positions in scenarios as they may be set anywhere }
      for i := 0 to Players.Count - 1 do
      begin
        Player := Players[i];
        if Player.IsCooping then Continue;
        with Player.InitialState do
        begin
          PlayerPen := TGPPen.Create(ColorRefToARGB(Player.Color));
          PlayerBrush := TGPSolidBrush.Create(ColorRefToARGB(Player.Color));
          try
            Graphics.DrawEllipse(PlayerPen, MakeRect(Rect(Position.X - 9,
              Position.Y - 9, Position.X + 9, Position.Y + 9)));

            Graphics.FillEllipse(PlayerBrush, MakeRect(Rect(Position.X - 4,
              Position.Y - 4, Position.X + 4, Position.Y + 4)));
          finally
            PlayerPen.Free;
            PlayerBrush.Free;
          end;
        end;
      end;
    end;

    Graphics.SetSmoothingMode(SmoothingModeNone);

    { draw player objects }
    for i := 0 to PlayerObjects.Count - 1 do
    begin
      UO := PlayerObjects[i] as TUnitObject;
      Player := Players.GetPlayerByIndex(UO.Owner);
      if not Assigned(Player) then
        Continue;
      PlayerBrush := TGPSolidBrush.Create(ColorRefToARGB(Player.Color));
      try
        Graphics.FillRectangle(PlayerBrush, MakeRect(Rect(UO.Position.X - 1,
          UO.Position.Y - 1, UO.Position.X + 1, UO.Position.Y + 1)));
      finally
        PlayerBrush.Free;
      end;
    end;

    ns := SizeAfterRotation(-45, FMapWidth, FMapHeight);
    sx := Width / ns.cx;
    sy := Height / ns.cy;

    GetEncoderClsid('image/png', encoderClsid);

    Png := TGPBitmap.Create(Width, Height, PixelFormat32bppARGB);
    PngGraphics := TGPGraphics.Create(Png);
    try
      PngGraphics.TranslateTransform(Png.GetWidth / 2.0, Png.GetHeight / 2.0);
      PngGraphics.ScaleTransform(sx, sy);
      PngGraphics.RotateTransform(-45);
      PngGraphics.DrawImage(Bmp, -(FMapWidth / 2), -(FMapHeight / 2));

      FMapImage.Clear;
      Stream := TStreamAdapter.Create(FMapImage, soReference);
      Png.Save(Stream, encoderClsid, nil);
      FMapImage.Position := 0;
    finally
      Png.Free;
      PngGraphics.Free;
    end;
  finally
    Bmp.Free;
    Graphics.Free;
    FinalizeGdiplus;
  end;
end;

procedure TRecAnalyst.Reset;
begin
  FIsMgl := False;
  FIsMgx := False;
  FIsMgz := False;
  FHeaderStream.Clear;
  FBodyStream.Clear;
  FMapWidth := 0;
  FMapHeight := 0;
  FileName := '';
  SetLength(FMapData, 0);
  FMapData := nil;
  GameSettings.Clear;
  Players.Clear;
  Teams.Clear;
  PreGameChatMessages.Clear;
  InGameChatMessages.Clear;
  Tributes.Clear;
  FAnalyzeTime := 0;
  Researches.Clear;
  Units.Clear;
  GaiaObjects.Clear;
  PlayerObjects.Clear;
  FMapImage.Clear;
  FMapImageSize.cx := 0;
  FMapImageSize.cy := 0;
  header_len := 0;
  next_pos := 0;
  {$IFDEF EXTENDED}
  FKeepStreams := False;
  objectives_pos := 0;
  CommentString := '';
  {$ENDIF}
  FAnalyzed := False;
end;

procedure TRecAnalyst.PostAnalyze;
var
  Player: TPlayer;
  Team: TTeam;
  i, j, idx: Integer;
  team_ary: array[0..7] of Integer;
  Lines, CoopList: TStringList;
  MapFound: Boolean;
  CP: String;
  ChatMessage: TChatMessage;
begin
  with GameSettings do
    if not IsScenario then
    begin
      Lines := TStringList.Create;
      try
        Lines.Text := String(ObjectivesString);
        { get map }
        if FIsMgl or (MapId = miCustom) then
        begin
          if (Lines.Count > 2) then
          begin
            idx := Pos(': ', Lines[2]);
            if (idx <> 0) then
              Map := AnsiString(Copy(AnsiString(Lines[2]), idx + Length(': '), Length(Lines[2])));

            if FIsMgl then
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
        Lines.Free;
      end;
    end;

  BuildTeams;

  { Players }
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

  CoopList := TStringList.Create;
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
    CoopList.Free;
  end;

  { fix: player could click age advance, but game finished before reaching specific age or player has resigned }
  for i := 0 to Players.Count - 1 do
  begin
    Player := Players[i];
    if not Assigned(Player) then Continue;
    with Player do
    begin
    if (FeudalTime > GameSettings.PlayTime) or ((ResignTime > 0) and (FeudalTime > ResignTime)) then
      Player.FeudalTime := 0;
    if (CastleTime > GameSettings.PlayTime) or ((ResignTime > 0) and (CastleTime > ResignTime)) then
      Player.CastleTime := 0;
    if (ImperialTime > GameSettings.PlayTime) or ((ResignTime > 0) and (ImperialTime > ResignTime)) then
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
      ChatMessage := TChatMessage.Create;
      ChatMessage.Time := Player.FeudalTime;
      ChatMessage.Msg := AnsiString(Format(c_feudal_age_advance, [Player.Name]));
      InGameChatMessages.Add(ChatMessage);
    end;
    if (Player.CastleTime <> 0) then
    begin
      ChatMessage := TChatMessage.Create;
      ChatMessage.Time := Player.CastleTime;
      ChatMessage.Msg := AnsiString(Format(c_castle_age_advance, [Player.Name]));
      InGameChatMessages.Add(ChatMessage);
    end;
    if (Player.ImperialTime <> 0) then
    begin
      ChatMessage := TChatMessage.Create;
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

  { AOC11 bug or feature? }
  if (GameSettings.GameVersion > gvAOC10c) then
    GameSettings.PopLimit := 25 * GameSettings.PopLimit;
end;
{$IFDEF EXTENDED}
procedure TRecAnalyst.Build(const FileName: String);
var
  outStream: TMemoryStream;
  header_len: longint;
  hs: TMemoryStream;
begin
  if not FKeepStreams or not FAnalyzed then Exit;

  hs := TMemoryStream.Create;
  outStream := TMemoryStream.Create;
  try
    try
      FHeaderStream.Seek(0, soFromBeginning);

      {$IFDEF FPC}
      if (ZCompressStream2(FHeaderStream, hs, Z_DEFAULT_COMPRESSION, -15, 9, Z_DEFAULT_STRATEGY) < 0) then
        raise ERecAnalystException.Create(c_cannotcompress, RECANALYST_COMP);
      // zError(code)
      {$ELSE}
      ZCompressStream2(FHeaderStream, hs, zcDefault, -15, 9, zsDefault);
      {$ENDIF}
      header_len := hs.Size + SizeOf(header_len);
      if FIsMgx then Inc(header_len, SizeOf(next_pos));
      hs.Seek(0, soFromBeginning);
      FBodyStream.Seek(0, soFromBeginning);
      outStream.Write(header_len, SizeOf(header_len));
      if FIsMgx then outStream.Write(next_pos, SizeOf(next_pos));
      outStream.CopyFrom(hs, hs.Size);
      outStream.CopyFrom(FBodyStream, FBodyStream.Size);
      outStream.SaveToFile(FileName);
    except
      on ERecAnalystException do
        raise;
      on EReadError do
        raise ERecAnalystException.Create(c_cannotreadsection, RECANALYST_FILEREAD);
      on EFCreateError do
        raise ERecAnalystException.Create(c_cannotcreatefile, RECANALYST_FILECREATE);
      {$IFNDEF FPC}
      on EZCompressionError do
        raise ERecAnalystException.Create(c_cannotcompress, RECANALYST_COMP);
      {$ENDIF}
      else
        raise ERecAnalystException.Create(c_unknown, RECANALYST_UNKNOWN);
    end;
  finally
    hs.Free;
    outStream.Free;
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
  if not FAnalyzed or not FKeepStreams or (objectives_pos = 0) then Exit;
  { scenarios are not supported for now }
  if GameSettings.IsScenario then Exit;
  if (CommentString = Comment) then Exit;

  temp := TMemoryStream.Create;
  try
    FHeaderStream.Seek(0, soFromBeginning);
    if (temp.CopyFrom(FHeaderStream, objectives_pos) <> objectives_pos) then
      Exit;

    FHeaderStream.Read(objectives_len, SizeOf(objectives_len));
    if (objectives_len > 0) then
    begin
      FillChar(buff65536, SizeOf(buff65536), #0);
      FHeaderStream.Read(buff65536[0], objectives_len);
      ObjectivesString := PAnsiChar(GameSettings.ObjectivesString + CommentSeparator + Comment);
    end else
      ObjectivesString := PAnsiChar(CommentSeparator + Comment);

    if (Comment = '') then
      ObjectivesString := PAnsiChar(GameSettings.ObjectivesString);

    objectives_len := Length(ObjectivesString) + 1;  { null-terminator character }

    temp.Write(objectives_len, SizeOf(objectives_len));
    temp.Write(ObjectivesString[0], objectives_len);
    temp.CopyFrom(FHeaderStream, FHeaderStream.Size - FHeaderStream.Position);

    temp.Seek(0, soFromBeginning);
    FHeaderStream.Clear;
    FHeaderStream.CopyFrom(temp, temp.Size);

    Result := True;
  finally
    temp.Free;
  end;
end;
{$ENDIF}
procedure TRecAnalyst.ReadPlayerInfoBlock(const num_player: Byte);
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
var
  i, j: Integer;
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
  separator_ptr: Pointer;
  separator_len: Integer;
  map_size_x, map_size_y: Int32;
begin
  map_size_x := FMapWidth;
  map_size_y := FMapHeight;
  try
    with FHeaderStream do
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
            Player.ColorId := P.ColorId;
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
          if FIsMgx then Seek(629) else Seek(593);
          ReadFloat(init_camera_pos_x);
          ReadFloat(init_camera_pos_y);
          if FIsMgx then Seek(9) else Seek(5);
          ReadChar(civilization);
          { sometimes(?) civilization is zero in scenarios when the first player is briton (only? always? rule?) }
          if (civilization = 0) then Inc(civilization);
          { skip unknown9[3] }
          Seek(3);
          ReadChar(player_color);

          with Player do
          begin
            CivId := TCivilization(civilization);
            ColorId := player_color;
            SetColor(player_color);
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
        end;

        if (i = 0) then
        begin
          { GAIA }
          if (GameSettings.GameVersion = gvAOKTrial) or
            (GameSettings.GameVersion = gvAOCTrial) then Seek(4);
          Seek(num_player + 70);
          if FIsMgx then Seek(792) else Seek(756);
        end;

        if FIsMgx then Seek(41249) else Seek(34277);
        Seek(map_size_x * map_size_y);

        { Getting exist_object_pos }
        exist_object_pos := 0;
        repeat
          ReadBuffer(buff256, SizeOf(exist_object_separator));
          if CompareMem(@buff256, @exist_object_separator, SizeOf(exist_object_separator)) then
          begin
            exist_object_pos := Position;
            Break;
          end;
          Seek(-SizeOf(exist_object_separator) + 1);
        until (Position >= Size);

        if (exist_object_pos = 0) then Exit;

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
                      GO := TGaiaObject.Create;
                      GO.Id := unit_id;
                      GO.Position.X := Round(pos_x);
                      GO.Position.Y := Round(pos_y);
                      GaiaObjects.Add(GO);
                      Seek(- 19 - SizeOf(pos_x) - SizeOf(pos_y));
                    end;
                end;
                Seek(63 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id));
                if FIsMgl then Seek(1);
              end;
            20:
              begin
                // not guaranteed
                if FIsMgx then
                begin
                  Seek(59);
                  ReadChar(b);
                  Seek(-59 - SizeOf(b));
                  Seek(68 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id));
                  if (b = 2) then Seek(34);
                end else
                  Seek(103 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id))
              end;
            30:
              begin
                if FIsMgx then
                begin
                  Seek(59);
                  ReadChar(b);
                  Seek(-59 - SizeOf(b));
                  Seek(204 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id));
                  if (b = 2) then Seek(17);
                end else
                begin
                  Seek(60);
                  ReadChar(b);
                  Seek(-60 - SizeOf(b));
                  Seek(205 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id));
                  if (b = 2) then Seek(17);
                end;
             end;
            60:
              begin
                // not guaranteed
                Seek(204);
                ReadChar(b);
                Seek(-204 - SizeOf(b));
                Seek(233 - SizeOf(object_type) - SizeOf(owner) - SizeOf(unit_id));
                if (b <> 0) then Seek(67);
              end;
            70:
              begin
                case unit_id of
                  uiRelic, uiDeer, uiBoar, uiJavelina, uiTurkey, uiSheep:
                    begin
                      Seek(19);
                      ReadFloat(pos_x);
                      ReadFloat(pos_y);
                      GO := TGaiaObject.Create;
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
                  UO := TUnitObject.Create;
                  UO.Id := unit_id;
                  UO.Owner := owner;
                  UO.Position.X := Round(pos_x);
                  UO.Position.Y := Round(pos_y);
                  PlayerObjects.Add(UO);
                end;
                if FIsMgx then
                begin
                  separator_ptr := @object_end_separator;
                  separator_len := SizeOf(object_end_separator);
                end else
                begin
                  separator_ptr := @aok_object_end_separator;
                  separator_len := SizeOf(aok_object_end_separator);
                end;
                { search up to 1000 bytes }
                for j := 0 to 1000 do
                begin
                  ReadBuffer(buff256, separator_len);
                  if CompareMem(@buff256, separator_ptr, separator_len) then
                    Break;
                  Seek(-separator_len + 1);
                end;
                if (j > 1000) then Exit;
              end;
            80:
              begin
                if (owner <> 0) then
                begin
                  Seek(19);
                  ReadFloat(pos_x);
                  ReadFloat(pos_y);
                  UO := TUnitObject.Create;
                  UO.Id := unit_id;
                  UO.Owner := owner;
                  UO.Position.X := Round(pos_x);
                  UO.Position.Y := Round(pos_y);
                  PlayerObjects.Add(UO);
                end;
                if FIsMgx then
                begin
                  separator_ptr := @object_end_separator;
                  separator_len := SizeOf(object_end_separator);
                end else
                begin
                  separator_ptr := @aok_object_end_separator;
                  separator_len := SizeOf(aok_object_end_separator);
                end;
                { search up to 1000 bytes }
                for j := 0 to 1000 do
                begin
                  ReadBuffer(buff256, separator_len);
                  if CompareMem(@buff256, separator_ptr, separator_len) then
                    Break;
                  Seek(-separator_len + 1);
                end;
                if (j > 1000) then Exit;
                Seek(126);
                if FIsMgx then Seek(1);
              end;
            00:
              begin
                Seek(-(SizeOf(object_type) + SizeOf(owner) + SizeOf(unit_id)));

                ReadBuffer(buff256, SizeOf(player_info_end_separator));
                Seek(-SizeOf(player_info_end_separator));

                if CompareMem(@buff256, @player_info_end_separator, SizeOf(player_info_end_separator)) then
                begin
                  Seek(SizeOf(player_info_end_separator));
                  Break;
                end;
                if CompareMem(@buff256, @objects_mid_separator_gaia, 2) then
                  Seek(SizeOf(objects_mid_separator_gaia))
                else Exit;
              end;
            else Exit;
          end;
        end;
      end;
    end;
  except
    on E: Exception do
      raise ERecAnalystException.Create(c_playerinforeaderror, RECANALYST_READPLAYER);
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

function UnitsCompare(Item1, Item2: Pointer): Integer;
begin
  if (TTrainedUnit(Item1).Count < TTrainedUnit(Item2).Count) then
    Result := 1
  else if (TTrainedUnit(Item1).Count > TTrainedUnit(Item2).Count) then
    Result := -1
  else Result := 0;
end;

function ResearchesCompare(Item1, Item2: Pointer): Integer;
begin
  if (TResearch(Item1).Time < TResearch(Item2).Time) then
    Result := -1
  else if (TResearch(Item1).Time > TResearch(Item2).Time) then
    Result := 1
  else Result := 0;
end;

function ChatCompare(Item1, Item2: Pointer): Integer;
begin
  if (TChatMessage(Item1).Time < TChatMessage(Item2).Time) then
    Result := -1
  else if (TChatMessage(Item1).Time > TChatMessage(Item2).Time) then
    Result := 1
  else Result := 0;
end;

function GaiaObjectsCompare(Item1, Item2: Pointer): Integer;
begin
  if (TGaiaObject(Item1).Id = uiRelic) and (TGaiaObject(Item2).Id <> uiRelic) then
    Result := 1
  else if (InArray(CliffsAry, TGaiaObject(Item1).Id) <> -1)
      and (InArray(CliffsAry, TGaiaObject(Item2).Id) = -1) then
    Result := -1
  else if (TGaiaObject(Item2).Id = uiRelic) and (TGaiaObject(Item1).Id <> uiRelic) then
    Result := -1
  else if (InArray(CliffsAry, TGaiaObject(Item2).Id) <> -1)
      and (InArray(CliffsAry, TGaiaObject(Item1).Id) = -1) then
    Result := 1
  else Result := 0;
end;

function ItemById(const Ary: array of TResearchRec; const Id: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(Ary) to High(Ary) do
  begin
    if (Ary[i].Id = Id) then
    begin
      Result := i;
      Exit;
    end;
  end;
end;

function ResearchById(const Id: Integer): Integer;
begin
  Result := ItemById(RESEARCHES, Id);
end;

function UnitById(const Id: Integer): Integer;
begin
  Result := ItemById(UNITS, Id);
end;

function BuildingById(const Id: Integer): Integer;
begin
  Result := ItemById(BUILDINGS, Id);
end;

function MapById(const Id: Integer): Integer;
begin
  Result := ItemById(MAPS, Id);
end;

function InArray(const Ary: array of Integer; Value: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := Low(Ary) to High(Ary) do
    if (Ary[i] = Value) then
    begin
      Result := i;
      Break;
    end;
end;

end.

