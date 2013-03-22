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
library recanalyst;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

{$IFNDEF FPC}
  {$IFDEF WIN64}
    {$LIBSUFFIX '64'}
{$ELSE}
    {$LIBSUFFIX '32'}
  {$ENDIF}
{$ENDIF}

{$IFNDEF FPC}
  {.$WEAKLINKRTTI ON}
  {.$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}


{.$DEFINE EXTENDED}

{$R *.res}

uses
  Classes,
  Windows,
  SysUtils,
  Contnrs,
  Math,
  uRecAnalystBase,
  uRecAnalyst;

type
  { Recanalyst is represented by a pointer, you can think of a recanalyst pointer
    as an object. The recanalyst_create() is its constructor and recanalyst_free()
    is its destructor. Other routines are methods on a recanalyst object. }
  PRecAnalyst = Pointer;

  { This structure contains information about an initial state of the player. }
  PInitialStateStruct = ^TInitialStateStruct;
  TInitialStateStruct = record
    dwFood: DWORD;
    dwWood: DWORD;
    dwStone: DWORD;
    dwGold: DWORD;
    iStartingAge: Integer;
    dwHouseCapacity: DWORD;
    dwPopulation: DWORD;
    dwCivilianPop: DWORD;
    dwMilitaryPop: DWORD;
    dwExtraPop: DWORD;
    ptPosition: TPoint;
    szStartingAge: array[0..MAXBYTE] of AnsiChar;
  end;

  { This structure contains information about a player. }
  PPlayerStruct = ^TPlayerStruct;
  TPlayerStruct = record
    szName: array[0..MAXBYTE] of AnsiChar;
    dwIndex: DWORD;
    bHuman: BOOL;
    dwTeam: DWORD;
    bOwner: BOOL;
    szCivilization: array[0..MAXBYTE] of AnsiChar;
    dwCivId: DWORD;
    dwColorId: DWORD;
    bIsCooping: BOOL;
    dwFeudalTime: DWORD;
    dwCastleTime: DWORD;
    dwImperialTime: DWORD;
    dwResignTime: DWORD;
    dwDisconnectTime: DWORD;
    lpInitialState: PInitialStateStruct; // pointer to InitialState struct data
  end;

  { This structure contains information about victory settings. }
  PVictoryStruct = ^TVictoryStruct;
  TVictoryStruct = record
    dwTimeLimit: DWORD;
    dwScoreLimit: DWORD;
    dwVictoryCondition: DWORD;
    szVictory: array[0..MAXBYTE] of AnsiChar;
  end;

  { This structure contains information about game settings. }
  PGameSettingsStruct = ^TGameSettingsStruct;
  TGameSettingsStruct = record
    dwGameType: DWORD;
    dwMapStyle: DWORD;
    dwDifficultyLevel: DWORD;
    dwGameSpeed: DWORD;
    dwRevealMap: DWORD;
    dwMapSize: DWORD;
    bIsScenario: BOOL;
    dwPlayers: DWORD;
    dwPOV: DWORD;
    dwMapId: DWORD;
    dwPopLimit: DWORD;
    bLockDiplomacy: BOOL;
    dwPlayTime: DWORD;
    bInGameCoop: BOOL;
    bIsFFA: BOOL;
    dwVersion: DWORD;
    szMap: array[0..MAXWORD] of AnsiChar;
    szPlayersType: array[0..MAXCHAR] of AnsiChar;
    szPOV: array[0..MAXBYTE] of AnsiChar;
//    szPOVEx
    szGameType: array[0..MAXBYTE] of AnsiChar;
    szMapStyle: array[0..MAXBYTE] of AnsiChar;
    szDifficultyLevel: array[0..MAXBYTE] of AnsiChar;
    szGameSpeed: array[0..MAXBYTE] of AnsiChar;
    szRevealMap: array[0..MAXBYTE] of AnsiChar;
    szMapSize: array[0..MAXBYTE] of AnsiChar;
    szVersion: array[0..MAXBYTE] of AnsiChar;
    szScFileName: array[0..MAXWORD] of AnsiChar;
    lpVictory: PVictoryStruct;  // pointer to TVictoryStruct structure
  end;

  { This structure contains information about a chat message. }
  PChatMessageStruct = ^TChatMessageStruct;
  TChatMessageStruct = record
    dwTime: DWORD;  // always zero for pre-game chat messages
    dwPlayerId: DWORD; // zero for players who left the game before its start in pre-game chat,
                       // zero for age advances, resign and disconnect messages in in-game chat
    szMessage: array[0..MAXBYTE] of AnsiChar;
  end;

  { This structure contains information about a tribute. }
  PTributeStruct = ^TTributeStruct;
  TTributeStruct = record
    dwTime: DWORD;
    dwPlayerFrom: DWORD;
    dwPlayerTo: DWORD;
    byResourceId: Byte;
    dwAmount: DWORD;
    fFee: Single;
  end;

  { This structure contains information about a research. }
  PResearchStruct = ^TResearchStruct;
  TResearchStruct = record
    dwTime: DWORD;
    dwId: DWORD;
    dwPlayerId: DWORD;
    szName: array[0..MAXBYTE] of AnsiChar;
  end;

  { The EnumPlayersProc routine is an application-defined callback function
    that receives player data in TPlayerStruct structures as a result of a call
    to the recanalyst_enumplayers() routine.

    lpPlayer contains player data. See TPlayerStruct structure.

    lParam specifies the application-defined value given in recanalyst_enumplayers().

    To continue enumeration, the callback function must return TRUE, to stop
    enumeration, it must return FALSE. }
  EnumPlayersProc = function(lpPlayer: PPlayerStruct; lParam: LPARAM): BOOL; stdcall;

  { The EnumChatMessagesProc routine is an application-defined callback function
    that receives chat message data in TChatMessageStruct structures as a result of a call
    to the recanalyst_enumpregamechat() or recanalyst_enumingamechat() routines.

    lpChatMessage contains chat message data. See TChatMessageStruct structure.

    lParam specifies the application-defined value given in recanalyst_enumpregamechat()
    or recanalyst_enumingamechat().

    To continue enumeration, the callback function must return TRUE, to stop
    enumeration, it must return FALSE. }
  EnumChatMessagesProc = function(lpChatMessage: PChatMessageStruct; lParam: LPARAM): BOOL; stdcall;

  { The EnumTributesProc routine is an application-defined callback function
    that receives tribute data in TTributeStruct structures as a result of a call
    to the recanalyst_enumtributes() routine.

    lpTribute contains tribute data. See TTributeStruct structure.

    lParam specifies the application-defined value given in recanalyst_enumtributes().

    To continue enumeration, the callback function must return TRUE, to stop
    enumeration, it must return FALSE. }
  EnumTributesProc = function(lpTribute: PTributeStruct; lParam: LPARAM): BOOL; stdcall;

  { The EnumResearchesProc routine is an application-defined callback function
    that receives research data in TResearchStruct structures as a result of a call
    to the recanalyst_enumresearches() routine.

    lpResearch contains research data. See TResearchStruct structure.

    lParam specifies the application-defined value given in recanalyst_enumresearches().

    To continue enumeration, the callback function must return TRUE, to stop
    enumeration, it must return FALSE. }
  EnumResearchesProc = function(lpResearch: PResearchStruct; lParam: LPARAM): BOOL; stdcall;

const
  { Return codes }
  RECANALYST_OK          = 0;
  { analyze error codes }
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
  { general error codes }
  GEN_BASE = {$IFDEF EXTENDED}RECANALYST_COMP - 1{$ELSE}RECANALYST_READPLAYER - 1{$ENDIF};
  RECANALYST_INVALIDPTR  = GEN_BASE;
  RECANALYST_FREEOBJ     = GEN_BASE - 1;
  RECANALYST_NOCALLBACK  = GEN_BASE - 2;
  RECANALYST_ENUMP       = GEN_BASE - 3;
  RECANALYST_ANALYZEF    = GEN_BASE - 4;
  RECANALYST_NOTANALYZED = GEN_BASE - 5;
  RECANALYST_TIMECONV    = GEN_BASE - 6;
  RECANALYST_OBJECTIVES  = GEN_BASE - 7;
  RECANALYST_ENUMPRECHAT = GEN_BASE - 8;
  RECANALYST_ENUMINCHAT  = GEN_BASE - 8;
  RECANALYST_ENUMT       = GEN_BASE - 10;
  RECANALYST_ENUMR       = GEN_BASE - 11;
  RECANALYST_GAMESETTS   = GEN_BASE - 12;
  RECANALYST_GENMAP      = GEN_BASE - 13;
  RECANALYST_ANLTIME     = GEN_BASE - 14;
  {$IFDEF EXTENDED}
  RECANALYST_GETCOMM     = GEN_BASE - 15;
  RECANALYST_SETCOMM     = GEN_BASE - 16;
  {$ENDIF}

var
  recanalyst_version: PAnsiChar = '1.3';

const
  ErrorMessages: array[{$IFDEF EXTENDED}RECANALYST_SETCOMM{$ELSE}
    RECANALYST_ANLTIME{$ENDIF}..RECANALYST_INVALIDPTR] of PAnsiChar = (
    {$IFDEF EXTENDED}
    'Error setting a comment.',
    'Error getting a comment.',
    {$ENDIF}
    'Error getting analyze time.',
    'Error generating map.',
    'Error getting game settings data.',
    'Error enumerating researches.',
    'Error enumerating tributes.',
    'Error enumerating in-game chat messages.',
    'Error enumerating pre-game chat messages.',
    'Error getting an objectives string.',
    'Error converting time to its string representation.',
    'File has not been analyzed yet.',
    'Error analyzing file.',
    'Error enumerating players.',
    'No callback function has been defined.',
    'Unable to release object instance.',
    'Invalid pointer.'
  );

{ exported api routines }

{ This routine creates a recanalyst object. If the object is created successfully,
  then its pointer is returned. Otherwise NULL is returned. }
function recanalyst_create(): PRecAnalyst; stdcall; forward;

{ This routine is the destructor for the recanalyst object. The parameter to
  recanalyst_free() must be a recanalyst object obtained from recanalyst_create(). }
function recanalyst_free(lpRecAnalyst: PRecAnalyst): Integer; stdcall; forward;

{ This routine analyzes a file set by the lpFileName parameter. recanalyst_analyze()
  can be called repeatedly on the recanalyst object.

  lpFileName pointer to ANSI filename string. }
function recanalyst_analyze(lpRecAnalyst: PRecAnalyst;
  lpFileName: PAnsiChar): Integer; stdcall; forward;

{ This routine gets the game settings data. recanalyst_analyze() must first be called.

  lpGameSettings points to a TGameSettingsStruct to receive the game settings data. }
function recanalyst_getgamesettings(lpRecAnalyst: PRecAnalyst;
  lpGameSettings: PGameSettingsStruct): Integer; stdcall; forward;

{ This routine enumerates all players by passing the TPlayerStruct of each player,
  in turn, to an application-defined callback function. recanalyst_enumplayers()
  continues until the last player is enumerated or the callback function returns
  FALSE. recanalyst_analyze() must first be called.

  lpEnumFunc points to an application-defined callback function. For more
    information, see EnumPlayersProc callback function.

  lParam specifies a 32-bit/64-bit, application-defined value to be passed
    to the callback function. }
function recanalyst_enumplayers(lpRecAnalyst: PRecAnalyst;
  lpEnumFunc: EnumPlayersProc; lParam: LPARAM): Integer; stdcall; forward;

{ This routine gets the objectives string from a file set by the recanalyst_setparams().
  recanalyst_analyze() must first be called.

  lpObjectives address of buffer for objectives string, you can get the size of
  buffer required for objectives string by passing in a NULL parameter. }
function recanalyst_getobjectives(lpRecAnalyst: PRecAnalyst;
  lpObjectives: PAnsiChar): Integer; stdcall; forward;

{ This routine enumerates pre-game chat messages by passing the TChatMessageStruct
  of each message, in turn, to an application-defined callback function.
  recanalyst_enumpregamechat() continues until the last message is enumerated or
  the callback function returns FALSE. recanalyst_analyze() must first be called.

  lpEnumFunc points to an application-defined callback function. For more
    information, see EnumChatMessagesProc callback function.

  lParam specifies a 32-bit/64-bit, application-defined value to be passed to the
    callback function. }
function recanalyst_enumpregamechat(lpRecAnalyst: PRecAnalyst;
  lpEnumFunc: EnumChatMessagesProc; lParam: LPARAM): Integer; stdcall; forward;

{ This routine enumerates in-game chat messages by passing the TChatMessageStruct
  of each message, in turn, to an application-defined callback function.
  recanalyst_enumingamechat() continues until the last message is enumerated or
  the callback function returns FALSE. recanalyst_analyze() must first be called.

  lpEnumFunc points to an application-defined callback function. For more information,
    see EnumChatMessagesProc callback function.

  lParam specifies a 32-bit/64-bit, application-defined value to be passed to the
    callback function. }
function recanalyst_enumingamechat(lpRecAnalyst: PRecAnalyst;
  lpEnumFunc: EnumChatMessagesProc; lParam: LPARAM): Integer; stdcall; forward;

{ This routine enumerates tributes by passing the TTributeStruct of each tribute,
  in turn, to an application-defined callback function. recanalyst_enumtributes()
  continues until the last tribute is enumerated or the callback function returns
  FALSE. recanalyst_analyze() must first be called.

  lpEnumFunc points to an application-defined callback function. For more
    information, see EnumTributesProc callback function.

  lParam specifies a 32-bit/64-bit, application-defined value to be passed to the
    callback function. }
function recanalyst_enumtributes(lpRecAnalyst: PRecAnalyst;
  lpEnumFunc: EnumTributesProc; lParam: LPARAM): Integer; stdcall; forward;

{ This routine enumerates researches by passing the TResearchStruct of each research,
  in turn, to an application-defined callback function. recanalyst_enumresearches()
  continues until the last research is enumerated or the callback function returns
  FALSE. recanalyst_analyze() must first be called.

  lpEnumFunc points to an application-defined callback function. For more
    information, see EnumResearchesProc callback function.

  lParam specifies a 32-bit/64-bit, application-defined value to be passed to the
    callback function. }
function recanalyst_enumresearches(lpRecAnalyst: PRecAnalyst;
  lpEnumFunc: EnumResearchesProc; lParam: LPARAM): Integer; stdcall; forward;

{ This routine generates a map image. recanalyst_analyze() must first be called.

  dwWidth defines the map image width.

  dwHeight defines the map image height.

  lpImageBuffer address of buffer for map image. }
function recanalyst_generatemap(lpRecAnalyst: PRecAnalyst; dwWidth, dwHeight: DWORD;
  lpImageBuffer: Pointer): Integer; stdcall; forward;

{ This routine gets the analyze time in miliseconds. }
function recanalyst_analyzetime(lpRecAnalyst: PRecAnalyst): Integer; stdcall; forward;

{ This routine converts internal time representation into a string that represents
  its value.

  dwTime internal time representation (miliseconds).

  lpTime address of buffer for time string, you can get the size of buffer
  required for time string by passing in a NULL parameter. }
function recanalyst_timetostring(dwTime: DWORD; lpTime: PAnsiChar): Integer;
  stdcall; forward;

{ This routine can be used to obtain an English language description
  of the error code defined by iErrCode parameter. }
function recanalyst_errmsg(iErrCode: Integer): PAnsiChar; stdcall; forward;

{ This routine returns the version of the recanalyst library. }
function recanalyst_libversion(): PAnsiChar; stdcall; forward;
{$IFDEF EXTENDED}
{ This routine set the comment. recanalyst_analyze() must first be called.

  lpComment pointer to comment string.
}
function recanalyst_setcomment(lpRecAnalyst: PRecAnalyst;
  lpComment: PAnsiChar): Integer; stdcall; forward;

{ This routine gets the comment. recanalyst_analyze() must first be called.

  lpComment address of buffer for comment string, you can get the size of buffer
  required for comment string by passing in a NULL parameter. }
function recanalyst_getcomment(lpRecAnalyst: PRecAnalyst;
  lpComment: PAnsiChar): Integer; stdcall; forward;
{$ENDIF}
function recanalyst_create(): PRecAnalyst; stdcall;
begin
  try
    Result := TRecAnalyst.Create();
  except
    Result := nil;
  end;
end;

function recanalyst_free(lpRecAnalyst: PRecAnalyst): Integer; stdcall;
begin
  if not Assigned(lpRecAnalyst) then
  begin
    Result := RECANALYST_INVALIDPTR;
    Exit;
  end;

  try
    TRecAnalyst(lpRecAnalyst).Free();
    Result := RECANALYST_OK;
  except
    Result := RECANALYST_FREEOBJ;
  end;
end;

function recanalyst_analyze(lpRecAnalyst: PRecAnalyst;
  lpFileName: PAnsiChar): Integer; stdcall;
var
  RecAnalyst: TRecAnalyst;
begin
  if not Assigned(lpRecAnalyst) then
  begin
    Result := RECANALYST_INVALIDPTR;
    Exit;
  end;

  try
    RecAnalyst := TRecAnalyst(lpRecAnalyst);
    RecAnalyst.Reset();
    RecAnalyst.FileName := String(AnsiString(lpFileName));
    {$IFDEF EXTENDED}RecAnalyst.KeepStreams := True;{$ENDIF}
    RecAnalyst.Analyze();
    Result := RECANALYST_OK;
  except
    on E: ERecAnalystException do
      Result := E.Code;
    on Exception do
      Result := RECANALYST_ANALYZEF;
  end;
end;

function recanalyst_getgamesettings(lpRecAnalyst: PRecAnalyst;
  lpGameSettings: PGameSettingsStruct): Integer; stdcall;
var
  RecAnalyst: TRecAnalyst;
begin
  if not Assigned(lpRecAnalyst) then
  begin
    Result := RECANALYST_INVALIDPTR;
    Exit;
  end;

  if not Assigned(lpGameSettings) then
  begin
    Result := RECANALYST_INVALIDPTR;
    Exit;
  end;

  try
    RecAnalyst := TRecAnalyst(lpRecAnalyst);

    if not RecAnalyst.Analyzed then
    begin
      Result := RECANALYST_NOTANALYZED;
      Exit;
    end;

    with RecAnalyst.GameSettings, lpGameSettings^ do
    begin
      dwGameType := Ord(GameType);
      dwMapStyle := Ord(MapStyle);
      dwDifficultyLevel := Ord(DifficultyLevel);
      dwGameSpeed := Ord(GameSpeed);
      dwRevealMap := Ord(RevealMap);
      dwMapSize := Ord(MapSize);
      bIsScenario := IsScenario;
      dwPlayers := RecAnalyst.Players.Count;
      if Assigned(Owner) then
        dwPOV := Owner.Index
      else dwPOV := 0;
      dwMapId := MapId;
      dwPopLimit := PopLimit;
      bLockDiplomacy := LockDiplomacy;
      dwPlayTime := PlayTime;
      bInGameCoop := InGameCoop;
      bIsFFA := IsFFA;
      dwVersion := Ord(GameVersion);
      CpyMem(szMap, Map);
      CpyMem(szPlayersType, PlayersType);
      CpyMem(szPOV, POV);
      CpyMem(szGameType, sGameType);
      CpyMem(szMapStyle, sMapStyle);
      CpyMem(szDifficultyLevel, sDifficultyLevel);
      CpyMem(szGameSpeed, sGameSpeed);
      CpyMem(szRevealMap, sRevealMap);
      CpyMem(szMapSize, sMapSize);
      CpyMem(szVersion, sGameVersion);
      CpyMem(szScFileName, ScFileName);
      if Assigned(lpVictory) then
      begin
        lpVictory^.dwTimeLimit := Victory.TimeLimit;
        lpVictory^.dwScoreLimit := Victory.ScoreLimit;
        lpVictory^.dwVictoryCondition := Ord(Victory.VictoryCondition);
        CpyMem(lpVictory^.szVictory, Victory.VictoryString);
      end;
    end;

    Result := RECANALYST_OK;
  except
    Result := RECANALYST_GAMESETTS;
  end;
end;

function recanalyst_enumplayers(lpRecAnalyst: PRecAnalyst;
  lpEnumFunc: EnumPlayersProc; lParam: LPARAM): Integer; stdcall;
var
  RecAnalyst: TRecAnalyst;
  P: TPlayer;
  Player: TPlayerStruct;
  InitialState: TInitialStateStruct;
  i: Integer;
begin
  if not Assigned(lpRecAnalyst) then
  begin
    Result := RECANALYST_INVALIDPTR;
    Exit;
  end;

  if not Assigned(lpEnumFunc) then
  begin
    Result := RECANALYST_NOCALLBACK;
    Exit;
  end;

  try
    RecAnalyst := TRecAnalyst(lpRecAnalyst);

    if not RecAnalyst.Analyzed then
    begin
      Result := RECANALYST_NOTANALYZED;
      Exit;
    end;

    for i := 0 to RecAnalyst.Players.Count - 1 do
    begin
      P := RecAnalyst.Players[i];
      ZeroMemory(@Player, SizeOf(TPlayerStruct));
      ZeroMemory(@InitialState, SizeOf(TInitialStateStruct));

      { fill player's initial state struct }
      with InitialState, P.InitialState do
      begin
        dwFood := Food;
        dwWood := Wood;
        dwStone := Stone;
        dwGold := Gold;
        iStartingAge := Ord(StartingAge);
        dwHouseCapacity := HouseCapacity;
        dwPopulation := Population;
        dwCivilianPop := CivilianPop;
        dwMilitaryPop := MilitaryPop;
        dwExtraPop := ExtraPop;
        ptPosition := Position;
        CpyMem(szStartingAge, StartingAgeString);
      end;

      with Player, P do
      begin
        CpyMem(szName, Name);
        dwIndex := Index;
        bHuman := Human;
        dwTeam := Team;
        bOwner := Owner;
        dwCivId := Ord(CivId);
        CpyMem(szCivilization, Civ);
        dwColorId := ColorId;
        bIsCooping := IsCooping;
        dwFeudalTime := FeudalTime;
        dwCastleTime := CastleTime;
        dwImperialTime := ImperialTime;
        dwResignTime := ResignTime;
        dwDisconnectTime := DisconnectTime;
      end;
      Player.lpInitialState := @InitialState;

      if not lpEnumFunc(@Player, lParam) then Break;
    end;
    Result := RECANALYST_OK;
  except
    Result := RECANALYST_ENUMP;
  end;
end;

function recanalyst_getobjectives(lpRecAnalyst: PRecAnalyst;
  lpObjectives: PAnsiChar): Integer; stdcall;
var
  RecAnalyst: TRecAnalyst;
begin
  if not Assigned(lpRecAnalyst) then
  begin
    Result := RECANALYST_INVALIDPTR;
    Exit;
  end;

  try
    RecAnalyst := TRecAnalyst(lpRecAnalyst);

    if not RecAnalyst.Analyzed then
    begin
      Result := RECANALYST_NOTANALYZED;
      Exit;
    end;

    if not Assigned(lpObjectives) then
      Result := Length(RecAnalyst.GameSettings.ObjectivesString) + 1
    else begin
      StrPCopyA(lpObjectives, RecAnalyst.GameSettings.ObjectivesString);
      Result := RECANALYST_OK;
    end;
  except
    Result := RECANALYST_OBJECTIVES;
  end;
end;

function recanalyst_enumpregamechat(lpRecAnalyst: PRecAnalyst;
  lpEnumFunc: EnumChatMessagesProc; lParam: LPARAM): Integer; stdcall;
var
  RecAnalyst: TRecAnalyst;
  M: TChatMessage;
  ChatMessage: TChatMessageStruct;
  i: Integer;
begin
  if not Assigned(lpRecAnalyst) then
  begin
    Result := RECANALYST_INVALIDPTR;
    Exit;
  end;

  try
    RecAnalyst := TRecAnalyst(lpRecAnalyst);

    if not RecAnalyst.Analyzed then
    begin
      Result := RECANALYST_NOTANALYZED;
      Exit;
    end;

    for i := 0 to RecAnalyst.PreGameChatMessages.Count - 1 do
    begin
      M := RecAnalyst.PreGameChatMessages[i] as TChatMessage;

      ZeroMemory(@ChatMessage, SizeOf(TChatMessageStruct));

      with ChatMessage do
      begin
        dwTime := M.Time;
        if Assigned(M.Player) then
          dwPlayerId := M.Player.Index
        else
          dwPlayerId := 0;
        CpyMem(szMessage, M.Msg);
      end;
      if not lpEnumFunc(@ChatMessage, lParam) then Break;
    end;
    Result := RECANALYST_OK;
  except
    Result := RECANALYST_ENUMPRECHAT;
  end;
end;

function recanalyst_enumingamechat(lpRecAnalyst: PRecAnalyst;
  lpEnumFunc: EnumChatMessagesProc; lParam: LPARAM): Integer; stdcall;
var
  RecAnalyst: TRecAnalyst;
  M: TChatMessage;
  ChatMessage: TChatMessageStruct;
  i: Integer;
begin
  if not Assigned(lpRecAnalyst) then
  begin
    Result := RECANALYST_INVALIDPTR;
    Exit;
  end;

  try
    RecAnalyst := TRecAnalyst(lpRecAnalyst);

    if not RecAnalyst.Analyzed then
    begin
      Result := RECANALYST_NOTANALYZED;
      Exit;
    end;

    for i := 0 to RecAnalyst.InGameChatMessages.Count - 1 do
    begin
      M := RecAnalyst.InGameChatMessages[i] as TChatMessage;

      ZeroMemory(@ChatMessage, SizeOf(TChatMessageStruct));

      with ChatMessage do
      begin
        dwTime := M.Time;
        if Assigned(M.Player) then
          dwPlayerId := M.Player.Index
        else
          dwPlayerId := 0;
        CpyMem(szMessage, M.Msg);
      end;
      if not lpEnumFunc(@ChatMessage, lParam) then Break;
    end;
    Result := RECANALYST_OK;
  except
    Result := RECANALYST_ENUMINCHAT;
  end;
end;

function recanalyst_enumtributes(lpRecAnalyst: PRecAnalyst;
  lpEnumFunc: EnumTributesProc; lParam: LPARAM): Integer; stdcall;
var
  RecAnalyst: TRecAnalyst;
  T: TTribute;
  Tribute: TTributeStruct;
  i: Integer;
begin
  if not Assigned(lpRecAnalyst) then
  begin
    Result := RECANALYST_INVALIDPTR;
    Exit;
  end;

  try
    RecAnalyst := TRecAnalyst(lpRecAnalyst);

    if not RecAnalyst.Analyzed then
    begin
      Result := RECANALYST_NOTANALYZED;
      Exit;
    end;

    for i := 0 to RecAnalyst.Tributes.Count - 1 do
    begin
      T := RecAnalyst.Tributes[i] as TTribute;

      ZeroMemory(@Tribute, SizeOf(TTributeStruct));

      with Tribute do
      begin
        dwTime := T.Time;
        dwPlayerFrom := T.PlayerFrom.Index;
        dwPlayerTo := T.PlayerTo.Index;
        byResourceId := Ord(T.ResourceId);
        dwAmount := T.Amount;
        fFee := T.Fee;
      end;
      if not lpEnumFunc(@Tribute, lParam) then Break;
    end;
    Result := RECANALYST_OK;
  except
    Result := RECANALYST_ENUMT;
  end;
end;

function recanalyst_enumresearches(lpRecAnalyst: PRecAnalyst;
  lpEnumFunc: EnumResearchesProc; lParam: LPARAM): Integer; stdcall;
var
  RecAnalyst: TRecAnalyst;
  R: TResearch;
  Research: TResearchStruct;
  i: Integer;
begin
  if not Assigned(lpRecAnalyst) then
  begin
    Result := RECANALYST_INVALIDPTR;
    Exit;
  end;

  try
    RecAnalyst := TRecAnalyst(lpRecAnalyst);

    if not RecAnalyst.Analyzed then
    begin
      Result := RECANALYST_NOTANALYZED;
      Exit;
    end;

    for i := 0 to RecAnalyst.Researches.Count - 1 do
    begin
      R := RecAnalyst.Researches[i] as TResearch;

      ZeroMemory(@Research, SizeOf(TResearchStruct));

      with Research do
      begin
        dwTime := R.Time;
        dwId := R.Id;
        dwPlayerId := R.Player.Index;
        CpyMem(szName, R.Name);
      end;
      if not lpEnumFunc(@Research, lParam) then Break;
    end;
    Result := RECANALYST_OK;
  except
    Result := RECANALYST_ENUMR;
  end;
end;

function recanalyst_generatemap(lpRecAnalyst: PRecAnalyst;
  dwWidth, dwHeight: DWORD; lpImageBuffer: Pointer): Integer; stdcall;
var
  RecAnalyst: TRecAnalyst;
  MapImage: TMemoryStream;
begin
  if not Assigned(lpRecAnalyst) then
  begin
    Result := RECANALYST_INVALIDPTR;
    Exit;
  end;

  try
    RecAnalyst := TRecAnalyst(lpRecAnalyst);

    if not RecAnalyst.Analyzed then
    begin
      Result := RECANALYST_NOTANALYZED;
      Exit;
    end;

    MapImage := RecAnalyst.GenerateMap(dwWidth, dwHeight);

    if not Assigned(lpImageBuffer) then
      Result := MapImage.Size
    else begin
      MapImage.Read(lpImageBuffer^, MapImage.Size);
      Result := RECANALYST_OK;
    end;
  except
    Result := RECANALYST_GENMAP;
  end;
end;

function recanalyst_analyzetime(lpRecAnalyst: PRecAnalyst): Integer; stdcall;
var
  RecAnalyst: TRecAnalyst;
begin
  if not Assigned(lpRecAnalyst) then
  begin
    Result := RECANALYST_INVALIDPTR;
    Exit;
  end;

  try
    RecAnalyst := TRecAnalyst(lpRecAnalyst);

    if not RecAnalyst.Analyzed then
    begin
      Result := RECANALYST_NOTANALYZED;
      Exit;
    end;

    Result := RecAnalyst.AnalyzeTime;
  except
    Result := RECANALYST_ANLTIME;
  end;
end;


function recanalyst_timetostring(dwTime: DWORD;
  lpTime: PAnsiChar): Integer; stdcall;
begin
  if not Assigned(lpTime) then
  begin
    Result := RECANALYST_INVALIDPTR;
    Exit;
  end;

  try
    if not Assigned(lpTime) then
      Result := Length(TRecAnalyst.GameTimeToString(dwTime)) + 1
    else begin
      StrPCopyA(lpTime, TRecAnalyst.GameTimeToString(dwTime));
      Result := RECANALYST_OK;
    end;
  except
    Result := RECANALYST_TIMECONV;
  end;
end;

function recanalyst_errmsg(iErrCode: Integer): PAnsiChar; stdcall;
begin
  try
    if (iErrCode > RECANALYST_INVALIDPTR) then
      Result := PAnsiChar(AnsiString(TRecAnalyst.ErrorCodeToString(iErrCode)))
    else
      Result := ErrorMessages[iErrCode];
  except
    Result := nil;
  end;
end;

function recanalyst_libversion(): PAnsiChar; stdcall;
begin
  Result := recanalyst_version;
end;
{$IFDEF EXTENDED}
function recanalyst_setcomment(lpRecAnalyst: PRecAnalyst;
  lpComment: PAnsiChar): Integer; stdcall;
var
  RecAnalyst: TRecAnalyst;
  fh, fdate: Integer;
begin
  if not Assigned(lpRecAnalyst) then
  begin
    Result := RECANALYST_INVALIDPTR;
    Exit;
  end;

  try
    RecAnalyst := TRecAnalyst(lpRecAnalyst);

    if not RecAnalyst.Analyzed then
    begin
      Result := RECANALYST_NOTANALYZED;
      Exit;
    end;

    if RecAnalyst.AddComment(lpComment) then
    begin
      fdate := 0;
      fh := FileOpen(RecAnalyst.FileName, fmOpenRead);
      if (fh > 0) then fdate := FileGetDate(fh);
      FileClose(fh);
      RecAnalyst.Build(RecAnalyst.FileName);
      FileSetDate(RecAnalyst.FileName, fdate);
      Result := RECANALYST_OK
    end else Result := RECANALYST_SETCOMM;
  except
    Result := RECANALYST_SETCOMM;
  end;
end;

function recanalyst_getcomment(lpRecAnalyst: PRecAnalyst;
  lpComment: PAnsiChar): Integer; stdcall;
var
  RecAnalyst: TRecAnalyst;
begin
  if not Assigned(lpRecAnalyst) then
  begin
    Result := RECANALYST_INVALIDPTR;
    Exit;
  end;

  try
    RecAnalyst := TRecAnalyst(lpRecAnalyst);

    if not RecAnalyst.Analyzed then
    begin
      Result := RECANALYST_NOTANALYZED;
      Exit;
    end;

    if not Assigned(lpComment) then
      Result := Length(RecAnalyst.CommentString) + 1
    else begin
      StrPCopy(lpComment, RecAnalyst.CommentString);
      Result := RECANALYST_OK;
    end;
  except
    Result := RECANALYST_GETCOMM;
  end;
end;
{$ENDIF}
exports
  recanalyst_create name 'recanalyst_create',
  recanalyst_free name 'recanalyst_free',
  recanalyst_analyze name 'recanalyst_analyze',
  recanalyst_getgamesettings name 'recanalyst_getgamesettings',
  recanalyst_enumplayers name 'recanalyst_enumplayers',
  recanalyst_getobjectives name 'recanalyst_getobjectives',
  recanalyst_enumpregamechat name 'recanalyst_enumpregamechat',
  recanalyst_enumingamechat name 'recanalyst_enumingamechat',
  recanalyst_enumtributes name 'recanalyst_enumtributes',
  recanalyst_enumresearches name 'recanalyst_enumresearches',
  recanalyst_generatemap name 'recanalyst_generatemap',
  recanalyst_analyzetime name 'recanalyst_analyzetime',
  recanalyst_timetostring name 'recanalyst_timetostring',
  recanalyst_errmsg name 'recanalyst_errmsg',
  {$IFDEF EXTENDED}
  recanalyst_setcomment name 'recanalyst_setcomment',
  recanalyst_getcomment name 'recanalyst_getcomment',
  {$ENDIF}
  recanalyst_libversion name 'recanalyst_libversion';
end.

