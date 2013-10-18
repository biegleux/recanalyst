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
unit uRecAnalystStructs;

interface

uses
  uRecAnalystBase, Windows;

{ definition of internal structures in mgl/mgx format }

type
  { human train unit command data struct }
  THumanTrainUnitCmdDataStruct = packed record
    object_id: Int32; { building_id }
    unit_type_id: Word;
    unit_num: Word;
  end;

  { computer train unit command data struct }

  { resign command data struct }
  TResignCmdDataStruct = packed record
    player_index: Byte;
    player_number: Byte;
    disconnected: Byte;
  end;

  { research command data struct }
  TResearchCmdDataStruct = packed record
    building_id: Int32;
    player_id: Word; { player_number }
    research_id: Word;
  end;

  { tribute command data struct }
  TTributeCmdDataStruct = packed record
    player_id_from: Byte;
    player_id_to: Byte;
    resource_id: Byte;
    amount_tributed: Single;
    market_fee: Single;
  end;

  TMilitaryStatsStruct = packed record
    MilitaryScore: WORD;
    UnitsKilled: WORD;
    u1: WORD;
    UnitsLost: WORD;
    BuildingsRazed: WORD;
    u2: WORD;
    BuildingsLost: WORD;
    UnitsConverted: WORD;
  end;

  TEconomyStatsStruct = packed record
    EconomyScore: WORD;
    u1: WORD;
    FoodCollected: DWORD;
    WoodCollected: DWORD;
    StoneCollected: DWORD;
    GoldCollected: DWORD;
    TributeSent: WORD;
    TributeRcvd: WORD;
    TradeProfit: WORD;
    RelicGold: WORD;
  end;

  TTechnologyStatsStruct = packed record
    TechnologyScore: WORD;
    u1: WORD;
    FeudalAge: DWORD;
    CastleAge: DWORD;
    ImperialAge: DWORD;
    MapExplored: Byte;
    ResearchCount: Byte;
    ResearchPercent: Byte;
  end;

  TSocietyStatsStruct = packed record
    SocietyScore: WORD;
    TotalWonders: Byte;
    TotalCastles: Byte;
    RelicsCaptured: Byte;
    u1: Byte;
    VillagerHigh: WORD;
  end;

  TPlayerStatsStruct = packed record
    PlayerName: array[0..15] of Char;
    TotalScore: WORD;
    TotalScores: array[0..7] of WORD; { Total Scores of all players }
    Victory: Byte; { 0 loss, 1 win }
    Civilization: Byte;
    ColorId: Byte;
    Team: Byte; { team - 1 }
    u1: array[0..1] of Byte;
    Medal: Byte; { Most Valuable Player, 0 No, 1 Yes }
    u2: array[0..2] of Byte;
    Result: Byte; { 0 loss, 1 win, 2 survival to finish, 3 disconnect }
    u3: array[0..2] of Byte;
    MilitaryStats: TMilitaryStatsStruct;
    u4: array[0..31] of Byte;
    EconomyStats: TEconomyStatsStruct;
    u5: array[0..15] of Byte;
    TechnologyStats: TTechnologyStatsStruct;
    u6: Byte;
    SocietyStats: TSocietyStatsStruct;
    u7: array[0..83] of Byte;
  end;

  TGameDataStruct = packed record
    u1: array[0..2] of Byte;
    ScenarioFileName: array[0..31] of Char;
    Players: Byte; { number of players }
    Computers: Byte; { number of computers }
    u2: array[0..1] of Byte;
    Duration: DWORD; { in seconds }
    AllowCheats: Byte;
    Complete: Byte;
    u3: array[0..13] of Byte;
    MapSize: Byte; { 0 tiny, 1 small, 2 medium, 3 normal, 4 large, 5 giant }
    Map: Byte; { map id }
    Population: Byte; { for UP multiply by 25 }
    u4: Byte;
    Victory: Byte; { 0 standard, 1 conquest, 7 time limit, 8 score, 11 last man standing }
    StartingAge: Byte; { standard - post-imperial }
    Resources: Byte; { 0 standard, 1 low, 2 medium, 3 high }
    AllTechs: Byte;
    TeamTogether: Byte; { 0 = yes, 1 = no }
    RevealMap: Byte; { 0 normal, 1 explored, 2 all visible }
    u5: array[0..2] of Byte;
    LockTeams: Byte;
    LockSpeed: Byte;
    u6: Byte;
    PlayerStats: array[0..7] of TPlayerStatsStruct;
    Index: Byte; { Index of the player we received this data from }
    u7: array[0..2] of Byte;
  end;

implementation

end.

