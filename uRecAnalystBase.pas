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
unit uRecAnalystBase;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Windows;

// Types
{$IFNDEF FPC}
type
  Int32 = Longint;
{$ENDIF}

// Routines
function SizeAfterRotation(const Angle: Extended; const Width, Height: Integer): TSize;
{$IFDEF FPC}
function ZDecompressStream2(inStream, outStream: TStream; windowBits: Integer): Integer;
function ZCompressStream2(inStream, outStream: TStream; level, windowBits,
    memLevel, strategy : Longint): Integer;
{$ENDIF}
function UnitsCompare(Item1, Item2: Pointer): Integer;
function ResearchesCompare(Item1, Item2: Pointer): Integer;
function ChatCompare(Item1, Item2: Pointer): Integer;
function GaiaObjectsCompare(Item1, Item2: Pointer): Integer;
function ResearchById(const Id: Integer): Integer;
function UnitById(const Id: Integer): Integer;
function BuildingById(const Id: Integer): Integer;
function MapById(const Id: Integer): Integer;
function MapIdToName(const MapId: Integer): AnsiString;
procedure FixUnitTypeId(var unit_type_id: Word);
procedure FixBuildingTypeId(var building_type_id: Word);
// see Remarks at http://msdn.microsoft.com/en-us/library/windows/desktop/ms534077%28v=vs.85%29.aspx
procedure InitializeGdiplus(var gdiplusToken: ULONG);
procedure FinalizeGdiplus(var gdiplusToken: ULONG);

type
  TResearchRec = record
    Id: Integer;
    Name: AnsiString;
  end;

  TMapRec = TResearchRec;
  TUnitRec = TResearchRec;
  TBuildingRec = TResearchRec;

  TGameVersion = (gvUnknown, gvAOK, gvAOKTrial, gvAOK20, gvAOK20a, gvAOC,
    gvAOCTrial, gvAOC10, gvAOC10c, gvAOE2HD, gvAOFE21, gvAOFE22, gvAOCUP11,
    gvAOCUP12, gvAOCUP13, gvAOCUP14);
  TMapStyle = (msStandard, msRealWorld, msCustom);
  TDifficultyLevel = (dlHardest, dlHard, dlModerate, dlStandard {dlEasy}, dlEasiest);
  TGameType = (gtRandomMap, gtRegicide, gtDeathMatch, gtScenario, gtCampaign,
    gtKingOfTheHill, gtWonderRace, gtDefendWonder, gtTurboRandomMap);
  TGameSpeed = (gsSlow = 100, gsNormal = 150, gsFast = 200);
  TRevealMap = (rmNormal, rmExplored, rmAllVisible);
  TMapSize = (msTiny, msSmall, msMedium, msNormal, msLarge, msGiant);
  TCivilization = (cNone, cBritons, cFranks, cGoths, cTeutons, cJapanese, cChinese,
    cByzantines, cPersians, cSaracens, cTurks, cVikings, cMongols, cCelts, cSpanish,
    cAztecs, cMayans, cHuns, cKoreans, cItalians, cIndians, cIncas, cMagyars, cSlavs);
  TResource = (rFood, rWood, rStone, rGold);
  TStartingAge = (saDarkAge = 0, saFeudalAge = 1, saCastleAge = 2, saImperialAge = 3,
    saPostImperialAge = 4);
  TVictoryCondition = (vcStandard, vcConquest, vcTimeLimit, vcScoreLimit, vcCustom);
  TGameMode = (gmSinglePlayer, gmMultiPlayer);
  TGameResult = (grLoss, grWin, grSurvivalToFinish, grDisconnect);
  TPlayerColor = (pclUndefined, pclBlue, pclRed, pclGreen, pclYellow, pclTeal,
    pclPurple, pclGrey, pclOrange);

  {$I id_maps.inc}
  {$I id_researches.inc}
  {$I id_buildings.inc}
  {$I id_units.inc}
  {$I id_units_ex.inc}

const
  { version strings }
  VER_94 = 'VER 9.4';
  VER_93 = 'VER 9.3';
  TRL_93 = 'TRL 9.3';
  VER_95 = 'VER 9.5';
  VER_98 = 'VER 9.8';
  VER_99 = 'VER 9.9';
  VER_9A = 'VER 9.A';
  VER_9B = 'VER 9.B';

  MAPS_NUM = 37;
  MAPS: array[0..MAPS_NUM - 1] of TMapRec = (
    (Id: miArabia;         Name: 'Arabia'),
    (Id: miArchipelago;    Name: 'Archipelago'),
    (Id: miBaltic;         Name: 'Baltic'),
    (Id: miBlackForest;    Name: 'Black Forest'),
    (Id: miCoastal;        Name: 'Coastal'),
    (Id: miContinental;    Name: 'Continental'),
    (Id: miCraterLake;     Name: 'Crater Lake'),
    (Id: miFortress;       Name: 'Fortress'),
    (Id: miGoldRush;       Name: 'Gold Rush'),
    (Id: miHighland;       Name: 'Highland'),
    (Id: miIslands;        Name: 'Islands'),
    (Id: miMediterranean;  Name: 'Mediterranean'),
    (Id: miMigration;      Name: 'Migration'),
    (Id: miRivers;         Name: 'Rivers'),
    (Id: miTeamIslands;    Name: 'Team Islands'),
    (Id: miRandom;         Name: 'Random'),
    (Id: miScandinavia;    Name: 'Scandinavia'),
    (Id: miMongolia;       Name: 'Mongolia'),
    (Id: miYucatan;        Name: 'Yucatan'),
    (Id: miSaltMarsh;      Name: 'Salt Marsh'),
    (Id: miArena;          Name: 'Arena'),
    (Id: miKingOfTheHill;  Name: 'King of the Hill'), //
    (Id: miOasis;          Name: 'Oasis'),
    (Id: miGhostLake;      Name: 'Ghost Lake'),
    (Id: miNomad;          Name: 'Nomad'),
    (Id: miIberia;         Name: 'Iberia'),
    (Id: miBritain;        Name: 'Britain'),
    (Id: miMideast;        Name: 'Mideast'),
    (Id: miTexas;          Name: 'Texas'),
    (Id: miItaly;          Name: 'Italy'),
    (id: miCentralAmerica; Name: 'Central America'),
    (Id: miFrance;         Name: 'France'),
    (Id: miNorseLands;     Name: 'Norse Lands'),
    (Id: miSeaOfJapan;     Name: 'Sea of Japan (East Sea)'),
    (Id: miByzantinum;     Name: 'Byzantinum'),
    (Id: miCustom;         Name: 'Custom'),
    (Id: miBlindRandom;    Name: 'Blind Random'));

  GAME_VERSIONS: array[TGameVersion] of AnsiString = (
    'Unknown', 'AOK', 'AOK Trial', 'AOK 2.0', 'AOK 2.0a', 'AOC', 'AOC Trial',
    'AOC 1.0', 'AOC 1.0c', 'AOE2 HD', 'AOFE 2.1', 'AOFE 2.2', 'AOC UP 1.1', 'AOC UP 1.2',
    'AOC UP 1.3', 'AOC UP 1.4');

  MAP_STYLES: array[TMapStyle] of AnsiString = (
    'Standard', 'Real World', 'Custom');

  AOC_DIFFICULTY_LEVELS: array[TDifficultyLevel] of AnsiString = (
    'Hardest', 'Hard', 'Moderate', 'Standard', 'Easiest');

  AOK_DIFFICULTY_LEVELS: array[TDifficultyLevel] of AnsiString = (
    'Hardest', 'Hard', 'Moderate', 'Easy', 'Easiest');

  GAME_TYPES: array[TGameType] of AnsiString = (
    'Random Map', 'Regicide', 'Death Match', 'Scenario', 'Campaign',
    'King of the Hill', 'Wonder Race', 'Defend the Wonder', 'Turbo Random Map');

  GAME_SPEEDS: array[0..2] of AnsiString = (
    'Slow', 'Normal', 'Fast');

  REVEAL_SETTINGS: array[TRevealMap] of AnsiString = (
    'Normal', 'Explored', 'All Visible');

  MAP_SIZES: array[TMapSize] of AnsiString = (
    'Tiny (2 players)', 'Small (3 players)', 'Medium (4 players)',
    'Normal (6 players)', 'Large (8 players)', 'Giant');

  STARTING_AGES: array[TStartingAge] of AnsiString = (
    'Dark Age', 'Feudal Age', 'Castle Age', 'Imperial Age', 'Post-Imperial Age');

  VICTORY_CONDITIONS: array[TVictoryCondition] of AnsiString = (
    'Standard', 'Conquest', 'Time Limit', 'Score Limit', 'Custom');

  CIVS: array[TCivilization] of AnsiString = (
    '',
    'Britons',
    'Franks',
    'Goths',
    'Teutons',
    'Japanese',
    'Chinese',
    'Byzantines',
    'Persians',
    'Saracens',
    'Turks',
    'Vikings',
    'Mongols',
    'Celts',
    'Spanish',
    'Aztecs',
    'Mayans',
    'Huns',
    'Koreans',
    'Italians',
    'Indians',
    'Incas',
    'Magyars',
    'Slavs');

  PLAYER_COLORS: array[TPlayerColor] of Cardinal = (
    $00000000,
    $00ff0000,
    $000000ff,
    $0000ff00,
    $0000ffff,
    $00ffff00,
    $00ff00ff,
    $00b9b9b9,
    $000182ff);

  TERRAIN_COLORS: array[$00..$28] of Cardinal = (
    $279733,  // Id: $00
    $b65d30,  // Id: $01
    $78b4e8,  // Id: $02
    $52a2e4,  // Id: $03
    $b09254,  // Id: $04
    $279733,  // Id: $05
    $52a2e4,  // Id: $06
    $4d8882,
    $4d8882,
    $279733,  // Id: $09
    $157615,  // Id: $0A
    $52a2e4,  // Id: $0B
    $279733,  // Id: $0C
    $157615,  // Id: $0D
    $78b4e8,  // Id: $0E
    $b65d30,
    $279733,
    $157615,  // Id: $11
    $157615,  // Id: $12
    $157615,  // Id: $13
    $157615,  // Id: $14
    $157615,  // Id: $15
    $a14a00,  // Id: $16
    $bb4a00,  // Id: $17
    $52a2e4,  // Id: $18
    $52a2e4,  // Id: $19
    $49ecff,
    $52a2e4,  // Id: $1B
    $b65d30,
    $4d8882,
    $4d8882,
    $4d8882,
    $ffd8c8,  // Id: $20
    $ffd8c8,  // Id: $21
    $ffd8c8,  // Id: $22
    $f0c098,  // Id: $23
    $ffd8c8,
    $f0c098,  // Id: $25
    $ffd8c8,  // Id: $26
    $ffd8c8,  // Id: $27
    $52a2e4
  );
  UNKNOWN_TERRAIN_COLOR = $ff00ff;  // fuchsia

  GOLD_COLOR  = $00c7ff;
  STONE_COLOR = $919191;
  CLIFF_COLOR = $334b71;
  RELIC_COLOR = $ffffff;
  FOOD_COLOR  = $6cc4a5;

  RESOURCES: array[TResource] of AnsiString = (
    'Food', 'Wood', 'Stone', 'Gold'
  );

  RESEARCHES_NUM = 139 + 35;
  RESEARCHES: array[0..RESEARCHES_NUM - 1] of TResearchRec = (
    // town center
    (Id: riFeudalAge;           Name: 'Feudal Age'),
    (Id: riCastleAge;           Name: 'Castle Age'),
    (Id: riImperialAge;         Name: 'Imperial Age'),
    (Id: riLoom;                Name: 'Loom'),
    (Id: riWheelBarrow;         Name: 'Wheel Barrow'),
    (Id: riHandCart;            Name: 'Hand Cart'),
    (Id: riTownWatch;           Name: 'Town Watch'),
    (Id: riTownPatrol;          Name: 'Town Patrol'),
    // mill
    (Id: riHorseCollar;         Name: 'Horse Collar'),
    (Id: riHeavyPlow;           Name: 'Heavy Plow'),
    (Id: riCropRotation;        Name: 'Crop Rotation'),
    // lumber camp
    (Id: riDoubleBitAxe;        Name: 'Double-Bit Axe'),
    (Id: riBowSaw;              Name: 'Bow Saw'),
    (Id: riTwoManSaw;           Name: 'Two-Man Saw'),
    // mining camp
    (Id: riStoneMining;         Name: 'Stone Mining'),
    (Id: riGoldMining;          Name: 'Gold Mining'),
    (Id: riStoneShaftMining;    Name: 'Stone Shaft Mining'),
    (Id: riGoldShaftMining;     Name: 'Gold Shaft Mining'),
    // blacksmith
    (Id: riPaddedArcherArmor;   Name: 'Padded Archer Armor'),
    (Id: riLeatherArcherArmor;  Name: 'Leather Archer Armor'),
    (Id: riRingArcherArmor;     Name: 'Ring Archer Armor'),
    (Id: riFletching;           Name: 'Fletching'),
    (Id: riBodkinArrow;         Name: 'Bodkin Arrow'),
    (Id: riBracer;              Name: 'Bracer'),
    (Id: riForging;             Name: 'Forging'),
    (Id: riIronCasting;         Name: 'Iron Casting'),
    (Id: riBlastFurnace;        Name: 'Blast Furnace'),
    (Id: riScaleBardingArmor;   Name: 'Scale Barding Armor'),
    (Id: riChainBardingArmor;   Name: 'Chain Barding Armor'),
    (Id: riPlateBardingArmor;   Name: 'Plate Barding Armor'),
    (Id: riScaleMailArmor;      Name: 'Scale Mail Armor'),
    (Id: riChainMailArmor;      Name: 'Chain Mail Armor'),
    (Id: riPlateMailArmor;      Name: 'Plate Mail Armor'),
    // university
    (Id: riMasonry;             Name: 'Masonry'),
    (Id: riFortifiedWall;       Name: 'Fortified Wall'),
    (Id: riBallistics;          Name: 'Ballistics'),
    (Id: riGuardTower;          Name: 'Guard Tower'),
    (Id: riHeatedShot;          Name: 'Heated Shot'),
    (Id: riMurderHoles;         Name: 'Murder Holes'),
    (Id: riTreadmillCrane;      Name: 'Treadmill Crane'),
    (Id: riArchitecture;        Name: 'Architecture'),
    (Id: riChemistry;           Name: 'Chemistry'),
    (Id: riSiegeEngineers;      Name: 'Siege Engineers'),
    (Id: riKeep;                Name: 'Keep'),
    (Id: riBombardTower;        Name: 'Bombard Tower'),
    // monastery
    (Id: riRedemption;          Name: 'Redemption'),
    (Id: riFervor;              Name: 'Fervor'),
    (Id: riSanctity;            Name: 'Sanctity'),
    (Id: riAtonement;           Name: 'Atonement'),
    (Id: riHerbalMedicine;      Name: 'Herbal Medicine'),
    (Id: riHeresy;              Name: 'Heresy'),
    (Id: riBlockPrinting;       Name: 'Block Printing'),
    (Id: riIllumination;        Name: 'Illumination'),
    (Id: riFaith;               Name: 'Faith'),
    (Id: riTheocracy;           Name: 'Theocracy'),
    // market
    (Id: riCartography;         Name: 'Cartography'),
    (Id: riCaravan;             Name: 'Caravan'),
    (Id: riGuilds;              Name: 'Guilds'),
    (Id: riCoinage;             Name: 'Coinage'),
    (Id: riBanking;             Name: 'Banking'),
    // castle
    (Id: riHoardings;           Name: 'Hoardings'),
    (Id: riSappers;             Name: 'Sappers'),
    (Id: riConscription;        Name: 'Conscription'),
    (Id: riSpiesTreason;        Name: 'Spies / Treason'),
    // barrack
    (Id: riManAtArms;           Name: 'Man-at-Arms'),
    (Id: riLongSwordsman;       Name: 'Long Swordsman'),
    (Id: riTwoHandedSwordsman;  Name: 'Two-Handed Swordsman'),
    (Id: riChampion;            Name: 'Champion'),
    (Id: riPikeman;             Name: 'Pikeman'),
    (Id: riHalberdier;          Name: 'Halberdier'),
    (Id: riEliteEagleWarrior;   Name: 'Elite Eagle Warrior'),
    (Id: riTracking;            Name: 'Tracking'),
    (Id: riSquires;             Name: 'Squires'),
    // archery range
    (Id: riCrossbow;            Name: 'Crossbow'),
    (Id: riArbalest;            Name: 'Arbalest'),
    (Id: riEliteSkirmisher;     Name: 'Elite Skirmisher'),
    (Id: riHeavyCavalryArcher;  Name: 'Heavy Cavalry Archer'),
    (Id: riThumbRing;           Name: 'Thumb Ring'),
    (Id: riParthianTactics;     Name: 'Parthian Tactics'),
    // stable
    (Id: riLightCavalry;        Name: 'Light Cavalry'),
    (Id: riHussar;              Name: 'Hussar'),
    (Id: riCavalier;            Name: 'Cavalier'),
    (Id: riPaladin;             Name: 'Paladin'),
    (Id: riHeavyCamel;          Name: 'Heavy Camel'),
    (Id: riBloodlines;          Name: 'Bloodlines'),
    (Id: riHusbandry;           Name: 'Husbandry'),
    // siege workshop
    (Id: riOnager;              Name: 'Onager'),
    (Id: riSiegeOnager;         Name: 'Siege Onager'),
    (Id: riCappedRam;           Name: 'Capped Ram'),
    (Id: riSiegeRam;            Name: 'Siege Ram'),
    (Id: riHeavyScorpion;       Name: 'Heavy Scorpion'),
    // dock
    (Id: riWarGalley;           Name: 'War Galley'),
    (Id: riGalleon;             Name: 'Galleon'),
    (Id: riFastFireShip;        Name: 'Fast Fire Ship'),
    (Id: riHeavyDemolitionShip; Name: 'Heavy Demolition Ship'),
    (Id: riCannonGalleon;       Name: 'Cannon Galleon'),
    (Id: riEliteCannonGalleon;  Name: 'Elite Cannon Galleon'),
    (Id: riCareening;           Name: 'Careening'),
    (Id: riDryDock;             Name: 'Dry Dock'),
    (Id: riShipwright;          Name: 'Shipwright'),
    // unique-unit-upgrade
    (Id: riEliteJaguarMan;      Name: 'Elite Jaguar Man'),
    (Id: riEliteCataphract;     Name: 'Elite Cataphract'),
    (Id: riEliteWoadRaider;     Name: 'Elite Woad Raider'),
    (Id: riEliteChuKoNu;        Name: 'Elite Chu-Ko-Nu'),
    (Id: riEliteLongbowman;     Name: 'Elite Longbowman'),
    (Id: riEliteThrowingAxeman; Name: 'Elite Throwing Axeman'),
    (Id: riEliteHuskarl;        Name: 'Elite Huskarl'),
    (Id: riEliteTarkan;         Name: 'Elite Tarkan'),
    (Id: riEliteSamurai;        Name: 'Elite Samurai'),
    (Id: riEliteWarWagon;       Name: 'Elite War Wagon'),
    (Id: riEliteTurtleShip;     Name: 'Elite Turtle Ship'),
    (Id: riElitePlumedArcher;   Name: 'Elite Plumed Archer'),
    (Id: riEliteMangudai;       Name: 'Elite Mangudai'),
    (Id: riEliteWarElephant;    Name: 'Elite War Elephant'),
    (Id: riEliteMameluke;       Name: 'Elite Mameluke'),
    (Id: riEliteConquistador;   Name: 'Elite Conquistador'),
    (Id: riEliteTeutonicKnight; Name: 'Elite Teutonic Knight'),
    (Id: riEliteJanissary;      Name: 'Elite Janissary'),
    (Id: riEliteBerserk;        Name: 'Elite Berserk'),
    (Id: riEliteLongboat;       Name: 'Elite Longboat'),
    // unique-research
    (Id: riGarlandWars;         Name: 'Garland Wars'),
    (Id: riLogistica;           Name: 'Logistica'),
    (Id: riFurorCeltica;        Name: 'Furor Celtica'),
    (Id: riRocketry;            Name: 'Rocketry'),
    (Id: riYeomen;              Name: 'Yeomen'),
    (Id: riBeardedAxe;          Name: 'Bearded Axe'),
    (Id: riAnarchy;             Name: 'Anarchy'),
    (Id: riPerfusion;           Name: 'Perfusion'),
    (Id: riAtheism;             Name: 'Atheism'),
    (Id: riKataparuto;          Name: 'Kataparuto'),
    (Id: riShinkichon;          Name: 'Shinkichon'),
    (Id: riElDorado;            Name: 'El Dorado'),
    (Id: riDrill;               Name: 'Drill'),
    (Id: riMahouts;             Name: 'Mahouts'),
    (Id: riZealotry;            Name: 'Zealotry'),
    (Id: riSupremacy;           Name: 'Supremacy'),
    (Id: riCrenellations;       Name: 'Crenellations'),
    (Id: riArtillery;           Name: 'Artillery'),
    (Id: riBerserkergang;       Name: 'Berserkergang'),
    // AoFE
    (Id: riHuntingDogs;         Name: 'Hunting Dogs'),
    (Id: riImperialCamel;       Name: 'Imperial Camel'),
    (Id: riCouriers;            Name: 'Couriers'),
    (Id: riAndeanSling;         Name: 'Andean Sling'),
    (Id: riRecurveBow;          Name: 'Recurve Bow'),
    (Id: riMercenaries;         Name: 'Mercenaries'),
    (Id: riDruzhina;            Name: 'Druzhina'),
    (Id: riOrthodoxy;           Name: 'Orthodoxy'),
    (Id: riShatagni;            Name: 'Shatagni'),
    (Id: riSultans;             Name: 'Sultans'),
    (Id: riSilkRoad;            Name: 'Silk Road'),
    (Id: riPavise;              Name: 'Pavise'),
    (Id: riChivalry;            Name: 'Chivalry'),
    (Id: riInquisition;         Name: 'Inquisition'),
    (Id: riSipahi;              Name: 'Sipahi'),
    (Id: riMadrasah;            Name: 'Madrasah'),
    (Id: riIronclad;            Name: 'Ironclad'),
    (Id: riBoilingOil;          Name: 'Boiling Oil'),
    (Id: riNomads;              Name: 'Nomads'),
    (Id: riPanokseon;           Name: 'Panokseon'),
    (Id: riTlatoani;            Name: 'Tlatoani'),
    (Id: riMarauders;           Name: 'Marauders'),
    (Id: riStronghold;          Name: 'Stronghold'),
    (Id: riGreekFire;           Name: 'Greek Fire'),
    (Id: riChieftains;          Name: 'Chieftains'),
    (Id: riGreatWall;           Name: 'Great Wall'),
    (Id: riWarwolf;             Name: 'Warwolf'),
    (Id: riAtlatl;              Name: 'Atlatl'),
    (Id: riEagleWarrior;        Name: 'Eagle Warrior'),
    (Id: riGillnets;            Name: 'Gillnets'),
    (Id: riEliteKamayuk;        Name: 'Elite Kamayuk'),
    (Id: riEliteBoyar;          Name: 'Elite Boyar'),
    (Id: riEliteElephantArcher; Name: 'Elite Elephant Archer'),
    (Id: riEliteMagyarHuszar;   Name: 'Elite Magyar Huszar'),
    (Id: riEliteGenoeseCrossbowman; Name: 'Elite Genoese Crossbowman'));

  UNITS_NUM = 96 + 17;
  UNITS: array[0..UNITS_NUM - 1] of TUnitRec = (
    (Id: uiArcher;              Name: 'Archer'),
    (Id: uiHandCannoneer;       Name: 'Hand Cannoneer'),
    (Id: uiEliteSkirmisher;     Name: 'Elite Skirmisher'),
    (Id: uiSkirmisher;          Name: 'Skirmisher'),
    (Id: uiLongbowman;          Name: 'Longbowman'),
    (Id: uiMangudai;            Name: 'Mangudai'),
    (Id: uiFishingShip;         Name: 'Fishing Ship'),
    (Id: uiTradeCog;            Name: 'Trade Cog'),
    (Id: uiWarGalley;           Name: 'War Galley'),
    (Id: uiCrossbowman;         Name: 'Crossbowman'),
    (Id: uiTeutonicKnight;      Name: 'Teutonic Knight'),
    (Id: uiBatteringRam;        Name: 'Battering Ram'),
    (Id: uiBombardCannon;       Name: 'Bombard Cannon'),
    (Id: uiKnight;              Name: 'Knight'),
    (Id: uiCavalryArcher;       Name: 'Cavalry Archer'),
    (Id: uiCataphract;          Name: 'Cataphract'),
    (Id: uiHuskarl;             Name: 'Huskarl'),
//    (Id: uiTrebuchetUnpacked;   Name: 'Trebuchet (Unpacked)'),
    (Id: uiJanissary;           Name: 'Janissary'),
    (Id: uiChuKoNu;             Name: 'Chu Ko Nu'),
    (Id: uiMilitia;             Name: 'Militia'),
    (Id: uiManAtArms;           Name: 'Man At Arms'),
    (Id: uiHeavySwordsman;      Name: 'Heavy Swordsman'),
    (Id: uiLongSwordsman;       Name: 'Long Swordsman'),
    (Id: uiVillager;            Name: 'Villager'),
    (Id: uiSpearman;            Name: 'Spearman'),
    (Id: uiMonk;                Name: 'Monk'),
//    (Id: uiTradeCartEmpty;      Name: 'Trade Cart, Empty'),
    (Id: uiTradeCart;           Name: 'Trade Cart'),
//    (Id: uiTradeCartFull;       Name: 'Trade Cart, Full'),
    (Id: uiWoadRaider;          Name: 'Woad Raider'),
    (Id: uiWarElephant;         Name: 'War Elephant'),
    (Id: uiLongboat;            Name: 'Longboat'),
    (Id: uiScorpion;            Name: 'Scorpion'),
    (Id: uiMangonel;            Name: 'Mangonel'),
    (Id: uiThrowingAxeman;      Name: 'Throwing Axeman'),
    (Id: uiMameluke;            Name: 'Mameluke'),
    (Id: uiCavalier;            Name: 'Cavalier'),
//    (Id: uiMonkWithRelic;       Name: 'Monk With Relic'),
    (Id: uiSamurai;             Name: 'Samurai'),
    (Id: uiCamel;               Name: 'Camel'),
    (Id: uiHeavyCamel;          Name: 'Heavy Camel'),
//    (Id: uiTrebuchetPacked;     Name: 'Trebuchet, P'),
    (Id: uiTrebuchet;           Name: 'Trebuchet'),
    (Id: uiPikeman;             Name: 'Pikeman'),
    (Id: uiHalberdier;          Name: 'Halberdier'),
    (Id: uiCannonGalleon;       Name: 'Cannon Galleon'),
    (Id: uiCappedRam;           Name: 'Capped Ram'),
    (Id: uiKing;                Name: 'King'),
    (Id: uiPetard;              Name: 'Petard'),
    (Id: uiHussar;              Name: 'Hussar'),
    (Id: uiGalleon;             Name: 'Galleon'),
    (Id: uiScoutCavalry;        Name: 'Scout Cavalry'),
    (Id: uiTwoHandedSwordsman;  Name: 'Two Handed Swordsman'),
    (Id: uiHeavyCavalryArcher;  Name: 'Heavy Cavalry Archer'),
    (Id: uiArbalest;            Name: 'Arbalest'),
//    (Id: uiAdvHeavyCrossbowman; Name: 'Adv Heavy Crossbowman'),
    (Id: uiDemolitionShip;      Name: 'Demolition Ship'),
    (Id: uiHeavyDemolitionShip; Name: 'Heavy Demolition Ship'),
    (Id: uiFireShip;            Name: 'Fire Ship'),
    (Id: uiEliteLongbowman;     Name: 'Elite Longbowman'),
    (Id: uiEliteThrowingAxeman; Name: 'Elite Throwing Axeman'),
    (Id: uiFastFireShip;        Name: 'Fast Fire Ship'),
    (Id: uiEliteLongboat;       Name: 'Elite Longboat'),
    (Id: uiEliteWoadRaider;     Name: 'Elite Woad Raider'),
    (Id: uiGalley;              Name: 'Galley'),
    (Id: uiHeavyScorpion;       Name: 'Heavy Scorpion'),
    (Id: uiTransportShip;       Name: 'Transport Ship'),
    (Id: uiLightCavalry;        Name: 'Light Cavalry'),
    (Id: uiSiegeRam;            Name: 'Siege Ram'),
    (Id: uiOnager;              Name: 'Onager'),
    (Id: uiEliteCataphract;     Name: 'Elite Cataphract'),
    (Id: uiEliteTeutonicKnight; Name: 'Elite Teutonic Knight'),
    (Id: uiEliteHuskarl;        Name: 'Elite Huskarl'),
    (Id: uiEliteMameluke;       Name: 'Elite Mameluke'),
    (Id: uiEliteJanissary;      Name: 'Elite Janissary'),
    (Id: uiEliteWarElephant;    Name: 'Elite War Elephant'),
    (Id: uiEliteChuKoNu;        Name: 'Elite Chu Ko Nu'),
    (Id: uiEliteSamurai;        Name: 'Elite Samurai'),
    (Id: uiEliteMangudai;       Name: 'Elite Mangudai'),
    (Id: uiChampion;            Name: 'Champion'),
    (Id: uiPaladin;             Name: 'Paladin'),
    (Id: uiSiegeOnager;         Name: 'Siege Onager'),
    (Id: uiBerserk;             Name: 'Berserk'),
    (Id: uiEliteBerserk;        Name: 'Elite Berserk'),
    (Id: uiJaguarWarrior;       Name: 'Jaguar Warrior'),
    (Id: uiEliteJaguarWarrior;  Name: 'Elite Jaguar Warrior'),
//    (Id: uiCobraCar;            Name: 'Cobra Car'),
    (Id: uiEagleWarrior;        Name: 'Eagle Warrior'),
    (Id: uiEliteEagleWarrior;   Name: 'Elite Eagle Warrior'),
    (Id: uiTarkan;              Name: 'Tarkan'),
    (Id: uiEliteTarkan;         Name: 'Elite Tarkan'),
    (Id: uiHuskarl2;            Name: 'Huskarl'),
    (Id: uiEliteHuskarl2;       Name: 'Elite Huskarl'),
    (Id: uiPlumedArcher;        Name: 'Plumed Archer'),
    (Id: uiElitePlumedArcher;   Name: 'Elite Plumed Archer'),
    (Id: uiConquistador;        Name: 'Conquistador'),
    (Id: uiEliteConquistador;   Name: 'Elite Conquistador'),
    (Id: uiMissionary;          Name: 'Missionary'),
//    (Id: uiJaguar;              Name: 'Jaguar'),
    (Id: uiWarWagon;            Name: 'War Wagon'),
    (Id: uiEliteWarWagon;       Name: 'Elite War Wagon'),
    (Id: uiTurtleShip;          Name: 'Turtle Ship'),
    (Id: uiEliteTurtleShip;     Name: 'Elite Turtle Ship'),
    // AoFE
    (Id: uiGenoeseCrossbowman;      Name: 'Genoese Crossbowman'),
    (Id: uiEliteGenoeseCrossbowman; Name: 'Elite Genoese Crossbowman'),
    (Id: uiTarkan2;                 Name: 'Tarkan'),
    (Id: uiEliteTarkan2;            Name: 'Elite Tarkan'),
    (Id: uiCondottiero;             Name: 'Condottiero'),
    (Id: uiCondottiero2;            Name: 'Condottiero'),
    (Id: uiKamayuk;                 Name: 'Kamayuk'),
    (Id: uiEliteKamayuk;            Name: 'Elite Kamayuk'),
    (Id: uiBoyar;                   Name: 'Boyar'),
    (Id: uiEliteBoyar;              Name: 'Elite Boyar'),
    (Id: uiElephantArcher;          Name: 'Elephant Archer'),
    (Id: uiEliteElephantArcher;     Name: 'Elite Elephant Archer'),
    (Id: uiMagyarHuszar;            Name: 'Magyar Huszar'),
    (Id: uiEliteMagyarHuszar;       Name: 'Elite Magyar Huszar'),
    (Id: uiEagleWarrior2;           Name: 'Eagle Warrior'),
    (Id: uiImperialCamel;           Name: 'Imperial Camel'),
    (Id: uiSlinger;                 Name: 'Slinger'));

  BUILDINGS_NUM = 31 + 4;
  BUILDINGS: array[0..BUILDINGS_NUM - 1] of TBuildingRec = (
    (Id: biBarracks;      Name: 'Barracks'),
    (Id: biDock;          Name: 'Dock'),
    (Id: biSiegeWorkshop; Name: 'Siege Workshop'),
    (Id: biFarm;          Name: 'Farm'),
    (Id: biMill;          Name: 'Mill'),
    (Id: biHouse;         Name: 'House'),
    (Id: biWallPalisade;  Name: 'Wall, Palisade'),
    (Id: biWatchTower;    Name: 'Watch Tower'),
    (Id: biCastle;        Name: 'Castle'),
    (Id: biMarket;        Name: 'Market'),
    (Id: biArcheryRange;  Name: 'Archery Range'),
    (Id: biStable;        Name: 'Stable'),
    (Id: biBlacksmith;    Name: 'Blacksmith'),
    (Id: biMonastery;     Name: 'Monastery'),
    (Id: biTownCenter;    Name: 'Town Center'),
    (Id: biWallStone;     Name: 'Wall, Stone'),
    (Id: biWallFortified; Name: 'Wall, Fortified'),
    (Id: biFishTrap;      Name: 'Fish Trap'),
    (Id: biUniversity;    Name: 'University'),
    (Id: biGuardTower;    Name: 'Guard Tower'),
    (Id: biKeep;          Name: 'Keep'),
    (Id: biBombardTower;  Name: 'Bombard Tower'),
    (Id: biWonder;        Name: 'Wonder'),
    (Id: biGate;          Name: 'Gate'),
    (Id: biGate2;         Name: 'Gate'),
    (Id: biLumberCamp;    Name: 'Lumber Camp'),
    (Id: biMiningCamp;    Name: 'Mining Camp'),
    (Id: biOutpost;       Name: 'Outpost'),
    (Id: biTownCenter2;   Name: 'Town Center'),
    (Id: biGate3;         Name: 'Gate'),
    (Id: biGate4;         Name: 'Gate'),
    (Id: biPalisadeGate;  Name: 'Palisade Gate'),
    (Id: biPalisadeGate2; Name: 'Palisade Gate'),
    (Id: biPalisadeGate3; Name: 'Palisade Gate'),
    (Id: biPalisadeGate4; Name: 'Palisade Gate'));

{ localized map names (translations) }

  LANGUAGES_NUM = 5;
  LOC_MAP_NAMES: array[0..LANGUAGES_NUM - 1] of array[0..MAPS_NUM - 1] of AnsiString = (
    // english
    ('Arabia', 'Archipelago', 'Baltic', 'Black Forest', 'Coastal', 'Continental',
     'Crater Lake', 'Fortress', 'Gold Rush', 'Highland', 'Islands', 'Mediterranean',
     'Migration', 'Rivers', 'Team Islands', 'Random', 'Scandinavia', 'Mongolia',
     'Yucatan', 'Salt Marsh', 'Arena', 'King of the Hill', 'Oasis', 'Ghost Lake',
     'Nomad', 'Iberia', 'Britain', 'Mideast', 'Texas', 'Italy', 'Central America',
     'France', 'Norse Lands', 'Sea of Japan (East Sea)', 'Byzantinum', 'Custom', 'Blind Random'),

    // spanish (european) provided by dauro ibero
    ('Arabia', 'ArchipiÈlago', 'B·ltico', 'Selva negra', 'Costa', 'Continental',
     'Lago de cr·ter', 'Fortaleza', 'Fiebre del oro', 'Montanas', 'Islas', 'Mediterr·neo',
     'MigraciÛn', 'RÌos', 'Islas de equipo', 'Aleatorio', 'Escandinavia', 'Mongolia',
     'Yucat·n', 'Marisma', 'Arena', 'Rey de la colina', 'Oasis', 'Lago fantasma',
     'NÛmada', 'Iberia', 'Inglaterra', 'Oriente Medio', 'Texas', 'Italia', 'AmÈrica Central',
     'Francia', 'Tierras NÛrdicas', 'Mar del JapÛn', 'Bizancio', 'Personalizado', 'Aleatorio a ciegas'),

    // czech from aoe.cz (missing Blind Random)
    ('Arabie', 'Clenity Teren', 'Primori', 'Cerny Les', 'Pobrezi', 'Kontinent',
     'Jezero', 'Les', 'Zlata Horecka', 'Pohori', 'Ostrovy', 'Stredomori',
     'Stehovani', 'Reky', 'Spojene Ostrovy', 'Nahodne', 'Scandinavie', 'Mongolsko',
     'Yucatan', 'Salt Marsh', 'Arena', 'Kr·l Vrchu', 'Oasa', 'Jezero duchu',
     'KoËovnÌk', 'Iberia', 'Anglie', 'Mideast', 'Texas', 'Italie', 'St¯ednÌ Amerika',
     'Francie', 'SeverskÈ zemÏ', 'JaponskÈ mo¯e (V˝chodnÌ mo¯e)', 'Byzantium', 'Voliteln˝', ''),

    // polish from empires2.net (missing Blind Random)
    ('Pustynia', 'Archipelag', 'Ba≥tyk', 'Czarny las', 'Wybrzeøe', 'Kontynent',
     'Krater-Jezioro', 'Zamek', 'Gorπczka Z≥ota', 'Szczyt', 'Wyspy', 'Morze årÛdziemne',
     'Migracja', 'Rzeki', 'Wielkie wyspy', 'Losowa', 'Skandynawia', 'Mongolia',
     'Jukatan', 'S≥one bagna', 'Arena', 'KrÛl GÛry', 'Oaza', 'Jezioro z widmami',
     'Koczownik', 'Iberia', 'Brytania', 'Bliski WschÛd', 'Teksas', 'W≥ochy', 'årodkowa Ameryka',
     'Francja', 'Ziemie PÛ≥nocne', 'Morze JapoÒskie (Morze Wschodnie)', 'Bizancjum', 'W≥asny', ''),

    // slovak from slovenciny.com provided by Maximus
    ('Ar·bia', 'S˙ostrovie', 'More', 'Temn˝ les', 'Pobreûie', 'Kontinent',
     'Jazero', 'Pevnosti', 'Zlat· hor˙Ëka', 'Pohorie', 'Ostrovy', 'Stredomorie',
     'Sùahovanie', 'Rieky', 'SpojeneckÈ ostrovy', 'N·hodn·', 'äkandin·via', 'Mongolsko',
     'Yucatan', 'Solisko', 'ArÈna', 'Kr·æ vrchu', 'O·za', 'Jazero duchov',
     'KoËovanie', 'Pyreneje', 'Brit·nia', 'Stredn˝ v˝chod', 'Texas', 'Taliansko', 'Stredn· Amerika',
     'Franc˙zsko', 'äkandin·via', 'JaponskÈ more', 'Byzancia', 'Voliteæn˝', '⁄plne n·hodn·')
    );

implementation

uses
  Math, GDIPAPI, uRecAnalyst;

function InArray(const Ary: array of Integer; Value: Integer): Integer; forward;

function SizeAfterRotation(const Angle: Extended; const Width, Height: Integer): TSize;
var
  fRadians: Extended;
  fCosine, fSine: Double;
  fPoint1x, fPoint1y, fPoint2x, fPoint2y, fPoint3x, fPoint3y: Double;
  fMinx, fMiny, fMaxx, fMaxy: Double;
begin
  { Convert degrees to radians }
  fRadians := (2 * Pi * Angle) / 360;

  fCosine := Abs(Cos(fRadians));
  fSine := Abs(Sin(fRadians));

  fPoint1x := (-Height * fSine);
  fPoint1y := (Height * fCosine);
  fPoint2x := (Width * fCosine - Height * fSine);
  fPoint2y := (Height * fCosine + Width * fSine);
  fPoint3x := (Width * fCosine);
  fPoint3y := (Width * fSine);

  fMinx := Min(0, Min(fPoint1x, Min(fPoint2x, fPoint3x)));
  fMiny := Min(0, Min(fPoint1y, Min(fPoint2y, fPoint3y)));
  fMaxx := Max(fPoint1x, Max(fPoint2x, fPoint3x));
  fMaxy := Max(fPoint1y, Max(fPoint2y, fPoint3y));

  Result.cx := Ceil(fMaxx - fMinx);
  Result.cy := Ceil(fMaxy - fMiny);
end;

{$IFDEF FPC}
function ZDecompressStream2(inStream, outStream: TStream; windowBits: Integer): Integer;
const
  bufferSize = 32768;
var
  zstream: TZStream;
  zresult: Integer;
  inBuffer: array [0..bufferSize - 1] of Byte;
  outBuffer: array [0..bufferSize - 1] of Byte;
  outSize: Integer;
begin
  Result := Z_OK;
  FillChar(zstream, SizeOf(zstream), 0);

  zresult := InflateInit2(zstream, windowBits);
  if (zresult < 0) then
  begin
    Result := zresult;
    Exit;
  end;

  zresult := Z_STREAM_END;

  zstream.avail_in := inStream.Read(inBuffer, bufferSize);

  while (zstream.avail_in > 0) do
  begin
    zstream.next_in := inBuffer;

    repeat
      zstream.next_out := outBuffer;
      zstream.avail_out := bufferSize;

      zresult := inflate(zstream, Z_NO_FLUSH);
      if (zresult < 0) then
      begin
        Result := zresult;
        Exit;
      end;

      outSize := bufferSize - zstream.avail_out;

      outStream.Write(outBuffer, outSize);
    until (zresult = Z_STREAM_END) or (zstream.avail_in = 0);

    if (zresult <> Z_STREAM_END) then
      zstream.avail_in := inStream.Read(inBuffer, bufferSize)
    else if (zstream.avail_in > 0) then
    begin
      inStream.Position := inStream.Position - zstream.avail_in;
      zstream.avail_in := 0;
    end;
  end;

  while (zresult <> Z_STREAM_END) do
  begin
    zstream.next_out := outBuffer;
    zstream.avail_out := bufferSize;

    zresult := inflate (zstream, Z_FINISH);
    if (zresult < 0) then
    begin
      { TODO: check why this sometimes flushes an error for fpc }
      //Result := zresult;
      Result := Z_OK;
      Exit;
    end;

    outSize := bufferSize - zstream.avail_out;

    outStream.Write(outBuffer, outSize);
  end;

  zresult := inflateEnd(zstream);
  if (zresult < 0) then
  begin
    Result := zresult;
    Exit;
  end;
end;

function ZCompressStream2(inStream, outStream: TStream; level, windowBits,
  memLevel, strategy : Longint): Integer;
const
  bufferSize = 32768;
var
  zstream: TZStream;
  zresult: Integer;
  inBuffer: array [0..bufferSize - 1] of Byte;
  outBuffer: array [0..bufferSize - 1] of Byte;
  outSize: Integer;
begin
  Result := Z_OK;
  FillChar(zstream, SizeOf(zstream), 0);

  zresult := DeflateInit2(zstream, level, Z_DEFLATED, windowBits, memLevel, strategy);
  if (zresult < 0) then
  begin
    Result := zresult;
    Exit;
  end;

  zresult := Z_STREAM_END;

  zstream.avail_in := inStream.Read(inBuffer, bufferSize);

  while (zstream.avail_in > 0) do
  begin
    zstream.next_in := inBuffer;

    repeat
      zstream.next_out := outBuffer;
      zstream.avail_out := bufferSize;

      zresult := deflate(zstream, Z_NO_FLUSH);
      if (zresult < 0) then
      begin
        Result := zresult;
        Exit;
      end;

      outSize := bufferSize - zstream.avail_out;

      outStream.Write(outBuffer, outSize);
    until (zresult = Z_STREAM_END) or (zstream.avail_in = 0);

    zstream.avail_in := inStream.Read(inBuffer, bufferSize);
  end;

  while (zresult <> Z_STREAM_END) do
  begin
    zstream.next_out := outBuffer;
    zstream.avail_out := bufferSize;

    zresult := deflate(zstream, Z_FINISH);
    if (zresult < 0) then
    begin
      { TODO: check, ci robi to iste ako pri inflate }
      //Result := zresult;
      Result := Z_OK;
      Exit;
    end;

    outSize := bufferSize - zstream.avail_out;

    outStream.Write(outBuffer, outSize);
  end;

  zresult := inflateEnd(zstream);
  if (zresult < 0) then
  begin
    Result := zresult;
    Exit;
  end;
end;
{$ENDIF}

procedure InitializeGdiplus(var gdiplusToken: ULONG);
var
  StartupInput: TGDIPlusStartupInput;
begin
  StartupInput.DebugEventCallback := nil;
  StartupInput.SuppressBackgroundThread := False;
  StartupInput.SuppressExternalCodecs := False;
  StartupInput.GdiplusVersion := 1;
  // Initialize GDI+
  GdiplusStartup(gdiplusToken, @StartupInput, nil);
end;

procedure FinalizeGdiplus(var gdiplusToken: ULONG);
begin
  // Close GDI +
  GdiplusShutdown(gdiplusToken);
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

function MapIdToName(const MapId: Integer): String;
var
  i: Integer;
begin
  i := MapById(MapId);
  if (i <> -1) then
    Result := MAPS[i].Name
  else
    Result := '';
end;

function BuildingIdToName(const BuildingId: Integer): AnsiString;
var
  i: Integer;
begin
  i := BuildingById(BuildingId);
  if (i <> -1) then
    Result := BUILDINGS[i].Name
  else
    Result := '';
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

procedure FixUnitTypeId(var unit_type_id: Word);
begin
  if (unit_type_id = uiHuskarl2) then unit_type_id := uiHuskarl
  else if (unit_type_id = uiEliteHuskarl2) then unit_type_id := uiEliteHuskarl
  else if (unit_type_id = uiTarkan2) then unit_type_id := uiTarkan
  else if (unit_type_id = uiEliteTarkan2) then unit_type_id := uiEliteTarkan
  else if (unit_type_id = uiCondottiero2) then unit_type_id := uiCondottiero
  else if (unit_type_id = uiEagleWarrior2) then unit_type_id := uiEagleWarrior;
end;

procedure FixBuildingTypeId(var building_type_id: Word);
begin
  if (InArray(biGates, building_type_id) <> -1) then
    building_type_id := biGate
  else if (InArray(biPalisadeGates, building_type_id) <> -1) then
    building_type_id := biPalisadeGate;
end;

end.

