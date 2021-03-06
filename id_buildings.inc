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
{ building ids }
const
  biBarracks      = 12;
  biDock          = 45;
  biSiegeWorkshop = 49;
  biFarm          = 50;
//  biGoldMine      = 66;
  biMill          = 68;
  biHouse         = 70;
  biWallPalisade  = 72;
  biWatchTower    = 79;
  biCastle        = 82;
  biMarket        = 84;
  biArcheryRange  = 87;
  biStable        = 101;
//  biStoneMine     = 102;
  biBlacksmith    = 103;
  biMonastery     = 104;
  biTownCenter    = 109;
//  biTradeWorkshop = 110;
  biWallStone     = 117;
  biWallFortified = 155;
  biFishTrap      = 199;
  biUniversity    = 209;
  biGuardTower    = 234;
  biKeep          = 235;
  biBombardTower  = 236;
  biWonder        = 276;
  biGate          = 487;
  biGate2         = 490;
  biLumberCamp    = 562;
  biMiningCamp    = 584;
  biOutpost       = 598;
  biTownCenter2   = 621;
  biGate3         = 665;
  biGate4         = 673;
  // AoFE
  biPalisadeGate  = 792;
  biPalisadeGate2 = 796;
  biPalisadeGate3 = 800;
  biPalisadeGate4 = 804;

  biGates: array[0..3] of Integer = (biGate, biGate2, biGate3, biGate4);
  biPalisadeGates: array[0..3] of Integer = (biPalisadeGate, biPalisadeGate2,
                   biPalisadeGate3, biPalisadeGate4);

