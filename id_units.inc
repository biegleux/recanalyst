{*
 * $Id$
 * This file is part of the recanalyst project.
 *
 * Copyright (c) 2009-2013 biegleux <biegleux[at]gmail[dot]com>
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, see <http://www.gnu.org/licenses>.
 *}
{ unit ids }
const
  uiArcher              = 4;
  uiHandCannoneer       = 5;
  uiEliteSkirmisher     = 6;
  uiSkirmisher          = 7;
  uiLongbowman          = 8;
  uiMangudai            = 11;
  uiFishingShip         = 13;
  uiTradeCog            = 17;
  uiWarGalley           = 21;
  uiCrossbowman         = 24;
  uiTeutonicKnight      = 25;
  uiBatteringRam        = 35;
  uiBombardCannon       = 36;
  uiKnight              = 38;
  uiCavalryArcher       = 39;
  uiCataphract          = 40;
  uiHuskarl             = 41;
//  uiTrebuchetUnpacked   = 42;
  uiJanissary           = 46;
  uiChuKoNu             = 73;
  uiMilitia             = 74;
  uiManAtArms           = 75;
  uiHeavySwordsman      = 76;
  uiLongSwordsman       = 77;
  uiVillager            = 83;
  uiSpearman            = 93;
  uiMonk                = 125;
//  uiTradeCartEmpty      = 128;
  uiTradeCart           = 128;
//  uiTradeCartFull       = 204;
  uiWoadRaider          = 232;
  uiWarElephant         = 239;
  uiLongboat            = 250;
  uiScorpion            = 279;
  uiMangonel            = 280;
  uiThrowingAxeman      = 281;
  uiMameluke            = 282;
  uiCavalier            = 283;
//  uiMonkWithRelic       = 286;
  uiSamurai             = 291;
  uiCamel               = 329;
  uiHeavyCamel          = 330;
//  uiTrebuchetPacked     = 331;
  uiTrebuchet           = 331;
  uiPikeman             = 358;
  uiHalberdier          = 359;
  uiCannonGalleon       = 420;
  uiCappedRam           = 422;
  uiKing                = 434;
  uiPetard              = 440;
  uiHussar              = 441;
  uiGalleon             = 442;
  uiScoutCavalry        = 448;
  uiTwoHandedSwordsman  = 473;
  uiHeavyCavalryArcher  = 474;
  uiArbalest            = 492;
//  uiAdvHeavyCrossbowman = 493;
  uiDemolitionShip      = 527;
  uiHeavyDemolitionShip = 528;
  uiFireShip            = 529;
  uiEliteLongbowman     = 530;
  uiEliteThrowingAxeman = 531;
  uiFastFireShip        = 532;
  uiEliteLongboat       = 533;
  uiEliteWoadRaider     = 534;
  uiGalley              = 539;
  uiHeavyScorpion       = 542;
  uiTransportShip       = 545;
  uiLightCavalry        = 546;
  uiSiegeRam            = 548;
  uiOnager              = 550;
  uiEliteCataphract     = 553;
  uiEliteTeutonicKnight = 554;
  uiEliteHuskarl        = 555;
  uiEliteMameluke       = 556;
  uiEliteJanissary      = 557;
  uiEliteWarElephant    = 558;
  uiEliteChuKoNu        = 559;
  uiEliteSamurai        = 560;
  uiEliteMangudai       = 561;
  uiChampion            = 567;
  uiPaladin             = 569;
  uiSiegeOnager         = 588;
  uiBerserk             = 692;
  uiEliteBerserk        = 694;
  uiJaguarWarrior       = 725;
  uiEliteJaguarWarrior  = 726;
//  uiCobraCar            = 748;
  uiEagleWarrior        = 751;
  uiEliteEagleWarrior   = 752;
  uiTarkan              = 755;
  uiEliteTarkan         = 757;
  uiHuskarl2            = 759;
  uiEliteHuskarl2       = 761;
  uiPlumedArcher        = 763;
  uiElitePlumedArcher   = 765;
  uiConquistador        = 771;
  uiEliteConquistador   = 773;
  uiMissionary          = 775;
//  uiJaguar              = 812;
  uiWarWagon            = 827;
  uiEliteWarWagon       = 829;
  uiTurtleShip          = 831;
  uiEliteTurtleShip     = 832;
  // AoFE
  uiGenoeseCrossbowman      = 866;
  uiEliteGenoeseCrossbowman = 868;
  uiTarkan2                 = 886;
  uiEliteTarkan2            = 887;
  uiCondottiero             = 882;
  uiCondottiero2            = 184;
  uiKamayuk                 = 879;
  uiEliteKamayuk            = 881;
  uiBoyar                   = 876;
  uiEliteBoyar              = 878;
  uiElephantArcher          = 873;
  uiEliteElephantArcher     = 875;
  uiMagyarHuszar            = 869;
  uiEliteMagyarHuszar       = 871;
  uiEagleWarrior2           = 753;
  uiImperialCamel           = 207;
  uiSlinger                 = 185;

  uiHuskarls: array[0..1] of Integer = (uiHuskarl, uiHuskarl2);
  uiEliteHuskarls: array[0..1] of Integer = (uiEliteHuskarl, uiEliteHuskarl2);
  uiTarkans: array[0..1] of Integer = (uiTarkan, uiTarkan2);
  uiEliteTarkans: array[0..1] of Integer = (uiEliteTarkan, uiEliteTarkan2);
  uiCondottieros: array[0..1] of Integer = (uiCondottiero, uiCondottiero2);
  uiEagleWarriors: array[0..1] of Integer = (uiEagleWarrior, uiEagleWarrior2);
