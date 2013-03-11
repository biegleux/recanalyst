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
unit uRecAnalystBase;

{$IFDEF FPC}
  {$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Types;

// Types
{$IFNDEF FPC}
type
  Int32 = Longint;
{$ENDIF}

// Routines
function StrLCopyA(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Integer): PAnsiChar;
function StrPCopyA(Dest: PAnsiChar; const Source: AnsiString): PAnsiChar;
procedure CpyMem(var Dest: array of AnsiChar; const Source: AnsiString);
function SizeAfterRotation(const Angle: Extended; const Width, Height: Integer): TSize;
{$IFDEF FPC}
function ZDecompressStream2(inStream, outStream: TStream; windowBits: Integer): Integer;
function ZCompressStream2(inStream, outStream: TStream; level, windowBits,
    memLevel, strategy : Longint): Integer;
{$ENDIF}
// see Remarks at http://msdn.microsoft.com/en-us/library/windows/desktop/ms534077%28v=vs.85%29.aspx
procedure InitializeGdiplus;
procedure FinalizeGdiplus;

implementation

uses
  Windows, Math, GDIPAPI;

var
  gdiplusToken: ULONG;

function StrLCopyA(Dest: PAnsiChar; const Source: PAnsiChar; MaxLen: Integer): PAnsiChar;
begin
  if (lstrlenA(Source) >= MaxLen) then
  begin
    lstrcpynA(Dest, Source, MaxLen);
    Dest[MaxLen - 1] := #0;
  end else
    lstrcpyA(Dest, Source);
  Result := Dest;
end;

function StrPCopyA(Dest: PAnsiChar; const Source: AnsiString): PAnsiChar;
begin
  Result := StrLCopyA(Dest, PAnsiChar(Source), Length(Source));
end;

procedure ZeroMem(var Dest: array of AnsiChar);
begin
  FillChar(Dest, SizeOf(Dest), #0);
end;

procedure CpyMem(var Dest: array of AnsiChar; const Source: AnsiString);
begin
  ZeroMem(Dest);
  CopyMemory(@Dest, PAnsiChar(Source), Min(Length(Source), SizeOf(Dest) - 1));
end;

function SizeAfterRotation(const Angle: Extended; const Width, Height: Integer): TSize;
var
  fRadians: Extended;
  fCosine, fSine: Double;
  fPoint1x, fPoint1y, fPoint2x, fPoint2y, fPoint3x, fPoint3y: Double;
  fMinx, fMiny, fMaxx, fMaxy: Double;
begin
  {Convert degrees to radians}
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

procedure InitializeGdiplus;
var
  StartupInput: TGDIPlusStartupInput;
begin
  StartupInput.DebugEventCallback := nil;
  StartupInput.SuppressBackgroundThread := False;
  StartupInput.SuppressExternalCodecs   := False;
  StartupInput.GdiplusVersion := 1;
  // Initialize GDI+
  GdiplusStartup(gdiplusToken, @StartupInput, nil);
end;

procedure FinalizeGdiplus;
begin
  // Close GDI +
  GdiplusShutdown(gdiplusToken);
end;

end.
