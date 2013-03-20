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
unit MemStream;

interface

uses
  Classes, uRecAnalystBase;

type
  TMemStream = class(TMemoryStream)
  private
    function Find(Needle: Pointer; SizeOfNeedle: Integer): Longint; overload;
  public
    function Seek(Offset: Longint): Longint; overload;
    procedure ReadInt32(var Buffer: Int32);
    procedure ReadWord(var Buffer: Word);
    procedure ReadChar(var Buffer: Byte);
    procedure ReadFloat(var Buffer: Single);
    procedure ReadBool(var Buffer: Boolean);
    procedure ReadString(var Buffer: array of AnsiChar; Len: Integer = 4);
    function Find(const Needle256: array of AnsiChar): Longint; overload;
    function Find(const Needle256: array of Byte): Longint; overload;
    function FindReverse(const Needle256: array of AnsiChar): Longint;
  end;

implementation

uses
  Windows, SysUtils;

function TMemStream.Seek(Offset: Longint): Longint;
begin
  Result := Seek(Offset, soFromCurrent);
end;

procedure TMemStream.ReadInt32(var Buffer: Int32);
begin
  ReadBuffer(Buffer, SizeOf(Int32));
end;

procedure TMemStream.ReadWord(var Buffer: Word);
begin
  ReadBuffer(Buffer, SizeOf(Word));
end;

procedure TMemStream.ReadChar(var Buffer: Byte);
begin
  ReadBuffer(Buffer, SizeOf(Byte));
end;

procedure TMemStream.ReadFloat(var Buffer: Single);
begin
  ReadBuffer(Buffer, SizeOf(Single));
end;

procedure TMemStream.ReadBool(var Buffer: Boolean);
var
  Bool: Int32;
begin
  ReadInt32(Bool);
  Buffer := (Bool <> 0);
end;

procedure TMemStream.ReadString(var Buffer: array of AnsiChar; Len: Integer = 4);
var
  iLen: Int32;
  wLen: Word;
begin
  case Len of
    2: begin
      ReadWord(wLen);
      ReadBuffer(Buffer, wLen);
      Buffer[wLen] := #0;
    end;
    4: begin
      ReadInt32(iLen);
      ReadBuffer(Buffer, iLen);
      Buffer[iLen] := #0;
    end;
  end;
end;

function TMemStream.Find(Needle: Pointer; SizeOfNeedle: Integer): Longint;
var
  buff256: array[0..MAXBYTE] of AnsiChar;
begin
  Result := -1;
  repeat
    ReadBuffer(buff256, SizeOfNeedle);
    if CompareMem(@buff256, Needle, SizeOfNeedle) then
    begin
      Result := Position;
      Break;
    end;
    Seek(-SizeOfNeedle + 1);
  until (Position >= Size);
end;

function TMemStream.Find(const Needle256: array of AnsiChar): Longint;
begin
  Result := Find(@Needle256, SizeOf(Needle256));
end;

function TMemStream.Find(const Needle256: array of Byte): Longint;
begin
  Result := Find(@Needle256, SizeOf(Needle256));
end;

function TMemStream.FindReverse(const Needle256: array of AnsiChar): Longint;
var
  buff256: array[0..MAXBYTE] of AnsiChar;
  needle_size: Integer;
begin
  Result := -1;
  needle_size := SizeOf(Needle256);
  repeat
    ReadBuffer(buff256, needle_size);
    if CompareMem(@buff256, @Needle256, needle_size) then
    begin
      Result := Position;
      Break;
    end;
    Seek(-(needle_size + 1));
  until (Position < 0);
end;

end.

