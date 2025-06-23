{==============================================================================
   _____ _          ___                  _ ™
  |_   _(_)_ _ _  _| _ \__ _ ___ __ __ _| |
    | | | | ' \ || |  _/ _` (_-</ _/ _` | |
    |_| |_|_||_\_, |_| \__,_/__/\__\__,_|_|
               |__/ Pascal in your Pocket!

 Copyright © 2025-present tinyBigGAMES™ LLC
 All Rights Reserved.

 https://github.com/tinyBigGAMES/TinyPascal

 BSD 3-Clause License

 Copyright (c) 2025-present, tinyBigGAMES LLC

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 1. Redistributions of source code must retain the above copyright notice, this
    list of conditions and the following disclaimer.

 2. Redistributions in binary form must reproduce the above copyright notice,
    this list of conditions and the following disclaimer in the documentation
    and/or other materials provided with the distribution.

 3. Neither the name of the copyright holder nor the names of its
    contributors may be used to endorse or promote products derived from
    this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
==============================================================================}

unit TinyPascal.Runtime;

{$I TinyPascal.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  TinyPascal.Value;

type
  // Runtime result enumeration
  TRuntimeResult = (
    rtSuccess,     // Function executed successfully
    rtTypeError,   // Invalid type for operation
    rtInputError,  // Input/output error
    rtError        // General runtime error
  );

// VM Runtime functions - simple approach
function Runtime_Print(const AValue: TValue; const AStringPool: TStringList): TRuntimeResult;
function Runtime_PrintLn(const AValue: TValue; const AStringPool: TStringList): TRuntimeResult;
function Runtime_PrintLn_Empty(): TRuntimeResult;
function Runtime_ReadLn(const AStringPool: TStringList): TValue;
function Runtime_IntToStr(const AValue: TValue; const AStringPool: TStringList): TValue;
function Runtime_StrToInt(const AValue: TValue; const AStringPool: TStringList): TValue;
function Runtime_FloatToStr(const AValue: TValue; const AStringPool: TStringList): TValue;
function Runtime_StrToFloat(const AValue: TValue; const AStringPool: TStringList): TValue;
function Runtime_ResultToString(const AResult: TRuntimeResult): string;

// Native X64-callable runtime functions
procedure Runtime_Print_X64(const AValue: Int64); cdecl;
procedure Runtime_PrintLn_X64(const AValue: Int64); cdecl;
procedure Runtime_PrintLn_Empty_X64(); cdecl;
function Runtime_IntToStr_X64(const AValue: Int64): PAnsiChar; cdecl;
function Runtime_StrToInt_X64(const AString: PAnsiChar): Int64; cdecl;
procedure Runtime_PrintString_X64(const AString: PAnsiChar); cdecl;
procedure Runtime_PrintLnString_X64(const AString: PAnsiChar); cdecl;

implementation

{ VM Runtime Functions - Original Implementation }

function Runtime_Print(const AValue: TValue; const AStringPool: TStringList): TRuntimeResult;
var
  LText: string;
begin
  Result := rtSuccess;

  try
    LText := TValueOperations.ValueToDisplayString(AValue, AStringPool);
    System.Write(LText);
  except
    on E: Exception do
      Result := rtError;
  end;
end;

function Runtime_PrintLn(const AValue: TValue; const AStringPool: TStringList): TRuntimeResult;
var
  LText: string;
begin
  Result := rtSuccess;

  try
    LText := TValueOperations.ValueToDisplayString(AValue, AStringPool);
    System.WriteLn(LText);
  except
    on E: Exception do
      Result := rtError;
  end;
end;

function Runtime_PrintLn_Empty(): TRuntimeResult;
begin
  Result := rtSuccess;

  try
    System.WriteLn;
  except
    on E: Exception do
      Result := rtError;
  end;
end;

function Runtime_ReadLn(const AStringPool: TStringList): TValue;
var
  LInput: string;
begin
  try
    System.ReadLn(LInput);
    Result := TValueFactory.CreateFromString(UTF8String(LInput), AStringPool);
  except
    Result := TValue.CreateNone();
  end;
end;

function Runtime_IntToStr(const AValue: TValue; const AStringPool: TStringList): TValue;
begin
  try
    if not AValue.IsInt64() then
    begin
      Result := TValue.CreateNone();
      Exit;
    end;

    Result := TValueOperations.ConvertToString(AValue, AStringPool);
  except
    Result := TValue.CreateNone();
  end;
end;

function Runtime_StrToInt(const AValue: TValue; const AStringPool: TStringList): TValue;
begin
  try
    if not AValue.IsString() then
    begin
      Result := TValue.CreateNone();
      Exit;
    end;

    Result := TValueOperations.ConvertToInt64(AValue, AStringPool);
  except
    Result := TValue.CreateNone();
  end;
end;

function Runtime_FloatToStr(const AValue: TValue; const AStringPool: TStringList): TValue;
begin
  try
    if not AValue.IsFloat64() then
    begin
      Result := TValue.CreateNone();
      Exit;
    end;

    Result := TValueOperations.ConvertToString(AValue, AStringPool);
  except
    Result := TValue.CreateNone();
  end;
end;

function Runtime_StrToFloat(const AValue: TValue; const AStringPool: TStringList): TValue;
begin
  try
    if not AValue.IsString() then
    begin
      Result := TValue.CreateNone();
      Exit;
    end;

    Result := TValueOperations.ConvertToFloat64(AValue, AStringPool);
  except
    Result := TValue.CreateNone();
  end;
end;

function Runtime_ResultToString(const AResult: TRuntimeResult): string;
begin
  case AResult of
    rtSuccess: Result := 'Success';
    rtTypeError: Result := 'Type error';
    rtInputError: Result := 'Input/output error';
    rtError: Result := 'Runtime error';
  else
    Result := 'Unknown result';
  end;
end;

{ Native X64 Runtime Functions }

procedure Runtime_Print_X64(const AValue: Int64); cdecl;
begin
  try
    System.Write(IntToStr(AValue));
  except
    // Ignore errors in native calls
  end;
end;

procedure Runtime_PrintLn_X64(const AValue: Int64); cdecl;
begin
  try
    System.WriteLn(IntToStr(AValue));
  except
    // Ignore errors in native calls
  end;
end;

procedure Runtime_PrintLn_Empty_X64(); cdecl;
begin
  try
    System.WriteLn;
  except
    // Ignore errors in native calls
  end;
end;

procedure Runtime_PrintString_X64(const AString: PAnsiChar); cdecl;
begin
  try
    if Assigned(AString) then
      System.Write(string(AnsiString(AString)));
  except
    // Ignore errors in native calls
  end;
end;

procedure Runtime_PrintLnString_X64(const AString: PAnsiChar); cdecl;
begin
  try
    if Assigned(AString) then
      System.WriteLn(string(AnsiString(AString)));
  except
    // Ignore errors in native calls
  end;
end;

var
  LTempStringBuffer: array[0..255] of AnsiChar;

function Runtime_IntToStr_X64(const AValue: Int64): PAnsiChar; cdecl;
var
  LStr: AnsiString;
begin
  try
    LStr := AnsiString(IntToStr(AValue));
    if Length(LStr) < Length(LTempStringBuffer) then
    begin
      StrPCopy(LTempStringBuffer, LStr);
      Result := @LTempStringBuffer[0];
    end
    else
      Result := nil;
  except
    Result := nil;
  end;
end;

function Runtime_StrToInt_X64(const AString: PAnsiChar): Int64; cdecl;
begin
  try
    if Assigned(AString) then
      Result := StrToInt64(string(AnsiString(AString)))
    else
      Result := 0;
  except
    Result := 0;
  end;
end;

end.
