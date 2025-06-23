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

unit TinyPascal.Value;

{$I TinyPascal.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  TinyPascal.Lexer;

type
  // Complete value type enumeration for TinyPascal VM
  TValueType = (
    vtNone,      // Uninitialized value
    vtInt64,     // 64-bit signed integer (Int)
    vtUInt64,    // 64-bit unsigned integer (UInt)
    vtFloat64,   // 64-bit floating point (Float)
    vtString,    // UTF8 string (String/PString)
    vtBoolean    // Boolean value
  );

  // High-performance VM value - NO VARIANTS, direct field access
  TValue = packed record
  private
    LValueType: TValueType;
    LIntValue: Int64;
    LUIntValue: UInt64;
    LFloatValue: Double;
    LStringIndex: Integer;
    LBoolValue: Boolean;

  public
    // Fast constructors
    class function CreateInt64(const AValue: Int64): TValue; static; inline;
    class function CreateUInt64(const AValue: UInt64): TValue; static; inline;
    class function CreateFloat64(const AValue: Double): TValue; static; inline;
    class function CreateString(const AStringIndex: Integer): TValue; static; inline;
    class function CreateBoolean(const AValue: Boolean): TValue; static; inline;
    class function CreateNone(): TValue; static; inline;

    // Fast type checking
    function GetValueType(): TValueType; inline;
    function IsInt64(): Boolean; inline;
    function IsUInt64(): Boolean; inline;
    function IsFloat64(): Boolean; inline;
    function IsString(): Boolean; inline;
    function IsBoolean(): Boolean; inline;
    function IsNone(): Boolean; inline;
    function IsNumeric(): Boolean; inline;
    function IsInteger(): Boolean; inline;

    // Fast value access
    function AsInt64(): Int64; inline;
    function AsUInt64(): UInt64; inline;
    function AsFloat64(): Double; inline;
    function AsStringIndex(): Integer; inline;
    function AsBoolean(): Boolean; inline;

    // Properties for direct access
    property ValueType: TValueType read LValueType;
    property IntValue: Int64 read LIntValue;
    property UIntValue: UInt64 read LUIntValue;
    property FloatValue: Double read LFloatValue;
    property StringIndex: Integer read LStringIndex;
    property BoolValue: Boolean read LBoolValue;
  end;

  // Forward declarations
  TValueFactory = class;
  TValueOperations = class;
  TRuntimeVariableStorage = class;

  // Fast value factory
  TValueFactory = class
  public
    class function CreateInt64(const AValue: Int64): TValue; static; inline;
    class function CreateUInt64(const AValue: UInt64): TValue; static; inline;
    class function CreateFloat64(const AValue: Double): TValue; static; inline;
    class function CreateString(const AStringIndex: Integer): TValue; static; inline;
    class function CreateBoolean(const AValue: Boolean): TValue; static; inline;
    class function CreateFromString(const AString: UTF8String; const AStringPool: TStringList): TValue; static;
    class function CreateFromPascalType(const APascalType: TPascalType; const AValue: UTF8String; const AStringPool: TStringList): TValue; static;
  end;

  // High-performance arithmetic operations
  TValueOperations = class
  public
    // Arithmetic operations
    class function Add(const ALeft: TValue; const ARight: TValue): TValue; static; inline;
    class function Subtract(const ALeft: TValue; const ARight: TValue): TValue; static; inline;
    class function Multiply(const ALeft: TValue; const ARight: TValue): TValue; static; inline;
    class function Divide(const ALeft: TValue; const ARight: TValue): TValue; static; inline;
    class function Modulo(const ALeft: TValue; const ARight: TValue): TValue; static; inline;
    class function Negate(const AValue: TValue): TValue; static; inline;

    // Comparison operations
    class function Equal(const ALeft: TValue; const ARight: TValue; const AStringPool: TStringList): Boolean; static; inline;
    class function NotEqual(const ALeft: TValue; const ARight: TValue; const AStringPool: TStringList): Boolean; static; inline;
    class function LessThan(const ALeft: TValue; const ARight: TValue; const AStringPool: TStringList): Boolean; static; inline;
    class function LessOrEqual(const ALeft: TValue; const ARight: TValue; const AStringPool: TStringList): Boolean; static; inline;
    class function GreaterThan(const ALeft: TValue; const ARight: TValue; const AStringPool: TStringList): Boolean; static; inline;
    class function GreaterOrEqual(const ALeft: TValue; const ARight: TValue; const AStringPool: TStringList): Boolean; static; inline;

    // Type conversions
    class function ConvertToInt64(const AValue: TValue; const AStringPool: TStringList): TValue; static;
    class function ConvertToUInt64(const AValue: TValue; const AStringPool: TStringList): TValue; static;
    class function ConvertToFloat64(const AValue: TValue; const AStringPool: TStringList): TValue; static;
    class function ConvertToString(const AValue: TValue; const AStringPool: TStringList): TValue; static;
    class function ConvertToBoolean(const AValue: TValue; const AStringPool: TStringList): TValue; static;

    // Utility functions
    class function GetTypeName(const AValueType: TValueType): string; static;
    class function ValueToDisplayString(const AValue: TValue; const AStringPool: TStringList): string; static;
  end;

  // Runtime variable storage
  TRuntimeVariableStorage = class
  private
    LVariables: TDictionary<UTF8String, TValue>;

  public
    constructor Create();
    destructor Destroy(); override;

    procedure SetVariable(const AName: UTF8String; const AValue: TValue);
    function GetVariable(const AName: UTF8String): TValue;
    function HasVariable(const AName: UTF8String): Boolean;
    procedure DeleteVariable(const AName: UTF8String);
    procedure Clear();
    function GetVariableCount(): Integer;

    property Variables: TDictionary<UTF8String, TValue> read LVariables;
  end;

implementation

{ TValue }

class function TValue.CreateInt64(const AValue: Int64): TValue;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.LValueType := vtInt64;
  Result.LIntValue := AValue;
end;

class function TValue.CreateUInt64(const AValue: UInt64): TValue;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.LValueType := vtUInt64;
  Result.LUIntValue := AValue;
end;

class function TValue.CreateFloat64(const AValue: Double): TValue;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.LValueType := vtFloat64;
  Result.LFloatValue := AValue;
end;

class function TValue.CreateString(const AStringIndex: Integer): TValue;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.LValueType := vtString;
  Result.LStringIndex := AStringIndex;
end;

class function TValue.CreateBoolean(const AValue: Boolean): TValue;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.LValueType := vtBoolean;
  Result.LBoolValue := AValue;
end;

class function TValue.CreateNone(): TValue;
begin
  FillChar(Result, SizeOf(Result), 0);
  Result.LValueType := vtNone;
end;

function TValue.GetValueType(): TValueType;
begin
  Result := LValueType;
end;

function TValue.IsInt64(): Boolean;
begin
  Result := LValueType = vtInt64;
end;

function TValue.IsUInt64(): Boolean;
begin
  Result := LValueType = vtUInt64;
end;

function TValue.IsFloat64(): Boolean;
begin
  Result := LValueType = vtFloat64;
end;

function TValue.IsString(): Boolean;
begin
  Result := LValueType = vtString;
end;

function TValue.IsBoolean(): Boolean;
begin
  Result := LValueType = vtBoolean;
end;

function TValue.IsNone(): Boolean;
begin
  Result := LValueType = vtNone;
end;

function TValue.IsNumeric(): Boolean;
begin
  Result := LValueType in [vtInt64, vtUInt64, vtFloat64];
end;

function TValue.IsInteger(): Boolean;
begin
  Result := LValueType in [vtInt64, vtUInt64];
end;

function TValue.AsInt64(): Int64;
begin
  Result := LIntValue;
end;

function TValue.AsUInt64(): UInt64;
begin
  Result := LUIntValue;
end;

function TValue.AsFloat64(): Double;
begin
  Result := LFloatValue;
end;

function TValue.AsStringIndex(): Integer;
begin
  Result := LStringIndex;
end;

function TValue.AsBoolean(): Boolean;
begin
  Result := LBoolValue;
end;

{ TValueFactory }

class function TValueFactory.CreateInt64(const AValue: Int64): TValue;
begin
  Result := TValue.CreateInt64(AValue);
end;

class function TValueFactory.CreateUInt64(const AValue: UInt64): TValue;
begin
  Result := TValue.CreateUInt64(AValue);
end;

class function TValueFactory.CreateFloat64(const AValue: Double): TValue;
begin
  Result := TValue.CreateFloat64(AValue);
end;

class function TValueFactory.CreateString(const AStringIndex: Integer): TValue;
begin
  Result := TValue.CreateString(AStringIndex);
end;

class function TValueFactory.CreateBoolean(const AValue: Boolean): TValue;
begin
  Result := TValue.CreateBoolean(AValue);
end;

class function TValueFactory.CreateFromString(const AString: UTF8String; const AStringPool: TStringList): TValue;
var
  LIndex: Integer;
begin
  LIndex := AStringPool.IndexOf(string(AString));
  if LIndex = -1 then
    LIndex := AStringPool.Add(string(AString));
  Result := CreateString(LIndex);
end;

class function TValueFactory.CreateFromPascalType(const APascalType: TPascalType; const AValue: UTF8String; const AStringPool: TStringList): TValue;
begin
  case APascalType of
    ptInt: Result := CreateInt64(StrToInt64(string(AValue)));
    ptUInt: Result := CreateUInt64(StrToUInt64(string(AValue)));
    ptFloat: Result := CreateFloat64(StrToFloat(string(AValue)));
    ptString, ptPString: Result := CreateFromString(AValue, AStringPool);
  else
    Result := TValue.CreateNone();
  end;
end;

{ TValueOperations }

class function TValueOperations.Add(const ALeft: TValue; const ARight: TValue): TValue;
begin
  case ALeft.ValueType of
    vtInt64:
      case ARight.ValueType of
        vtInt64: Result := TValue.CreateInt64(ALeft.IntValue + ARight.IntValue);
        vtUInt64: Result := TValue.CreateInt64(ALeft.IntValue + Int64(ARight.UIntValue));
        vtFloat64: Result := TValue.CreateFloat64(ALeft.IntValue + ARight.FloatValue);
      else
        raise Exception.Create('Invalid types for addition');
      end;
    vtUInt64:
      case ARight.ValueType of
        vtInt64: Result := TValue.CreateUInt64(ALeft.UIntValue + UInt64(ARight.IntValue));
        vtUInt64: Result := TValue.CreateUInt64(ALeft.UIntValue + ARight.UIntValue);
        vtFloat64: Result := TValue.CreateFloat64(ALeft.UIntValue + ARight.FloatValue);
      else
        raise Exception.Create('Invalid types for addition');
      end;
    vtFloat64:
      case ARight.ValueType of
        vtInt64: Result := TValue.CreateFloat64(ALeft.FloatValue + ARight.IntValue);
        vtUInt64: Result := TValue.CreateFloat64(ALeft.FloatValue + ARight.UIntValue);
        vtFloat64: Result := TValue.CreateFloat64(ALeft.FloatValue + ARight.FloatValue);
      else
        raise Exception.Create('Invalid types for addition');
      end;
  else
    raise Exception.Create('Invalid types for addition');
  end;
end;

class function TValueOperations.Subtract(const ALeft: TValue; const ARight: TValue): TValue;
begin
  case ALeft.ValueType of
    vtInt64:
      case ARight.ValueType of
        vtInt64: Result := TValue.CreateInt64(ALeft.IntValue - ARight.IntValue);
        vtUInt64: Result := TValue.CreateInt64(ALeft.IntValue - Int64(ARight.UIntValue));
        vtFloat64: Result := TValue.CreateFloat64(ALeft.IntValue - ARight.FloatValue);
      else
        raise Exception.Create('Invalid types for subtraction');
      end;
    vtUInt64:
      case ARight.ValueType of
        vtInt64: Result := TValue.CreateUInt64(ALeft.UIntValue - UInt64(ARight.IntValue));
        vtUInt64: Result := TValue.CreateUInt64(ALeft.UIntValue - ARight.UIntValue);
        vtFloat64: Result := TValue.CreateFloat64(ALeft.UIntValue - ARight.FloatValue);
      else
        raise Exception.Create('Invalid types for subtraction');
      end;
    vtFloat64:
      case ARight.ValueType of
        vtInt64: Result := TValue.CreateFloat64(ALeft.FloatValue - ARight.IntValue);
        vtUInt64: Result := TValue.CreateFloat64(ALeft.FloatValue - ARight.UIntValue);
        vtFloat64: Result := TValue.CreateFloat64(ALeft.FloatValue - ARight.FloatValue);
      else
        raise Exception.Create('Invalid types for subtraction');
      end;
  else
    raise Exception.Create('Invalid types for subtraction');
  end;
end;

class function TValueOperations.Multiply(const ALeft: TValue; const ARight: TValue): TValue;
begin
  case ALeft.ValueType of
    vtInt64:
      case ARight.ValueType of
        vtInt64: Result := TValue.CreateInt64(ALeft.IntValue * ARight.IntValue);
        vtUInt64: Result := TValue.CreateInt64(ALeft.IntValue * Int64(ARight.UIntValue));
        vtFloat64: Result := TValue.CreateFloat64(ALeft.IntValue * ARight.FloatValue);
      else
        raise Exception.Create('Invalid types for multiplication');
      end;
    vtUInt64:
      case ARight.ValueType of
        vtInt64: Result := TValue.CreateUInt64(ALeft.UIntValue * UInt64(ARight.IntValue));
        vtUInt64: Result := TValue.CreateUInt64(ALeft.UIntValue * ARight.UIntValue);
        vtFloat64: Result := TValue.CreateFloat64(ALeft.UIntValue * ARight.FloatValue);
      else
        raise Exception.Create('Invalid types for multiplication');
      end;
    vtFloat64:
      case ARight.ValueType of
        vtInt64: Result := TValue.CreateFloat64(ALeft.FloatValue * ARight.IntValue);
        vtUInt64: Result := TValue.CreateFloat64(ALeft.FloatValue * ARight.UIntValue);
        vtFloat64: Result := TValue.CreateFloat64(ALeft.FloatValue * ARight.FloatValue);
      else
        raise Exception.Create('Invalid types for multiplication');
      end;
  else
    raise Exception.Create('Invalid types for multiplication');
  end;
end;

class function TValueOperations.Divide(const ALeft: TValue; const ARight: TValue): TValue;
begin
  case ALeft.ValueType of
    vtInt64:
      case ARight.ValueType of
        vtInt64:
        begin
          if ARight.IntValue = 0 then raise Exception.Create('Division by zero');
          Result := TValue.CreateFloat64(ALeft.IntValue / ARight.IntValue);
        end;
        vtUInt64:
        begin
          if ARight.UIntValue = 0 then raise Exception.Create('Division by zero');
          Result := TValue.CreateFloat64(ALeft.IntValue / ARight.UIntValue);
        end;
        vtFloat64:
        begin
          if ARight.FloatValue = 0.0 then raise Exception.Create('Division by zero');
          Result := TValue.CreateFloat64(ALeft.IntValue / ARight.FloatValue);
        end;
      else
        raise Exception.Create('Invalid types for division');
      end;
    vtUInt64:
      case ARight.ValueType of
        vtInt64:
        begin
          if ARight.IntValue = 0 then raise Exception.Create('Division by zero');
          Result := TValue.CreateFloat64(ALeft.UIntValue / ARight.IntValue);
        end;
        vtUInt64:
        begin
          if ARight.UIntValue = 0 then raise Exception.Create('Division by zero');
          Result := TValue.CreateFloat64(ALeft.UIntValue / ARight.UIntValue);
        end;
        vtFloat64:
        begin
          if ARight.FloatValue = 0.0 then raise Exception.Create('Division by zero');
          Result := TValue.CreateFloat64(ALeft.UIntValue / ARight.FloatValue);
        end;
      else
        raise Exception.Create('Invalid types for division');
      end;
    vtFloat64:
      case ARight.ValueType of
        vtInt64:
        begin
          if ARight.IntValue = 0 then raise Exception.Create('Division by zero');
          Result := TValue.CreateFloat64(ALeft.FloatValue / ARight.IntValue);
        end;
        vtUInt64:
        begin
          if ARight.UIntValue = 0 then raise Exception.Create('Division by zero');
          Result := TValue.CreateFloat64(ALeft.FloatValue / ARight.UIntValue);
        end;
        vtFloat64:
        begin
          if ARight.FloatValue = 0.0 then raise Exception.Create('Division by zero');
          Result := TValue.CreateFloat64(ALeft.FloatValue / ARight.FloatValue);
        end;
      else
        raise Exception.Create('Invalid types for division');
      end;
  else
    raise Exception.Create('Invalid types for division');
  end;
end;

class function TValueOperations.Modulo(const ALeft: TValue; const ARight: TValue): TValue;
begin
  if not (ALeft.IsInteger() and ARight.IsInteger()) then
    raise Exception.Create('Modulo requires integer operands');

  case ALeft.ValueType of
    vtInt64:
      case ARight.ValueType of
        vtInt64:
        begin
          if ARight.IntValue = 0 then raise Exception.Create('Modulo by zero');
          Result := TValue.CreateInt64(ALeft.IntValue mod ARight.IntValue);
        end;
        vtUInt64:
        begin
          if ARight.UIntValue = 0 then raise Exception.Create('Modulo by zero');
          Result := TValue.CreateInt64(ALeft.IntValue mod Int64(ARight.UIntValue));
        end;
      end;
    vtUInt64:
      case ARight.ValueType of
        vtInt64:
        begin
          if ARight.IntValue = 0 then raise Exception.Create('Modulo by zero');
          Result := TValue.CreateUInt64(ALeft.UIntValue mod UInt64(ARight.IntValue));
        end;
        vtUInt64:
        begin
          if ARight.UIntValue = 0 then raise Exception.Create('Modulo by zero');
          Result := TValue.CreateUInt64(ALeft.UIntValue mod ARight.UIntValue);
        end;
      end;
  end;
end;

class function TValueOperations.Negate(const AValue: TValue): TValue;
begin
  case AValue.ValueType of
    vtInt64: Result := TValue.CreateInt64(-AValue.IntValue);
    vtFloat64: Result := TValue.CreateFloat64(-AValue.FloatValue);
  else
    raise Exception.Create('Cannot negate non-numeric value');
  end;
end;

class function TValueOperations.Equal(const ALeft: TValue; const ARight: TValue; const AStringPool: TStringList): Boolean;
begin
  if ALeft.ValueType <> ARight.ValueType then
  begin
    Result := False;
    Exit;
  end;

  case ALeft.ValueType of
    vtInt64: Result := ALeft.IntValue = ARight.IntValue;
    vtUInt64: Result := ALeft.UIntValue = ARight.UIntValue;
    vtFloat64: Result := ALeft.FloatValue = ARight.FloatValue;
    vtString: Result := AStringPool[ALeft.StringIndex] = AStringPool[ARight.StringIndex];
    vtBoolean: Result := ALeft.BoolValue = ARight.BoolValue;
  else
    Result := False;
  end;
end;

class function TValueOperations.NotEqual(const ALeft: TValue; const ARight: TValue; const AStringPool: TStringList): Boolean;
begin
  Result := not Equal(ALeft, ARight, AStringPool);
end;

class function TValueOperations.LessThan(const ALeft: TValue; const ARight: TValue; const AStringPool: TStringList): Boolean;
begin
  if ALeft.ValueType <> ARight.ValueType then
    raise Exception.Create('Cannot compare different types');

  case ALeft.ValueType of
    vtInt64: Result := ALeft.IntValue < ARight.IntValue;
    vtUInt64: Result := ALeft.UIntValue < ARight.UIntValue;
    vtFloat64: Result := ALeft.FloatValue < ARight.FloatValue;
    vtString: Result := AStringPool[ALeft.StringIndex] < AStringPool[ARight.StringIndex];
  else
    raise Exception.Create('Cannot compare this type');
  end;
end;

class function TValueOperations.LessOrEqual(const ALeft: TValue; const ARight: TValue; const AStringPool: TStringList): Boolean;
begin
  Result := LessThan(ALeft, ARight, AStringPool) or Equal(ALeft, ARight, AStringPool);
end;

class function TValueOperations.GreaterThan(const ALeft: TValue; const ARight: TValue; const AStringPool: TStringList): Boolean;
begin
  Result := not LessOrEqual(ALeft, ARight, AStringPool);
end;

class function TValueOperations.GreaterOrEqual(const ALeft: TValue; const ARight: TValue; const AStringPool: TStringList): Boolean;
begin
  Result := not LessThan(ALeft, ARight, AStringPool);
end;

class function TValueOperations.ConvertToInt64(const AValue: TValue; const AStringPool: TStringList): TValue;
begin
  case AValue.ValueType of
    vtInt64: Result := AValue;
    vtUInt64: Result := TValue.CreateInt64(Int64(AValue.UIntValue));
    vtFloat64: Result := TValue.CreateInt64(Trunc(AValue.FloatValue));
    vtString: Result := TValue.CreateInt64(StrToInt64(AStringPool[AValue.StringIndex]));
    vtBoolean: Result := TValue.CreateInt64(Ord(AValue.BoolValue));
  else
    raise Exception.Create('Cannot convert to Int64');
  end;
end;

class function TValueOperations.ConvertToUInt64(const AValue: TValue; const AStringPool: TStringList): TValue;
begin
  case AValue.ValueType of
    vtInt64: Result := TValue.CreateUInt64(UInt64(AValue.IntValue));
    vtUInt64: Result := AValue;
    vtFloat64: Result := TValue.CreateUInt64(Trunc(AValue.FloatValue));
    vtString: Result := TValue.CreateUInt64(StrToUInt64(AStringPool[AValue.StringIndex]));
    vtBoolean: Result := TValue.CreateUInt64(Ord(AValue.BoolValue));
  else
    raise Exception.Create('Cannot convert to UInt64');
  end;
end;

class function TValueOperations.ConvertToFloat64(const AValue: TValue; const AStringPool: TStringList): TValue;
begin
  case AValue.ValueType of
    vtInt64: Result := TValue.CreateFloat64(AValue.IntValue);
    vtUInt64: Result := TValue.CreateFloat64(AValue.UIntValue);
    vtFloat64: Result := AValue;
    vtString: Result := TValue.CreateFloat64(StrToFloat(AStringPool[AValue.StringIndex]));
  else
    raise Exception.Create('Cannot convert to Float64');
  end;
end;

class function TValueOperations.ConvertToString(const AValue: TValue; const AStringPool: TStringList): TValue;
var
  LString: string;
  LIndex: Integer;
begin
  case AValue.ValueType of
    vtInt64: LString := IntToStr(AValue.IntValue);
    vtUInt64: LString := UIntToStr(AValue.UIntValue);
    vtFloat64: LString := FloatToStr(AValue.FloatValue);
    vtString:
    begin
      Result := AValue;
      Exit;
    end;
    vtBoolean: LString := BoolToStr(AValue.BoolValue, True);
  else
    raise Exception.Create('Cannot convert to String');
  end;

  LIndex := AStringPool.IndexOf(LString);
  if LIndex = -1 then
    LIndex := AStringPool.Add(LString);
  Result := TValue.CreateString(LIndex);
end;

class function TValueOperations.ConvertToBoolean(const AValue: TValue; const AStringPool: TStringList): TValue;
begin
  case AValue.ValueType of
    vtInt64: Result := TValue.CreateBoolean(AValue.IntValue <> 0);
    vtUInt64: Result := TValue.CreateBoolean(AValue.UIntValue <> 0);
    vtFloat64: Result := TValue.CreateBoolean(AValue.FloatValue <> 0.0);
    vtString: Result := TValue.CreateBoolean(StrToBool(AStringPool[AValue.StringIndex]));
    vtBoolean: Result := AValue;
  else
    raise Exception.Create('Cannot convert to Boolean');
  end;
end;

class function TValueOperations.GetTypeName(const AValueType: TValueType): string;
begin
  case AValueType of
    vtNone: Result := 'None';
    vtInt64: Result := 'Int64';
    vtUInt64: Result := 'UInt64';
    vtFloat64: Result := 'Float64';
    vtString: Result := 'String';
    vtBoolean: Result := 'Boolean';
  else
    Result := 'Unknown';
  end;
end;

class function TValueOperations.ValueToDisplayString(const AValue: TValue; const AStringPool: TStringList): string;
begin
  case AValue.ValueType of
    vtNone: Result := '<none>';
    vtInt64: Result := IntToStr(AValue.IntValue);
    vtUInt64: Result := UIntToStr(AValue.UIntValue);
    vtFloat64: Result := FloatToStr(AValue.FloatValue);
    vtString: Result := AStringPool[AValue.StringIndex];
    vtBoolean: Result := BoolToStr(AValue.BoolValue, True);
  else
    Result := '<unknown>';
  end;
end;

{ TRuntimeVariableStorage }

constructor TRuntimeVariableStorage.Create();
begin
  inherited Create();
  LVariables := TDictionary<UTF8String, TValue>.Create();
end;

destructor TRuntimeVariableStorage.Destroy();
begin
  LVariables.Free();
  inherited Destroy();
end;

procedure TRuntimeVariableStorage.SetVariable(const AName: UTF8String; const AValue: TValue);
begin
  LVariables.AddOrSetValue(AName, AValue);
end;

function TRuntimeVariableStorage.GetVariable(const AName: UTF8String): TValue;
begin
  if not LVariables.TryGetValue(AName, Result) then
    Result := TValue.CreateNone();
end;

function TRuntimeVariableStorage.HasVariable(const AName: UTF8String): Boolean;
begin
  Result := LVariables.ContainsKey(AName);
end;

procedure TRuntimeVariableStorage.DeleteVariable(const AName: UTF8String);
begin
  LVariables.Remove(AName);
end;

procedure TRuntimeVariableStorage.Clear();
begin
  LVariables.Clear();
end;

function TRuntimeVariableStorage.GetVariableCount(): Integer;
begin
  Result := LVariables.Count;
end;

end.
