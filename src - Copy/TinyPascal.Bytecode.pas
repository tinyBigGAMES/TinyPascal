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

unit TinyPascal.Bytecode;

{$I TinyPascal.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  TinyPascal.Value;

type
  // Bytecode instruction opcodes
  TOpcode = (
    // === Constants & Variables ===
    OP_NOP              = $00,    // No operation
    OP_LOAD_CONST       = $01,    // Load constant value to stack
    OP_LOAD_VAR         = $10,    // Load variable to stack
    OP_STORE_VAR        = $18,    // Store value from stack to variable

    // === Arithmetic Operations ===
    OP_ADD              = $20,    // Add two values
    OP_SUB              = $21,    // Subtract values (top - second)
    OP_MUL              = $22,    // Multiply values
    OP_DIV              = $23,    // Divide values
    OP_MOD              = $24,    // Modulo operation
    OP_NEG              = $25,    // Negate value

    // === Comparison Operations ===
    OP_CMP_EQ           = $40,    // Compare equal
    OP_CMP_NE           = $41,    // Compare not equal
    OP_CMP_LT           = $42,    // Compare less than
    OP_CMP_LE           = $43,    // Compare less or equal
    OP_CMP_GT           = $44,    // Compare greater than
    OP_CMP_GE           = $45,    // Compare greater or equal

    // === Boolean Operations ===
    OP_AND_BOOL         = $50,    // Logical AND
    OP_OR_BOOL          = $51,    // Logical OR
    OP_NOT_BOOL         = $52,    // Logical NOT

    // === Control Flow ===
    OP_JMP              = $60,    // Unconditional jump
    OP_JMP_TRUE         = $61,    // Jump if top of stack is true
    OP_JMP_FALSE        = $62,    // Jump if top of stack is false
    OP_LABEL            = $63,    // Label marker (no operation, just for reference)

    // === Function Calls ===
    OP_CALL             = $70,    // Call function/procedure
    OP_CALL_BUILTIN     = $71,    // Call built-in runtime function
    OP_RET              = $72,    // Return from function
    OP_RET_VALUE        = $73,    // Return value from function

    // === Stack Operations ===
    OP_POP              = $80,    // Pop value from stack
    OP_DUP              = $81,    // Duplicate top stack value
    OP_SWAP             = $82,    // Swap top two stack values

    // === Type Conversions ===
    OP_CONV_TO_INT64    = $90,    // Convert to integer
    OP_CONV_TO_UINT64   = $91,    // Convert to unsigned integer
    OP_CONV_TO_FLOAT64  = $92,    // Convert to float
    OP_CONV_TO_STRING   = $93,    // Convert to string
    OP_CONV_TO_BOOLEAN  = $94,    // Convert to boolean

    // === Special ===
    OP_HALT             = $FF     // Halt execution
  );

  // Instruction data structure
  TInstruction = packed record
    Opcode: Byte;                 // Operation code
    Operand1: Cardinal;           // First operand (32-bit)
    Operand2: Cardinal;           // Second operand (32-bit)
    Operand3: Cardinal;           // Third operand (32-bit)
  end;

  // Forward declarations
  TBytecodeProgram = class;
  TBytecodeValidator = class;
  TBytecodeDisassembler = class;

  // Bytecode program class with integrated Value system
  TBytecodeProgram = class
  private
    LInstructions: TArray<TInstruction>;
    LConstantPool: TArray<TValue>;
    LStringPool: TStringList;
    LVariableNames: TStringList;
    LEntryPoint: Integer;

  public
    constructor Create();
    destructor Destroy(); override;

    // Instruction management
    procedure AddInstruction(const AInstruction: TInstruction);
    function GetInstruction(const AIndex: Integer): TInstruction;
    procedure SetInstruction(const AIndex: Integer; const AInstruction: TInstruction);
    function GetInstructionCount(): Integer;
    procedure SetEntryPoint(const AEntryPoint: Integer);

    // Value-based constant pool management
    function AddConstantValue(const AValue: TValue): Integer;
    function GetConstantValue(const AIndex: Integer): TValue;
    function GetConstantCount(): Integer;

    // Convenience methods for adding typed constants
    function AddInt64Constant(const AValue: Int64): Integer;
    function AddUInt64Constant(const AValue: UInt64): Integer;
    function AddFloat64Constant(const AValue: Double): Integer;
    function AddStringConstant(const AString: UTF8String): Integer;
    function AddBooleanConstant(const AValue: Boolean): Integer;

    // String pool management (integrated with Value system)
    function AddString(const AString: UTF8String): Integer;
    function GetString(const AIndex: Integer): UTF8String;
    function GetStringCount(): Integer;

    // Variable management
    function AddVariable(const AName: UTF8String): Integer;
    function GetVariableIndex(const AName: UTF8String): Integer;
    function GetVariableName(const AIndex: Integer): UTF8String;
    function GetVariableCount(): Integer;

    // Utility methods
    procedure Clear();
    function IsEmpty(): Boolean;

    // Properties
    property Instructions: TArray<TInstruction> read LInstructions;
    property ConstantPool: TArray<TValue> read LConstantPool;
    property StringPool: TStringList read LStringPool;
    property VariableNames: TStringList read LVariableNames;
    property EntryPoint: Integer read LEntryPoint write LEntryPoint;
  end;

  // Bytecode generation result class
  TBytecodeResult = class
  private
    LSuccess: Boolean;
    LErrorMessage: string;
    LProgram: TBytecodeProgram;

  public
    constructor Create(const ASuccess: Boolean; const AErrorMessage: string = '');
    destructor Destroy(); override;

    property Success: Boolean read LSuccess;
    property ErrorMessage: string read LErrorMessage;
    property Program_: TBytecodeProgram read LProgram;
  end;

  // Enhanced instruction factory with Value system support
  TInstructionFactory = class
  public
    class function CreateInstruction(const AOpcode: TOpcode; const AOperand1: Cardinal = 0;
      const AOperand2: Cardinal = 0; const AOperand3: Cardinal = 0): TInstruction; static;

    // Value-aware instruction creation
    class function CreateLoadConst(const AConstantIndex: Cardinal): TInstruction; static;
    class function CreateLoadVar(const AVariableIndex: Cardinal): TInstruction; static;
    class function CreateStoreVar(const AVariableIndex: Cardinal): TInstruction; static;

    // Arithmetic operations
    class function CreateAdd(): TInstruction; static;
    class function CreateSub(): TInstruction; static;
    class function CreateMul(): TInstruction; static;
    class function CreateDiv(): TInstruction; static;
    class function CreateMod(): TInstruction; static;
    class function CreateNeg(): TInstruction; static;

    // Comparison operations
    class function CreateCmpEq(): TInstruction; static;
    class function CreateCmpNe(): TInstruction; static;
    class function CreateCmpLt(): TInstruction; static;
    class function CreateCmpLe(): TInstruction; static;
    class function CreateCmpGt(): TInstruction; static;
    class function CreateCmpGe(): TInstruction; static;

    // Control flow
    class function CreateJmp(const ATargetAddress: Cardinal): TInstruction; static;
    class function CreateJmpTrue(const ATargetAddress: Cardinal): TInstruction; static;
    class function CreateJmpFalse(const ATargetAddress: Cardinal): TInstruction; static;

    // Type conversions
    class function CreateConvToInt64(): TInstruction; static;
    class function CreateConvToUInt64(): TInstruction; static;
    class function CreateConvToFloat64(): TInstruction; static;
    class function CreateConvToString(): TInstruction; static;
    class function CreateConvToBoolean(): TInstruction; static;

    // Function calls and special operations
    class function CreateCallBuiltin(const AFunctionIndex: Cardinal): TInstruction; static;
    class function CreatePop(): TInstruction; static;
    class function CreateDup(): TInstruction; static;
    class function CreateHalt(): TInstruction; static;
  end;

  // Bytecode validator class
  TBytecodeValidator = class
  private
    LProgram: TBytecodeProgram;
    LLastError: string;

  public
    constructor Create(const AProgram: TBytecodeProgram);

    function ValidateProgram(): Boolean;
    function ValidateInstruction(const AInstruction: TInstruction): Boolean;
    function GetLastError(): string;

  private
    function ValidateConstantIndex(const AIndex: Cardinal): Boolean;
    function ValidateVariableIndex(const AIndex: Cardinal): Boolean;
    function ValidateStringIndex(const AIndex: Cardinal): Boolean;
  end;

  // Enhanced bytecode disassembler with Value system support
  TBytecodeDisassembler = class
  private
    LProgram: TBytecodeProgram;

    function GetOpcodeString(const AOpcode: TOpcode): string;
    function GetOpcodeDescription(const AOpcode: TOpcode): string;
    function DisassembleInstruction(const AInstruction: TInstruction; const AIndex: Integer): string;
    function FormatValue(const AValue: TValue): string;

  public
    constructor Create(const AProgram: TBytecodeProgram);

    procedure DisassembleToStrings(const AOutput: TStrings);
    function DisassembleToString(): string;
    procedure DisassembleToFile(const AFileName: string);
  end;

// Instruction size constant
const
  INSTRUCTION_SIZE = SizeOf(TInstruction);

// Opcode utility functions
function IsArithmeticOpcode(const AOpcode: TOpcode): Boolean;
function IsComparisonOpcode(const AOpcode: TOpcode): Boolean;
function IsControlFlowOpcode(const AOpcode: TOpcode): Boolean;
function IsConversionOpcode(const AOpcode: TOpcode): Boolean;

implementation

{ TBytecodeProgram }

constructor TBytecodeProgram.Create();
begin
  inherited Create();
  LStringPool := TStringList.Create();
  LVariableNames := TStringList.Create();
  LEntryPoint := 0;
  Clear();
end;

destructor TBytecodeProgram.Destroy();
begin
  LStringPool.Free();
  LVariableNames.Free();
  inherited Destroy();
end;

procedure TBytecodeProgram.AddInstruction(const AInstruction: TInstruction);
begin
  SetLength(LInstructions, Length(LInstructions) + 1);
  LInstructions[High(LInstructions)] := AInstruction;
end;

function TBytecodeProgram.GetInstruction(const AIndex: Integer): TInstruction;
begin
  if (AIndex >= 0) and (AIndex < Length(LInstructions)) then
    Result := LInstructions[AIndex]
  else
    raise Exception.CreateFmt('Invalid instruction index: %d', [AIndex]);
end;

procedure TBytecodeProgram.SetInstruction(const AIndex: Integer; const AInstruction: TInstruction);
begin
  if (AIndex >= 0) and (AIndex < Length(LInstructions)) then
    LInstructions[AIndex] := AInstruction
  else
    raise Exception.CreateFmt('Invalid instruction index: %d', [AIndex]);
end;

function TBytecodeProgram.GetInstructionCount(): Integer;
begin
  Result := Length(LInstructions);
end;

procedure TBytecodeProgram.SetEntryPoint(const AEntryPoint: Integer);
begin
  LEntryPoint := AEntryPoint;
end;

function TBytecodeProgram.AddConstantValue(const AValue: TValue): Integer;
begin
  Result := Length(LConstantPool);
  SetLength(LConstantPool, Result + 1);
  LConstantPool[Result] := AValue;
end;

function TBytecodeProgram.GetConstantValue(const AIndex: Integer): TValue;
begin
  if (AIndex >= 0) and (AIndex < Length(LConstantPool)) then
    Result := LConstantPool[AIndex]
  else
    raise Exception.CreateFmt('Invalid constant index: %d', [AIndex]);
end;

function TBytecodeProgram.GetConstantCount(): Integer;
begin
  Result := Length(LConstantPool);
end;

function TBytecodeProgram.AddInt64Constant(const AValue: Int64): Integer;
begin
  Result := AddConstantValue(TValue.CreateInt64(AValue));
end;

function TBytecodeProgram.AddUInt64Constant(const AValue: UInt64): Integer;
begin
  Result := AddConstantValue(TValue.CreateUInt64(AValue));
end;

function TBytecodeProgram.AddFloat64Constant(const AValue: Double): Integer;
begin
  Result := AddConstantValue(TValue.CreateFloat64(AValue));
end;

function TBytecodeProgram.AddStringConstant(const AString: UTF8String): Integer;
var
  LStringIndex: Integer;
begin
  LStringIndex := AddString(AString);
  Result := AddConstantValue(TValue.CreateString(LStringIndex));
end;

function TBytecodeProgram.AddBooleanConstant(const AValue: Boolean): Integer;
begin
  Result := AddConstantValue(TValue.CreateBoolean(AValue));
end;

function TBytecodeProgram.AddString(const AString: UTF8String): Integer;
begin
  Result := LStringPool.IndexOf(string(AString));
  if Result = -1 then
  begin
    Result := LStringPool.Add(string(AString));
  end;
end;

function TBytecodeProgram.GetString(const AIndex: Integer): UTF8String;
begin
  if (AIndex >= 0) and (AIndex < LStringPool.Count) then
    Result := UTF8String(LStringPool[AIndex])
  else
    raise Exception.CreateFmt('Invalid string index: %d', [AIndex]);
end;

function TBytecodeProgram.GetStringCount(): Integer;
begin
  Result := LStringPool.Count;
end;

function TBytecodeProgram.AddVariable(const AName: UTF8String): Integer;
begin
  Result := LVariableNames.IndexOf(string(AName));
  if Result = -1 then
  begin
    Result := LVariableNames.Add(string(AName));
  end;
end;

function TBytecodeProgram.GetVariableIndex(const AName: UTF8String): Integer;
begin
  Result := LVariableNames.IndexOf(string(AName));
end;

function TBytecodeProgram.GetVariableName(const AIndex: Integer): UTF8String;
begin
  if (AIndex >= 0) and (AIndex < LVariableNames.Count) then
    Result := UTF8String(LVariableNames[AIndex])
  else
    raise Exception.CreateFmt('Invalid variable index: %d', [AIndex]);
end;

function TBytecodeProgram.GetVariableCount(): Integer;
begin
  Result := LVariableNames.Count;
end;

procedure TBytecodeProgram.Clear();
begin
  SetLength(LInstructions, 0);
  SetLength(LConstantPool, 0);
  LStringPool.Clear();
  LVariableNames.Clear();
  LEntryPoint := 0;
end;

function TBytecodeProgram.IsEmpty(): Boolean;
begin
  Result := Length(LInstructions) = 0;
end;

{ TBytecodeResult }

constructor TBytecodeResult.Create(const ASuccess: Boolean; const AErrorMessage: string);
begin
  inherited Create();
  LSuccess := ASuccess;
  LErrorMessage := AErrorMessage;
  LProgram := TBytecodeProgram.Create();
end;

destructor TBytecodeResult.Destroy();
begin
  LProgram.Free();
  inherited Destroy();
end;

{ TInstructionFactory }

class function TInstructionFactory.CreateInstruction(const AOpcode: TOpcode; const AOperand1: Cardinal = 0;
  const AOperand2: Cardinal = 0; const AOperand3: Cardinal = 0): TInstruction;
begin
  Result.Opcode := Byte(AOpcode);
  Result.Operand1 := AOperand1;
  Result.Operand2 := AOperand2;
  Result.Operand3 := AOperand3;
end;

class function TInstructionFactory.CreateLoadConst(const AConstantIndex: Cardinal): TInstruction;
begin
  Result := CreateInstruction(OP_LOAD_CONST, AConstantIndex);
end;

class function TInstructionFactory.CreateLoadVar(const AVariableIndex: Cardinal): TInstruction;
begin
  Result := CreateInstruction(OP_LOAD_VAR, AVariableIndex);
end;

class function TInstructionFactory.CreateStoreVar(const AVariableIndex: Cardinal): TInstruction;
begin
  Result := CreateInstruction(OP_STORE_VAR, AVariableIndex);
end;

class function TInstructionFactory.CreateAdd(): TInstruction;
begin
  Result := CreateInstruction(OP_ADD);
end;

class function TInstructionFactory.CreateSub(): TInstruction;
begin
  Result := CreateInstruction(OP_SUB);
end;

class function TInstructionFactory.CreateMul(): TInstruction;
begin
  Result := CreateInstruction(OP_MUL);
end;

class function TInstructionFactory.CreateDiv(): TInstruction;
begin
  Result := CreateInstruction(OP_DIV);
end;

class function TInstructionFactory.CreateMod(): TInstruction;
begin
  Result := CreateInstruction(OP_MOD);
end;

class function TInstructionFactory.CreateNeg(): TInstruction;
begin
  Result := CreateInstruction(OP_NEG);
end;

class function TInstructionFactory.CreateCmpEq(): TInstruction;
begin
  Result := CreateInstruction(OP_CMP_EQ);
end;

class function TInstructionFactory.CreateCmpNe(): TInstruction;
begin
  Result := CreateInstruction(OP_CMP_NE);
end;

class function TInstructionFactory.CreateCmpLt(): TInstruction;
begin
  Result := CreateInstruction(OP_CMP_LT);
end;

class function TInstructionFactory.CreateCmpLe(): TInstruction;
begin
  Result := CreateInstruction(OP_CMP_LE);
end;

class function TInstructionFactory.CreateCmpGt(): TInstruction;
begin
  Result := CreateInstruction(OP_CMP_GT);
end;

class function TInstructionFactory.CreateCmpGe(): TInstruction;
begin
  Result := CreateInstruction(OP_CMP_GE);
end;

class function TInstructionFactory.CreateJmp(const ATargetAddress: Cardinal): TInstruction;
begin
  Result := CreateInstruction(OP_JMP, ATargetAddress);
end;

class function TInstructionFactory.CreateJmpTrue(const ATargetAddress: Cardinal): TInstruction;
begin
  Result := CreateInstruction(OP_JMP_TRUE, ATargetAddress);
end;

class function TInstructionFactory.CreateJmpFalse(const ATargetAddress: Cardinal): TInstruction;
begin
  Result := CreateInstruction(OP_JMP_FALSE, ATargetAddress);
end;

class function TInstructionFactory.CreateConvToInt64(): TInstruction;
begin
  Result := CreateInstruction(OP_CONV_TO_INT64);
end;

class function TInstructionFactory.CreateConvToUInt64(): TInstruction;
begin
  Result := CreateInstruction(OP_CONV_TO_UINT64);
end;

class function TInstructionFactory.CreateConvToFloat64(): TInstruction;
begin
  Result := CreateInstruction(OP_CONV_TO_FLOAT64);
end;

class function TInstructionFactory.CreateConvToString(): TInstruction;
begin
  Result := CreateInstruction(OP_CONV_TO_STRING);
end;

class function TInstructionFactory.CreateConvToBoolean(): TInstruction;
begin
  Result := CreateInstruction(OP_CONV_TO_BOOLEAN);
end;

class function TInstructionFactory.CreateCallBuiltin(const AFunctionIndex: Cardinal): TInstruction;
begin
  Result := CreateInstruction(OP_CALL_BUILTIN, AFunctionIndex);
end;

class function TInstructionFactory.CreatePop(): TInstruction;
begin
  Result := CreateInstruction(OP_POP);
end;

class function TInstructionFactory.CreateDup(): TInstruction;
begin
  Result := CreateInstruction(OP_DUP);
end;

class function TInstructionFactory.CreateHalt(): TInstruction;
begin
  Result := CreateInstruction(OP_HALT);
end;

{ TBytecodeValidator }

constructor TBytecodeValidator.Create(const AProgram: TBytecodeProgram);
begin
  inherited Create();
  LProgram := AProgram;
  LLastError := '';
end;

function TBytecodeValidator.ValidateProgram(): Boolean;
var
  LIndex: Integer;
begin
  Result := True;
  LLastError := '';

  // Check entry point
  if (LProgram.EntryPoint < 0) or (LProgram.EntryPoint >= LProgram.GetInstructionCount()) then
  begin
    LLastError := 'Invalid entry point';
    Result := False;
    Exit;
  end;

  // Validate all instructions
  for LIndex := 0 to LProgram.GetInstructionCount() - 1 do
  begin
    if not ValidateInstruction(LProgram.GetInstruction(LIndex)) then
    begin
      LLastError := Format('Invalid instruction at index %d: %s', [LIndex, LLastError]);
      Result := False;
      Exit;
    end;
  end;
end;

function TBytecodeValidator.ValidateInstruction(const AInstruction: TInstruction): Boolean;
var
  LOpcode: TOpcode;
begin
  Result := True;
  LLastError := '';

  // Check if opcode is valid
  if Byte(AInstruction.Opcode) > Byte(High(TOpcode)) then
  begin
    LLastError := 'Invalid opcode';
    Result := False;
    Exit;
  end;

  LOpcode := TOpcode(AInstruction.Opcode);

  // Validate operands based on opcode
  case LOpcode of
    OP_LOAD_CONST:
    begin
      if not ValidateConstantIndex(AInstruction.Operand1) then
      begin
        LLastError := 'Invalid constant index';
        Result := False;
      end;
    end;

    OP_LOAD_VAR, OP_STORE_VAR:
    begin
      if not ValidateVariableIndex(AInstruction.Operand1) then
      begin
        LLastError := 'Invalid variable index';
        Result := False;
      end;
    end;

    OP_JMP, OP_JMP_TRUE, OP_JMP_FALSE:
    begin
      if AInstruction.Operand1 >= Cardinal(LProgram.GetInstructionCount()) then
      begin
        LLastError := 'Invalid jump target';
        Result := False;
      end;
    end;
  end;
end;

function TBytecodeValidator.ValidateConstantIndex(const AIndex: Cardinal): Boolean;
begin
  Result := AIndex < Cardinal(LProgram.GetConstantCount());
end;

function TBytecodeValidator.ValidateVariableIndex(const AIndex: Cardinal): Boolean;
begin
  Result := AIndex < Cardinal(LProgram.GetVariableCount());
end;

function TBytecodeValidator.ValidateStringIndex(const AIndex: Cardinal): Boolean;
begin
  Result := AIndex < Cardinal(LProgram.GetStringCount());
end;

function TBytecodeValidator.GetLastError(): string;
begin
  Result := LLastError;
end;

{ TBytecodeDisassembler }

constructor TBytecodeDisassembler.Create(const AProgram: TBytecodeProgram);
begin
  inherited Create();
  LProgram := AProgram;
end;

function TBytecodeDisassembler.GetOpcodeString(const AOpcode: TOpcode): string;
begin
  case AOpcode of
    OP_NOP: Result := 'NOP';
    OP_LOAD_CONST: Result := 'LOAD_CONST';
    OP_LOAD_VAR: Result := 'LOAD_VAR';
    OP_STORE_VAR: Result := 'STORE_VAR';
    OP_ADD: Result := 'ADD';
    OP_SUB: Result := 'SUB';
    OP_MUL: Result := 'MUL';
    OP_DIV: Result := 'DIV';
    OP_MOD: Result := 'MOD';
    OP_NEG: Result := 'NEG';
    OP_CMP_EQ: Result := 'CMP_EQ';
    OP_CMP_NE: Result := 'CMP_NE';
    OP_CMP_LT: Result := 'CMP_LT';
    OP_CMP_LE: Result := 'CMP_LE';
    OP_CMP_GT: Result := 'CMP_GT';
    OP_CMP_GE: Result := 'CMP_GE';
    OP_AND_BOOL: Result := 'AND_BOOL';
    OP_OR_BOOL: Result := 'OR_BOOL';
    OP_NOT_BOOL: Result := 'NOT_BOOL';
    OP_JMP: Result := 'JMP';
    OP_JMP_TRUE: Result := 'JMP_TRUE';
    OP_JMP_FALSE: Result := 'JMP_FALSE';
    OP_CALL_BUILTIN: Result := 'CALL_BUILTIN';
    OP_POP: Result := 'POP';
    OP_DUP: Result := 'DUP';
    OP_CONV_TO_INT64: Result := 'CONV_TO_INT64';
    OP_CONV_TO_UINT64: Result := 'CONV_TO_UINT64';
    OP_CONV_TO_FLOAT64: Result := 'CONV_TO_FLOAT64';
    OP_CONV_TO_STRING: Result := 'CONV_TO_STRING';
    OP_CONV_TO_BOOLEAN: Result := 'CONV_TO_BOOLEAN';
    OP_HALT: Result := 'HALT';
  else
    Result := Format('UNKNOWN_%02X', [Byte(AOpcode)]);
  end;
end;

function TBytecodeDisassembler.GetOpcodeDescription(const AOpcode: TOpcode): string;
begin
  case AOpcode of
    OP_NOP: Result := 'No operation';
    OP_LOAD_CONST: Result := 'Load constant value';
    OP_LOAD_VAR: Result := 'Load variable';
    OP_STORE_VAR: Result := 'Store variable';
    OP_ADD: Result := 'Add two values';
    OP_SUB: Result := 'Subtract values';
    OP_MUL: Result := 'Multiply values';
    OP_DIV: Result := 'Divide values';
    OP_MOD: Result := 'Modulo operation';
    OP_NEG: Result := 'Negate value';
    OP_CMP_EQ: Result := 'Compare equal';
    OP_CMP_NE: Result := 'Compare not equal';
    OP_CMP_LT: Result := 'Compare less than';
    OP_CMP_LE: Result := 'Compare less or equal';
    OP_CMP_GT: Result := 'Compare greater than';
    OP_CMP_GE: Result := 'Compare greater or equal';
    OP_CALL_BUILTIN: Result := 'Call built-in function';
    OP_HALT: Result := 'Halt execution';
  else
    Result := 'Unknown instruction';
  end;
end;

function TBytecodeDisassembler.FormatValue(const AValue: TValue): string;
begin
  Result := TValueOperations.ValueToDisplayString(AValue, LProgram.StringPool);
end;

function TBytecodeDisassembler.DisassembleInstruction(const AInstruction: TInstruction; const AIndex: Integer): string;
var
  LOpcode: TOpcode;
  LValue: TValue;
begin
  LOpcode := TOpcode(AInstruction.Opcode);
  Result := Format('%04d: %s', [AIndex, GetOpcodeString(LOpcode)]);

  // Add operand information based on instruction type
  case LOpcode of
    OP_LOAD_CONST:
    begin
      if AInstruction.Operand1 < Cardinal(LProgram.GetConstantCount()) then
      begin
        LValue := LProgram.GetConstantValue(AInstruction.Operand1);
        Result := Result + Format(' %d (%s)', [AInstruction.Operand1, FormatValue(LValue)]);
      end
      else
        Result := Result + Format(' %d (INVALID)', [AInstruction.Operand1]);
    end;

    OP_LOAD_VAR, OP_STORE_VAR:
    begin
      if AInstruction.Operand1 < Cardinal(LProgram.GetVariableCount()) then
        Result := Result + Format(' %d (%s)', [AInstruction.Operand1, string(LProgram.GetVariableName(AInstruction.Operand1))])
      else
        Result := Result + Format(' %d (INVALID)', [AInstruction.Operand1]);
    end;

    OP_JMP, OP_JMP_TRUE, OP_JMP_FALSE:
      Result := Result + Format(' %d', [AInstruction.Operand1]);

    OP_CALL_BUILTIN:
      Result := Result + Format(' %d', [AInstruction.Operand1]);
  else
    // For instructions without operands or with generic operands
    if (AInstruction.Operand1 <> 0) or (AInstruction.Operand2 <> 0) or (AInstruction.Operand3 <> 0) then
      Result := Result + Format(' %d, %d, %d', [AInstruction.Operand1, AInstruction.Operand2, AInstruction.Operand3]);
  end;
end;

procedure TBytecodeDisassembler.DisassembleToStrings(const AOutput: TStrings);
var
  LIndex: Integer;
  LInstruction: TInstruction;
begin
  AOutput.Clear();
  AOutput.Add('=== TinyPascal Bytecode Disassembly ===');
  AOutput.Add(Format('Instructions: %d', [LProgram.GetInstructionCount()]));
  AOutput.Add(Format('Constants: %d', [LProgram.GetConstantCount()]));
  AOutput.Add(Format('Variables: %d', [LProgram.GetVariableCount()]));
  AOutput.Add(Format('Strings: %d', [LProgram.GetStringCount()]));
  AOutput.Add(Format('Entry Point: %d', [LProgram.EntryPoint]));
  AOutput.Add('');

  // Show constants
  if LProgram.GetConstantCount() > 0 then
  begin
    AOutput.Add('=== Constants ===');
    for LIndex := 0 to LProgram.GetConstantCount() - 1 do
    begin
      AOutput.Add(Format('%d: %s', [LIndex, FormatValue(LProgram.GetConstantValue(LIndex))]));
    end;
    AOutput.Add('');
  end;

  // Show variables
  if LProgram.GetVariableCount() > 0 then
  begin
    AOutput.Add('=== Variables ===');
    for LIndex := 0 to LProgram.GetVariableCount() - 1 do
    begin
      AOutput.Add(Format('%d: %s', [LIndex, string(LProgram.GetVariableName(LIndex))]));
    end;
    AOutput.Add('');
  end;

  // Show instructions
  AOutput.Add('=== Instructions ===');
  for LIndex := 0 to LProgram.GetInstructionCount() - 1 do
  begin
    LInstruction := LProgram.GetInstruction(LIndex);
    AOutput.Add(DisassembleInstruction(LInstruction, LIndex));
  end;
end;

function TBytecodeDisassembler.DisassembleToString(): string;
var
  LStrings: TStringList;
begin
  LStrings := TStringList.Create();
  try
    DisassembleToStrings(LStrings);
    Result := LStrings.Text;
  finally
    LStrings.Free();
  end;
end;

procedure TBytecodeDisassembler.DisassembleToFile(const AFileName: string);
var
  LStrings: TStringList;
begin
  LStrings := TStringList.Create();
  try
    DisassembleToStrings(LStrings);
    LStrings.SaveToFile(AFileName);
  finally
    LStrings.Free();
  end;
end;

{ Utility Functions }

function IsArithmeticOpcode(const AOpcode: TOpcode): Boolean;
begin
  Result := AOpcode in [OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_MOD, OP_NEG];
end;

function IsComparisonOpcode(const AOpcode: TOpcode): Boolean;
begin
  Result := AOpcode in [OP_CMP_EQ, OP_CMP_NE, OP_CMP_LT, OP_CMP_LE, OP_CMP_GT, OP_CMP_GE];
end;

function IsControlFlowOpcode(const AOpcode: TOpcode): Boolean;
begin
  Result := AOpcode in [OP_JMP, OP_JMP_TRUE, OP_JMP_FALSE, OP_CALL, OP_CALL_BUILTIN, OP_RET, OP_RET_VALUE];
end;

function IsConversionOpcode(const AOpcode: TOpcode): Boolean;
begin
  Result := AOpcode in [OP_CONV_TO_INT64, OP_CONV_TO_UINT64, OP_CONV_TO_FLOAT64, OP_CONV_TO_STRING, OP_CONV_TO_BOOLEAN];
end;

end.
