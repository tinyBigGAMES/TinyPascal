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

unit TinyPascal.VM;

{$I TinyPascal.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Diagnostics,
  TinyPascal.Value,
  TinyPascal.Bytecode,
  TinyPascal.Runtime;

type
  // VM execution result
  TVMResult = (
    vmrSuccess,        // Execution completed successfully
    vmrError,          // Runtime error occurred
    vmrHalted,         // Program halted normally
    vmrInvalidOpcode,  // Unknown instruction
    vmrStackUnderflow, // Not enough values on stack
    vmrStackOverflow,  // Stack full
    vmrInvalidAddress, // Invalid memory access
    vmrDivisionByZero, // Division by zero
    vmrTypeError       // Type mismatch
  );

  // Built-in function enumeration
  TBuiltinFunction = (
    bfWriteLn,     // WriteLn(text)
    bfWrite,       // Write(text)
    bfReadLn,      // ReadLn() -> string
    bfIntToStr,    // IntToStr(int) -> string
    bfStrToInt,    // StrToInt(string) -> int
    bfFloatToStr,  // FloatToStr(float) -> string
    bfStrToFloat   // StrToFloat(string) -> float
  );

  // VM execution statistics
  TVMStats = record
    InstructionsExecuted: Int64;
    MaxStackSize: Integer;
    ExecutionTimeMs: Double;
  end;

  // Forward declaration
  TVirtualMachine = class;

  // VM execution context
  TVMExecutionResult = class
  private
    LResult: TVMResult;
    LErrorMessage: string;
    LReturnValue: TValue;
    LStats: TVMStats;

  public
    constructor Create(const AResult: TVMResult; const AErrorMessage: string = '');

    property Result: TVMResult read LResult;
    property ErrorMessage: string read LErrorMessage;
    property ReturnValue: TValue read LReturnValue write LReturnValue;
    property Stats: TVMStats read LStats write LStats;
  end;

  // Core Virtual Machine class
  TVirtualMachine = class
  private
    LProgram: TBytecodeProgram;
    LStack: TArray<TValue>;
    LStackPointer: Integer;
    LInstructionPointer: Integer;
    LVariableStorage: TRuntimeVariableStorage;
    LRunning: Boolean;
    LStats: TVMStats;
    LMaxStackSize: Integer;

    // Stack operations
    procedure PushValue(const AValue: TValue);
    function PopValue(): TValue;
    function PeekValue(): TValue;
    function IsStackEmpty(): Boolean;
    function GetStackSize(): Integer;
    procedure CheckStackUnderflow(const ARequiredValues: Integer);
    procedure CheckStackOverflow();

    // Instruction execution
    function ExecuteInstruction(const AInstruction: TInstruction): TVMResult;
    function ExecuteArithmetic(const AOpcode: TOpcode): TVMResult;
    function ExecuteComparison(const AOpcode: TOpcode): TVMResult;
    function ExecuteConversion(const AOpcode: TOpcode): TVMResult;
    function ExecuteBuiltinCall(const AFunctionIndex: Cardinal): TVMResult;

    // Error handling
    {$HINTS OFF}
    function CreateError(const AResult: TVMResult; const AMessage: string): TVMResult;
    {$HINTS ON}
    function GetResultDescription(const AResult: TVMResult): string;

  public
    constructor Create(const AMaxStackSize: Integer = 10000);
    destructor Destroy(); override;

    // Program execution
    function LoadProgram(const AProgram: TBytecodeProgram): Boolean;
    function Execute(): TVMExecutionResult;
    function Reset(): Boolean;

    // Debug and inspection
    function GetCurrentInstruction(): TInstruction;
    function GetStackContents(): TArray<TValue>;
    function GetVariableValue(const AName: UTF8String): TValue;
    procedure SetVariableValue(const AName: UTF8String; const AValue: TValue);

    // Properties
    property Program_: TBytecodeProgram read LProgram;
    property InstructionPointer: Integer read LInstructionPointer;
    property StackPointer: Integer read LStackPointer;
    property VariableStorage: TRuntimeVariableStorage read LVariableStorage;
    property Stats: TVMStats read LStats;
    property Running: Boolean read LRunning;
  end;

// Utility functions
function VMResultToString(const AResult: TVMResult): string;
function BuiltinFunctionToString(const AFunction: TBuiltinFunction): string;

implementation

const
  // Default stack size
  DEFAULT_STACK_SIZE = 10000;

{ TVMExecutionResult }

constructor TVMExecutionResult.Create(const AResult: TVMResult; const AErrorMessage: string);
begin
  inherited Create();
  LResult := AResult;
  LErrorMessage := AErrorMessage;
  LReturnValue := TValue.CreateNone();
  FillChar(LStats, SizeOf(LStats), 0);
end;

{ TVirtualMachine }

constructor TVirtualMachine.Create(const AMaxStackSize: Integer);
begin
  inherited Create();
  LMaxStackSize := AMaxStackSize;
  SetLength(LStack, LMaxStackSize);
  LVariableStorage := TRuntimeVariableStorage.Create();
  Reset();
end;

destructor TVirtualMachine.Destroy();
begin
  LVariableStorage.Free();
  inherited Destroy();
end;

function TVirtualMachine.LoadProgram(const AProgram: TBytecodeProgram): Boolean;
begin
  Result := False;
  if not Assigned(AProgram) then Exit;
  if AProgram.IsEmpty() then Exit;

  LProgram := AProgram;
  Reset();
  Result := True;
end;

function TVirtualMachine.Reset(): Boolean;
begin
  LStackPointer := 0;
  LInstructionPointer := 0;
  LRunning := False;
  LVariableStorage.Clear();
  FillChar(LStats, SizeOf(LStats), 0);

  // Clear stack
  FillChar(LStack[0], Length(LStack) * SizeOf(TValue), 0);

  Result := True;
end;

function TVirtualMachine.Execute(): TVMExecutionResult;
var
  LStopwatch: TStopwatch;
  LInstruction: TInstruction;
  LResult: TVMResult;
begin
  LStopwatch := TStopwatch.StartNew();
  LStats.InstructionsExecuted := 0;
  LStats.MaxStackSize := 0;

  Result := TVMExecutionResult.Create(vmrSuccess);

  if not Assigned(LProgram) then
  begin
    Result := TVMExecutionResult.Create(vmrError, 'No program loaded');
    Exit;
  end;

  LInstructionPointer := LProgram.EntryPoint;
  LRunning := True;

  try
    while LRunning and (LInstructionPointer < LProgram.GetInstructionCount()) do
    begin
      // Get current instruction
      LInstruction := LProgram.GetInstruction(LInstructionPointer);

      // Execute instruction
      LResult := ExecuteInstruction(LInstruction);

      // Check for errors
      if LResult <> vmrSuccess then
      begin
        Result := TVMExecutionResult.Create(LResult,
          Format('Runtime error at instruction %d: %s', [LInstructionPointer, GetResultDescription(LResult)]));
        Exit;
      end;

      // Update statistics
      Inc(LStats.InstructionsExecuted);
      if GetStackSize() > LStats.MaxStackSize then
        LStats.MaxStackSize := GetStackSize();

      // Move to next instruction (unless it was a jump)
      if TOpcode(LInstruction.Opcode) <> OP_JMP then
        Inc(LInstructionPointer);
    end;

    // Calculate execution time with high precision
    LStopwatch.Stop();
    LStats.ExecutionTimeMs := LStopwatch.Elapsed.TotalMilliseconds;

    // Set result
    Result.LStats := LStats;
    if not LRunning then
      Result.LResult := vmrHalted
    else
      Result.LResult := vmrSuccess;

  except
    on E: Exception do
    begin
      Result := TVMExecutionResult.Create(vmrError, 'VM Exception: ' + E.Message);
    end;
  end;
end;

function TVirtualMachine.ExecuteInstruction(const AInstruction: TInstruction): TVMResult;
var
  LOpcode: TOpcode;
  LValue: TValue;
  LVarIndex: Integer;
  LConstIndex: Integer;
  LTarget: Integer;
  LCondition: Boolean;
begin
  Result := vmrSuccess;
  LOpcode := TOpcode(AInstruction.Opcode);

  case LOpcode of
    OP_NOP:
      ; // No operation

    OP_LOAD_CONST:
    begin
      LConstIndex := Integer(AInstruction.Operand1);
      if (LConstIndex < 0) or (LConstIndex >= LProgram.GetConstantCount()) then
      begin
        Result := vmrInvalidAddress;
        Exit;
      end;
      LValue := LProgram.GetConstantValue(LConstIndex);
      PushValue(LValue);
    end;

    OP_LOAD_VAR:
    begin
      LVarIndex := Integer(AInstruction.Operand1);
      if (LVarIndex < 0) or (LVarIndex >= LProgram.GetVariableCount()) then
      begin
        Result := vmrInvalidAddress;
        Exit;
      end;
      LValue := LVariableStorage.GetVariable(LProgram.GetVariableName(LVarIndex));
      PushValue(LValue);
    end;

    OP_STORE_VAR:
    begin
      CheckStackUnderflow(1);
      LVarIndex := Integer(AInstruction.Operand1);
      if (LVarIndex < 0) or (LVarIndex >= LProgram.GetVariableCount()) then
      begin
        Result := vmrInvalidAddress;
        Exit;
      end;
      LValue := PopValue();
      LVariableStorage.SetVariable(LProgram.GetVariableName(LVarIndex), LValue);
    end;

    OP_ADD, OP_SUB, OP_MUL, OP_DIV, OP_MOD:
      Result := ExecuteArithmetic(LOpcode);

    OP_NEG:
    begin
      CheckStackUnderflow(1);
      LValue := PopValue();
      try
        LValue := TValueOperations.Negate(LValue);
        PushValue(LValue);
      except
        on E: Exception do
          Result := vmrTypeError;
      end;
    end;

    OP_CMP_EQ, OP_CMP_NE, OP_CMP_LT, OP_CMP_LE, OP_CMP_GT, OP_CMP_GE:
      Result := ExecuteComparison(LOpcode);

    OP_JMP:
    begin
      LTarget := Integer(AInstruction.Operand1);
      if (LTarget < 0) or (LTarget >= LProgram.GetInstructionCount()) then
      begin
        Result := vmrInvalidAddress;
        Exit;
      end;
      LInstructionPointer := LTarget;
    end;

    OP_JMP_TRUE:
    begin
      CheckStackUnderflow(1);
      LValue := PopValue();
      if not LValue.IsBoolean() then
      begin
        Result := vmrTypeError;
        Exit;
      end;
      LCondition := LValue.AsBoolean();
      if LCondition then
      begin
        LTarget := Integer(AInstruction.Operand1);
        if (LTarget < 0) or (LTarget >= LProgram.GetInstructionCount()) then
        begin
          Result := vmrInvalidAddress;
          Exit;
        end;
        LInstructionPointer := LTarget;
        Dec(LInstructionPointer); // Will be incremented by main loop
      end;
    end;

    OP_JMP_FALSE:
    begin
      CheckStackUnderflow(1);
      LValue := PopValue();
      if not LValue.IsBoolean() then
      begin
        Result := vmrTypeError;
        Exit;
      end;
      LCondition := LValue.AsBoolean();
      if not LCondition then
      begin
        LTarget := Integer(AInstruction.Operand1);
        if (LTarget < 0) or (LTarget >= LProgram.GetInstructionCount()) then
        begin
          Result := vmrInvalidAddress;
          Exit;
        end;
        LInstructionPointer := LTarget;
        Dec(LInstructionPointer); // Will be incremented by main loop
      end;
    end;

    OP_CALL_BUILTIN:
      Result := ExecuteBuiltinCall(AInstruction.Operand1);

    OP_POP:
    begin
      CheckStackUnderflow(1);
      PopValue();
    end;

    OP_DUP:
    begin
      CheckStackUnderflow(1);
      LValue := PeekValue();
      PushValue(LValue);
    end;

    OP_CONV_TO_INT64, OP_CONV_TO_UINT64, OP_CONV_TO_FLOAT64, OP_CONV_TO_STRING, OP_CONV_TO_BOOLEAN:
      Result := ExecuteConversion(LOpcode);

    OP_HALT:
      LRunning := False;

  else
    Result := vmrInvalidOpcode;
  end;
end;

function TVirtualMachine.ExecuteArithmetic(const AOpcode: TOpcode): TVMResult;
var
  LLeft: TValue;
  LRight: TValue;
  LResult: TValue;
begin
  Result := vmrSuccess;

  CheckStackUnderflow(2);
  LRight := PopValue();
  LLeft := PopValue();

  try
    case AOpcode of
      OP_ADD: LResult := TValueOperations.Add(LLeft, LRight);
      OP_SUB: LResult := TValueOperations.Subtract(LLeft, LRight);
      OP_MUL: LResult := TValueOperations.Multiply(LLeft, LRight);
      OP_DIV: LResult := TValueOperations.Divide(LLeft, LRight);
      OP_MOD: LResult := TValueOperations.Modulo(LLeft, LRight);
    else
      Result := vmrInvalidOpcode;
      Exit;
    end;

    PushValue(LResult);

  except
    on E: Exception do
    begin
      if Pos('Division by zero', E.Message) > 0 then
        Result := vmrDivisionByZero
      else
        Result := vmrTypeError;
    end;
  end;
end;

function TVirtualMachine.ExecuteComparison(const AOpcode: TOpcode): TVMResult;
var
  LLeft: TValue;
  LRight: TValue;
  LResult: Boolean;
begin
  Result := vmrSuccess;
  CheckStackUnderflow(2);
  LRight := PopValue();
  LLeft := PopValue();

  try
    case AOpcode of
      OP_CMP_EQ: LResult := TValueOperations.Equal(LLeft, LRight, LProgram.StringPool);
      OP_CMP_NE: LResult := TValueOperations.NotEqual(LLeft, LRight, LProgram.StringPool);
      OP_CMP_LT: LResult := TValueOperations.LessThan(LLeft, LRight, LProgram.StringPool);
      OP_CMP_LE: LResult := TValueOperations.LessOrEqual(LLeft, LRight, LProgram.StringPool);
      OP_CMP_GT: LResult := TValueOperations.GreaterThan(LLeft, LRight, LProgram.StringPool);
      OP_CMP_GE: LResult := TValueOperations.GreaterOrEqual(LLeft, LRight, LProgram.StringPool);
    else
      Result := vmrInvalidOpcode;
      Exit;
    end;

    PushValue(TValue.CreateBoolean(LResult));

  except
    on E: Exception do
    begin
      Result := vmrTypeError;
    end;
  end;
end;

function TVirtualMachine.ExecuteConversion(const AOpcode: TOpcode): TVMResult;
var
  LValue: TValue;
  LResult: TValue;
begin
  Result := vmrSuccess;

  CheckStackUnderflow(1);
  LValue := PopValue();

  try
    case AOpcode of
      OP_CONV_TO_INT64: LResult := TValueOperations.ConvertToInt64(LValue, LProgram.StringPool);
      OP_CONV_TO_UINT64: LResult := TValueOperations.ConvertToUInt64(LValue, LProgram.StringPool);
      OP_CONV_TO_FLOAT64: LResult := TValueOperations.ConvertToFloat64(LValue, LProgram.StringPool);
      OP_CONV_TO_STRING: LResult := TValueOperations.ConvertToString(LValue, LProgram.StringPool);
      OP_CONV_TO_BOOLEAN: LResult := TValueOperations.ConvertToBoolean(LValue, LProgram.StringPool);
    else
      Result := vmrInvalidOpcode;
      Exit;
    end;

    PushValue(LResult);

  except
    on E: Exception do
      Result := vmrTypeError;
  end;
end;

function TVirtualMachine.ExecuteBuiltinCall(const AFunctionIndex: Cardinal): TVMResult;
var
  LValue: TValue;
  LResult: TValue;
  LRuntimeResult: TRuntimeResult;
begin
  Result := vmrSuccess;

  case TBuiltinFunction(AFunctionIndex) of
    bfWriteLn:
    begin
      if IsStackEmpty() then
      begin
        // WriteLn() with no arguments
        LRuntimeResult := Runtime_PrintLn_Empty();
      end
      else
      begin
        CheckStackUnderflow(1);
        LValue := PopValue();
        LRuntimeResult := Runtime_PrintLn(LValue, LProgram.StringPool);
      end;

      if LRuntimeResult <> rtSuccess then
        Result := vmrError;
    end;

    bfWrite:
    begin
      CheckStackUnderflow(1);
      LValue := PopValue();
      LRuntimeResult := Runtime_Print(LValue, LProgram.StringPool);

      if LRuntimeResult <> rtSuccess then
        Result := vmrError;
    end;

    bfReadLn:
    begin
      LValue := Runtime_ReadLn(LProgram.StringPool);
      PushValue(LValue);
    end;

    bfIntToStr:
    begin
      CheckStackUnderflow(1);
      LValue := PopValue();
      LResult := Runtime_IntToStr(LValue, LProgram.StringPool);

      if LResult.IsNone() then
        Result := vmrTypeError
      else
        PushValue(LResult);
    end;

    bfStrToInt:
    begin
      CheckStackUnderflow(1);
      LValue := PopValue();
      LResult := Runtime_StrToInt(LValue, LProgram.StringPool);

      if LResult.IsNone() then
        Result := vmrTypeError
      else
        PushValue(LResult);
    end;

    bfFloatToStr:
    begin
      CheckStackUnderflow(1);
      LValue := PopValue();
      LResult := Runtime_FloatToStr(LValue, LProgram.StringPool);

      if LResult.IsNone() then
        Result := vmrTypeError
      else
        PushValue(LResult);
    end;

    bfStrToFloat:
    begin
      CheckStackUnderflow(1);
      LValue := PopValue();
      LResult := Runtime_StrToFloat(LValue, LProgram.StringPool);

      if LResult.IsNone() then
        Result := vmrTypeError
      else
        PushValue(LResult);
    end;

  else
    Result := vmrInvalidAddress;
  end;
end;

procedure TVirtualMachine.PushValue(const AValue: TValue);
begin
  CheckStackOverflow();
  LStack[LStackPointer] := AValue;
  Inc(LStackPointer);
end;

function TVirtualMachine.PopValue(): TValue;
begin
  CheckStackUnderflow(1);
  Dec(LStackPointer);
  Result := LStack[LStackPointer];
end;

function TVirtualMachine.PeekValue(): TValue;
begin
  CheckStackUnderflow(1);
  Result := LStack[LStackPointer - 1];
end;

function TVirtualMachine.IsStackEmpty(): Boolean;
begin
  Result := LStackPointer = 0;
end;

function TVirtualMachine.GetStackSize(): Integer;
begin
  Result := LStackPointer;
end;

procedure TVirtualMachine.CheckStackUnderflow(const ARequiredValues: Integer);
begin
  if LStackPointer < ARequiredValues then
    raise Exception.Create('Stack underflow');
end;

procedure TVirtualMachine.CheckStackOverflow();
begin
  if LStackPointer >= LMaxStackSize then
    raise Exception.Create('Stack overflow');
end;

function TVirtualMachine.CreateError(const AResult: TVMResult; const AMessage: string): TVMResult;
begin
  Result := AResult;
  // Error message handling could be expanded here
end;

function TVirtualMachine.GetResultDescription(const AResult: TVMResult): string;
begin
  Result := VMResultToString(AResult);
end;

function TVirtualMachine.GetCurrentInstruction(): TInstruction;
begin
  if (LInstructionPointer >= 0) and (LInstructionPointer < LProgram.GetInstructionCount()) then
    Result := LProgram.GetInstruction(LInstructionPointer)
  else
    FillChar(Result, SizeOf(Result), 0);
end;

function TVirtualMachine.GetStackContents(): TArray<TValue>;
var
  LIndex: Integer;
begin
  SetLength(Result, LStackPointer);
  for LIndex := 0 to LStackPointer - 1 do
    Result[LIndex] := LStack[LIndex];
end;

function TVirtualMachine.GetVariableValue(const AName: UTF8String): TValue;
begin
  Result := LVariableStorage.GetVariable(AName);
end;

procedure TVirtualMachine.SetVariableValue(const AName: UTF8String; const AValue: TValue);
begin
  LVariableStorage.SetVariable(AName, AValue);
end;

{ Utility Functions }

function VMResultToString(const AResult: TVMResult): string;
begin
  case AResult of
    vmrSuccess: Result := 'Success';
    vmrError: Result := 'Error';
    vmrHalted: Result := 'Halted';
    vmrInvalidOpcode: Result := 'Invalid opcode';
    vmrStackUnderflow: Result := 'Stack underflow';
    vmrStackOverflow: Result := 'Stack overflow';
    vmrInvalidAddress: Result := 'Invalid address';
    vmrDivisionByZero: Result := 'Division by zero';
    vmrTypeError: Result := 'Type error';
  else
    Result := 'Unknown result';
  end;
end;

function BuiltinFunctionToString(const AFunction: TBuiltinFunction): string;
begin
  case AFunction of
    bfWriteLn: Result := 'WriteLn';
    bfWrite: Result := 'Write';
    bfReadLn: Result := 'ReadLn';
    bfIntToStr: Result := 'IntToStr';
    bfStrToInt: Result := 'StrToInt';
    bfFloatToStr: Result := 'FloatToStr';
    bfStrToFloat: Result := 'StrToFloat';
  else
    Result := 'Unknown';
  end;
end;

end.
