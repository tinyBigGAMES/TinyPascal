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

unit TinyPascal.X64Gen;

{$I TinyPascal.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  Winapi.Windows,
  TinyPascal.Bytecode,
  TinyPascal.Value,
  TinyPascal.Runtime,
  TinyPascal.Common;

type
  // X64 native compilation result
  TX64CompileResult = class
  private
    LSuccess: Boolean;
    LErrorMessage: string;
    LExecutablePath: string;
    LGeneratedCodeSize: Integer;
    LCanExecuteInMemory: Boolean;

  public
    constructor Create(const ASuccess: Boolean; const AErrorMessage: string = '');

    property Success: Boolean read LSuccess;
    property ErrorMessage: string read LErrorMessage;
    property ExecutablePath: string read LExecutablePath write LExecutablePath;
    property GeneratedCodeSize: Integer read LGeneratedCodeSize write LGeneratedCodeSize;
    property CanExecuteInMemory: Boolean read LCanExecuteInMemory write LCanExecuteInMemory;
  end;

  // Function pointer type for executing generated code
  TGeneratedCodeFunction = procedure; cdecl;

  // Simple x64 instruction encoder with proper calling convention
  TX64InstructionEncoder = class
  private
    LCodeBuffer: TMemoryStream;

    // Helper methods for writing data
    procedure WriteByte(const AValue: Byte);
    procedure WriteInteger(const AValue: Integer);
    procedure WriteInt64(const AValue: Int64);

  public
    constructor Create();
    destructor Destroy(); override;

    // Function setup
    procedure EmitFunctionPrologue();
    procedure EmitFunctionEpilogue();

    // Basic x64 instruction encoding
    procedure EmitMoveImmediateToRAX(const AImmediate: Int64);
    procedure EmitPushRAX();
    procedure EmitPopRAX();
    procedure EmitPopRBX();
    procedure EmitPushRBP();
    procedure EmitPopRBP();
    procedure EmitMoveRSPToRBP();
    procedure EmitSubRSP(const ABytes: Integer);
    procedure EmitAddRSP(const ABytes: Integer);
    procedure EmitAddRAXRBX();
    procedure EmitSubRAXRBX();
    procedure EmitMulRAXRBX();
    procedure EmitDivRAXRBX();
    procedure EmitMoveRAXToMemory(const AOffset: Integer);
    procedure EmitMoveMemoryToRAX(const AOffset: Integer);
    procedure EmitMoveRAXToRCX();
    procedure EmitCallRelative(const ATargetFunction: Pointer);
    procedure EmitCallAbsolute(const ATargetFunction: Pointer);
    procedure EmitReturn();
    procedure EmitNop();

    // Get generated code
    function GetCodeBuffer(): TMemoryStream;
    procedure Clear();
  end;

  // Win64 PE executable generator
  TPEGenerator = class
  private
    LCode: TMemoryStream;
    LDataSection: TMemoryStream;
    LImportTable: TMemoryStream;

  public
    constructor Create();
    destructor Destroy(); override;

    function GenerateExecutable(const ACodeStream: TMemoryStream; const AOutputPath: string): Boolean;
  end;

  // Main X64 compiler class with stack type tracking
  TX64Compiler = class
  private
    LEncoder: TX64InstructionEncoder;
    LPEGenerator: TPEGenerator;
    LProgram: TBytecodeProgram;
    LVariableOffsets: TDictionary<Integer, Integer>;
    LErrorMessage: string;
    LStackValueTypes: TArray<TValueType>; // Track types on stack
    LStackPointer: Integer;

    // Instruction translation
    function TranslateInstruction(const AInstruction: TInstruction): Boolean;
    function TranslateLoadConst(const AOperand: Cardinal): Boolean;
    function TranslateStoreVar(const AOperand: Cardinal): Boolean;
    function TranslateLoadVar(const AOperand: Cardinal): Boolean;
    function TranslateAdd(): Boolean;
    function TranslateSub(): Boolean;
    function TranslateMul(): Boolean;
    function TranslateDiv(): Boolean;
    function TranslateBuiltinCall(const AOperand: Cardinal): Boolean;

    // Stack type tracking
    procedure PushStackType(const AType: TValueType);
    function PopStackType(): TValueType;
    function PeekStackType(): TValueType;

    // Helper methods
    function GetVariableOffset(const AVariableIndex: Integer): Integer;
    procedure SetError(const AMessage: string);

  public
    constructor Create();
    destructor Destroy(); override;

    // Main compilation method
    function CompileProgram(const AProgram: TBytecodeProgram): TX64CompileResult;
    function CompileToFile(const AProgram: TBytecodeProgram; const AOutputPath: string): TX64CompileResult;
    function ExecuteInMemory(): Boolean;

    property ErrorMessage: string read LErrorMessage;
  end;

implementation

const
  // Stack frame layout: RBP-based addressing with Windows x64 calling convention
  STACK_FRAME_SIZE = 128;     // Total frame size (includes shadow space)
  SHADOW_SPACE_SIZE = 32;     // Windows x64 shadow space requirement
  VARIABLE_BASE_OFFSET = -64; // Variables start at RBP-64 (after shadow space)
  VARIABLE_SIZE = 8;          // Size of Int64

{ TX64CompileResult }

constructor TX64CompileResult.Create(const ASuccess: Boolean; const AErrorMessage: string);
begin
  inherited Create();
  LSuccess := ASuccess;
  LErrorMessage := AErrorMessage;
  LExecutablePath := '';
  LGeneratedCodeSize := 0;
  LCanExecuteInMemory := False;
end;

{ TX64InstructionEncoder }

constructor TX64InstructionEncoder.Create();
begin
  inherited Create();
  LCodeBuffer := TMemoryStream.Create();
end;

destructor TX64InstructionEncoder.Destroy();
begin
  LCodeBuffer.Free();
  inherited Destroy();
end;

procedure TX64InstructionEncoder.WriteByte(const AValue: Byte);
begin
  LCodeBuffer.Write(AValue, SizeOf(Byte));
end;

procedure TX64InstructionEncoder.WriteInteger(const AValue: Integer);
begin
  LCodeBuffer.Write(AValue, SizeOf(Integer));
end;

procedure TX64InstructionEncoder.WriteInt64(const AValue: Int64);
begin
  LCodeBuffer.Write(AValue, SizeOf(Int64));
end;

procedure TX64InstructionEncoder.EmitFunctionPrologue();
begin
  // Standard Windows x64 function prologue
  // push rbp
  WriteByte($55);

  // mov rbp, rsp
  WriteByte($48);  // REX.W prefix
  WriteByte($89);  // MOV r/m64, r64
  WriteByte($E5);  // ModR/M: 11 100 101 (rsp to rbp)

  // sub rsp, STACK_FRAME_SIZE (allocate stack space)
  WriteByte($48);  // REX.W prefix
  WriteByte($81);  // SUB r/m64, imm32
  WriteByte($EC);  // ModR/M: 11 101 100 (sub rsp)
  WriteInteger(STACK_FRAME_SIZE);
end;

procedure TX64InstructionEncoder.EmitFunctionEpilogue();
begin
  // Standard Windows x64 function epilogue
  // add rsp, STACK_FRAME_SIZE (restore stack)
  WriteByte($48);  // REX.W prefix
  WriteByte($81);  // ADD r/m64, imm32
  WriteByte($C4);  // ModR/M: 11 000 100 (add rsp)
  WriteInteger(STACK_FRAME_SIZE);

  // pop rbp
  WriteByte($5D);

  // ret
  WriteByte($C3);
end;

procedure TX64InstructionEncoder.EmitMoveImmediateToRAX(const AImmediate: Int64);
begin
  // mov rax, immediate (REX.W + B8 + immediate64)
  WriteByte($48);  // REX.W prefix
  WriteByte($B8);  // MOV rax, imm64
  WriteInt64(AImmediate);
end;

procedure TX64InstructionEncoder.EmitPushRAX();
begin
  // push rax (50)
  WriteByte($50);
end;

procedure TX64InstructionEncoder.EmitPopRAX();
begin
  // pop rax (58)
  WriteByte($58);
end;

procedure TX64InstructionEncoder.EmitPopRBX();
begin
  // pop rbx (5B)
  WriteByte($5B);
end;

procedure TX64InstructionEncoder.EmitPushRBP();
begin
  // push rbp (55)
  WriteByte($55);
end;

procedure TX64InstructionEncoder.EmitPopRBP();
begin
  // pop rbp (5D)
  WriteByte($5D);
end;

procedure TX64InstructionEncoder.EmitMoveRSPToRBP();
begin
  // mov rbp, rsp (REX.W + 89 /r)
  WriteByte($48);  // REX.W prefix
  WriteByte($89);  // MOV r/m64, r64
  WriteByte($E5);  // ModR/M: 11 100 101 (rsp to rbp)
end;

procedure TX64InstructionEncoder.EmitSubRSP(const ABytes: Integer);
begin
  // sub rsp, immediate (REX.W + 81 /5 + imm32)
  WriteByte($48);  // REX.W prefix
  WriteByte($81);  // SUB r/m64, imm32
  WriteByte($EC);  // ModR/M: 11 101 100 (sub rsp)
  WriteInteger(ABytes);
end;

procedure TX64InstructionEncoder.EmitAddRSP(const ABytes: Integer);
begin
  // add rsp, immediate (REX.W + 81 /0 + imm32)
  WriteByte($48);  // REX.W prefix
  WriteByte($81);  // ADD r/m64, imm32
  WriteByte($C4);  // ModR/M: 11 000 100 (add rsp)
  WriteInteger(ABytes);
end;

procedure TX64InstructionEncoder.EmitAddRAXRBX();
begin
  // add rax, rbx (REX.W + 01 /r)
  WriteByte($48);  // REX.W prefix
  WriteByte($01);  // ADD r/m64, r64
  WriteByte($D8);  // ModR/M: 11 011 000 (rbx to rax)
end;

procedure TX64InstructionEncoder.EmitSubRAXRBX();
begin
  // sub rax, rbx (REX.W + 29 /r)
  WriteByte($48);  // REX.W prefix
  WriteByte($29);  // SUB r/m64, r64
  WriteByte($D8);  // ModR/M: 11 011 000 (rbx from rax)
end;

procedure TX64InstructionEncoder.EmitMulRAXRBX();
begin
  // imul rax, rbx (REX.W + 0F AF /r)
  WriteByte($48);  // REX.W prefix
  WriteByte($0F);  // Two-byte opcode prefix
  WriteByte($AF);  // IMUL r64, r/m64
  WriteByte($C3);  // ModR/M: 11 000 011 (rax, rbx)
end;

procedure TX64InstructionEncoder.EmitDivRAXRBX();
begin
  // For division, we need to use idiv which requires rdx:rax
  // First clear rdx, then divide
  // xor rdx, rdx (REX.W + 31 /r)
  WriteByte($48);  // REX.W prefix
  WriteByte($31);  // XOR r/m64, r64
  WriteByte($D2);  // ModR/M: 11 010 010 (rdx, rdx)

  // idiv rbx (REX.W + F7 /7)
  WriteByte($48);  // REX.W prefix
  WriteByte($F7);  // Unary Group 3
  WriteByte($FB);  // ModR/M: 11 111 011 (idiv rbx)
end;

procedure TX64InstructionEncoder.EmitMoveRAXToMemory(const AOffset: Integer);
begin
  // mov [rbp + offset], rax (REX.W + 89 /r + disp32)
  WriteByte($48);  // REX.W prefix
  WriteByte($89);  // MOV r/m64, r64
  WriteByte($85);  // ModR/M: 10 000 101 (32-bit disp + rbp)
  WriteInteger(AOffset);
end;

procedure TX64InstructionEncoder.EmitMoveMemoryToRAX(const AOffset: Integer);
begin
  // mov rax, [rbp + offset] (REX.W + 8B /r + disp32)
  WriteByte($48);  // REX.W prefix
  WriteByte($8B);  // MOV r64, r/m64
  WriteByte($85);  // ModR/M: 10 000 101 (32-bit disp + rbp)
  WriteInteger(AOffset);
end;

procedure TX64InstructionEncoder.EmitMoveRAXToRCX();
begin
  // mov rcx, rax (REX.W + 89 /r)
  WriteByte($48);  // REX.W prefix
  WriteByte($89);  // MOV r/m64, r64
  WriteByte($C1);  // ModR/M: 11 000 001 (rax to rcx)
end;

procedure TX64InstructionEncoder.EmitCallAbsolute(const ATargetFunction: Pointer);
begin
  // mov rax, immediate64 (load function address)
  WriteByte($48);  // REX.W prefix
  WriteByte($B8);  // MOV rax, imm64
  WriteInt64(Int64(NativeInt(ATargetFunction)));

  // call rax (call via register)
  WriteByte($FF);  // Call r/m64
  WriteByte($D0);  // ModR/M: 11 010 000 (call rax)
end;

procedure TX64InstructionEncoder.EmitCallRelative(const ATargetFunction: Pointer);
var
  LRelativeAddress: Int32;
  LCurrentPos: Int64;
begin
  // call relative (E8 + rel32)
  WriteByte($E8);

  // Calculate relative address from current position + 5 (size of call instruction)
  LCurrentPos := LCodeBuffer.Position;
  LRelativeAddress := Int32(NativeInt(ATargetFunction) - (NativeInt(LCodeBuffer.Memory) + LCurrentPos + 4));
  WriteInteger(LRelativeAddress);
end;

procedure TX64InstructionEncoder.EmitReturn();
begin
  // ret (C3)
  WriteByte($C3);
end;

procedure TX64InstructionEncoder.EmitNop();
begin
  // nop (90)
  WriteByte($90);
end;

function TX64InstructionEncoder.GetCodeBuffer(): TMemoryStream;
begin
  Result := LCodeBuffer;
end;

procedure TX64InstructionEncoder.Clear();
begin
  LCodeBuffer.Clear();
end;

{ TPEGenerator }

constructor TPEGenerator.Create();
begin
  inherited Create();
  LCode := TMemoryStream.Create();
  LDataSection := TMemoryStream.Create();
  LImportTable := TMemoryStream.Create();
end;

destructor TPEGenerator.Destroy();
begin
  LImportTable.Free();
  LDataSection.Free();
  LCode.Free();
  inherited Destroy();
end;

function TPEGenerator.GenerateExecutable(const ACodeStream: TMemoryStream; const AOutputPath: string): Boolean;
begin
  // Simplified implementation - real PE generation is complex
  // For now, just save the raw code to a file for inspection
  Result := False;
  if not Assigned(ACodeStream) then Exit;

  try
    ACodeStream.SaveToFile(ChangeFileExt(AOutputPath, '.bin'));
    DebugWriteLn('Generated raw machine code saved to: %s', [ChangeFileExt(AOutputPath, '.bin')]);
    DebugWriteLn('Code size: %d bytes', [ACodeStream.Size]);
    DebugWriteLn('Note: This is raw x64 machine code, not a PE executable yet.');
    Result := True;
  except
    on E: Exception do
    begin
      DebugWriteLn('Error saving code: %s', [E.Message]);
      Result := False;
    end;
  end;
end;

{ TX64Compiler }

constructor TX64Compiler.Create();
begin
  inherited Create();
  LEncoder := TX64InstructionEncoder.Create();
  LPEGenerator := TPEGenerator.Create();
  LVariableOffsets := TDictionary<Integer, Integer>.Create();
  LErrorMessage := '';
  SetLength(LStackValueTypes, 1000); // Initialize stack type tracking
  LStackPointer := 0;
end;

destructor TX64Compiler.Destroy();
begin
  LVariableOffsets.Free();
  LPEGenerator.Free();
  LEncoder.Free();
  inherited Destroy();
end;

procedure TX64Compiler.PushStackType(const AType: TValueType);
begin
  if LStackPointer < Length(LStackValueTypes) then
  begin
    LStackValueTypes[LStackPointer] := AType;
    Inc(LStackPointer);
  end;
end;

function TX64Compiler.PopStackType(): TValueType;
begin
  if LStackPointer > 0 then
  begin
    Dec(LStackPointer);
    Result := LStackValueTypes[LStackPointer];
  end
  else
    Result := vtNone;
end;

function TX64Compiler.PeekStackType(): TValueType;
begin
  if LStackPointer > 0 then
    Result := LStackValueTypes[LStackPointer - 1]
  else
    Result := vtNone;
end;

function TX64Compiler.CompileProgram(const AProgram: TBytecodeProgram): TX64CompileResult;
var
  LIndex: Integer;
  LInstruction: TInstruction;
begin
  LProgram := AProgram;
  LErrorMessage := '';
  LEncoder.Clear();
  LVariableOffsets.Clear();
  LStackPointer := 0;

  Result := TX64CompileResult.Create(False);

  try
    DebugWriteLn('Starting X64 compilation...');
    DebugWriteLn('Instructions to translate: %d', [AProgram.GetInstructionCount()]);

    // Generate function prologue (proper stack frame setup)
    LEncoder.EmitFunctionPrologue();
    DebugWriteLn('Generated function prologue');

    // Translate each bytecode instruction
    for LIndex := 0 to AProgram.GetInstructionCount() - 1 do
    begin
      LInstruction := AProgram.GetInstruction(LIndex);
      DebugWriteLn('Translating instruction %d: Opcode=%d', [LIndex, LInstruction.Opcode]);

      if not TranslateInstruction(LInstruction) then
      begin
        Result.LErrorMessage := Format('Failed to translate instruction %d: %s', [LIndex, LErrorMessage]);
        Exit;
      end;
    end;

    // Generate function epilogue (restore stack and return)
    LEncoder.EmitFunctionEpilogue();
    DebugWriteLn('Generated function epilogue');

    DebugWriteLn('X64 compilation successful!');
    DebugWriteLn('Generated code size: %d bytes', [LEncoder.GetCodeBuffer().Size]);

    Result.LSuccess := True;
    Result.LGeneratedCodeSize := LEncoder.GetCodeBuffer().Size;
    Result.LCanExecuteInMemory := True; // Mark as ready for execution

  except
    on E: Exception do
    begin
      Result.LErrorMessage := 'Compilation exception: ' + E.Message;
    end;
  end;
end;

function TX64Compiler.CompileToFile(const AProgram: TBytecodeProgram; const AOutputPath: string): TX64CompileResult;
begin
  Result := CompileProgram(AProgram);

  if Result.Success then
  begin
    if LPEGenerator.GenerateExecutable(LEncoder.GetCodeBuffer(), AOutputPath) then
    begin
      Result.LExecutablePath := AOutputPath;
    end
    else
    begin
      Result.LSuccess := False;
      Result.LErrorMessage := 'Failed to generate executable file';
    end;
  end;
end;

function TX64Compiler.ExecuteInMemory(): Boolean;
var
  LCodeBuffer: TMemoryStream;
  LExecutableMemory: Pointer;
  LGeneratedFunction: TGeneratedCodeFunction;
  LCodeSize: Integer;
begin
  Result := False;

  try
    LCodeBuffer := LEncoder.GetCodeBuffer();
    LCodeSize := LCodeBuffer.Size;

    if LCodeSize = 0 then
    begin
      SetError('No code generated to execute');
      Exit;
    end;

    DebugWriteLn('=== EXECUTING GENERATED X64 CODE IN MEMORY ===');
    DebugWriteLn('Code size: %d bytes', [LCodeSize]);
    DebugWriteLn('Making memory executable...');

    // Allocate executable memory
    LExecutableMemory := VirtualAlloc(nil, LCodeSize, MEM_COMMIT or MEM_RESERVE, PAGE_EXECUTE_READWRITE);
    if not Assigned(LExecutableMemory) then
    begin
      SetError('Failed to allocate executable memory');
      Exit;
    end;

    try
      // Copy our generated code to executable memory
      Move(LCodeBuffer.Memory^, LExecutableMemory^, LCodeSize);

      DebugWriteLn('Code copied to executable memory at: 0x%s', [IntToHex(NativeInt(LExecutableMemory), 16)]);
      DebugWriteLn('Executing generated code...');
      DebugWriteLn('--- NATIVE CODE OUTPUT STARTS HERE ---');

      // Create function pointer and execute!
      LGeneratedFunction := TGeneratedCodeFunction(LExecutableMemory);
      LGeneratedFunction(); // *** EXECUTE THE GENERATED CODE! ***

      DebugWriteLn('--- NATIVE CODE OUTPUT ENDS HERE ---');
      DebugWriteLn('🎉 IN-MEMORY EXECUTION SUCCESSFUL! 🎉');

      Result := True;

    finally
      // Clean up executable memory
      VirtualFree(LExecutableMemory, 0, MEM_RELEASE);
    end;

  except
    on E: Exception do
    begin
      SetError('In-memory execution failed: ' + E.Message);
      DebugWriteLn('❌ Execution error: %s', [E.Message]);
    end;
  end;
end;

function TX64Compiler.TranslateInstruction(const AInstruction: TInstruction): Boolean;
var
  LOpcode: TOpcode;
begin
  Result := True;
  LOpcode := TOpcode(AInstruction.Opcode);

  case LOpcode of
    OP_NOP:
      LEncoder.EmitNop();

    OP_LOAD_CONST:
      Result := TranslateLoadConst(AInstruction.Operand1);

    OP_STORE_VAR:
      Result := TranslateStoreVar(AInstruction.Operand1);

    OP_LOAD_VAR:
      Result := TranslateLoadVar(AInstruction.Operand1);

    OP_ADD:
      Result := TranslateAdd();

    OP_SUB:
      Result := TranslateSub();

    OP_MUL:
      Result := TranslateMul();

    OP_DIV:
      Result := TranslateDiv();

    OP_CALL_BUILTIN:
      Result := TranslateBuiltinCall(AInstruction.Operand1);

    OP_HALT:
      ; // Function epilogue will handle return

  else
    SetError(Format('Unsupported instruction: Opcode=%d', [Ord(LOpcode)]));
    Result := False;
  end;
end;

function TX64Compiler.TranslateLoadConst(const AOperand: Cardinal): Boolean;
var
  LConstIndex: Integer;
  LValue: TValue;
begin
  Result := False;
  LConstIndex := Integer(AOperand);

  if (LConstIndex < 0) or (LConstIndex >= LProgram.GetConstantCount()) then
  begin
    SetError(Format('Invalid constant index: %d', [LConstIndex]));
    Exit;
  end;

  LValue := LProgram.GetConstantValue(LConstIndex);

  // For now, only handle Int64 constants
  if LValue.IsInt64() then
  begin
    // mov rax, immediate; push rax
    LEncoder.EmitMoveImmediateToRAX(LValue.AsInt64());
    LEncoder.EmitPushRAX();
    PushStackType(vtInt64); // Track that we pushed an Int64
    Result := True;
    DebugWriteLn('  → mov rax, %d; push rax', [LValue.AsInt64()]);
  end
  else
  begin
    SetError(Format('Unsupported constant type for X64: ValueType=%d', [Ord(LValue.ValueType)]));
  end;
end;

function TX64Compiler.TranslateStoreVar(const AOperand: Cardinal): Boolean;
var
  LVarIndex: Integer;
  LOffset: Integer;
begin
  Result := False;
  LVarIndex := Integer(AOperand);

  if (LVarIndex < 0) or (LVarIndex >= LProgram.GetVariableCount()) then
  begin
    SetError(Format('Invalid variable index: %d', [LVarIndex]));
    Exit;
  end;

  LOffset := GetVariableOffset(LVarIndex);

  // Track that we're popping a value
  PopStackType();

  // pop rax; mov [rbp + offset], rax
  LEncoder.EmitPopRAX();
  LEncoder.EmitMoveRAXToMemory(LOffset);

  Result := True;
  DebugWriteLn('  → pop rax; mov [rbp%+d], rax  ; Store variable "%s"', [LOffset, string(LProgram.GetVariableName(LVarIndex))]);
end;

function TX64Compiler.TranslateLoadVar(const AOperand: Cardinal): Boolean;
var
  LVarIndex: Integer;
  LOffset: Integer;
begin
  Result := False;
  LVarIndex := Integer(AOperand);

  if (LVarIndex < 0) or (LVarIndex >= LProgram.GetVariableCount()) then
  begin
    SetError(Format('Invalid variable index: %d', [LVarIndex]));
    Exit;
  end;

  LOffset := GetVariableOffset(LVarIndex);

  // mov rax, [rbp + offset]; push rax
  LEncoder.EmitMoveMemoryToRAX(LOffset);
  LEncoder.EmitPushRAX();

  // Variables are assumed to be Int64 for now
  PushStackType(vtInt64);

  Result := True;
  DebugWriteLn('  → mov rax, [rbp%+d]; push rax  ; Load variable "%s"', [LOffset, string(LProgram.GetVariableName(LVarIndex))]);
end;

function TX64Compiler.TranslateAdd(): Boolean;
begin
  // Track stack types
  PopStackType(); // Right operand
  PopStackType(); // Left operand

  // pop rbx; pop rax; add rax, rbx; push rax
  LEncoder.EmitPopRBX();  // Right operand
  LEncoder.EmitPopRAX();  // Left operand
  LEncoder.EmitAddRAXRBX();
  LEncoder.EmitPushRAX();

  // Result is same type as operands (assuming both are Int64 for now)
  PushStackType(vtInt64);

  Result := True;
  DebugWriteLn('  → pop rbx; pop rax; add rax, rbx; push rax');
end;

function TX64Compiler.TranslateSub(): Boolean;
begin
  PopStackType(); // Right operand
  PopStackType(); // Left operand

  LEncoder.EmitPopRBX();  // Right operand
  LEncoder.EmitPopRAX();  // Left operand
  LEncoder.EmitSubRAXRBX();
  LEncoder.EmitPushRAX();

  PushStackType(vtInt64);

  Result := True;
  DebugWriteLn('  → pop rbx; pop rax; sub rax, rbx; push rax');
end;

function TX64Compiler.TranslateMul(): Boolean;
begin
  PopStackType(); // Right operand
  PopStackType(); // Left operand

  LEncoder.EmitPopRBX();  // Right operand
  LEncoder.EmitPopRAX();  // Left operand
  LEncoder.EmitMulRAXRBX();
  LEncoder.EmitPushRAX();

  PushStackType(vtInt64);

  Result := True;
  DebugWriteLn('  → pop rbx; pop rax; imul rax, rbx; push rax');
end;

function TX64Compiler.TranslateDiv(): Boolean;
begin
  PopStackType(); // Right operand (divisor)
  PopStackType(); // Left operand (dividend)

  LEncoder.EmitPopRBX();  // Right operand (divisor)
  LEncoder.EmitPopRAX();  // Left operand (dividend)
  LEncoder.EmitDivRAXRBX(); // This handles the xor rdx,rdx and idiv
  LEncoder.EmitPushRAX();

  PushStackType(vtInt64);

  Result := True;
  DebugWriteLn('  → pop rbx; pop rax; xor rdx, rdx; idiv rbx; push rax');
end;

function TX64Compiler.TranslateBuiltinCall(const AOperand: Cardinal): Boolean;
var
  LStackType: TValueType;
begin
  Result := False;

  case AOperand of
    0: // PrintLn
    begin
      // Check what type is on the stack
      LStackType := PeekStackType();
      PopStackType(); // Remove it from stack tracking

      // pop rax (get the value from stack)
      LEncoder.EmitPopRAX();
      // mov rcx, rax (move parameter to correct register)
      LEncoder.EmitMoveRAXToRCX();

      try
        if LStackType = vtString then
        begin
          // Call string version
          LEncoder.EmitCallAbsolute(@Runtime_PrintLnString_X64);
          DebugWriteLn('  → pop rax; mov rcx, rax; call Runtime_PrintLnString_X64  ; Print string');
        end
        else
        begin
          // Call integer version (default)
          LEncoder.EmitCallAbsolute(@Runtime_PrintLn_X64);
          DebugWriteLn('  → pop rax; mov rcx, rax; call Runtime_PrintLn_X64  ; Print integer');
        end;
        Result := True;
      except
        on E: Exception do
        begin
          SetError('Failed to generate call to Runtime_PrintLn: ' + E.Message);
        end;
      end;
    end;

    1: // Print (Write)
    begin
      LStackType := PeekStackType();
      PopStackType();

      LEncoder.EmitPopRAX();
      LEncoder.EmitMoveRAXToRCX();

      try
        if LStackType = vtString then
        begin
          LEncoder.EmitCallAbsolute(@Runtime_PrintString_X64);
          DebugWriteLn('  → pop rax; mov rcx, rax; call Runtime_PrintString_X64  ; Print string without newline');
        end
        else
        begin
          LEncoder.EmitCallAbsolute(@Runtime_Print_X64);
          DebugWriteLn('  → pop rax; mov rcx, rax; call Runtime_Print_X64  ; Print integer without newline');
        end;
        Result := True;
      except
        on E: Exception do
        begin
          SetError('Failed to generate call to Runtime_Print: ' + E.Message);
        end;
      end;
    end;

    3: // IntToStr
    begin
      // Should have Int64 on stack, will return string pointer
      PopStackType(); // Remove Int64

      // pop rax (get integer from stack)
      LEncoder.EmitPopRAX();
      // mov rcx, rax (move parameter to correct register)
      LEncoder.EmitMoveRAXToRCX();

      try
        // Call Runtime_IntToStr_X64 function (returns PAnsiChar in RAX)
        LEncoder.EmitCallAbsolute(@Runtime_IntToStr_X64);
        // push rax (push returned string pointer back onto stack)
        LEncoder.EmitPushRAX();

        // Track that we now have a string pointer on stack
        PushStackType(vtString);

        Result := True;
        DebugWriteLn('  → pop rax; mov rcx, rax; call Runtime_IntToStr_X64; push rax  ; Convert int to string');
      except
        on E: Exception do
        begin
          SetError('Failed to generate call to Runtime_IntToStr_X64: ' + E.Message);
        end;
      end;
    end;

  else
    SetError(Format('Unsupported builtin function index: %d', [AOperand]));
  end;
end;

function TX64Compiler.GetVariableOffset(const AVariableIndex: Integer): Integer;
begin
  if not LVariableOffsets.TryGetValue(AVariableIndex, Result) then
  begin
    // Assign new offset for this variable
    Result := VARIABLE_BASE_OFFSET - (AVariableIndex * VARIABLE_SIZE);
    LVariableOffsets.Add(AVariableIndex, Result);
  end;
end;

procedure TX64Compiler.SetError(const AMessage: string);
begin
  LErrorMessage := AMessage;
  DebugWriteLn('X64 Compiler Error: %s', [AMessage]);
end;

end.
