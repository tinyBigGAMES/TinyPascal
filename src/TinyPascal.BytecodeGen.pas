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

unit TinyPascal.BytecodeGen;

{$I TinyPascal.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  TinyPascal.Lexer,
  TinyPascal.AST,
  TinyPascal.Value,
  TinyPascal.Bytecode,
  TinyPascal.VM;

type
  // Bytecode generation result
  TBytecodeGenResult = class
  private
    LSuccess: Boolean;
    LErrorMessage: string;
    LProgram: TBytecodeProgram;
    LBytecodeSize: Integer;

  public
    constructor Create(const ASuccess: Boolean; const AErrorMessage: string = '');
    destructor Destroy(); override;

    property Success: Boolean read LSuccess;
    property ErrorMessage: string read LErrorMessage;
    property Program_: TBytecodeProgram read LProgram;
    property BytecodeSize: Integer read LBytecodeSize write LBytecodeSize;
  end;

  // Built-in function mapping
  TBuiltinFunctionMap = class
  private
    LFunctionMap: TDictionary<UTF8String, Integer>;

  public
    constructor Create();
    destructor Destroy(); override;

    function GetFunctionIndex(const AFunctionName: UTF8String): Integer;
    function IsFunctionBuiltin(const AFunctionName: UTF8String): Boolean;
  end;

  // Label management for jumps
  TLabelManager = class
  private
    LLabelCounter: Integer;
    LLabelMap: TDictionary<Integer, Integer>; // LabelId -> InstructionIndex

  public
    constructor Create();
    destructor Destroy(); override;

    function CreateLabel(): Integer;
    procedure SetLabelPosition(const ALabelId: Integer; const AInstructionIndex: Integer);
    function GetLabelPosition(const ALabelId: Integer): Integer;
    procedure Clear();
  end;

  // Bytecode generator class
  TBytecodeGenerator = class(TInterfacedObject, IASTVisitor)
  private
    LProgram: TBytecodeProgram;
    LBuiltinMap: TBuiltinFunctionMap;
    LLabelManager: TLabelManager;
    LErrorMessage: string;
    LHasError: Boolean;

    // Helper methods for code generation
    procedure EmitInstruction(const AInstruction: TInstruction);
    procedure EmitLoadConst(const AValue: TValue);
    procedure EmitLoadVar(const AVariableName: UTF8String);
    procedure EmitStoreVar(const AVariableName: UTF8String);
    procedure EmitArithmetic(const AOperation: TBinaryOpType);
    procedure EmitComparison(const AOperation: TComparisonOpType);
    procedure EmitLogical(const AOperation: TLogicalOpType);
    procedure EmitBuiltinCall(const AFunctionName: UTF8String);
    procedure EmitJump(const ALabelId: Integer);
    procedure EmitJumpTrue(const ALabelId: Integer);
    procedure EmitJumpFalse(const ALabelId: Integer);

    // Array-specific helper methods
    procedure EmitArrayCreate(const AElementType: TValueType; const AStartIndex: Integer = 0; const AEndIndex: Integer = -1; const AIsDynamic: Boolean = False);
    procedure EmitArrayAccess();
    procedure EmitArrayAssign();
    procedure EmitArrayLength();

    // Variable and constant management
    function GetOrCreateVariable(const AVariableName: UTF8String): Integer;
    {$HINTS OFF}
    function CreateConstantFromLiteral(const AValue: UTF8String; const AType: TPascalType): TValue;
    {$HINTS ON}

    // Type conversion helpers
    function PascalTypeToValueType(const APascalType: TPascalType): TValueType;

    // Jump patching
    procedure PatchJumps();

    // Error handling
    procedure SetError(const AMessage: string);
    function HasError(): Boolean;

  public
    constructor Create();
    destructor Destroy(); override;

    // Main generation method
    function Generate(const AAST: TProgramNode): TBytecodeGenResult;

    // IASTVisitor implementation
    function VisitProgram(const ANode: TProgramNode): Pointer;
    function VisitBeginEnd(const ANode: TBeginEndNode): Pointer;
    function VisitProcedureCall(const ANode: TProcedureCallNode): Pointer;
    function VisitStringLiteral(const ANode: TStringLiteralNode): Pointer;
    function VisitIntegerLiteral(const ANode: TIntegerLiteralNode): Pointer;
    function VisitFloatLiteral(const ANode: TFloatLiteralNode): Pointer;
    function VisitBooleanLiteral(const ANode: TBooleanLiteralNode): Pointer;
    function VisitVariableReference(const ANode: TVariableReferenceNode): Pointer;
    function VisitBinaryOp(const ANode: TBinaryOpNode): Pointer;
    function VisitComparison(const ANode: TComparisonNode): Pointer;
    function VisitLogicalOp(const ANode: TLogicalOpNode): Pointer;
    function VisitAssignment(const ANode: TAssignmentNode): Pointer;
    function VisitIf(const ANode: TIfNode): Pointer;
    function VisitWhile(const ANode: TWhileNode): Pointer;
    function VisitFor(const ANode: TForNode): Pointer;
    function VisitType(const ANode: TTypeNode): Pointer;
    function VisitArrayType(const ANode: TArrayTypeNode): Pointer;
    function VisitArrayAccess(const ANode: TArrayAccessNode): Pointer;
    function VisitArrayLiteral(const ANode: TArrayLiteralNode): Pointer;
    function VisitVarDecl(const ANode: TVarDeclNode): Pointer;
    function VisitVarSection(const ANode: TVarSectionNode): Pointer;

    // Properties
    property Program_: TBytecodeProgram read LProgram;
    property ErrorMessage: string read LErrorMessage;
  end;

implementation

const
  // Built-in function indices (must match TBuiltinFunction in VM)
  BUILTIN_WRITELN_INDEX = 0;
  BUILTIN_WRITE_INDEX = 1;
  BUILTIN_READLN_INDEX = 2;
  BUILTIN_INTTOSTR_INDEX = 3;
  BUILTIN_STRTOINT_INDEX = 4;
  BUILTIN_FLOATTOSTR_INDEX = 5;
  BUILTIN_STRTOFLOAT_INDEX = 6;
  BUILTIN_ARRAYLENGTH_INDEX = 7;

{ TBytecodeGenResult }

constructor TBytecodeGenResult.Create(const ASuccess: Boolean; const AErrorMessage: string);
begin
  inherited Create();
  LSuccess := ASuccess;
  LErrorMessage := AErrorMessage;
  LProgram := TBytecodeProgram.Create();
  LBytecodeSize := 0;
end;

destructor TBytecodeGenResult.Destroy();
begin
  LProgram.Free();
  inherited Destroy();
end;

{ TBuiltinFunctionMap }

constructor TBuiltinFunctionMap.Create();
begin
  inherited Create();
  LFunctionMap := TDictionary<UTF8String, Integer>.Create();

  // Map function names to indices - updated for Print/PrintLn
  LFunctionMap.Add(UTF8String('PrintLn'), BUILTIN_WRITELN_INDEX);
  LFunctionMap.Add(UTF8String('Print'), BUILTIN_WRITE_INDEX);
  LFunctionMap.Add(UTF8String('ReadLn'), BUILTIN_READLN_INDEX);
  LFunctionMap.Add(UTF8String('IntToStr'), BUILTIN_INTTOSTR_INDEX);
  LFunctionMap.Add(UTF8String('StrToInt'), BUILTIN_STRTOINT_INDEX);
  LFunctionMap.Add(UTF8String('FloatToStr'), BUILTIN_FLOATTOSTR_INDEX);
  LFunctionMap.Add(UTF8String('StrToFloat'), BUILTIN_STRTOFLOAT_INDEX);
  LFunctionMap.Add(UTF8String('Length'), BUILTIN_ARRAYLENGTH_INDEX);
end;

destructor TBuiltinFunctionMap.Destroy();
begin
  LFunctionMap.Free();
  inherited Destroy();
end;

function TBuiltinFunctionMap.GetFunctionIndex(const AFunctionName: UTF8String): Integer;
begin
  if not LFunctionMap.TryGetValue(AFunctionName, Result) then
    Result := -1;
end;

function TBuiltinFunctionMap.IsFunctionBuiltin(const AFunctionName: UTF8String): Boolean;
begin
  Result := LFunctionMap.ContainsKey(AFunctionName);
end;

{ TLabelManager }

constructor TLabelManager.Create();
begin
  inherited Create();
  LLabelCounter := 0;
  LLabelMap := TDictionary<Integer, Integer>.Create();
end;

destructor TLabelManager.Destroy();
begin
  LLabelMap.Free();
  inherited Destroy();
end;

function TLabelManager.CreateLabel(): Integer;
begin
  Inc(LLabelCounter);
  Result := LLabelCounter;
end;

procedure TLabelManager.SetLabelPosition(const ALabelId: Integer; const AInstructionIndex: Integer);
begin
  LLabelMap.AddOrSetValue(ALabelId, AInstructionIndex);
end;

function TLabelManager.GetLabelPosition(const ALabelId: Integer): Integer;
begin
  if not LLabelMap.TryGetValue(ALabelId, Result) then
    Result := -1;
end;

procedure TLabelManager.Clear();
begin
  LLabelCounter := 0;
  LLabelMap.Clear();
end;

{ TBytecodeGenerator }

constructor TBytecodeGenerator.Create();
begin
  inherited Create();
  LProgram := TBytecodeProgram.Create();
  LBuiltinMap := TBuiltinFunctionMap.Create();
  LLabelManager := TLabelManager.Create();
  LErrorMessage := '';
  LHasError := False;
end;

destructor TBytecodeGenerator.Destroy();
begin
  LLabelManager.Free();
  LBuiltinMap.Free();
  LProgram.Free();
  inherited Destroy();
end;

function TBytecodeGenerator.Generate(const AAST: TProgramNode): TBytecodeGenResult;
begin
  // Reset state
  LProgram.Clear();
  LLabelManager.Clear();
  LErrorMessage := '';
  LHasError := False;

  try
    // Visit the AST to generate bytecode
    AAST.Accept(Self);

    // Patch all jump instructions
    PatchJumps();

    // Add final HALT instruction
    EmitInstruction(TInstructionFactory.CreateHalt());

    // Create result
    if LHasError then
    begin
      Result := TBytecodeGenResult.Create(False, LErrorMessage);
    end
    else
    begin
      Result := TBytecodeGenResult.Create(True);
      Result.LProgram.Free();
      Result.LProgram := LProgram;
      LProgram := TBytecodeProgram.Create(); // Create new instance for next use
      Result.LBytecodeSize := Result.LProgram.GetInstructionCount() * INSTRUCTION_SIZE;
    end;

  except
    on E: Exception do
    begin
      Result := TBytecodeGenResult.Create(False, 'Bytecode generation error: ' + E.Message);
    end;
  end;
end;

procedure TBytecodeGenerator.EmitInstruction(const AInstruction: TInstruction);
begin
  LProgram.AddInstruction(AInstruction);
end;

procedure TBytecodeGenerator.EmitLoadConst(const AValue: TValue);
var
  LConstIndex: Integer;
begin
  LConstIndex := LProgram.AddConstantValue(AValue);
  EmitInstruction(TInstructionFactory.CreateLoadConst(Cardinal(LConstIndex)));
end;

procedure TBytecodeGenerator.EmitLoadVar(const AVariableName: UTF8String);
var
  LVarIndex: Integer;
begin
  LVarIndex := GetOrCreateVariable(AVariableName);
  EmitInstruction(TInstructionFactory.CreateLoadVar(Cardinal(LVarIndex)));
end;

procedure TBytecodeGenerator.EmitStoreVar(const AVariableName: UTF8String);
var
  LVarIndex: Integer;
begin
  LVarIndex := GetOrCreateVariable(AVariableName);
  EmitInstruction(TInstructionFactory.CreateStoreVar(Cardinal(LVarIndex)));
end;

procedure TBytecodeGenerator.EmitArithmetic(const AOperation: TBinaryOpType);
begin
  case AOperation of
    btAdd: EmitInstruction(TInstructionFactory.CreateAdd());
    btSubtract: EmitInstruction(TInstructionFactory.CreateSub());
    btMultiply: EmitInstruction(TInstructionFactory.CreateMul());
    btDivide: EmitInstruction(TInstructionFactory.CreateDiv());
  else
    SetError('Unsupported arithmetic operation');
  end;
end;

procedure TBytecodeGenerator.EmitComparison(const AOperation: TComparisonOpType);
begin
  case AOperation of
    ctEqual: EmitInstruction(TInstructionFactory.CreateCmpEq());
    ctNotEqual: EmitInstruction(TInstructionFactory.CreateCmpNe());
    ctLess: EmitInstruction(TInstructionFactory.CreateCmpLt());
    ctLessEqual: EmitInstruction(TInstructionFactory.CreateCmpLe());
    ctGreater: EmitInstruction(TInstructionFactory.CreateCmpGt());
    ctGreaterEqual: EmitInstruction(TInstructionFactory.CreateCmpGe());
  else
    SetError('Unsupported comparison operation');
  end;
end;

procedure TBytecodeGenerator.EmitLogical(const AOperation: TLogicalOpType);
begin
  case AOperation of
    ltAnd: EmitInstruction(TInstructionFactory.CreateInstruction(OP_AND_BOOL));
    ltOr: EmitInstruction(TInstructionFactory.CreateInstruction(OP_OR_BOOL));
    ltNot: EmitInstruction(TInstructionFactory.CreateInstruction(OP_NOT_BOOL));
  else
    SetError('Unsupported logical operation');
  end;
end;

procedure TBytecodeGenerator.EmitBuiltinCall(const AFunctionName: UTF8String);
var
  LFunctionIndex: Integer;
begin
  LFunctionIndex := LBuiltinMap.GetFunctionIndex(AFunctionName);
  if LFunctionIndex = -1 then
  begin
    SetError('Unknown built-in function: ' + string(AFunctionName));
    Exit;
  end;

  EmitInstruction(TInstructionFactory.CreateCallBuiltin(Cardinal(LFunctionIndex)));
end;

procedure TBytecodeGenerator.EmitJump(const ALabelId: Integer);
begin
  // Emit jump with placeholder address - will be patched later
  EmitInstruction(TInstructionFactory.CreateJmp(Cardinal(ALabelId) or Cardinal($80000000))); // Use high bit to mark as label
end;

procedure TBytecodeGenerator.EmitJumpTrue(const ALabelId: Integer);
begin
  // Emit conditional jump with placeholder address - will be patched later
  EmitInstruction(TInstructionFactory.CreateJmpTrue(Cardinal(ALabelId) or Cardinal($80000000))); // Use high bit to mark as label
end;

procedure TBytecodeGenerator.EmitJumpFalse(const ALabelId: Integer);
begin
  // Emit conditional jump with placeholder address - will be patched later
  EmitInstruction(TInstructionFactory.CreateJmpFalse(Cardinal(ALabelId) or Cardinal($80000000))); // Use high bit to mark as label
end;

procedure TBytecodeGenerator.EmitArrayCreate(const AElementType: TValueType; const AStartIndex: Integer = 0; const AEndIndex: Integer = -1; const AIsDynamic: Boolean = False);
var
  LArrayValue: TValue;
begin
  // Create array value and emit it as a constant
  LArrayValue := TValueFactory.CreateArray(AElementType, AStartIndex, AEndIndex, AIsDynamic);
  EmitLoadConst(LArrayValue);
end;

procedure TBytecodeGenerator.EmitArrayAccess();
begin
  // Stack: [array] [index]
  // Result: [element]
  EmitInstruction(TInstructionFactory.CreateInstruction(OP_ARRAY_ACCESS));
end;

procedure TBytecodeGenerator.EmitArrayAssign();
begin
  // Stack: [array] [index] [value]
  // Result: [array] (modified)
  EmitInstruction(TInstructionFactory.CreateInstruction(OP_ARRAY_ASSIGN));
end;

procedure TBytecodeGenerator.EmitArrayLength();
begin
  // Stack: [array]
  // Result: [length]
  EmitInstruction(TInstructionFactory.CreateInstruction(OP_ARRAY_LENGTH));
end;

function TBytecodeGenerator.GetOrCreateVariable(const AVariableName: UTF8String): Integer;
begin
  Result := LProgram.GetVariableIndex(AVariableName);
  if Result = -1 then
    Result := LProgram.AddVariable(AVariableName);
end;

function TBytecodeGenerator.CreateConstantFromLiteral(const AValue: UTF8String; const AType: TPascalType): TValue;
begin
  case AType of
    ptInt: Result := TValue.CreateInt64(StrToInt64(string(AValue)));
    ptUInt: Result := TValue.CreateUInt64(StrToUInt64(string(AValue)));
    ptFloat: Result := TValue.CreateFloat64(StrToFloat(string(AValue)));
    ptString, ptPString:
    begin
      Result := TValue.CreateString(LProgram.AddString(AValue));
    end;
  else
    Result := TValue.CreateNone();
  end;
end;

function TBytecodeGenerator.PascalTypeToValueType(const APascalType: TPascalType): TValueType;
begin
  case APascalType of
    ptInt: Result := vtInt64;
    ptUInt: Result := vtUInt64;
    ptFloat: Result := vtFloat64;
    ptString, ptPString: Result := vtString;
  else
    Result := vtNone;
  end;
end;

procedure TBytecodeGenerator.PatchJumps();
var
  LIndex: Integer;
  LInstruction: TInstruction;
  LLabelId: Integer;
  LTargetAddress: Integer;
begin
  // Patch all jump instructions that reference labels
  for LIndex := 0 to LProgram.GetInstructionCount() - 1 do
  begin
    LInstruction := LProgram.GetInstruction(LIndex);

    // Check if this is a jump instruction with a label reference
    if (LInstruction.Operand1 and $80000000) <> 0 then
    begin
      LLabelId := Integer(LInstruction.Operand1 and $7FFFFFFF); // Remove high bit
      LTargetAddress := LLabelManager.GetLabelPosition(LLabelId);

      if LTargetAddress >= 0 then
      begin
        // Patch the instruction using the new SetInstruction method
        LInstruction.Operand1 := Cardinal(LTargetAddress);
        LProgram.SetInstruction(LIndex, LInstruction);
      end
      else
      begin
        SetError(Format('Undefined label: %d', [LLabelId]));
      end;
    end;
  end;
end;

procedure TBytecodeGenerator.SetError(const AMessage: string);
begin
  LErrorMessage := AMessage;
  LHasError := True;
end;

function TBytecodeGenerator.HasError(): Boolean;
begin
  Result := LHasError;
end;

{ IASTVisitor Implementation }

function TBytecodeGenerator.VisitProgram(const ANode: TProgramNode): Pointer;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('Program node is nil');
    Exit;
  end;

  // Visit variable section if present
  if Assigned(ANode.VarSection) then
    ANode.VarSection.Accept(Self);

  // Visit main block
  if Assigned(ANode.MainBlock) then
    ANode.MainBlock.Accept(Self);
end;

function TBytecodeGenerator.VisitBeginEnd(const ANode: TBeginEndNode): Pointer;
var
  LIndex: Integer;
  LStatement: TASTNode;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('BeginEnd node is nil');
    Exit;
  end;

  // Visit all statements
  for LIndex := 0 to ANode.GetStatementCount() - 1 do
  begin
    LStatement := ANode.GetStatement(LIndex);
    if Assigned(LStatement) then
      LStatement.Accept(Self);

    if HasError() then
      Break;
  end;
end;

function TBytecodeGenerator.VisitProcedureCall(const ANode: TProcedureCallNode): Pointer;
var
  LIndex: Integer;
  LArgument: TASTNode;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('ProcedureCall node is nil');
    Exit;
  end;

  // Generate code for arguments (in order, they'll be on stack)
  for LIndex := 0 to ANode.GetArgumentCount() - 1 do
  begin
    LArgument := ANode.GetArgument(LIndex);
    if Assigned(LArgument) then
      LArgument.Accept(Self);

    if HasError() then
      Exit;
  end;

  // Check if it's a built-in function
  if LBuiltinMap.IsFunctionBuiltin(ANode.ProcedureName) then
  begin
    EmitBuiltinCall(ANode.ProcedureName);
  end
  else
  begin
    SetError('Unknown procedure: ' + string(ANode.ProcedureName));
  end;
end;

function TBytecodeGenerator.VisitStringLiteral(const ANode: TStringLiteralNode): Pointer;
var
  LValue: TValue;
  LStringIndex: Integer;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('StringLiteral node is nil');
    Exit;
  end;

  // Create string value and emit load constant
  LStringIndex := LProgram.AddString(ANode.Value);
  LValue := TValue.CreateString(LStringIndex);
  EmitLoadConst(LValue);
end;

function TBytecodeGenerator.VisitIntegerLiteral(const ANode: TIntegerLiteralNode): Pointer;
var
  LValue: TValue;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('IntegerLiteral node is nil');
    Exit;
  end;

  // Create integer value and emit load constant
  LValue := TValue.CreateInt64(ANode.Value);
  EmitLoadConst(LValue);
end;

function TBytecodeGenerator.VisitFloatLiteral(const ANode: TFloatLiteralNode): Pointer;
var
  LValue: TValue;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('FloatLiteral node is nil');
    Exit;
  end;

  // Create float value and emit load constant
  LValue := TValue.CreateFloat64(ANode.Value);
  EmitLoadConst(LValue);
end;

function TBytecodeGenerator.VisitBooleanLiteral(const ANode: TBooleanLiteralNode): Pointer;
var
  LValue: TValue;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('BooleanLiteral node is nil');
    Exit;
  end;

  // Create boolean value and emit load constant
  LValue := TValue.CreateBoolean(ANode.Value);
  EmitLoadConst(LValue);
end;

function TBytecodeGenerator.VisitVariableReference(const ANode: TVariableReferenceNode): Pointer;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('VariableReference node is nil');
    Exit;
  end;

  // Emit load variable instruction
  EmitLoadVar(ANode.VariableName);
end;

function TBytecodeGenerator.VisitBinaryOp(const ANode: TBinaryOpNode): Pointer;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('BinaryOp node is nil');
    Exit;
  end;

  // Generate code for left operand
  if Assigned(ANode.LeftOperand) then
    ANode.LeftOperand.Accept(Self);

  if HasError() then
    Exit;

  // Generate code for right operand
  if Assigned(ANode.RightOperand) then
    ANode.RightOperand.Accept(Self);

  if HasError() then
    Exit;

  // Generate arithmetic operation
  EmitArithmetic(ANode.Operation);
end;

function TBytecodeGenerator.VisitComparison(const ANode: TComparisonNode): Pointer;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('Comparison node is nil');
    Exit;
  end;

  // Generate code for left operand
  if Assigned(ANode.LeftOperand) then
    ANode.LeftOperand.Accept(Self);

  if HasError() then
    Exit;

  // Generate code for right operand
  if Assigned(ANode.RightOperand) then
    ANode.RightOperand.Accept(Self);

  if HasError() then
    Exit;

  // Generate comparison operation
  EmitComparison(ANode.Operation);
end;

function TBytecodeGenerator.VisitLogicalOp(const ANode: TLogicalOpNode): Pointer;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('LogicalOp node is nil');
    Exit;
  end;

  // Handle unary NOT operation
  if ANode.Operation = ltNot then
  begin
    if Assigned(ANode.RightOperand) then
      ANode.RightOperand.Accept(Self);

    if HasError() then
      Exit;

    EmitLogical(ANode.Operation);
  end
  else
  begin
    // Handle binary AND/OR operations
    if Assigned(ANode.LeftOperand) then
      ANode.LeftOperand.Accept(Self);

    if HasError() then
      Exit;

    if Assigned(ANode.RightOperand) then
      ANode.RightOperand.Accept(Self);

    if HasError() then
      Exit;

    EmitLogical(ANode.Operation);
  end;
end;

function TBytecodeGenerator.VisitAssignment(const ANode: TAssignmentNode): Pointer;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('Assignment node is nil');
    Exit;
  end;

  // Handle array assignment vs simple variable assignment
  if ANode.Target is TArrayAccessNode then
  begin
    // Array assignment: arr[index] := value
    // Generate array expression
    TArrayAccessNode(ANode.Target).ArrayExpression.Accept(Self);

    // Generate all indices (for multi-dimensional arrays)
    var LArrayAccess := TArrayAccessNode(ANode.Target);
    var LIndex: Integer;
    for LIndex := 0 to LArrayAccess.GetIndexCount() - 1 do
    begin
      LArrayAccess.GetIndex(LIndex).Accept(Self);
    end;

    // Generate value expression
    if Assigned(ANode.Expression) then
      ANode.Expression.Accept(Self);

    if HasError() then
      Exit;

    // Emit array assignment instruction
    EmitArrayAssign();
  end
  else if ANode.Target is TVariableReferenceNode then
  begin
    // Simple variable assignment: var := value
    // Generate code for expression (right side)
    if Assigned(ANode.Expression) then
      ANode.Expression.Accept(Self);

    if HasError() then
      Exit;

    // Store result in variable
    EmitStoreVar(TVariableReferenceNode(ANode.Target).VariableName);
  end
  else
  begin
    SetError('Invalid assignment target');
  end;
end;

function TBytecodeGenerator.VisitIf(const ANode: TIfNode): Pointer;
var
  LElseLabel: Integer;
  LEndLabel: Integer;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('If node is nil');
    Exit;
  end;

  // Generate condition
  if Assigned(ANode.Condition) then
    ANode.Condition.Accept(Self);

  if HasError() then
    Exit;

  // Create labels for control flow
  LElseLabel := LLabelManager.CreateLabel();
  LEndLabel := LLabelManager.CreateLabel();

  // Jump to else if condition is false
  EmitJumpFalse(LElseLabel);

  // Generate then statement
  if Assigned(ANode.ThenStatement) then
    ANode.ThenStatement.Accept(Self);

  if HasError() then
    Exit;

  // Jump to end (skip else part)
  if Assigned(ANode.ElseStatement) then
    EmitJump(LEndLabel);

  // Set else label position
  LLabelManager.SetLabelPosition(LElseLabel, LProgram.GetInstructionCount());

  // Generate else statement if present
  if Assigned(ANode.ElseStatement) then
  begin
    ANode.ElseStatement.Accept(Self);

    if HasError() then
      Exit;

    // Set end label position
    LLabelManager.SetLabelPosition(LEndLabel, LProgram.GetInstructionCount());
  end;
end;

function TBytecodeGenerator.VisitWhile(const ANode: TWhileNode): Pointer;
var
  LStartLabel: Integer;
  LEndLabel: Integer;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('While node is nil');
    Exit;
  end;

  // Create labels
  LStartLabel := LLabelManager.CreateLabel();
  LEndLabel := LLabelManager.CreateLabel();

  // Set start label position
  LLabelManager.SetLabelPosition(LStartLabel, LProgram.GetInstructionCount());

  // Generate condition
  if Assigned(ANode.Condition) then
    ANode.Condition.Accept(Self);

  if HasError() then
    Exit;

  // Jump to end if condition is false
  EmitJumpFalse(LEndLabel);

  // Generate loop body
  if Assigned(ANode.Statement) then
    ANode.Statement.Accept(Self);

  if HasError() then
    Exit;

  // Jump back to start
  EmitJump(LStartLabel);

  // Set end label position
  LLabelManager.SetLabelPosition(LEndLabel, LProgram.GetInstructionCount());
end;

function TBytecodeGenerator.VisitFor(const ANode: TForNode): Pointer;
var
  LStartLabel: Integer;
  LEndLabel: Integer;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('For node is nil');
    Exit;
  end;

  // Generate start expression and store in loop variable
  if Assigned(ANode.StartExpression) then
    ANode.StartExpression.Accept(Self);

  if HasError() then
    Exit;

  EmitStoreVar(ANode.VariableName);

  // Create labels
  LStartLabel := LLabelManager.CreateLabel();
  LEndLabel := LLabelManager.CreateLabel();

  // Set start label position
  LLabelManager.SetLabelPosition(LStartLabel, LProgram.GetInstructionCount());

  // Load loop variable and end value for comparison
  EmitLoadVar(ANode.VariableName);

  if Assigned(ANode.EndExpression) then
    ANode.EndExpression.Accept(Self);

  if HasError() then
    Exit;

  // Compare: if variable > end then exit
  EmitComparison(ctGreater);
  EmitJumpTrue(LEndLabel);

  // Generate loop body
  if Assigned(ANode.Statement) then
    ANode.Statement.Accept(Self);

  if HasError() then
    Exit;

  // Increment loop variable
  EmitLoadVar(ANode.VariableName);
  EmitLoadConst(TValue.CreateInt64(1));
  EmitArithmetic(btAdd);
  EmitStoreVar(ANode.VariableName);

  // Jump back to start
  EmitJump(LStartLabel);

  // Set end label position
  LLabelManager.SetLabelPosition(LEndLabel, LProgram.GetInstructionCount());
end;

function TBytecodeGenerator.VisitType(const ANode: TTypeNode): Pointer;
begin
  Result := nil;
  // Type nodes don't generate code - they're used for declarations
end;

function TBytecodeGenerator.VisitArrayType(const ANode: TArrayTypeNode): Pointer;
begin
  Result := nil;
  // Array type nodes are handled during variable declarations
  // No runtime code is generated for type definitions
end;

function TBytecodeGenerator.VisitArrayAccess(const ANode: TArrayAccessNode): Pointer;
var
  LIndex: Integer;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('ArrayAccess node is nil');
    Exit;
  end;

  // Generate code for array expression
  if Assigned(ANode.ArrayExpression) then
    ANode.ArrayExpression.Accept(Self);

  if HasError() then
    Exit;

  // Generate code for all indices (for multi-dimensional arrays)
  for LIndex := 0 to ANode.GetIndexCount() - 1 do
  begin
    ANode.GetIndex(LIndex).Accept(Self);
    if HasError() then
      Exit;
  end;

  // For now, only support single-dimensional arrays
  if ANode.GetIndexCount() = 1 then
  begin
    EmitArrayAccess();
  end
  else
  begin
    SetError('Multi-dimensional arrays not yet supported');
  end;
end;

function TBytecodeGenerator.VisitArrayLiteral(const ANode: TArrayLiteralNode): Pointer;
var
  LIndex: Integer;
  LElementCount: Integer;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('ArrayLiteral node is nil');
    Exit;
  end;

  LElementCount := ANode.GetElementCount();

  if LElementCount > 0 then
  begin
    // Create static array with proper size for array literals
    // Array literals have known size, so use static arrays
    EmitArrayCreate(vtInt64, 0, LElementCount - 1, False);

    // Now stack has: [array]
    // Assign each element to the array
    for LIndex := 0 to LElementCount - 1 do
    begin
      // Duplicate array reference for this assignment
      EmitInstruction(TInstructionFactory.CreateDup());
      // Stack: [array] [array]

      // Push the index
      EmitLoadConst(TValue.CreateInt64(LIndex));
      // Stack: [array] [array] [index]

      // Generate the element value
      ANode.GetElement(LIndex).Accept(Self);
      if HasError() then Exit;
      // Stack: [array] [array] [index] [element]

      // Assign element to array (this consumes array, index, element and returns modified array)
      EmitArrayAssign();
      // Stack: [array] [modified_array]

      // Pop the modified array result (we keep the original array reference)
      EmitInstruction(TInstructionFactory.CreatePop());
      // Stack: [array]
    end;
    // Final stack: [array] (the populated array)
  end
  else
  begin
    // Empty array literal - create dynamic array
    EmitArrayCreate(vtInt64, 0, -1, True);
  end;
end;

function TBytecodeGenerator.VisitVarDecl(const ANode: TVarDeclNode): Pointer;
var
  LTypeNode: TASTNode;
  LArrayTypeNode: TArrayTypeNode;
  LElementType: TValueType;
  LStartIndex: Integer;
  LEndIndex: Integer;
  LArrayValue: TValue;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('VarDecl node is nil');
    Exit;
  end;

  // Ensure variable exists in variable table
  GetOrCreateVariable(ANode.VariableName);

  // Check if this is an array type declaration
  LTypeNode := ANode.TypeNode;
  if LTypeNode is TArrayTypeNode then
  begin
    LArrayTypeNode := TArrayTypeNode(LTypeNode);

    // Determine element type
    if LArrayTypeNode.ElementType is TTypeNode then
    begin
      LElementType := PascalTypeToValueType(TTypeNode(LArrayTypeNode.ElementType).PascalType);
    end
    else
    begin
      SetError('Unsupported array element type');
      Exit;
    end;

    // Initialize array based on type
    if LArrayTypeNode.ArrayType = atStatic then
    begin
      // Static array: evaluate start and end indices
      if Assigned(LArrayTypeNode.StartIndex) then
      begin
        // For now, assume literal integer indices
        if LArrayTypeNode.StartIndex is TIntegerLiteralNode then
          LStartIndex := TIntegerLiteralNode(LArrayTypeNode.StartIndex).Value
        else
        begin
          SetError('Non-literal array bounds not supported yet');
          Exit;
        end;
      end
      else
        LStartIndex := 0;

      if Assigned(LArrayTypeNode.EndIndex) then
      begin
        if LArrayTypeNode.EndIndex is TIntegerLiteralNode then
          LEndIndex := TIntegerLiteralNode(LArrayTypeNode.EndIndex).Value
        else
        begin
          SetError('Non-literal array bounds not supported yet');
          Exit;
        end;
      end
      else
        LEndIndex := -1;

      // Create static array
      LArrayValue := TValueFactory.CreateArray(LElementType, LStartIndex, LEndIndex, False);
    end
    else
    begin
      // Dynamic array: starts empty
      LArrayValue := TValueFactory.CreateArray(LElementType, 0, -1, True);
    end;

    // Store the array in the variable
    EmitLoadConst(LArrayValue);
    EmitStoreVar(ANode.VariableName);
  end;

  // Simple variable declarations don't generate runtime code
  // The variable is just registered for later use
end;

function TBytecodeGenerator.VisitVarSection(const ANode: TVarSectionNode): Pointer;
var
  LIndex: Integer;
  LDeclaration: TVarDeclNode;
begin
  Result := nil;

  if not Assigned(ANode) then
  begin
    SetError('VarSection node is nil');
    Exit;
  end;

  // Visit all variable declarations
  for LIndex := 0 to ANode.GetDeclarationCount() - 1 do
  begin
    LDeclaration := ANode.GetDeclaration(LIndex);
    if Assigned(LDeclaration) then
      LDeclaration.Accept(Self);

    if HasError() then
      Break;
  end;
end;

end.
