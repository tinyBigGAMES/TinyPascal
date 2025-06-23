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

unit TinyPascal.AST;

{$I TinyPascal.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  TinyPascal.Lexer;

type
  // Binary operation types
  TBinaryOpType = (
    btAdd,       // +
    btSubtract,  // -
    btMultiply,  // *
    btDivide     // /
  );

  // Comparison operation types
  TComparisonOpType = (
    ctEqual,        // =
    ctNotEqual,     // <>
    ctLess,         // <
    ctLessEqual,    // <=
    ctGreater,      // >
    ctGreaterEqual  // >=
  );

  // Logical operation types
  TLogicalOpType = (
    ltAnd,    // and
    ltOr,     // or
    ltNot     // not
  );

  // Forward declarations
  TASTNode = class;
  TProgramNode = class;
  TBeginEndNode = class;
  TProcedureCallNode = class;
  TStringLiteralNode = class;
  TIntegerLiteralNode = class;
  TFloatLiteralNode = class;
  TBooleanLiteralNode = class;
  TVariableReferenceNode = class;
  TBinaryOpNode = class;
  TComparisonNode = class;
  TLogicalOpNode = class;
  TAssignmentNode = class;
  TIfNode = class;
  TWhileNode = class;
  TForNode = class;
  TTypeNode = class;
  TVarDeclNode = class;
  TVarSectionNode = class;
  IASTVisitor = interface;

  // Base AST node class
  TASTNode = class abstract
  private
    LLine: Integer;
    LColumn: Integer;
    LParent: TASTNode;
    LChildren: TObjectList<TASTNode>;

  protected
    procedure AddChild(const AChild: TASTNode);
    procedure SetParent(const AParent: TASTNode);

  public
    constructor Create(const ALine, AColumn: Integer);
    destructor Destroy(); override;

    function Accept(const AVisitor: IASTVisitor): Pointer; virtual; abstract;

    property Line: Integer read LLine;
    property Column: Integer read LColumn;
    property Parent: TASTNode read LParent;
    property Children: TObjectList<TASTNode> read LChildren;
  end;

  // Type node: Int, Float, UInt, String, PString
  TTypeNode = class(TASTNode)
  private
    LPascalType: TPascalType;

  public
    constructor Create(const ALine, AColumn: Integer; const APascalType: TPascalType);

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    property PascalType: TPascalType read LPascalType;
  end;

  // Variable declaration node: identifier: Type
  TVarDeclNode = class(TASTNode)
  private
    LVariableName: UTF8String;
    LTypeNode: TTypeNode;

  public
    constructor Create(const ALine, AColumn: Integer; const AVariableName: UTF8String; const ATypeNode: TTypeNode);
    destructor Destroy(); override;

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    property VariableName: UTF8String read LVariableName;
    property TypeNode: TTypeNode read LTypeNode;
  end;

  // Variable section node: var ... declarations
  TVarSectionNode = class(TASTNode)
  private
    LDeclarations: TObjectList<TVarDeclNode>;

  public
    constructor Create(const ALine, AColumn: Integer);
    destructor Destroy(); override;

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    procedure AddDeclaration(const ADeclaration: TVarDeclNode);
    function GetDeclarationCount(): Integer;
    function GetDeclaration(const AIndex: Integer): TVarDeclNode;

    property Declarations: TObjectList<TVarDeclNode> read LDeclarations;
  end;

  // Program node: program HelloWorld;
  TProgramNode = class(TASTNode)
  private
    LProgramName: UTF8String;
    LVarSection: TVarSectionNode;
    LMainBlock: TBeginEndNode;

  public
    constructor Create(const ALine, AColumn: Integer; const AProgramName: UTF8String);
    destructor Destroy(); override;

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    procedure SetVarSection(const AVarSection: TVarSectionNode);
    procedure SetMainBlock(const AMainBlock: TBeginEndNode);

    property ProgramName: UTF8String read LProgramName;
    property VarSection: TVarSectionNode read LVarSection;
    property MainBlock: TBeginEndNode read LMainBlock;
  end;

  // Begin/End block node
  TBeginEndNode = class(TASTNode)
  private
    LStatements: TObjectList<TASTNode>;

  public
    constructor Create(const ALine, AColumn: Integer);
    destructor Destroy(); override;

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    procedure AddStatement(const AStatement: TASTNode);
    function GetStatementCount(): Integer;
    function GetStatement(const AIndex: Integer): TASTNode;

    property Statements: TObjectList<TASTNode> read LStatements;
  end;

  // Procedure call node: WriteLn(...)
  TProcedureCallNode = class(TASTNode)
  private
    LProcedureName: UTF8String;
    LArguments: TObjectList<TASTNode>;

  public
    constructor Create(const ALine, AColumn: Integer; const AProcedureName: UTF8String);
    destructor Destroy(); override;

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    procedure AddArgument(const AArgument: TASTNode);
    function GetArgumentCount(): Integer;
    function GetArgument(const AIndex: Integer): TASTNode;

    property ProcedureName: UTF8String read LProcedureName;
    property Arguments: TObjectList<TASTNode> read LArguments;
  end;

  // String literal node: 'Hello, World!'
  TStringLiteralNode = class(TASTNode)
  private
    LValue: UTF8String;

  public
    constructor Create(const ALine, AColumn: Integer; const AValue: UTF8String);

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    property Value: UTF8String read LValue;
  end;

  // Integer literal node: 42, -13, etc.
  TIntegerLiteralNode = class(TASTNode)
  private
    LValue: Int64;

  public
    constructor Create(const ALine, AColumn: Integer; const AValue: Int64);

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    property Value: Int64 read LValue;
  end;

  // Float literal node: 3.14, -2.5, etc.
  TFloatLiteralNode = class(TASTNode)
  private
    LValue: Double;

  public
    constructor Create(const ALine, AColumn: Integer; const AValue: Double);

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    property Value: Double read LValue;
  end;

  // Boolean literal node: true, false
  TBooleanLiteralNode = class(TASTNode)
  private
    LValue: Boolean;

  public
    constructor Create(const ALine, AColumn: Integer; const AValue: Boolean);

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    property Value: Boolean read LValue;
  end;

  // Variable reference node: x, myVar, etc.
  TVariableReferenceNode = class(TASTNode)
  private
    LVariableName: UTF8String;

  public
    constructor Create(const ALine, AColumn: Integer; const AVariableName: UTF8String);

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    property VariableName: UTF8String read LVariableName;
  end;

  // Binary operation node: x + y, a * b, etc.
  TBinaryOpNode = class(TASTNode)
  private
    LOperation: TBinaryOpType;
    LLeftOperand: TASTNode;
    LRightOperand: TASTNode;

  public
    constructor Create(const ALine, AColumn: Integer; const AOperation: TBinaryOpType; const ALeftOperand, ARightOperand: TASTNode);
    destructor Destroy(); override;

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    property Operation: TBinaryOpType read LOperation;
    property LeftOperand: TASTNode read LLeftOperand;
    property RightOperand: TASTNode read LRightOperand;
  end;

  // Comparison node: x > y, a = b, etc.
  TComparisonNode = class(TASTNode)
  private
    LOperation: TComparisonOpType;
    LLeftOperand: TASTNode;
    LRightOperand: TASTNode;

  public
    constructor Create(const ALine, AColumn: Integer; const AOperation: TComparisonOpType; const ALeftOperand, ARightOperand: TASTNode);
    destructor Destroy(); override;

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    property Operation: TComparisonOpType read LOperation;
    property LeftOperand: TASTNode read LLeftOperand;
    property RightOperand: TASTNode read LRightOperand;
  end;

  // Logical operation node: x and y, not z, etc.
  TLogicalOpNode = class(TASTNode)
  private
    LOperation: TLogicalOpType;
    LLeftOperand: TASTNode;  // nil for unary operations like 'not'
    LRightOperand: TASTNode;

  public
    constructor Create(const ALine, AColumn: Integer; const AOperation: TLogicalOpType; const ALeftOperand, ARightOperand: TASTNode);
    destructor Destroy(); override;

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    property Operation: TLogicalOpType read LOperation;
    property LeftOperand: TASTNode read LLeftOperand;
    property RightOperand: TASTNode read LRightOperand;
  end;

  // Assignment node: x := expression
  TAssignmentNode = class(TASTNode)
  private
    LVariableName: UTF8String;
    LExpression: TASTNode;

  public
    constructor Create(const ALine, AColumn: Integer; const AVariableName: UTF8String; const AExpression: TASTNode);
    destructor Destroy(); override;

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    property VariableName: UTF8String read LVariableName;
    property Expression: TASTNode read LExpression;
  end;

  // If statement node: if condition then statement [else statement]
  TIfNode = class(TASTNode)
  private
    LCondition: TASTNode;
    LThenStatement: TASTNode;
    LElseStatement: TASTNode; // Can be nil

  public
    constructor Create(const ALine, AColumn: Integer; const ACondition, AThenStatement: TASTNode; const AElseStatement: TASTNode = nil);
    destructor Destroy(); override;

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    property Condition: TASTNode read LCondition;
    property ThenStatement: TASTNode read LThenStatement;
    property ElseStatement: TASTNode read LElseStatement;
  end;

  // While loop node: while condition do statement
  TWhileNode = class(TASTNode)
  private
    LCondition: TASTNode;
    LStatement: TASTNode;

  public
    constructor Create(const ALine, AColumn: Integer; const ACondition, AStatement: TASTNode);
    destructor Destroy(); override;

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    property Condition: TASTNode read LCondition;
    property Statement: TASTNode read LStatement;
  end;

  // For loop node: for variable := start to end do statement
  TForNode = class(TASTNode)
  private
    LVariableName: UTF8String;
    LStartExpression: TASTNode;
    LEndExpression: TASTNode;
    LStatement: TASTNode;

  public
    constructor Create(const ALine, AColumn: Integer; const AVariableName: UTF8String; const AStartExpression, AEndExpression, AStatement: TASTNode);
    destructor Destroy(); override;

    function Accept(const AVisitor: IASTVisitor): Pointer; override;

    property VariableName: UTF8String read LVariableName;
    property StartExpression: TASTNode read LStartExpression;
    property EndExpression: TASTNode read LEndExpression;
    property Statement: TASTNode read LStatement;
  end;

  // Visitor interface for tree traversal
  IASTVisitor = interface
    ['{12345678-1234-1234-1234-123456789ABC}']
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
    function VisitVarDecl(const ANode: TVarDeclNode): Pointer;
    function VisitVarSection(const ANode: TVarSectionNode): Pointer;
  end;

implementation

{ TASTNode }

constructor TASTNode.Create(const ALine, AColumn: Integer);
begin
  inherited Create();
  LLine := ALine;
  LColumn := AColumn;
  LParent := nil;
  LChildren := TObjectList<TASTNode>.Create(False); // Don't own children - specific lists own them
end;

destructor TASTNode.Destroy();
begin
  LChildren.Free();
  inherited Destroy();
end;

procedure TASTNode.AddChild(const AChild: TASTNode);
begin
  if Assigned(AChild) then
  begin
    LChildren.Add(AChild);
    AChild.SetParent(Self);
  end;
end;

procedure TASTNode.SetParent(const AParent: TASTNode);
begin
  LParent := AParent;
end;

{ TTypeNode }

constructor TTypeNode.Create(const ALine, AColumn: Integer; const APascalType: TPascalType);
begin
  inherited Create(ALine, AColumn);
  LPascalType := APascalType;
end;

function TTypeNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitType(Self);
end;

{ TVarDeclNode }

constructor TVarDeclNode.Create(const ALine, AColumn: Integer; const AVariableName: UTF8String; const ATypeNode: TTypeNode);
begin
  inherited Create(ALine, AColumn);
  LVariableName := AVariableName;
  LTypeNode := ATypeNode;
  if Assigned(ATypeNode) then
    AddChild(ATypeNode);
end;

destructor TVarDeclNode.Destroy();
begin
  LTypeNode.Free();
  inherited Destroy();
end;

function TVarDeclNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitVarDecl(Self);
end;

{ TVarSectionNode }

constructor TVarSectionNode.Create(const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  LDeclarations := TObjectList<TVarDeclNode>.Create(True); // Owns declarations
end;

destructor TVarSectionNode.Destroy();
begin
  LDeclarations.Free();
  inherited Destroy();
end;

function TVarSectionNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitVarSection(Self);
end;

procedure TVarSectionNode.AddDeclaration(const ADeclaration: TVarDeclNode);
begin
  if Assigned(ADeclaration) then
  begin
    LDeclarations.Add(ADeclaration);
    AddChild(ADeclaration);
  end;
end;

function TVarSectionNode.GetDeclarationCount(): Integer;
begin
  Result := LDeclarations.Count;
end;

function TVarSectionNode.GetDeclaration(const AIndex: Integer): TVarDeclNode;
begin
  Result := LDeclarations[AIndex];
end;

{ TProgramNode }

constructor TProgramNode.Create(const ALine, AColumn: Integer; const AProgramName: UTF8String);
begin
  inherited Create(ALine, AColumn);
  LProgramName := AProgramName;
  LVarSection := nil;
  LMainBlock := nil;
end;

destructor TProgramNode.Destroy();
begin
  LVarSection.Free();
  LMainBlock.Free();
  inherited Destroy();
end;

function TProgramNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitProgram(Self);
end;

procedure TProgramNode.SetVarSection(const AVarSection: TVarSectionNode);
begin
  LVarSection := AVarSection;
  if Assigned(AVarSection) then
    AddChild(AVarSection);
end;

procedure TProgramNode.SetMainBlock(const AMainBlock: TBeginEndNode);
begin
  LMainBlock := AMainBlock;
  if Assigned(AMainBlock) then
    AddChild(AMainBlock);
end;

{ TBeginEndNode }

constructor TBeginEndNode.Create(const ALine, AColumn: Integer);
begin
  inherited Create(ALine, AColumn);
  LStatements := TObjectList<TASTNode>.Create(True); // Owns statements
end;

destructor TBeginEndNode.Destroy();
begin
  LStatements.Free();
  inherited Destroy();
end;

function TBeginEndNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitBeginEnd(Self);
end;

procedure TBeginEndNode.AddStatement(const AStatement: TASTNode);
begin
  if Assigned(AStatement) then
  begin
    LStatements.Add(AStatement);
    AddChild(AStatement);
  end;
end;

function TBeginEndNode.GetStatementCount(): Integer;
begin
  Result := LStatements.Count;
end;

function TBeginEndNode.GetStatement(const AIndex: Integer): TASTNode;
begin
  Result := LStatements[AIndex];
end;

{ TProcedureCallNode }

constructor TProcedureCallNode.Create(const ALine, AColumn: Integer; const AProcedureName: UTF8String);
begin
  inherited Create(ALine, AColumn);
  LProcedureName := AProcedureName;
  LArguments := TObjectList<TASTNode>.Create(True); // Owns arguments
end;

destructor TProcedureCallNode.Destroy();
begin
  LArguments.Free();
  inherited Destroy();
end;

function TProcedureCallNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitProcedureCall(Self);
end;

procedure TProcedureCallNode.AddArgument(const AArgument: TASTNode);
begin
  if Assigned(AArgument) then
  begin
    LArguments.Add(AArgument);
    AddChild(AArgument);
  end;
end;

function TProcedureCallNode.GetArgumentCount(): Integer;
begin
  Result := LArguments.Count;
end;

function TProcedureCallNode.GetArgument(const AIndex: Integer): TASTNode;
begin
  Result := LArguments[AIndex];
end;

{ TStringLiteralNode }

constructor TStringLiteralNode.Create(const ALine, AColumn: Integer; const AValue: UTF8String);
begin
  inherited Create(ALine, AColumn);
  LValue := AValue;
end;

function TStringLiteralNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitStringLiteral(Self);
end;

{ TIntegerLiteralNode }

constructor TIntegerLiteralNode.Create(const ALine, AColumn: Integer; const AValue: Int64);
begin
  inherited Create(ALine, AColumn);
  LValue := AValue;
end;

function TIntegerLiteralNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitIntegerLiteral(Self);
end;

{ TFloatLiteralNode }

constructor TFloatLiteralNode.Create(const ALine, AColumn: Integer; const AValue: Double);
begin
  inherited Create(ALine, AColumn);
  LValue := AValue;
end;

function TFloatLiteralNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitFloatLiteral(Self);
end;

{ TBooleanLiteralNode }

constructor TBooleanLiteralNode.Create(const ALine, AColumn: Integer; const AValue: Boolean);
begin
  inherited Create(ALine, AColumn);
  LValue := AValue;
end;

function TBooleanLiteralNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitBooleanLiteral(Self);
end;

{ TVariableReferenceNode }

constructor TVariableReferenceNode.Create(const ALine, AColumn: Integer; const AVariableName: UTF8String);
begin
  inherited Create(ALine, AColumn);
  LVariableName := AVariableName;
end;

function TVariableReferenceNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitVariableReference(Self);
end;

{ TBinaryOpNode }

constructor TBinaryOpNode.Create(const ALine, AColumn: Integer; const AOperation: TBinaryOpType; const ALeftOperand, ARightOperand: TASTNode);
begin
  inherited Create(ALine, AColumn);
  LOperation := AOperation;
  LLeftOperand := ALeftOperand;
  LRightOperand := ARightOperand;
  if Assigned(ALeftOperand) then
    AddChild(ALeftOperand);
  if Assigned(ARightOperand) then
    AddChild(ARightOperand);
end;

destructor TBinaryOpNode.Destroy();
begin
  LLeftOperand.Free();
  LRightOperand.Free();
  inherited Destroy();
end;

function TBinaryOpNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitBinaryOp(Self);
end;

{ TComparisonNode }

constructor TComparisonNode.Create(const ALine, AColumn: Integer; const AOperation: TComparisonOpType; const ALeftOperand, ARightOperand: TASTNode);
begin
  inherited Create(ALine, AColumn);
  LOperation := AOperation;
  LLeftOperand := ALeftOperand;
  LRightOperand := ARightOperand;
  if Assigned(ALeftOperand) then
    AddChild(ALeftOperand);
  if Assigned(ARightOperand) then
    AddChild(ARightOperand);
end;

destructor TComparisonNode.Destroy();
begin
  LLeftOperand.Free();
  LRightOperand.Free();
  inherited Destroy();
end;

function TComparisonNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitComparison(Self);
end;

{ TLogicalOpNode }

constructor TLogicalOpNode.Create(const ALine, AColumn: Integer; const AOperation: TLogicalOpType; const ALeftOperand, ARightOperand: TASTNode);
begin
  inherited Create(ALine, AColumn);
  LOperation := AOperation;
  LLeftOperand := ALeftOperand;
  LRightOperand := ARightOperand;
  if Assigned(ALeftOperand) then
    AddChild(ALeftOperand);
  if Assigned(ARightOperand) then
    AddChild(ARightOperand);
end;

destructor TLogicalOpNode.Destroy();
begin
  LLeftOperand.Free();
  LRightOperand.Free();
  inherited Destroy();
end;

function TLogicalOpNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitLogicalOp(Self);
end;

{ TAssignmentNode }

constructor TAssignmentNode.Create(const ALine, AColumn: Integer; const AVariableName: UTF8String; const AExpression: TASTNode);
begin
  inherited Create(ALine, AColumn);
  LVariableName := AVariableName;
  LExpression := AExpression;
  if Assigned(AExpression) then
    AddChild(AExpression);
end;

destructor TAssignmentNode.Destroy();
begin
  LExpression.Free();
  inherited Destroy();
end;

function TAssignmentNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitAssignment(Self);
end;

{ TIfNode }

constructor TIfNode.Create(const ALine, AColumn: Integer; const ACondition, AThenStatement: TASTNode; const AElseStatement: TASTNode = nil);
begin
  inherited Create(ALine, AColumn);
  LCondition := ACondition;
  LThenStatement := AThenStatement;
  LElseStatement := AElseStatement;
  if Assigned(ACondition) then
    AddChild(ACondition);
  if Assigned(AThenStatement) then
    AddChild(AThenStatement);
  if Assigned(AElseStatement) then
    AddChild(AElseStatement);
end;

destructor TIfNode.Destroy();
begin
  LCondition.Free();
  LThenStatement.Free();
  LElseStatement.Free();
  inherited Destroy();
end;

function TIfNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitIf(Self);
end;

{ TWhileNode }

constructor TWhileNode.Create(const ALine, AColumn: Integer; const ACondition, AStatement: TASTNode);
begin
  inherited Create(ALine, AColumn);
  LCondition := ACondition;
  LStatement := AStatement;
  if Assigned(ACondition) then
    AddChild(ACondition);
  if Assigned(AStatement) then
    AddChild(AStatement);
end;

destructor TWhileNode.Destroy();
begin
  LCondition.Free();
  LStatement.Free();
  inherited Destroy();
end;

function TWhileNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitWhile(Self);
end;

{ TForNode }

constructor TForNode.Create(const ALine, AColumn: Integer; const AVariableName: UTF8String; const AStartExpression, AEndExpression, AStatement: TASTNode);
begin
  inherited Create(ALine, AColumn);
  LVariableName := AVariableName;
  LStartExpression := AStartExpression;
  LEndExpression := AEndExpression;
  LStatement := AStatement;
  if Assigned(AStartExpression) then
    AddChild(AStartExpression);
  if Assigned(AEndExpression) then
    AddChild(AEndExpression);
  if Assigned(AStatement) then
    AddChild(AStatement);
end;

destructor TForNode.Destroy();
begin
  LStartExpression.Free();
  LEndExpression.Free();
  LStatement.Free();
  inherited Destroy();
end;

function TForNode.Accept(const AVisitor: IASTVisitor): Pointer;
begin
  Result := AVisitor.VisitFor(Self);
end;

end.
