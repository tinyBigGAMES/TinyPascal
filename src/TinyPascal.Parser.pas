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

unit TinyPascal.Parser;

{$I TinyPascal.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.TypInfo,
  TinyPascal.Lexer,
  TinyPascal.AST;

type
  EParseError = class(Exception)
  private
    LLine: Integer;
    LColumn: Integer;

  public
    constructor Create(const AMessage: String; const ALine, AColumn: Integer);

    property Line: Integer read LLine;
    property Column: Integer read LColumn;
  end;

  TParser = class
  private
    LLexer: TLexer;
    LCurrentToken: TToken;
    LOwnsLexer: Boolean;

    procedure NextToken();
    procedure ExpectToken(const ATokenType: TTokenType; const AMessage: UTF8String);
    function CheckToken(const ATokenType: TTokenType): Boolean;

    procedure RaiseParseError(const AMessage: UTF8String);

    // Parsing methods
    function ParseProgram(): TProgramNode;
    function ParseVarSection(): TVarSectionNode;
    function ParseVarDecl(): TVarDeclNode;
    function ParseType(): TASTNode;
    function ParseArrayType(): TArrayTypeNode;
    function ParseBeginEnd(): TBeginEndNode;
    function ParseStatement(): TASTNode;
    function ParseAssignment(const ATarget: TASTNode): TAssignmentNode;
    function ParseIfStatement(): TIfNode;
    function ParseWhileStatement(): TWhileNode;
    function ParseForStatement(): TForNode;
    function ParseExpression(): TASTNode;
    function ParseLogicalOrExpression(): TASTNode;
    function ParseLogicalAndExpression(): TASTNode;
    function ParseComparisonExpression(): TASTNode;
    function ParseAddExpression(): TASTNode;
    function ParseMulExpression(): TASTNode;
    function ParseUnaryExpression(): TASTNode;
    function ParsePrimaryExpression(): TASTNode;
    function ParsePostfixExpression(): TASTNode;
    function ParseArrayAccess(const AArrayExpression: TASTNode): TArrayAccessNode;
    function ParseArrayLiteral(): TArrayLiteralNode;
    function ParseProcedureCall(): TProcedureCallNode;
    function ParseStringLiteral(): TStringLiteralNode;
    function ParseIntegerLiteral(): TIntegerLiteralNode;
    function ParseFloatLiteral(): TFloatLiteralNode;
    function ParseBooleanLiteral(): TBooleanLiteralNode;
    function ParseVariableReference(): TVariableReferenceNode;

    // Helper methods
    function IsIdentifierAProcedure(const AIdentifier: UTF8String): Boolean;
    function TokenTypeToBinaryOp(const ATokenType: TTokenType): TBinaryOpType;
    function TokenTypeToComparisonOp(const ATokenType: TTokenType): TComparisonOpType;
    function TokenTypeToLogicalOp(const ATokenType: TTokenType): TLogicalOpType;

  public
    constructor Create(const ALexer: TLexer; const AOwnsLexer: Boolean = False);
    constructor CreateFromSource(const ASource: UTF8String);
    destructor Destroy(); override;

    function Parse(): TProgramNode;

    property CurrentToken: TToken read LCurrentToken;
  end;

implementation

{ EParseError }

constructor EParseError.Create(const AMessage: String; const ALine, AColumn: Integer);
begin
  inherited Create(Format('%s at line %d, column %d', [AMessage, ALine, AColumn]));
  LLine := ALine;
  LColumn := AColumn;
end;

{ TParser }

constructor TParser.Create(const ALexer: TLexer; const AOwnsLexer: Boolean = False);
begin
  inherited Create();
  LLexer := ALexer;
  LOwnsLexer := AOwnsLexer;
  NextToken(); // Initialize with first token
end;

constructor TParser.CreateFromSource(const ASource: UTF8String);
begin
  inherited Create();
  LLexer := TLexer.Create(ASource);
  LOwnsLexer := True;
  NextToken(); // Initialize with first token
end;

destructor TParser.Destroy();
begin
  if LOwnsLexer then
    LLexer.Free();
  inherited Destroy();
end;

procedure TParser.NextToken();
begin
  LCurrentToken := LLexer.NextToken();

  // Handle lexical errors
  if LCurrentToken.TokenType = ttError then
    RaiseParseError(LCurrentToken.Value);
end;

procedure TParser.ExpectToken(const ATokenType: TTokenType; const AMessage: UTF8String);
begin
  if LCurrentToken.TokenType <> ATokenType then
    RaiseParseError(AMessage + UTF8String(Format(' (got %s)', [GetEnumName(TypeInfo(TTokenType), Ord(LCurrentToken.TokenType))])));

  NextToken();
end;

function TParser.CheckToken(const ATokenType: TTokenType): Boolean;
begin
  Result := LCurrentToken.TokenType = ATokenType;
end;

procedure TParser.RaiseParseError(const AMessage: UTF8String);
begin
  raise EParseError.Create(string(AMessage), LCurrentToken.Line, LCurrentToken.Column);
end;

function TParser.IsIdentifierAProcedure(const AIdentifier: UTF8String): Boolean;
begin
  // Known built-in procedures - updated for Print/PrintLn and array functions
  Result := (AIdentifier = 'PrintLn') or (AIdentifier = 'Print') or
            (AIdentifier = 'ReadLn') or (AIdentifier = 'IntToStr') or
            (AIdentifier = 'StrToInt') or (AIdentifier = 'FloatToStr') or
            (AIdentifier = 'StrToFloat') or (AIdentifier = 'Length');
end;

function TParser.TokenTypeToBinaryOp(const ATokenType: TTokenType): TBinaryOpType;
begin
  case ATokenType of
    ttPlus: Result := btAdd;
    ttMinus: Result := btSubtract;
    ttMultiply: Result := btMultiply;
    ttDivide: Result := btDivide;
  else
    raise EParseError.Create('Invalid binary operator token', LCurrentToken.Line, LCurrentToken.Column);
  end;
end;

function TParser.TokenTypeToComparisonOp(const ATokenType: TTokenType): TComparisonOpType;
begin
  case ATokenType of
    ttEqual: Result := ctEqual;
    ttNotEqual: Result := ctNotEqual;
    ttLess: Result := ctLess;
    ttLessEqual: Result := ctLessEqual;
    ttGreater: Result := ctGreater;
    ttGreaterEqual: Result := ctGreaterEqual;
  else
    raise EParseError.Create('Invalid comparison operator token', LCurrentToken.Line, LCurrentToken.Column);
  end;
end;

function TParser.TokenTypeToLogicalOp(const ATokenType: TTokenType): TLogicalOpType;
begin
  case ATokenType of
    ttAnd: Result := ltAnd;
    ttOr: Result := ltOr;
    ttNot: Result := ltNot;
  else
    raise EParseError.Create('Invalid logical operator token', LCurrentToken.Line, LCurrentToken.Column);
  end;
end;

function TParser.Parse(): TProgramNode;
begin
  try
    Result := ParseProgram();
  except
    on E: EParseError do
      raise;
    on E: Exception do
      raise EParseError.Create('Unexpected error: ' + E.Message, LCurrentToken.Line, LCurrentToken.Column);
  end;
end;

function TParser.ParseProgram(): TProgramNode;
var
  LProgramName: UTF8String;
  LVarSection: TVarSectionNode;
  LMainBlock: TBeginEndNode;
begin
  // Expect 'program'
  ExpectToken(ttProgram, UTF8String('Expected program keyword'));

  // Expect program name (identifier)
  if not CheckToken(ttIdentifier) then
    RaiseParseError(UTF8String('Expected program name'));

  LProgramName := LCurrentToken.Value;
  NextToken();

  // Expect semicolon
  ExpectToken(ttSemicolon, UTF8String('Expected semicolon after program name'));

  // Create program node
  Result := TProgramNode.Create(LCurrentToken.Line, LCurrentToken.Column, LProgramName);

  try
    // Check for optional var section
    if CheckToken(ttVar) then
    begin
      LVarSection := ParseVarSection();
      Result.SetVarSection(LVarSection);
    end;

    // Parse main block
    LMainBlock := ParseBeginEnd();
    Result.SetMainBlock(LMainBlock);

    // Expect final dot
    ExpectToken(ttDot, UTF8String('Expected dot at end of program'));

    // Should be at EOF
    if not CheckToken(ttEOF) then
      RaiseParseError(UTF8String('Expected end of file'));

  except
    Result.Free();
    raise;
  end;
end;

function TParser.ParseVarSection(): TVarSectionNode;
var
  LVarDecl: TVarDeclNode;
begin
  // Expect 'var'
  ExpectToken(ttVar, UTF8String('Expected var keyword'));

  Result := TVarSectionNode.Create(LCurrentToken.Line, LCurrentToken.Column);

  try
    // Parse variable declarations until we hit 'begin'
    while not CheckToken(ttBegin) do
    begin
      LVarDecl := ParseVarDecl();
      Result.AddDeclaration(LVarDecl);

      // Expect semicolon after each declaration
      ExpectToken(ttSemicolon, UTF8String('Expected semicolon after variable declaration'));
    end;

    // Must have at least one declaration
    if Result.GetDeclarationCount() = 0 then
      RaiseParseError(UTF8String('Expected at least one variable declaration'));

  except
    Result.Free();
    raise;
  end;
end;

function TParser.ParseVarDecl(): TVarDeclNode;
var
  LVariableName: UTF8String;
  LTypeNode: TASTNode;
begin
  // Expect identifier (variable name)
  if not CheckToken(ttIdentifier) then
    RaiseParseError(UTF8String('Expected variable name'));

  LVariableName := LCurrentToken.Value;
  NextToken();

  // Expect colon
  ExpectToken(ttColon, UTF8String('Expected colon after variable name'));

  // Parse type (can be simple type or array type)
  LTypeNode := ParseType();

  // Create variable declaration node
  Result := TVarDeclNode.Create(LCurrentToken.Line, LCurrentToken.Column, LVariableName, LTypeNode);
end;

function TParser.ParseType(): TASTNode;
begin
  // Check if this is an array type
  if CheckToken(ttArray) then
  begin
    Result := ParseArrayType();
  end
  else
  begin
    // Parse basic type
    case LCurrentToken.TokenType of
      ttInt: Result := TTypeNode.Create(LCurrentToken.Line, LCurrentToken.Column, ptInt);
      ttFloat: Result := TTypeNode.Create(LCurrentToken.Line, LCurrentToken.Column, ptFloat);
      ttUInt: Result := TTypeNode.Create(LCurrentToken.Line, LCurrentToken.Column, ptUInt);
      ttString: Result := TTypeNode.Create(LCurrentToken.Line, LCurrentToken.Column, ptString);
      ttPString: Result := TTypeNode.Create(LCurrentToken.Line, LCurrentToken.Column, ptPString);
    else
      RaiseParseError(UTF8String('Expected type name (Int, Float, UInt, String, PString, or array)'));
      Result := nil; // Suppress compiler warning
    end;

    NextToken();
  end;
end;

function TParser.ParseArrayType(): TArrayTypeNode;
var
  LElementType: TASTNode;
  LStartIndex: TASTNode;
  LEndIndex: TASTNode;
begin
  // Expect 'array'
  ExpectToken(ttArray, UTF8String('Expected array keyword'));

  // Check for static array syntax: array[start..end] of Type
  if CheckToken(ttLeftBracket) then
  begin
    NextToken(); // consume '['

    // Parse start index expression
    LStartIndex := ParseExpression();

    // Expect '..'
    ExpectToken(ttDotDot, UTF8String('Expected ".." in array range'));

    // Parse end index expression
    LEndIndex := ParseExpression();

    // Expect ']'
    ExpectToken(ttRightBracket, UTF8String('Expected "]" after array range'));

    // Expect 'of'
    ExpectToken(ttOf, UTF8String('Expected "of" after array range'));

    // Parse element type (can be another array type for multi-dimensional)
    LElementType := ParseType();

    Result := TArrayTypeNode.Create(LCurrentToken.Line, LCurrentToken.Column, atStatic, LElementType, LStartIndex, LEndIndex);
  end
  else
  begin
    // Dynamic array syntax: array of Type
    ExpectToken(ttOf, UTF8String('Expected "of" after array keyword'));

    // Parse element type
    LElementType := ParseType();

    Result := TArrayTypeNode.Create(LCurrentToken.Line, LCurrentToken.Column, atDynamic, LElementType);
  end;
end;

function TParser.ParseBeginEnd(): TBeginEndNode;
var
  LStatement: TASTNode;
begin
  // Expect 'begin'
  ExpectToken(ttBegin, UTF8String('Expected begin keyword'));

  Result := TBeginEndNode.Create(LCurrentToken.Line, LCurrentToken.Column);

  try
    // Parse statements until 'end'
    while not CheckToken(ttEnd) do
    begin
      LStatement := ParseStatement();
      Result.AddStatement(LStatement);

      // Expect semicolon after statement (except before 'end')
      if CheckToken(ttSemicolon) then
        NextToken()
      else if not CheckToken(ttEnd) then
        RaiseParseError(UTF8String('Expected semicolon or end'));
    end;

    // Expect 'end'
    ExpectToken(ttEnd, UTF8String('Expected end keyword'));

  except
    Result.Free();
    raise;
  end;
end;

function TParser.ParseStatement(): TASTNode;
var
  LTarget: TASTNode;
  LNextToken: TToken;
begin
  case LCurrentToken.TokenType of
    ttIdentifier:
    begin
      // Look ahead to see if this is assignment or procedure call
      LNextToken := LLexer.PeekToken();

      if (LNextToken.TokenType = ttAssign) or (LNextToken.TokenType = ttLeftBracket) then
      begin
        // Parse target (variable or array access)
        LTarget := ParsePostfixExpression();
        Result := ParseAssignment(LTarget);
      end
      else
      begin
        // Procedure call
        Result := ParseProcedureCall();
      end;
    end;

    ttIf:
      Result := ParseIfStatement();

    ttWhile:
      Result := ParseWhileStatement();

    ttFor:
      Result := ParseForStatement();

    ttBegin:
      Result := ParseBeginEnd();

  else
    RaiseParseError(UTF8String('Expected statement'));
    Result := nil; // Suppress compiler warning
  end;
end;

function TParser.ParseAssignment(const ATarget: TASTNode): TAssignmentNode;
var
  LExpression: TASTNode;
begin
  // Expect ':='
  ExpectToken(ttAssign, UTF8String('Expected assignment operator (:=)'));

  // Parse the expression (right side of assignment)
  LExpression := ParseExpression();

  Result := TAssignmentNode.Create(LCurrentToken.Line, LCurrentToken.Column, ATarget, LExpression);
end;

function TParser.ParseIfStatement(): TIfNode;
var
  LCondition: TASTNode;
  LThenStatement: TASTNode;
  LElseStatement: TASTNode;
begin
  // Expect 'if'
  ExpectToken(ttIf, UTF8String('Expected if keyword'));

  // Parse condition expression
  LCondition := ParseExpression();

  // Expect 'then'
  ExpectToken(ttThen, UTF8String('Expected then keyword'));

  // Parse then statement
  LThenStatement := ParseStatement();

  // Check for optional 'else'
  LElseStatement := nil;
  if CheckToken(ttElse) then
  begin
    NextToken(); // consume 'else'
    LElseStatement := ParseStatement();
  end;

  Result := TIfNode.Create(LCurrentToken.Line, LCurrentToken.Column, LCondition, LThenStatement, LElseStatement);
end;

function TParser.ParseWhileStatement(): TWhileNode;
var
  LCondition: TASTNode;
  LStatement: TASTNode;
begin
  // Expect 'while'
  ExpectToken(ttWhile, UTF8String('Expected while keyword'));

  // Parse condition expression
  LCondition := ParseExpression();

  // Expect 'do'
  ExpectToken(ttDo, UTF8String('Expected do keyword'));

  // Parse statement
  LStatement := ParseStatement();

  Result := TWhileNode.Create(LCurrentToken.Line, LCurrentToken.Column, LCondition, LStatement);
end;

function TParser.ParseForStatement(): TForNode;
var
  LVariableName: UTF8String;
  LStartExpression: TASTNode;
  LEndExpression: TASTNode;
  LStatement: TASTNode;
begin
  // Expect 'for'
  ExpectToken(ttFor, UTF8String('Expected for keyword'));

  // Expect variable name
  if not CheckToken(ttIdentifier) then
    RaiseParseError(UTF8String('Expected variable name'));

  LVariableName := LCurrentToken.Value;
  NextToken();

  // Expect ':='
  ExpectToken(ttAssign, UTF8String('Expected assignment operator (:=)'));

  // Parse start expression
  LStartExpression := ParseExpression();

  // Expect 'to'
  ExpectToken(ttTo, UTF8String('Expected to keyword'));

  // Parse end expression
  LEndExpression := ParseExpression();

  // Expect 'do'
  ExpectToken(ttDo, UTF8String('Expected do keyword'));

  // Parse statement
  LStatement := ParseStatement();

  Result := TForNode.Create(LCurrentToken.Line, LCurrentToken.Column, LVariableName, LStartExpression, LEndExpression, LStatement);
end;

function TParser.ParseProcedureCall(): TProcedureCallNode;
var
  LProcedureName: UTF8String;
  LArgument: TASTNode;
begin
  // Get procedure name
  if not CheckToken(ttIdentifier) then
    RaiseParseError(UTF8String('Expected procedure name'));

  LProcedureName := LCurrentToken.Value;
  NextToken();

  Result := TProcedureCallNode.Create(LCurrentToken.Line, LCurrentToken.Column, LProcedureName);

  try
    // Check for arguments
    if CheckToken(ttLeftParen) then
    begin
      NextToken(); // consume '('

      // Parse arguments
      if not CheckToken(ttRightParen) then
      begin
        repeat
          LArgument := ParseExpression(); // Use full expression parsing for arguments
          Result.AddArgument(LArgument);

          // Check for more arguments
          if CheckToken(ttComma) then
            NextToken()
          else
            Break;

        until False;
      end;

      // Expect closing parenthesis
      ExpectToken(ttRightParen, UTF8String('Expected closing parenthesis'));
    end;

  except
    Result.Free();
    raise;
  end;
end;

function TParser.ParseExpression(): TASTNode;
begin
  // Expression parsing with full operator precedence
  // Start with logical OR (lowest precedence)
  Result := ParseLogicalOrExpression();
end;

function TParser.ParseLogicalOrExpression(): TASTNode;
var
  LLeft: TASTNode;
  LOperation: TLogicalOpType;
  LRight: TASTNode;
begin
  LLeft := ParseLogicalAndExpression();

  while CheckToken(ttOr) do
  begin
    LOperation := TokenTypeToLogicalOp(LCurrentToken.TokenType);
    NextToken(); // consume operator

    LRight := ParseLogicalAndExpression();
    LLeft := TLogicalOpNode.Create(LCurrentToken.Line, LCurrentToken.Column, LOperation, LLeft, LRight);
  end;

  Result := LLeft;
end;

function TParser.ParseLogicalAndExpression(): TASTNode;
var
  LLeft: TASTNode;
  LOperation: TLogicalOpType;
  LRight: TASTNode;
begin
  LLeft := ParseComparisonExpression();

  while CheckToken(ttAnd) do
  begin
    LOperation := TokenTypeToLogicalOp(LCurrentToken.TokenType);
    NextToken(); // consume operator

    LRight := ParseComparisonExpression();
    LLeft := TLogicalOpNode.Create(LCurrentToken.Line, LCurrentToken.Column, LOperation, LLeft, LRight);
  end;

  Result := LLeft;
end;

function TParser.ParseComparisonExpression(): TASTNode;
var
  LLeft: TASTNode;
  LOperation: TComparisonOpType;
  LRight: TASTNode;
begin
  LLeft := ParseAddExpression();

  if CheckToken(ttEqual) or CheckToken(ttNotEqual) or CheckToken(ttLess) or
     CheckToken(ttLessEqual) or CheckToken(ttGreater) or CheckToken(ttGreaterEqual) then
  begin
    LOperation := TokenTypeToComparisonOp(LCurrentToken.TokenType);
    NextToken(); // consume operator

    LRight := ParseAddExpression();
    Result := TComparisonNode.Create(LCurrentToken.Line, LCurrentToken.Column, LOperation, LLeft, LRight);
  end
  else
    Result := LLeft;
end;

function TParser.ParseAddExpression(): TASTNode;
var
  LLeft: TASTNode;
  LOperation: TBinaryOpType;
  LRight: TASTNode;
begin
  // Start with multiplication/division level (higher precedence)
  LLeft := ParseMulExpression();

  // Handle addition and subtraction (left associative)
  while CheckToken(ttPlus) or CheckToken(ttMinus) do
  begin
    LOperation := TokenTypeToBinaryOp(LCurrentToken.TokenType);
    NextToken(); // consume operator

    LRight := ParseMulExpression();
    LLeft := TBinaryOpNode.Create(LCurrentToken.Line, LCurrentToken.Column, LOperation, LLeft, LRight);
  end;

  Result := LLeft;
end;

function TParser.ParseMulExpression(): TASTNode;
var
  LLeft: TASTNode;
  LOperation: TBinaryOpType;
  LRight: TASTNode;
begin
  // Start with unary expressions (highest precedence)
  LLeft := ParseUnaryExpression();

  // Handle multiplication and division (left associative)
  while CheckToken(ttMultiply) or CheckToken(ttDivide) do
  begin
    LOperation := TokenTypeToBinaryOp(LCurrentToken.TokenType);
    NextToken(); // consume operator

    LRight := ParseUnaryExpression();
    LLeft := TBinaryOpNode.Create(LCurrentToken.Line, LCurrentToken.Column, LOperation, LLeft, LRight);
  end;

  Result := LLeft;
end;

function TParser.ParseUnaryExpression(): TASTNode;
var
  LOperation: TLogicalOpType;
  LOperand: TASTNode;
  LValue: Int64;
begin
  // Handle logical NOT
  if CheckToken(ttNot) then
  begin
    LOperation := TokenTypeToLogicalOp(LCurrentToken.TokenType);
    NextToken(); // consume 'not'

    LOperand := ParseUnaryExpression(); // Right associative
    Result := TLogicalOpNode.Create(LCurrentToken.Line, LCurrentToken.Column, LOperation, nil, LOperand);
  end
  // Handle unary minus (negative numbers)
  else if CheckToken(ttMinus) then
  begin
    NextToken(); // consume the minus sign

    // Special case for integer literals
    if CheckToken(ttIntegerLiteral) then
    begin
      LValue := -StrToInt64(string(LCurrentToken.Value));
      NextToken();
      Result := TIntegerLiteralNode.Create(LCurrentToken.Line, LCurrentToken.Column, LValue);
    end
    else
    begin
      // For other expressions, create 0 - expression
      LOperand := ParseUnaryExpression();
      Result := TBinaryOpNode.Create(LCurrentToken.Line, LCurrentToken.Column, btSubtract,
        TIntegerLiteralNode.Create(LCurrentToken.Line, LCurrentToken.Column, 0), LOperand);
    end;
  end
  else
    Result := ParsePostfixExpression();
end;

function TParser.ParsePostfixExpression(): TASTNode;
begin
  // Start with primary expression
  Result := ParsePrimaryExpression();

  // Handle postfix operations (array access)
  while CheckToken(ttLeftBracket) do
  begin
    Result := ParseArrayAccess(Result);
  end;
end;

function TParser.ParsePrimaryExpression(): TASTNode;
begin
  case LCurrentToken.TokenType of
    ttIntegerLiteral:
      Result := ParseIntegerLiteral();

    ttFloatLiteral:
      Result := ParseFloatLiteral();

    ttStringLiteral:
      Result := ParseStringLiteral();

    ttTrue, ttFalse:
      Result := ParseBooleanLiteral();

    ttLeftBracket:
      Result := ParseArrayLiteral();

    ttIdentifier:
    begin
      // Check if this is a function call or variable reference
      if IsIdentifierAProcedure(LCurrentToken.Value) then
        Result := ParseProcedureCall()
      else
        Result := ParseVariableReference();
    end;

    ttLeftParen:
    begin
      NextToken(); // consume '('
      Result := ParseExpression(); // parse nested expression
      ExpectToken(ttRightParen, UTF8String('Expected closing parenthesis'));
    end;

  else
    RaiseParseError(UTF8String('Expected expression (literal, variable, array, or parentheses)'));
    Result := nil; // Suppress compiler warning
  end;
end;

function TParser.ParseArrayAccess(const AArrayExpression: TASTNode): TArrayAccessNode;
var
  LIndex: TASTNode;
begin
  Result := TArrayAccessNode.Create(LCurrentToken.Line, LCurrentToken.Column, AArrayExpression);

  try
    // Expect '['
    ExpectToken(ttLeftBracket, UTF8String('Expected "["'));

    // Parse first index
    LIndex := ParseExpression();
    Result.AddIndex(LIndex);

    // Handle multi-dimensional arrays: [i, j, k]
    while CheckToken(ttComma) do
    begin
      NextToken(); // consume ','
      LIndex := ParseExpression();
      Result.AddIndex(LIndex);
    end;

    // Expect ']'
    ExpectToken(ttRightBracket, UTF8String('Expected "]"'));

  except
    Result.Free();
    raise;
  end;
end;

function TParser.ParseArrayLiteral(): TArrayLiteralNode;
var
  LElement: TASTNode;
begin
  Result := TArrayLiteralNode.Create(LCurrentToken.Line, LCurrentToken.Column);

  try
    // Expect '['
    ExpectToken(ttLeftBracket, UTF8String('Expected "["'));

    // Handle empty array literal []
    if not CheckToken(ttRightBracket) then
    begin
      // Parse first element
      LElement := ParseExpression();
      Result.AddElement(LElement);

      // Parse remaining elements
      while CheckToken(ttComma) do
      begin
        NextToken(); // consume ','
        LElement := ParseExpression();
        Result.AddElement(LElement);
      end;
    end;

    // Expect ']'
    ExpectToken(ttRightBracket, UTF8String('Expected "]"'));

  except
    Result.Free();
    raise;
  end;
end;

function TParser.ParseStringLiteral(): TStringLiteralNode;
var
  LValue: UTF8String;
begin
  if not CheckToken(ttStringLiteral) then
    RaiseParseError(UTF8String('Expected string literal'));

  LValue := LCurrentToken.Value;
  NextToken();

  Result := TStringLiteralNode.Create(LCurrentToken.Line, LCurrentToken.Column, LValue);
end;

function TParser.ParseIntegerLiteral(): TIntegerLiteralNode;
var
  LValue: Int64;
begin
  if not CheckToken(ttIntegerLiteral) then
    RaiseParseError(UTF8String('Expected integer literal'));

  try
    LValue := StrToInt64(string(LCurrentToken.Value));
  except
    RaiseParseError(UTF8String('Invalid integer literal: ' + LCurrentToken.Value));
    LValue := 0; // Suppress compiler warning
  end;

  NextToken();
  Result := TIntegerLiteralNode.Create(LCurrentToken.Line, LCurrentToken.Column, LValue);
end;

function TParser.ParseFloatLiteral(): TFloatLiteralNode;
var
  LValue: Double;
begin
  if not CheckToken(ttFloatLiteral) then
    RaiseParseError(UTF8String('Expected float literal'));

  try
    LValue := StrToFloat(string(LCurrentToken.Value));
  except
    RaiseParseError(UTF8String('Invalid float literal: ' + LCurrentToken.Value));
    LValue := 0.0; // Suppress compiler warning
  end;

  NextToken();
  Result := TFloatLiteralNode.Create(LCurrentToken.Line, LCurrentToken.Column, LValue);
end;

function TParser.ParseBooleanLiteral(): TBooleanLiteralNode;
var
  LValue: Boolean;
begin
  if CheckToken(ttTrue) then
    LValue := True
  else if CheckToken(ttFalse) then
    LValue := False
  else
  begin
    RaiseParseError(UTF8String('Expected boolean literal (true or false)'));
    LValue := False; // Suppress compiler warning
  end;

  NextToken();
  Result := TBooleanLiteralNode.Create(LCurrentToken.Line, LCurrentToken.Column, LValue);
end;

function TParser.ParseVariableReference(): TVariableReferenceNode;
var
  LVariableName: UTF8String;
begin
  if not CheckToken(ttIdentifier) then
    RaiseParseError(UTF8String('Expected variable name'));

  LVariableName := LCurrentToken.Value;
  NextToken();

  Result := TVariableReferenceNode.Create(LCurrentToken.Line, LCurrentToken.Column, LVariableName);
end;

end.
