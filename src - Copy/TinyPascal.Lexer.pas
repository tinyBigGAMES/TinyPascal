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

unit TinyPascal.Lexer;

{$I TinyPascal.Defines.inc}

interface

uses
  System.SysUtils,
  System.Classes;

type
  // TinyPascal core type enumeration
  TPascalType = (
    ptInt,      // Int64
    ptFloat,    // Double
    ptUInt,     // UInt64
    ptString,   // UTF8String
    ptPString   // PUTF8Char
  );

  TTokenType = (
    ttProgram,        // 'program'
    ttBegin,          // 'begin'
    ttEnd,            // 'end'
    ttVar,            // 'var'
    ttConst,          // 'const'
    ttProcedure,      // 'procedure'
    ttFunction,       // 'function'
    ttIf,             // 'if'
    ttThen,           // 'then'
    ttElse,           // 'else'
    ttWhile,          // 'while'
    ttDo,             // 'do'
    ttFor,            // 'for'
    ttTo,             // 'to'
    ttAnd,            // 'and'
    ttOr,             // 'or'
    ttNot,            // 'not'
    ttTrue,           // 'true'
    ttFalse,          // 'false'
    ttInt,            // 'Int' (Int64)
    ttFloat,          // 'Float' (Double)
    ttUInt,           // 'UInt' (UInt64)
    ttString,         // 'String' (UTF8String)
    ttPString,        // 'PString' (PUTF8Char)
    ttIdentifier,     // User-defined names
    ttStringLiteral,  // 'text'
    ttIntegerLiteral, // 123
    ttFloatLiteral,   // 123.45
    ttSemicolon,      // ';'
    ttDot,            // '.'
    ttComma,          // ','
    ttColon,          // ':'
    ttLeftParen,      // '('
    ttRightParen,     // ')'
    ttLeftBracket,    // '['
    ttRightBracket,   // ']'
    ttAssign,         // ':='
    ttEqual,          // '='
    ttNotEqual,       // '<>'
    ttLess,           // '<'
    ttLessEqual,      // '<='
    ttGreater,        // '>'
    ttGreaterEqual,   // '>='
    ttPlus,           // '+'
    ttMinus,          // '-'
    ttMultiply,       // '*'
    ttDivide,         // '/'
    ttEOF,            // End of file
    ttError           // Lexical error
  );

  TToken = record
    TokenType: TTokenType;
    Value: UTF8String;    // Token text as UTF-8
    Line: Integer;        // Line number (1-based)
    Column: Integer;      // Column number (1-based)

    constructor Create(const ATokenType: TTokenType; const AValue: UTF8String;
      const ALine, AColumn: Integer);
  end;

  TLexer = class
  private
    LSource: UTF8String;
    LPosition: Integer;    // Current position in source
    LLine: Integer;        // Current line (1-based)
    LColumn: Integer;      // Current column (1-based)
    LSourceLength: Integer;

    function GetCurrentChar(): UTF8Char;
    function GetNextChar(): UTF8Char;
    procedure AdvanceChar();
    procedure SkipWhitespace();
    procedure SkipLineComment();
    procedure SkipBlockComment();

    function ScanIdentifierOrKeyword(): TToken;
    function ScanStringLiteral(): TToken;
    function ScanNumber(): TToken;
    function ProcessEscapeSequence(): UTF8Char;

    function IsValidIdentifierStart(const AChar: UTF8Char): Boolean;
    function IsValidIdentifierChar(const AChar: UTF8Char): Boolean;
    function IsKeyword(const AIdentifier: UTF8String): TTokenType;

  public
    constructor Create(const ASource: UTF8String);
    function NextToken(): TToken;
    function PeekToken(): TToken;

    property CurrentLine: Integer read LLine;
    property CurrentColumn: Integer read LColumn;
  end;

// Helper functions for type conversion
function TokenTypeToPascalType(const ATokenType: TTokenType): TPascalType;
function PascalTypeToString(const APascalType: TPascalType): UTF8String;

implementation

const
  // Keywords table - case sensitive
  Keywords: array[0..23] of record
    Text: UTF8String;
    TokenType: TTokenType;
  end = (
    (Text: 'program'; TokenType: ttProgram),
    (Text: 'begin'; TokenType: ttBegin),
    (Text: 'end'; TokenType: ttEnd),
    (Text: 'var'; TokenType: ttVar),
    (Text: 'const'; TokenType: ttConst),
    (Text: 'procedure'; TokenType: ttProcedure),
    (Text: 'function'; TokenType: ttFunction),
    (Text: 'if'; TokenType: ttIf),
    (Text: 'then'; TokenType: ttThen),
    (Text: 'else'; TokenType: ttElse),
    (Text: 'while'; TokenType: ttWhile),
    (Text: 'do'; TokenType: ttDo),
    (Text: 'for'; TokenType: ttFor),
    (Text: 'to'; TokenType: ttTo),
    (Text: 'and'; TokenType: ttAnd),
    (Text: 'or'; TokenType: ttOr),
    (Text: 'not'; TokenType: ttNot),
    (Text: 'true'; TokenType: ttTrue),
    (Text: 'false'; TokenType: ttFalse),
    (Text: 'Int'; TokenType: ttInt),
    (Text: 'Float'; TokenType: ttFloat),
    (Text: 'UInt'; TokenType: ttUInt),
    (Text: 'String'; TokenType: ttString),
    (Text: 'PString'; TokenType: ttPString)
  );

{ TToken }

constructor TToken.Create(const ATokenType: TTokenType; const AValue: UTF8String;
  const ALine, AColumn: Integer);
begin
  TokenType := ATokenType;
  Value := AValue;
  Line := ALine;
  Column := AColumn;
end;

{ TLexer }

constructor TLexer.Create(const ASource: UTF8String);
begin
  inherited Create();
  LSource := ASource;
  LPosition := 1;
  LLine := 1;
  LColumn := 1;
  LSourceLength := Length(ASource);
end;

function TLexer.GetCurrentChar(): UTF8Char;
begin
  if LPosition <= LSourceLength then
    Result := LSource[LPosition]
  else
    Result := #0;
end;

function TLexer.GetNextChar(): UTF8Char;
begin
  if LPosition + 1 <= LSourceLength then
    Result := LSource[LPosition + 1]
  else
    Result := #0;
end;

procedure TLexer.AdvanceChar();
begin
  if (LPosition <= LSourceLength) and (LSource[LPosition] = #10) then
  begin
    Inc(LLine);
    LColumn := 1;
  end
  else
    Inc(LColumn);

  Inc(LPosition);
end;

procedure TLexer.SkipWhitespace();
begin
  while (LPosition <= LSourceLength) and CharInSet(GetCurrentChar(), [#9, #10, #13, #32]) do
    AdvanceChar();
end;

procedure TLexer.SkipLineComment();
begin
  // Skip '//' comment to end of line
  while (LPosition <= LSourceLength) and (GetCurrentChar() <> #10) do
    AdvanceChar();
end;

procedure TLexer.SkipBlockComment();
begin
  // Skip '{ }' or '(* *)' block comment
  AdvanceChar(); // Skip opening
  if GetCurrentChar() = '*' then
    AdvanceChar(); // Skip '*' in '(*'

  while LPosition <= LSourceLength do
  begin
    if (GetCurrentChar() = '}') or
       ((GetCurrentChar() = '*') and (GetNextChar() = ')')) then
    begin
      AdvanceChar();
      if GetCurrentChar() = ')' then
        AdvanceChar();
      Break;
    end;
    AdvanceChar();
  end;
end;

function TLexer.IsValidIdentifierStart(const AChar: UTF8Char): Boolean;
begin
  Result := CharInSet(AChar, ['A'..'Z', 'a'..'z', '_']) or (Ord(AChar) > 127);
end;

function TLexer.IsValidIdentifierChar(const AChar: UTF8Char): Boolean;
begin
  Result := CharInSet(AChar, ['A'..'Z', 'a'..'z', '0'..'9', '_']) or (Ord(AChar) > 127);
end;

function TLexer.IsKeyword(const AIdentifier: UTF8String): TTokenType;
var
  LIndex: Integer;
begin
  for LIndex := Low(Keywords) to High(Keywords) do
  begin
    if Keywords[LIndex].Text = AIdentifier then
    begin
      Result := Keywords[LIndex].TokenType;
      Exit;
    end;
  end;
  Result := ttIdentifier;
end;

function TLexer.ProcessEscapeSequence(): UTF8Char;
begin
  AdvanceChar(); // Skip '\'

  case GetCurrentChar() of
    'n': Result := #10;  // LF
    'r': Result := #13;  // CR
    't': Result := #9;   // TAB
    '\': Result := '\';
    '''': Result := '''';
    '"': Result := '"';
    '0': Result := #0;   // NULL
    // TODO: Add \uXXXX Unicode support
  else
    Result := GetCurrentChar(); // Return as-is for unknown sequences
  end;

  AdvanceChar();
end;

function TLexer.ScanStringLiteral(): TToken;
var
  LStartLine, LStartColumn: Integer;
  LValue: UTF8String;
  LQuoteChar: UTF8Char;
begin
  LStartLine := LLine;
  LStartColumn := LColumn;
  LQuoteChar := GetCurrentChar();
  LValue := '';

  AdvanceChar(); // Skip opening quote

  while (LPosition <= LSourceLength) and (GetCurrentChar() <> LQuoteChar) do
  begin
    if GetCurrentChar() = '\' then
      LValue := LValue + UTF8String(ProcessEscapeSequence())
    else
    begin
      LValue := LValue + UTF8String(GetCurrentChar());
      AdvanceChar();
    end;
  end;

  if GetCurrentChar() = LQuoteChar then
    AdvanceChar() // Skip closing quote
  else
  begin
    Result := TToken.Create(ttError, UTF8String('Unterminated string literal'), LStartLine, LStartColumn);
    Exit;
  end;

  Result := TToken.Create(ttStringLiteral, LValue, LStartLine, LStartColumn);
end;

function TLexer.ScanIdentifierOrKeyword(): TToken;
var
  LStartLine, LStartColumn: Integer;
  LValue: UTF8String;
  LTokenType: TTokenType;
begin
  LStartLine := LLine;
  LStartColumn := LColumn;
  LValue := '';

  while IsValidIdentifierChar(GetCurrentChar()) do
  begin
    LValue := LValue + UTF8String(GetCurrentChar());
    AdvanceChar();
  end;

  LTokenType := IsKeyword(LValue);
  Result := TToken.Create(LTokenType, LValue, LStartLine, LStartColumn);
end;

function TLexer.ScanNumber(): TToken;
var
  LStartLine, LStartColumn: Integer;
  LValue: UTF8String;
  LHasDot: Boolean;
  LTokenType: TTokenType;
begin
  LStartLine := LLine;
  LStartColumn := LColumn;
  LValue := '';
  LHasDot := False;

  while CharInSet(GetCurrentChar(), ['0'..'9']) or
        ((GetCurrentChar() = '.') and not LHasDot and CharInSet(GetNextChar(), ['0'..'9'])) do
  begin
    if GetCurrentChar() = '.' then
      LHasDot := True;

    LValue := LValue + UTF8String(GetCurrentChar());
    AdvanceChar();
  end;

  if LHasDot then
    LTokenType := ttFloatLiteral
  else
    LTokenType := ttIntegerLiteral;

  Result := TToken.Create(LTokenType, LValue, LStartLine, LStartColumn);
end;

function TLexer.NextToken(): TToken;
var
  LStartLine, LStartColumn: Integer;
begin
  SkipWhitespace();

  if LPosition > LSourceLength then
  begin
    Result := TToken.Create(ttEOF, UTF8String(''), LLine, LColumn);
    Exit;
  end;

  LStartLine := LLine;
  LStartColumn := LColumn;

  case GetCurrentChar() of
    'A'..'Z', 'a'..'z', '_':
      Result := ScanIdentifierOrKeyword();

    '0'..'9':
      Result := ScanNumber();

    '''', '"':
      Result := ScanStringLiteral();

    ';':
    begin
      AdvanceChar();
      Result := TToken.Create(ttSemicolon, UTF8String(';'), LStartLine, LStartColumn);
    end;

    '.':
    begin
      AdvanceChar();
      Result := TToken.Create(ttDot, UTF8String('.'), LStartLine, LStartColumn);
    end;

    ',':
    begin
      AdvanceChar();
      Result := TToken.Create(ttComma, UTF8String(','), LStartLine, LStartColumn);
    end;

    ':':
    begin
      AdvanceChar();
      if GetCurrentChar() = '=' then
      begin
        AdvanceChar();
        Result := TToken.Create(ttAssign, UTF8String(':='), LStartLine, LStartColumn);
      end
      else
        Result := TToken.Create(ttColon, UTF8String(':'), LStartLine, LStartColumn);
    end;

    '(':
    begin
      AdvanceChar();
      if GetCurrentChar() = '*' then
      begin
        SkipBlockComment();
        Result := NextToken(); // Recursively get next token
      end
      else
        Result := TToken.Create(ttLeftParen, UTF8String('('), LStartLine, LStartColumn);
    end;

    ')':
    begin
      AdvanceChar();
      Result := TToken.Create(ttRightParen, UTF8String(')'), LStartLine, LStartColumn);
    end;

    '[':
    begin
      AdvanceChar();
      Result := TToken.Create(ttLeftBracket, UTF8String('['), LStartLine, LStartColumn);
    end;

    ']':
    begin
      AdvanceChar();
      Result := TToken.Create(ttRightBracket, UTF8String(']'), LStartLine, LStartColumn);
    end;

    '=':
    begin
      AdvanceChar();
      Result := TToken.Create(ttEqual, UTF8String('='), LStartLine, LStartColumn);
    end;

    '<':
    begin
      AdvanceChar();
      if GetCurrentChar() = '=' then
      begin
        AdvanceChar();
        Result := TToken.Create(ttLessEqual, UTF8String('<='), LStartLine, LStartColumn);
      end
      else if GetCurrentChar() = '>' then
      begin
        AdvanceChar();
        Result := TToken.Create(ttNotEqual, UTF8String('<>'), LStartLine, LStartColumn);
      end
      else
        Result := TToken.Create(ttLess, UTF8String('<'), LStartLine, LStartColumn);
    end;

    '>':
    begin
      AdvanceChar();
      if GetCurrentChar() = '=' then
      begin
        AdvanceChar();
        Result := TToken.Create(ttGreaterEqual, UTF8String('>='), LStartLine, LStartColumn);
      end
      else
        Result := TToken.Create(ttGreater, UTF8String('>'), LStartLine, LStartColumn);
    end;

    '+':
    begin
      AdvanceChar();
      Result := TToken.Create(ttPlus, UTF8String('+'), LStartLine, LStartColumn);
    end;

    '-':
    begin
      AdvanceChar();
      Result := TToken.Create(ttMinus, UTF8String('-'), LStartLine, LStartColumn);
    end;

    '*':
    begin
      AdvanceChar();
      Result := TToken.Create(ttMultiply, UTF8String('*'), LStartLine, LStartColumn);
    end;

    '/':
    begin
      AdvanceChar();
      if GetCurrentChar() = '/' then
      begin
        SkipLineComment();
        Result := NextToken(); // Recursively get next token
      end
      else
        Result := TToken.Create(ttDivide, UTF8String('/'), LStartLine, LStartColumn);
    end;

    '{':
    begin
      SkipBlockComment();
      Result := NextToken(); // Recursively get next token
    end;

  else
    // Handle Unicode identifier start or unknown character
    if IsValidIdentifierStart(GetCurrentChar()) then
      Result := ScanIdentifierOrKeyword()
    else
    begin
      AdvanceChar();
      Result := TToken.Create(ttError, UTF8String('Unexpected character'), LStartLine, LStartColumn);
    end;
  end;
end;

function TLexer.PeekToken(): TToken;
var
  LSavedPosition, LSavedLine, LSavedColumn: Integer;
begin
  // Save current state
  LSavedPosition := LPosition;
  LSavedLine := LLine;
  LSavedColumn := LColumn;

  // Get next token
  Result := NextToken();

  // Restore state
  LPosition := LSavedPosition;
  LLine := LSavedLine;
  LColumn := LSavedColumn;
end;

{ Helper Functions }

function TokenTypeToPascalType(const ATokenType: TTokenType): TPascalType;
begin
  case ATokenType of
    ttInt: Result := ptInt;
    ttFloat: Result := ptFloat;
    ttUInt: Result := ptUInt;
    ttString: Result := ptString;
    ttPString: Result := ptPString;
  else
    raise Exception.CreateFmt('Invalid token type for Pascal type: %d', [Ord(ATokenType)]);
  end;
end;

function PascalTypeToString(const APascalType: TPascalType): UTF8String;
begin
  case APascalType of
    ptInt: Result := UTF8String('Int');
    ptFloat: Result := UTF8String('Float');
    ptUInt: Result := UTF8String('UInt');
    ptString: Result := UTF8String('String');
    ptPString: Result := UTF8String('PString');
  else
    Result := UTF8String('Unknown');
  end;
end;

end.
