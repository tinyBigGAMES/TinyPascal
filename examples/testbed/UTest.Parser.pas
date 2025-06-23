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

unit UTest.Parser;

interface

uses
  System.SysUtils,
  System.TypInfo,
  TinyPascal.Lexer,
  TinyPascal.AST,
  TinyPascal.Parser,
  UTest.Framework;

// Individual test procedures
procedure TestHelloWorldParsing();
procedure TestProgramStructure();
procedure TestBeginEndBlocks();
procedure TestProcedureCalls();
procedure TestStringLiterals();
procedure TestASTStructure();
procedure TestErrorCases();

// Test suite runner
procedure RunAllParserTests();

implementation

procedure TestHelloWorldParsing();
var
  LParser: TParser;
  LASTRoot: TProgramNode;
  LMainBlock: TBeginEndNode;
  LStatement: TProcedureCallNode;
  LArgument: TStringLiteralNode;
  LSource: UTF8String;
begin
  LSource := 'program HelloWorld;' + #13#10 +
             'begin' + #13#10 +
             '  WriteLn(''Hello, World!'');' + #13#10 +
             'end.';

  LParser := TParser.CreateFromSource(LSource);
  try
    LASTRoot := LParser.Parse();
    try
      // Test program structure
      AssertTrue(Assigned(LASTRoot), 'AST root should be assigned');
      AssertEqual(UTF8String('HelloWorld'), LASTRoot.ProgramName, 'Program name should be HelloWorld');

      // Test main block
      LMainBlock := LASTRoot.MainBlock;
      AssertTrue(Assigned(LMainBlock), 'Main block should be assigned');
      AssertEqualInt(1, LMainBlock.GetStatementCount(), 'Should have exactly one statement');

      // Test WriteLn statement
      LStatement := LMainBlock.GetStatement(0) as TProcedureCallNode;
      AssertTrue(Assigned(LStatement), 'Statement should be assigned');
      AssertEqual(UTF8String('WriteLn'), LStatement.ProcedureName, 'Should be WriteLn procedure call');
      AssertEqualInt(1, LStatement.GetArgumentCount(), 'WriteLn should have one argument');

      // Test string argument
      LArgument := LStatement.GetArgument(0) as TStringLiteralNode;
      AssertTrue(Assigned(LArgument), 'Argument should be assigned');
      AssertEqual(UTF8String('Hello, World!'), LArgument.Value, 'String argument should match');

    finally
      LASTRoot.Free();
    end;
  finally
    LParser.Free();
  end;
end;

procedure TestProgramStructure();
var
  LParser: TParser;
  LASTRoot: TProgramNode;
begin
  // Test minimal program
  LParser := TParser.CreateFromSource(UTF8String('program Test; begin end.'));
  try
    LASTRoot := LParser.Parse();
    try
      AssertEqual(UTF8String('Test'), LASTRoot.ProgramName, 'Program name should be Test');
      AssertTrue(Assigned(LASTRoot.MainBlock), 'Should have main block');
      AssertEqualInt(0, LASTRoot.MainBlock.GetStatementCount(), 'Empty program should have no statements');
    finally
      LASTRoot.Free();
    end;
  finally
    LParser.Free();
  end;

  // Test program with different name
  LParser := TParser.CreateFromSource(UTF8String('program MyApplication; begin end.'));
  try
    LASTRoot := LParser.Parse();
    try
      AssertEqual(UTF8String('MyApplication'), LASTRoot.ProgramName, 'Program name should be MyApplication');
    finally
      LASTRoot.Free();
    end;
  finally
    LParser.Free();
  end;
end;

procedure TestBeginEndBlocks();
var
  LParser: TParser;
  LASTRoot: TProgramNode;
  LMainBlock: TBeginEndNode;
begin
  // Test empty begin/end
  LParser := TParser.CreateFromSource(UTF8String('program Empty; begin end.'));
  try
    LASTRoot := LParser.Parse();
    try
      LMainBlock := LASTRoot.MainBlock;
      AssertEqualInt(0, LMainBlock.GetStatementCount(), 'Empty block should have no statements');
    finally
      LASTRoot.Free();
    end;
  finally
    LParser.Free();
  end;

  // Test multiple statements
  LParser := TParser.CreateFromSource(UTF8String('program Multi; begin WriteLn(''First''); WriteLn(''Second''); end.'));
  try
    LASTRoot := LParser.Parse();
    try
      LMainBlock := LASTRoot.MainBlock;
      AssertEqualInt(2, LMainBlock.GetStatementCount(), 'Should have two statements');

      // Check first statement
      AssertTrue(LMainBlock.GetStatement(0) is TProcedureCallNode, 'First statement should be procedure call');
      AssertEqual(UTF8String('WriteLn'), (LMainBlock.GetStatement(0) as TProcedureCallNode).ProcedureName, 'First call should be WriteLn');

      // Check second statement
      AssertTrue(LMainBlock.GetStatement(1) is TProcedureCallNode, 'Second statement should be procedure call');
      AssertEqual(UTF8String('WriteLn'), (LMainBlock.GetStatement(1) as TProcedureCallNode).ProcedureName, 'Second call should be WriteLn');
    finally
      LASTRoot.Free();
    end;
  finally
    LParser.Free();
  end;
end;

procedure TestProcedureCalls();
var
  LParser: TParser;
  LASTRoot: TProgramNode;
  LStatement: TProcedureCallNode;
begin
  // Test procedure call without arguments
  LParser := TParser.CreateFromSource(UTF8String('program Test; begin ReadLn; end.'));
  try
    LASTRoot := LParser.Parse();
    try
      LStatement := LASTRoot.MainBlock.GetStatement(0) as TProcedureCallNode;
      AssertEqual(UTF8String('ReadLn'), LStatement.ProcedureName, 'Should be ReadLn');
      AssertEqualInt(0, LStatement.GetArgumentCount(), 'ReadLn should have no arguments');
    finally
      LASTRoot.Free();
    end;
  finally
    LParser.Free();
  end;

  // Test procedure call with one argument
  LParser := TParser.CreateFromSource(UTF8String('program Test; begin WriteLn(''Test''); end.'));
  try
    LASTRoot := LParser.Parse();
    try
      LStatement := LASTRoot.MainBlock.GetStatement(0) as TProcedureCallNode;
      AssertEqual(UTF8String('WriteLn'), LStatement.ProcedureName, 'Should be WriteLn');
      AssertEqualInt(1, LStatement.GetArgumentCount(), 'WriteLn should have one argument');
    finally
      LASTRoot.Free();
    end;
  finally
    LParser.Free();
  end;

  // Test procedure call with multiple arguments
  LParser := TParser.CreateFromSource(UTF8String('program Test; begin WriteLn(''Hello'', ''World''); end.'));
  try
    LASTRoot := LParser.Parse();
    try
      LStatement := LASTRoot.MainBlock.GetStatement(0) as TProcedureCallNode;
      AssertEqual(UTF8String('WriteLn'), LStatement.ProcedureName, 'Should be WriteLn');
      AssertEqualInt(2, LStatement.GetArgumentCount(), 'WriteLn should have two arguments');

      // Check arguments
      AssertEqual(UTF8String('Hello'), (LStatement.GetArgument(0) as TStringLiteralNode).Value, 'First argument should be Hello');
      AssertEqual(UTF8String('World'), (LStatement.GetArgument(1) as TStringLiteralNode).Value, 'Second argument should be World');
    finally
      LASTRoot.Free();
    end;
  finally
    LParser.Free();
  end;
end;

procedure TestStringLiterals();
var
  LParser: TParser;
  LASTRoot: TProgramNode;
  LStatement: TProcedureCallNode;
  LArgument: TStringLiteralNode;
begin
  // Test single quotes
  LParser := TParser.CreateFromSource(UTF8String('program Test; begin WriteLn(''Single quotes''); end.'));
  try
    LASTRoot := LParser.Parse();
    try
      LStatement := LASTRoot.MainBlock.GetStatement(0) as TProcedureCallNode;
      LArgument := LStatement.GetArgument(0) as TStringLiteralNode;
      AssertEqual(UTF8String('Single quotes'), LArgument.Value, 'Single quote string should match');
    finally
      LASTRoot.Free();
    end;
  finally
    LParser.Free();
  end;

  // Test double quotes
  LParser := TParser.CreateFromSource(UTF8String('program Test; begin WriteLn("Double quotes"); end.'));
  try
    LASTRoot := LParser.Parse();
    try
      LStatement := LASTRoot.MainBlock.GetStatement(0) as TProcedureCallNode;
      LArgument := LStatement.GetArgument(0) as TStringLiteralNode;
      AssertEqual(UTF8String('Double quotes'), LArgument.Value, 'Double quote string should match');
    finally
      LASTRoot.Free();
    end;
  finally
    LParser.Free();
  end;

  // Test empty string
  LParser := TParser.CreateFromSource(UTF8String('program Test; begin WriteLn(''''); end.'));
  try
    LASTRoot := LParser.Parse();
    try
      LStatement := LASTRoot.MainBlock.GetStatement(0) as TProcedureCallNode;
      LArgument := LStatement.GetArgument(0) as TStringLiteralNode;
      AssertEqual(UTF8String(''), LArgument.Value, 'Empty string should be empty');
    finally
      LASTRoot.Free();
    end;
  finally
    LParser.Free();
  end;

  // Test string with escape sequences
  LParser := TParser.CreateFromSource(UTF8String('program Test; begin WriteLn(''Line 1\nLine 2''); end.'));
  try
    LASTRoot := LParser.Parse();
    try
      LStatement := LASTRoot.MainBlock.GetStatement(0) as TProcedureCallNode;
      LArgument := LStatement.GetArgument(0) as TStringLiteralNode;
      AssertEqual(UTF8String('Line 1' + #10 + 'Line 2'), LArgument.Value, 'Escape sequences should be processed');
    finally
      LASTRoot.Free();
    end;
  finally
    LParser.Free();
  end;
end;

procedure TestASTStructure();
var
  LParser: TParser;
  LASTRoot: TProgramNode;
  LMainBlock: TBeginEndNode;
  LStatement: TProcedureCallNode;
  LArgument: TStringLiteralNode;
begin
  LParser := TParser.CreateFromSource(UTF8String('program Test; begin WriteLn(''Hello''); end.'));
  try
    LASTRoot := LParser.Parse();
    try
      // Test parent-child relationships
      LMainBlock := LASTRoot.MainBlock;
      AssertTrue(LMainBlock.Parent = LASTRoot, 'Main block parent should be program node');

      LStatement := LMainBlock.GetStatement(0) as TProcedureCallNode;
      AssertTrue(LStatement.Parent = LMainBlock, 'Statement parent should be main block');

      LArgument := LStatement.GetArgument(0) as TStringLiteralNode;
      AssertTrue(LArgument.Parent = LStatement, 'Argument parent should be statement');

      // Test node types
      AssertTrue(LASTRoot is TProgramNode, 'Root should be TProgramNode');
      AssertTrue(LMainBlock is TBeginEndNode, 'Main block should be TBeginEndNode');
      AssertTrue(LStatement is TProcedureCallNode, 'Statement should be TProcedureCallNode');
      AssertTrue(LArgument is TStringLiteralNode, 'Argument should be TStringLiteralNode');

      // Test position tracking
      AssertTrue(LASTRoot.Line > 0, 'Program node should have line number');
      AssertTrue(LASTRoot.Column > 0, 'Program node should have column number');
    finally
      LASTRoot.Free();
    end;
  finally
    LParser.Free();
  end;
end;

procedure TestErrorCases();
var
  LParser: TParser;
  LExceptionRaised: Boolean;
begin
  // Test missing program keyword
  LParser := TParser.CreateFromSource(UTF8String('Test; begin end.'));
  try
    LExceptionRaised := False;
    try
      LParser.Parse().Free();
    except
      on E: EParseError do
        LExceptionRaised := True;
    end;
    AssertTrue(LExceptionRaised, 'Should raise parse error for missing program keyword');
  finally
    LParser.Free();
  end;

  // Test missing semicolon after program name
  LParser := TParser.CreateFromSource(UTF8String('program Test begin end.'));
  try
    LExceptionRaised := False;
    try
      LParser.Parse().Free();
    except
      on E: EParseError do
        LExceptionRaised := True;
    end;
    AssertTrue(LExceptionRaised, 'Should raise parse error for missing semicolon');
  finally
    LParser.Free();
  end;

  // Test missing begin keyword
  LParser := TParser.CreateFromSource(UTF8String('program Test; WriteLn(''Hello''); end.'));
  try
    LExceptionRaised := False;
    try
      LParser.Parse().Free();
    except
      on E: EParseError do
        LExceptionRaised := True;
    end;
    AssertTrue(LExceptionRaised, 'Should raise parse error for missing begin');
  finally
    LParser.Free();
  end;

  // Test missing end keyword
  LParser := TParser.CreateFromSource(UTF8String('program Test; begin WriteLn(''Hello''); .'));
  try
    LExceptionRaised := False;
    try
      LParser.Parse().Free();
    except
      on E: EParseError do
        LExceptionRaised := True;
    end;
    AssertTrue(LExceptionRaised, 'Should raise parse error for missing end');
  finally
    LParser.Free();
  end;

  // Test missing final dot
  LParser := TParser.CreateFromSource(UTF8String('program Test; begin end'));
  try
    LExceptionRaised := False;
    try
      LParser.Parse().Free();
    except
      on E: EParseError do
        LExceptionRaised := True;
    end;
    AssertTrue(LExceptionRaised, 'Should raise parse error for missing final dot');
  finally
    LParser.Free();
  end;

  // Test unterminated procedure call
  LParser := TParser.CreateFromSource(UTF8String('program Test; begin WriteLn(''Hello''; end.'));
  try
    LExceptionRaised := False;
    try
      LParser.Parse().Free();
    except
      on E: EParseError do
        LExceptionRaised := True;
    end;
    AssertTrue(LExceptionRaised, 'Should raise parse error for unterminated procedure call');
  finally
    LParser.Free();
  end;
end;

procedure RunAllParserTests();
begin
  TestRunner.StartTestSuite(UTF8String('TinyPascal Parser Tests'));

  TestRunner.RunTest(UTF8String('Hello World Parsing'), @TestHelloWorldParsing);
  TestRunner.RunTest(UTF8String('Program Structure'), @TestProgramStructure);
  TestRunner.RunTest(UTF8String('Begin/End Blocks'), @TestBeginEndBlocks);
  TestRunner.RunTest(UTF8String('Procedure Calls'), @TestProcedureCalls);
  TestRunner.RunTest(UTF8String('String Literals'), @TestStringLiterals);
  TestRunner.RunTest(UTF8String('AST Structure'), @TestASTStructure);
  TestRunner.RunTest(UTF8String('Error Cases'), @TestErrorCases);

  TestRunner.EndTestSuite();
end;

end.
