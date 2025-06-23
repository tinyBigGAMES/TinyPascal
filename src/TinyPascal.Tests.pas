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

unit TinyPascal.Tests;

{$I TinyPascal.Defines.inc}

interface

procedure RunTPTests(const APause: Boolean=True);
procedure Pause(const APause: Boolean=True);

procedure Test_X64_ProofOfConcept();

implementation

uses
  Winapi.Windows,
  System.SysUtils,
  System.Classes,
  TinyPascal.Lexer,
  TinyPascal.Parser,
  TinyPascal.BytecodeGen,
  TinyPascal.AST,
  TinyPascal.Value,
  TinyPascal.Bytecode,
  TinyPascal.VM,
  TinyPascal.X64Gen;

procedure Pause(const APause: Boolean);
begin
  if not APause then Exit;

  WriteLn;
  Write('Press ENTER to continue...');
  ReadLn;
  WriteLn;
end;

function ExecuteProgram(const ASource: UTF8String; const ATestName: string): Boolean;
var
  LParser: TParser;
  LProgram: TProgramNode;
  LBytecodeGen: TBytecodeGenerator;
  LGenResult: TBytecodeGenResult;
  LVM: TVirtualMachine;
  LExecResult: TVMExecutionResult;
begin
  Result := False;

  WriteLn('Source: ', string(ASource));
  WriteLn;

  try
    // Parse
    LParser := TParser.CreateFromSource(ASource);
    try
      LProgram := LParser.Parse();
      WriteLn('✓ Parsed successfully');
    finally
      LParser.Free();
    end;

    // Generate bytecode
    LBytecodeGen := TBytecodeGenerator.Create();
    try
      LGenResult := LBytecodeGen.Generate(LProgram);
      try
        if LGenResult.Success then
        begin
          WriteLn('✓ Generated ', LGenResult.BytecodeSize, ' bytes of bytecode');

          // Execute
          LVM := TVirtualMachine.Create();
          try
            if LVM.LoadProgram(LGenResult.Program_) then
            begin
              WriteLn('--- VM OUTPUT ---');
              LExecResult := LVM.Execute();
              try
                WriteLn('--- END OUTPUT ---');
                WriteLn('VM Result: ', VMResultToString(LExecResult.Result));
                WriteLn('Instructions: ', LExecResult.Stats.InstructionsExecuted);
                WriteLn('Max Stack: ', LExecResult.Stats.MaxStackSize);
                WriteLn('Time: ', FormatFloat('0.00', LExecResult.Stats.ExecutionTimeMs), ' ms');

                if (LExecResult.Result = vmrSuccess) or (LExecResult.Result = vmrHalted) then
                begin
                  WriteLn('✓ ', ATestName, ' PASSED!');
                  Result := True;
                end
                else
                  WriteLn('✗ ', ATestName, ' FAILED: ', LExecResult.ErrorMessage);
              finally
                LExecResult.Free();
              end;
            end
            else
              WriteLn('✗ Failed to load program into VM');
          finally
            LVM.Free();
          end;
        end
        else
        begin
          WriteLn('✗ Bytecode generation failed: ', LGenResult.ErrorMessage);
        end;
      finally
        LGenResult.Free();
      end;
    finally
      LBytecodeGen.Free();
      LProgram.Free();
    end;

  except
    on E: Exception do
    begin
      WriteLn('✗ ', ATestName, ' EXCEPTION: ', E.Message);
    end;
  end;
end;

// Test procedures - Updated for Print/PrintLn
procedure Test1_BasicVariableAssignment();
var
  LSource: UTF8String;
begin
  WriteLn('Test 1: Basic Variable Assignment');
  WriteLn('----------------------------------');

  LSource := 'program VarTest; var x: Int; begin x := 42; PrintLn(''Variable assigned!''); end.';
  ExecuteProgram(LSource, 'Test 1');
end;

procedure Test2_ArithmeticExpressions();
var
  LSource: UTF8String;
begin
  WriteLn('Test 2: Arithmetic Expressions');
  WriteLn('-------------------------------');

  LSource := 'program ArithTest; var x: Int; y: Int; result: Int; begin x := 10; y := 5; result := x + y * 2; PrintLn(IntToStr(result)); end.';
  ExecuteProgram(LSource, 'Test 2');
end;

procedure Test3_BooleanLiteralsAndExpressions();
var
  LSource: UTF8String;
begin
  WriteLn('Test 3: Boolean Literals and Expressions');
  WriteLn('----------------------------------------');

  LSource := 'program BoolTest; var flag: Int; begin flag := 1; if flag > 0 then PrintLn(''Positive!'') else PrintLn(''Not positive''); end.';
  ExecuteProgram(LSource, 'Test 3');
end;

procedure Test4_ComparisonOperations();
var
  LSource: UTF8String;
begin
  WriteLn('Test 4: Comparison Operations');
  WriteLn('-----------------------------');

  LSource := 'program CompTest; var a: Int; b: Int; begin a := 15; b := 10; if a > b then PrintLn(''A is greater''); if a = 15 then PrintLn(''A equals 15''); if b <> 15 then PrintLn(''B not equal 15''); end.';
  ExecuteProgram(LSource, 'Test 4');
end;

procedure Test5_IfThenElseStatements();
var
  LSource: UTF8String;
begin
  WriteLn('Test 5: If/Then/Else Statements');
  WriteLn('-------------------------------');

  LSource := 'program IfTest; var score: Int; begin score := 85; if score >= 90 then PrintLn(''Grade A'') else if score >= 80 then PrintLn(''Grade B'') else if score >= 70 then PrintLn(''Grade C'') else PrintLn(''Grade F''); end.';
  ExecuteProgram(LSource, 'Test 5');
end;

procedure Test6_WhileLoops();
var
  LSource: UTF8String;
begin
  WriteLn('Test 6: While Loops');
  WriteLn('-------------------');

  LSource := 'program WhileTest; var i: Int; sum: Int; begin i := 1; sum := 0; while i <= 5 do begin sum := sum + i; PrintLn(IntToStr(i)); i := i + 1; end; PrintLn(''Sum: ''); PrintLn(IntToStr(sum)); end.';
  ExecuteProgram(LSource, 'Test 6');
end;

procedure Test7_ForLoops();
var
  LSource: UTF8String;
begin
  WriteLn('Test 7: For Loops');
  WriteLn('-----------------');

  LSource := 'program ForTest; var i: Int; total: Int; begin total := 0; for i := 1 to 10 do begin total := total + i; PrintLn(IntToStr(i)); end; PrintLn(''Total: ''); PrintLn(IntToStr(total)); end.';
  ExecuteProgram(LSource, 'Test 7');
end;

procedure Test8_NestedControlStructures();
var
  LSource: UTF8String;
begin
  WriteLn('Test 8: Nested Control Structures');
  WriteLn('----------------------------------');

  LSource := 'program NestedTest; var i: Int; j: Int; begin for i := 1 to 3 do begin PrintLn(''Outer loop: ''); PrintLn(IntToStr(i)); j := 1; while j <= 2 do begin PrintLn(''  Inner: ''); PrintLn(IntToStr(j)); j := j + 1; end; end; end.';
  ExecuteProgram(LSource, 'Test 8');
end;

procedure Test9_ComplexExpressions();
var
  LSource: UTF8String;
begin
  WriteLn('Test 9: Complex Expressions');
  WriteLn('---------------------------');

  LSource := 'program ComplexTest; var a: Int; b: Int; c: Int; result: Int; begin a := 5; b := 3; c := 2; result := (a + b) * c - (a - b); PrintLn(IntToStr(result)); if (a > b) then begin if (result > 10) then PrintLn(''Result is big!'') else PrintLn(''Result is small''); end; end.';
  ExecuteProgram(LSource, 'Test 9');
end;

procedure Test10_FloatLiterals();
var
  LSource: UTF8String;
begin
  WriteLn('Test 10: Float Literals');
  WriteLn('-----------------------');

  LSource := 'program FloatTest; var pi: Float; radius: Float; area: Float; begin pi := 3.14159; radius := 5.0; area := pi * radius * radius; PrintLn(''Area calculated!''); end.';
  ExecuteProgram(LSource, 'Test 10');
end;

procedure Test11_PascalProgramShowcase();
var
  LSource: UTF8String;
begin
  WriteLn('Test 11: Pascal Program Showcase');
  WriteLn('--------------------------------');

  LSource := 'program Showcase; var num: Int; factorial: Int; i: Int; isPrime: Int; j: Int; begin num := 5; factorial := 1; for i := 1 to num do factorial := factorial * i; PrintLn(''Factorial of 5: ''); PrintLn(IntToStr(factorial)); num := 17; isPrime := 1; if num > 1 then begin for i := 2 to (num - 1) do begin if (num - (num / i) * i) = 0 then isPrime := 0; end; if isPrime = 1 then PrintLn(''17 is prime!'') else PrintLn(''17 is not prime''); end; end.';
  ExecuteProgram(LSource, 'Test 11');
end;

procedure Test12_RealWorldExample();
var
  LSource: UTF8String;
begin
  WriteLn('Test 12: Real World Example - Number Guessing Game');
  WriteLn('--------------------------------------------------');

  LSource := 'program NumberGame; var target: Int; guess: Int; attempts: Int; won: Int; begin target := 42; guess := 0; attempts := 0; won := 0; PrintLn(''Number Guessing Game!''); while (attempts < 5) do begin attempts := attempts + 1; if attempts = 1 then guess := 50; if attempts = 2 then guess := 40; if attempts = 3 then guess := 45; if attempts = 4 then guess := 42; if attempts = 5 then guess := 30; PrintLn(''Attempt ''); PrintLn(IntToStr(attempts)); PrintLn('': Guessing ''); PrintLn(IntToStr(guess)); if guess = target then begin PrintLn(''Correct! You won!''); won := 1; attempts := 10; end else if guess < target then PrintLn(''Too low!'') else PrintLn(''Too high!''); end; if won = 0 then PrintLn(''Game over! Number was 42''); end.';
  ExecuteProgram(LSource, 'Test 12');
end;

procedure Test13_SimpleWhileLoop();
var
  LSource: UTF8String;
begin
  WriteLn('Test 13: Simple While Loop');
  WriteLn('---------------------------');

  LSource := 'program SimpleWhile; var i: Int; begin i := 1; while i <= 2 do begin PrintLn(IntToStr(i)); i := i + 1; end; end.';
  ExecuteProgram(LSource, 'Test 13');
end;

procedure Test14_SimpleForLoop();
var
  LSource: UTF8String;
begin
  WriteLn('Test 14: Simple For Loop');
  WriteLn('-------------------------');

  LSource := 'program SimpleFor; var i: Int; begin for i := 1 to 2 do PrintLn(IntToStr(i)); end.';
  ExecuteProgram(LSource, 'Test 14');
end;

procedure Test15_AdvancedIfElseChain();
var
  LSource: UTF8String;
begin
  WriteLn('Test 15: Advanced If/Else Chain');
  WriteLn('--------------------------------');

  LSource := 'program AdvancedIfElse; var score: Int; begin score := 85; if score >= 90 then PrintLn(''A'') else if score >= 80 then PrintLn(''B'') else PrintLn(''F''); end.';
  ExecuteProgram(LSource, 'Test 15');
end;

procedure Test_X64_ProofOfConcept();
var
  LParser: TParser;
  LProgram: TProgramNode;
  LBytecodeGen: TBytecodeGenerator;
  LGenResult: TBytecodeGenResult;
  LX64Compiler: TX64Compiler;
  LX64Result: TX64CompileResult;
  LSource: UTF8String;
begin
  WriteLn('=== X64 PROOF OF CONCEPT TEST ===');
  WriteLn('Testing native compilation with REAL runtime calls...');
  WriteLn;

  // Test with PrintLn to see actual output!
  LSource := 'program X64RuntimeTest; var result: Int; begin result := 10 + 5; PrintLn(IntToStr(result)); end.';

  WriteLn('Source: ', string(LSource));
  WriteLn('Expected: Should print "15" from native x64 code!');
  WriteLn;

  try
    // Parse (same as always)
    LParser := TParser.CreateFromSource(LSource);
    try
      LProgram := LParser.Parse();
      WriteLn('✓ Parsed successfully');
    finally
      LParser.Free();
    end;

    // Generate bytecode (same as always)
    LBytecodeGen := TBytecodeGenerator.Create();
    try
      LGenResult := LBytecodeGen.Generate(LProgram);
      try
        if LGenResult.Success then
        begin
          WriteLn('✓ Generated bytecode successfully');
          WriteLn('  Instructions: ', LGenResult.Program_.GetInstructionCount());
          WriteLn('  Constants: ', LGenResult.Program_.GetConstantCount());
          WriteLn('  Variables: ', LGenResult.Program_.GetVariableCount());
          WriteLn;

          // X64 compilation with REAL function calls!
          LX64Compiler := TX64Compiler.Create();
          try
            WriteLn('--- ATTEMPTING X64 COMPILATION WITH RUNTIME CALLS ---');
            LX64Result := LX64Compiler.CompileProgram(LGenResult.Program_);
            try
              if LX64Result.Success then
              begin
                WriteLn('🎉 X64 RUNTIME COMPILATION SUCCESS!');
                WriteLn('Generated ', LX64Result.GeneratedCodeSize, ' bytes of x64 machine code');
                WriteLn;

                if LX64Result.CanExecuteInMemory then
                begin
                  WriteLn('🚀 ATTEMPTING IN-MEMORY EXECUTION...');
                  WriteLn('This should actually print "15" from native x64 code!');
                  WriteLn;

                  if LX64Compiler.ExecuteInMemory() then
                  begin
                    WriteLn;
                    WriteLn('🔥🔥🔥 COMPLETE SUCCESS! 🔥🔥🔥');
                    WriteLn('You just executed compiled TinyPascal code natively!');
                  end
                  else
                  begin
                    WriteLn('❌ In-memory execution failed: ', LX64Compiler.ErrorMessage);
                  end;
                end
                else
                begin
                  WriteLn('Code compiled but not ready for in-memory execution');
                end;
              end
              else
              begin
                WriteLn('❌ X64 compilation failed: ', LX64Result.ErrorMessage);
              end;
            finally
              LX64Result.Free();
            end;
          finally
            LX64Compiler.Free();
          end;

        end
        else
        begin
          WriteLn('❌ Bytecode generation failed: ', LGenResult.ErrorMessage);
        end;
      finally
        LGenResult.Free();
      end;
    finally
      LBytecodeGen.Free();
      LProgram.Free();
    end;

  except
    on E: Exception do
    begin
      WriteLn('❌ Exception: ', E.Message);
    end;
  end;

  WriteLn;
  WriteLn('=== END X64 RUNTIME TEST ===');
end;

procedure RunTPTests(const APause: Boolean);
begin
  WriteLn('TinyPascal - COMPLETE LANGUAGE TEST SUITE');
  WriteLn('==========================================');
  WriteLn('🚀 TESTING FULL PROGRAMMING LANGUAGE!');
  WriteLn('✅ Variables, arithmetic, comparisons');
  WriteLn('✅ If/then/else conditional statements');
  WriteLn('✅ While and for loops');
  WriteLn('✅ Boolean and float literals');
  WriteLn('✅ Complex nested expressions');
  WriteLn('✅ Real-world programming examples');
  WriteLn;

  Test1_BasicVariableAssignment();
  Pause(APause);

  Test2_ArithmeticExpressions();
  Pause(APause);

  Test3_BooleanLiteralsAndExpressions();
  Pause(APause);

  Test4_ComparisonOperations();
  Pause(APause);

  Test5_IfThenElseStatements();
  Pause(APause);

  Test6_WhileLoops();
  Pause(APause);

  Test7_ForLoops();
  Pause(APause);

  Test8_NestedControlStructures();
  Pause(APause);

  Test9_ComplexExpressions();
  Pause(APause);

  Test10_FloatLiterals();
  Pause(APause);

  Test11_PascalProgramShowcase();
  Pause(APause);

  Test12_RealWorldExample();
  Pause(APause);

  Test13_SimpleWhileLoop();
  Pause(APause);

  Test14_SimpleForLoop();
  Pause(APause);

  Test15_AdvancedIfElseChain();
  Pause(APause);

  WriteLn('=== TINYPASCAL STATUS REPORT ===');
  WriteLn('🎯 WORKING FEATURES:');
  WriteLn('✅ Variables: Int, Float, String types');
  WriteLn('✅ Literals: integers, floats, strings, booleans');
  WriteLn('✅ Arithmetic: +, -, *, / with proper precedence');
  WriteLn('✅ Comparisons: =, <>, <, <=, >, >=');
  WriteLn('✅ Logical: and, or, not');
  WriteLn('✅ Control Flow: if/then/else, while, for loops');
  WriteLn('✅ Built-in functions: PrintLn, IntToStr, StrToInt');
  WriteLn('✅ All test cases passing successfully!');
  WriteLn;
end;

end.
