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

program Testbed;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  TinyPascal.Lexer in '..\..\src\TinyPascal.Lexer.pas',
  UTestbed in 'UTestbed.pas',
  TinyPascal.AST in '..\..\src\TinyPascal.AST.pas',
  TinyPascal.Parser in '..\..\src\TinyPascal.Parser.pas',
  TinyPascal.Tests in '..\..\src\TinyPascal.Tests.pas',
  TinyPascal.Bytecode in '..\..\src\TinyPascal.Bytecode.pas',
  TinyPascal.VM in '..\..\src\TinyPascal.VM.pas',
  TinyPascal.BytecodeGen in '..\..\src\TinyPascal.BytecodeGen.pas',
  TinyPascal.Value in '..\..\src\TinyPascal.Value.pas',
  TinyPascal.Runtime in '..\..\src\TinyPascal.Runtime.pas',
  TinyPascal.X64Gen in '..\..\src\TinyPascal.X64Gen.pas',
  TinyPascal.Common in '..\..\src\TinyPascal.Common.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  try
    RunTestbed();
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
