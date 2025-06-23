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

unit TinyPascal.Common;

{$I TinyPascal.Defines.inc}

interface

uses
  System.SysUtils;

// Debug output control - no global variables exposed
procedure SetDebugOutput(const AEnabled: Boolean);
function GetDebugOutput(): Boolean;

// Debug output routines - automatically check enabled state
procedure DebugWriteLn(const AMessage: string); overload;
procedure DebugWrite(const AMessage: string);
procedure DebugWriteLn(const AFormat: string; const AArgs: array of const); overload;

implementation

var
  LDebugEnabled: Boolean = True; // Private to unit - default enabled for development

{ Debug Control Functions }

procedure SetDebugOutput(const AEnabled: Boolean);
begin
  LDebugEnabled := AEnabled;
end;

function GetDebugOutput(): Boolean;
begin
  Result := LDebugEnabled;
end;

{ Debug Output Functions }

procedure DebugWriteLn(const AMessage: string);
begin
  if not LDebugEnabled then Exit; // Early exit for performance
  WriteLn(AMessage);
end;

procedure DebugWrite(const AMessage: string);
begin
  if not LDebugEnabled then Exit; // Early exit for performance
  Write(AMessage);
end;

procedure DebugWriteLn(const AFormat: string; const AArgs: array of const);
begin
  if not LDebugEnabled then Exit; // Early exit for performance
  WriteLn(Format(AFormat, AArgs));
end;

end.
