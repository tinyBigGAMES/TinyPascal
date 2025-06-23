# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- **Add array literals and X64 compilation proof of concept** (2025-06-23 – jarroddavis68)
  - Add static and dynamic array declaration syntax
  - Implement array literal initialization [1, 2, 3, 4, 5]
  - Add array element access and assignment (arr[index])
  - Implement Length() builtin function for arrays
  - Add X64 compilation proof of concept with in-memory execution
  - Generate working x64 machine code with runtime calls
  - Add 8 array operation tests (all passing)
  - Achieve 27/27 test suite success rate
  - Validate complex programs: factorial, prime detection, games
  - Establish dual execution paths: VM interpretation + X64 proof of concept

- **feat: Add native x64 code generation with in-memory execution** (2025-06-23 – jarroddavis68)
  - Complete TinyPascal → native x64 machine code compilation
  - Arithmetic operations (ADD, SUB, MUL, DIV) working
  - Variable management with Windows x64 calling convention
  - Runtime function calls (PrintLn, IntToStr) integrated
  - In-memory execution of generated code
  - Test case: "10 + 5" compiles to 97 bytes, executes natively, prints "15"
  - Added debug output integration with TinyPascal.Common

- **TinyPascal: 100% test pass, core language operational, solid VM & compiler pipeline** (2025-06-22 – jarroddavis68)
  - All 15/15 tests passing (variables, control flow, math, I/O)
  - Fully implemented: lexer, parser, bytecode compiler, VM
  - Supports Int/UInt/Float/String/Boolean types
  - Implements WriteLn, ReadLn, IntToStr, etc.
  - Sub-ms execution time, 419 instr max, 3-depth stack
  - No arrays, procedures, or advanced types yet
  - Architecture stable, ready for feature expansion

- **Create FUNDING.yml** (2025-06-22 – Jarrod Davis)


### Changed
- **Merge branch 'main' of https://github.com/tinyBigGAMES/TinyPascal** (2025-06-22 – jarroddavis68)

- **Update README.md** (2025-06-22 – jarroddavis68)

- **Repo Update** (2025-06-22 – jarroddavis68)
  - Setting up repo

- **Initial commit** (2025-06-22 – Jarrod Davis)


### Removed
- **Remove old files** (2025-06-22 – jarroddavis68)

