# GOLD

# Introduction

Gold is a language very similar in syntax to C and GO. The aim for the language is to borrow
general programming constructs from well-known languages, whilst combining them to facilitate
ease of development. 

GOLD aims to be a simple language that takes advantage of the concept of struct, pointers, and
easy pointer manipulation by removing the complex rules of pointer manipulation one sees in C.
It allows for creation of type structs and at the same time, member functions that one can use
for a specific struct. This allows developers to take advantage of object oriented concepts.

# Requirements

- Ocaml and OPAM
- LLVM and its development libraries

# Usage

The test script in the root directory has a flag called `--help` that should
describe its use. This is the easiest way to run the test scripts in the `test/`
directory. To develop new source files in the GOLD language, simply create a source
file with the `.gold` suffix, and place it in the `test/` directory. Following this,
the test script should work consistently across already-existing source files,
and recently developed ones.

Currently, for each invocation of the `run.sh` script, the entire compiler gets rebuilt,
and then the targeted source file is compiled and executed.

To showcase the strongest capabilities of the language, run the `stair-stepper.gold` source file.
It utilizes many of the language constructs that GOLD successfully employs.

# Overview of Language Capabilities

Primitive data types
- bool
- int
- float
- string

Derived data types
- pointers
	- note: pointer semantics use `~` for dereferencing and `@` for addressing
- arrays

Control Flow
- if
- else
- for
- while

Library Functions
- print
- println
- sprint
- input
- srand
- rand
- atoi
