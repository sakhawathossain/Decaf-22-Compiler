# Decaf-22 Compiler
### Python implementation of a compiler for the Decaf-22 (Decaf 19) programming language.

#### Usage:
Compiling machine code from decaf source file: \
```python3 ./generator.py <input-file>```

Producing output for intermediate steps: \
```python3 <module-name> <input-file>```

#### Executing the produced machine code:
The machine code can be executed on a MIPS simulator such as SPIM.

#### Modules:
- `scanner.py`: tokenize input stream.
- `parser.py:` abstract syntax tree generation.
- `analyzer.py:`syntactical analysis including symbol table generation and type checking.
- `generator.py`: machine code generation

#### Decaf 19 grammar:

```
Program      ::= Decl+ 
Decl         ::= VariableDecl | FunctionDecl
VariableDecl ::= Variable ;
Variable     ::= Type ident
Type         ::= int | double | bool | string
FunctionDecl ::= Type ident ( Formals ) StmtBlock |
                 void ident ( Formals ) StmtBlock
Formals      ::= Variable+, | epsilon
StmtBlock    ::= { VariableDecl* Stmt* }
Stmt         ::= <Expr>; | IfStmt | WhileStmt | ForStmt | BreakStmt |
                 ReturnStmt | PrintStmt | StmtBlock
IfStmt       ::= if ( Expr ) Stmt <else Stmt>
WhileStmt    ::= while ( Expr ) Stmt
ForStmt      ::= for ( <Expr>; Expr ; <Expr>) Stmt
ReturnStmt   ::= return <Expr> ;
BreakStmt    ::= break ;
PrintStmt    ::= Print ( Expr+, ) ;
Expr         ::= LValue = Expr | Constant | LValue | Call | ( Expr ) |
                 Expr + Expr | Expr - Expr | Expr * Expr | Expr = Expr |
                 Expr % Expr | - Expr | Expr < Expr | Expr <= Expr |
                 Expr > Expr | Expr >= Expr | Expr == Expr | Expr ! = Expr |
                 Expr && Expr | Expr || Expr | ! Expr | ReadInteger ( ) |
                 ReadLine ( )
LValue       ::= ident
Call         ::= ident ( Actuals )
Actuals      ::= Expr+, | epsilon
Constant     ::= intConstant | doubleConstant | boolConstant |
                 stringConstant | null
```
