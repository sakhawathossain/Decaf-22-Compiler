#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar  9 16:22:08 2022

@author: shsaimon
"""

import sys
from scanner import Scanner

class Token:
    def __init__(self, name, lexeme, value, line, start, end):
        self.name = name
        self.lexeme = lexeme
        self.value = value
        self.line = line
        self.start = start
        self.end = end
    def istype(self, name):
        return name == self.name
    
    def __str__(self):
        return \
            'Token\n' + \
            '-----------\n' + \
            'name:   {0}\n'.format(self.name) + \
            'lexeme: {0}\n'.format(self.lexeme) + \
            'value:  {0}\n'.format(self.value) + \
            'line:   {0}\n'.format(self.line) + \
            'start:  {0}\n'.format(self.start) + \
            'end:    {0}\n'.format(self.end)

class ParseError(Exception):
    pass
            
class ASTNode:
    pass

class Program(ASTNode):
    def __init__(self):
        self.decls = []
    def print_tree(self):
        print()
        print('   Program: ')
        for decl in self.decls:
            decl.print_tree(tablevel = 1)
        
class VariableDecl(ASTNode):
    def __init__(self, variable):
        self.type_ = variable.type_
        self.line = variable.line
        self.ident = variable.ident
            
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}{:}{:}VarDecl: '.format(self.line, ' '*3*tablevel, prefix))
        self.type_.print_tree(tablevel+1)
        print('{:>3}{:}Identifier: {:}'.format(self.ident.line, ' '*3*(tablevel+1), self.ident.lexeme))

class Variable():
    def __init__(self, type_, ident):
        self.type_ = type_
        self.line = type_.line
        self.ident = ident
    
class FunctionDecl(ASTNode):
    def __init__(self, type_, ident, formals, stmtblock):
        self.type_ = type_
        self.line = type_.line
        self.ident = ident
        self.formals = formals
        self.stmtblock = stmtblock
        
    def print_tree(self, tablevel = 0):
        print('{:>3}{:}FnDecl: '.format(self.type_.line, ' '*3*tablevel))
        self.type_.print_tree(tablevel+1, '(return type) ')
        print('{:>3}{:}Identifier: {:}'.format(self.ident.line, ' '*3*(tablevel+1), self.ident.lexeme))
        for vardecl in self.formals:
            vardecl.print_tree(tablevel + 1, '(formals) ')
        self.stmtblock.print_tree(tablevel + 1, '(body) ')
        
class Type(ASTNode):
    def __init__(self, token):
        self.typeval = token.lexeme
        self.line = token.line
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('   {:}{:}Type: {:}'.format(' '*3*tablevel, prefix, self.typeval))
            
class StmtBlock(ASTNode):
    def __init__(self):
        self.vardecls = []
        self.stmts = []
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('   {:}{:}StmtBlock: '.format(' '*3*tablevel, prefix))
        for vardecl in self.vardecls:
            vardecl.print_tree(tablevel+1)
        for stmt in self.stmts:
            stmt.print_tree(tablevel+1)

class IfStmt(ASTNode):
    def __init__(self, test, stmt, elsestmt = None):
        self.test = test
        self.stmt = stmt
        self.elsestmt = elsestmt
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('   {:}{:}IfStmt: '.format(' '*3*tablevel, prefix))
        self.test.print_tree(tablevel + 1, '(test) ')
        self.stmt.print_tree(tablevel + 1, '(then) ')
        if self.elsestmt != None:
            self.elsestmt.print_tree(tablevel+1, '(else) ')

class WhileStmt(ASTNode):
    def __init__(self, test, stmt):
        self.test = test
        self.stmt = stmt
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('   {:}{:}WhileStmt: '.format(' '*3*tablevel, prefix))
        self.test.print_tree(tablevel + 1, '(test) ')
        self.stmt.print_tree(tablevel + 1, '(body) ')

class ForStmt(ASTNode):
    def __init__(self, test, stmt, init = None, step = None):
        self.init = init
        self.test = test
        self.step = step
        self.stmt = stmt
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('   {:}{:}ForStmt: '.format(' '*3*tablevel, prefix))
        if self.init:
            self.init.print_tree(tablevel + 1, '(init) ')
        else:
            print('   {:}(init) Empty: '.format(' '*3*(tablevel+1)))
        self.test.print_tree(tablevel + 1, '(test) ')
        if self.step:
            self.step.print_tree(tablevel + 1, '(step) ')
        else:
            print('   {:}(step) Empty: '.format(' '*3*(tablevel+1)))
        self.stmt.print_tree(tablevel+1, '(body) ')

class BreakStmt(ASTNode):
    def __init__(self, token):
        self.line = token.line
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}{:}{:}BreakStmt: '.format(self.line, ' '*3*tablevel, prefix))

class ReturnStmt(ASTNode):
    def __init__(self, token, expr = None):
        self.line = token.line
        self.expr = expr
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}{:}{:}ReturnStmt: '.format(self.line, ' '*3*tablevel, prefix))
        if self.expr:
            self.expr.print_tree(tablevel + 1)
        else:
            print('   {:}Empty: '.format(' '*3*(tablevel+1)))

class PrintStmt(ASTNode):
    def __init__(self, token, exprs):
        self.line = token.line
        self.exprs = exprs
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('   {:}{:}PrintStmt: '.format(' '*3*tablevel, prefix))
        for expr in self.exprs:
            expr.print_tree(tablevel + 1, '(args) ')

class Expr(ASTNode):
    pass

class AssignExpr(Expr):
    def __init__(self, L, op, R):
        self.L = L
        self.op = op
        self.R = R
        self.line = L.line
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}{:}{:}AssignExpr: '.format(self.line, ' '*3*tablevel, prefix))
        self.L.print_tree(tablevel + 1)
        print('{:>3}{:}Operator: ='.format(self.op.line, ' '*3*(tablevel+1)))
        self.R.print_tree(tablevel + 1)

class CallExpr(Expr):
    def __init__(self, ident = None, actuals = []):
        self.ident = ident
        self.actuals = actuals
        self.line = self.ident.line
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}{:}{:}Call: '.format(self.line, ' '*3*tablevel, prefix))
        print('{:>3}{:}Identifier: {:}'.format(self.ident.line, ' '*3*(tablevel+1), self.ident.lexeme))
        for actual in self.actuals:
            actual.print_tree(tablevel+1, '(actuals) ')

class BinaryExpr(Expr):
    def __init__(self, exprname, L = None, op = None, R = None):
        self.exprname = exprname
        self.L = L
        self.op = op
        self.R = R
        self.line = self.L.line
    
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}{:}{:}{:}: '.format(self.line,' '*3*tablevel, prefix, self.exprname))
        self.L.print_tree(tablevel + 1)
        print('{:>3}{:}Operator: {:}'.format(self.op.line, ' '*3*(tablevel+1), self.op.lexeme))
        self.R.print_tree(tablevel + 1)

class UnaryExpr(Expr):
    def __init__(self, op, R):
        self.op = op
        self.R = R
        self.line = op.line
        self.exprname = 'ArithmeticExpr' if op.name == '-' else 'LogicalExpr'
    
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}{:}{:}{:}: '.format(self.line,' '*3*tablevel, prefix, self.exprname))
        print('{:>3}{:}Operator: {:}'.format(self.op.line, ' '*3*(tablevel+1), self.op.name))
        self.R.print_tree(tablevel + 1)

class IdentExpr(Expr):
    def __init__(self, token):
        self.token = token
        self.line = token.line
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}{:}{:}FieldAccess: '.format(self.line, ' '*3*tablevel, prefix))
        print('{:>3}{:}Identifier: {:}'.format(self.line, ' '*3*(tablevel+1), self.token.lexeme))

class ConstantExpr(Expr):
    def __init__(self, token):
        self.token = token
        self.line = token.line
        self.printval = token.value
        if self.token.name == 'T_StringConstant':
            self.printval = '"' + token.value + '"'
        if self.token.name == 'T_BoolConstant':
            self.printval = str(self.token.value).lower()
        #self.printval = '"' + token.value + '"' if self.token.name == 'T_StringConstant' else token.value
    
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}{:}{:}{:}: {:}'.format(self.line, ' '*3*tablevel, prefix, self.token.name[2:], self.printval))

class ReadIntegerExpr(Expr):
    def __init__(self, line):
        self.line = line
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}{:}{:}ReadIntegerExpr: '.format(self.line, ' '*3*tablevel, prefix))

class ReadLineExpr(Expr):
    def __init__(self, line):
        self.line = line
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}{:}{:}ReadLineExpr: '.format(self.line, ' '*3*tablevel, prefix))

class Parser:
    """Parser for Decaf-22"""
    
    def __init__(self, text, verbose = False):
        self.text = text
        self.textlines = text.split('\n')
        tokenlist = Scanner().scan(self.text)
        self.tokenlist = [Token(*item) for item in tokenlist]
        self.pos = 0
        self.verbose = verbose
        return
        
    def get_program(self):
        program = Program()
        program.decls.append(self.get_decl())
        while self.get_next_token():
            decl = self.get_decl()  
            program.decls.append(decl)
        return program
    
    def get_decl(self):
        type_ = self.get_type()
        ident = self.consume_token('T_Identifier')
        if self.is_next_token(';') and type_.typeval != 'void':
            self.consume_token()
            return VariableDecl(Variable(type_, ident))
        elif self.is_next_token('('):
            self.consume_token('(')
            formals = self.get_formals()
            self.consume_token(')')
            stmtblock = self.get_stmtblock()
            return FunctionDecl(type_, ident, formals, stmtblock)
        else:
            raise ParseError
            
    def get_type(self, allow_void = True):
        token = self.get_next_token()
        if not self.is_next_token(['T_Int', 'T_Double', 'T_Bool', 'T_String', 'T_Void']):
            raise ParseError
        if token.istype('T_Void') and not allow_void:
            raise ParseError
        return Type(self.consume_token())

    def get_formals(self):
        if self.is_next_token(')'):
            return []
        formals = [VariableDecl(self.get_variable())]
        if self.is_next_token(','):
            self.consume_token()
            formals.extend(self.get_formals())
        return formals
    
    def get_variable(self):
        type_ = self.get_type(allow_void = False)
        ident = self.consume_token('T_Identifier')
        return Variable(type_, ident)
    
    def get_stmtblock(self):
        self.consume_token('{')
        # consume variableDecls
        stmtBlock = StmtBlock()
        while self.is_next_token(['T_Int', 'T_Double', 'T_Bool', 'T_String']):
            stmtBlock.vardecls.append(VariableDecl(self.get_variable()))
            self.consume_token(';')
        # consume Stmts
        while not self.is_next_token('}'):
            stmt = self.get_stmt()
            if stmt:
                stmtBlock.stmts.append(stmt)
        self.consume_token('}')
        return stmtBlock
    
    def get_stmt(self):
        stmt_func_map = {'T_If': self.get_ifstmt,
                          'T_While': self.get_whilestmt,
                          'T_For': self.get_forstmt,
                          'T_Break': self.get_breakstmt,
                          'T_Return': self.get_returnstmt,
                          'T_Print': self.get_printstmt,
                          '{': self.get_stmtblock}
        if self.is_next_token(stmt_func_map.keys()):
            return stmt_func_map[self.get_next_token().name]()
        elif self.is_next_token(';'):
            self.consume_token(';')
            return None
        else:
            expr = self.get_expr()
            self.consume_token(';')
            return expr
    
    def get_ifstmt(self):
        self.consume_token('T_If')
        self.consume_token('(')
        test = self.get_expr()
        self.consume_token(')')
        stmt = self.get_stmt()
        if self.get_next_token().istype('T_Else'):
            self.consume_token('T_Else')
            return IfStmt(test, stmt, self.get_stmt())
        return IfStmt(test, stmt)
    
    def get_whilestmt(self):
        self.consume_token('T_While')
        self.consume_token('(')
        test = self.get_expr()
        self.consume_token(')')
        return WhileStmt(test, self.get_stmt())
    
    def get_forstmt(self):
        init, step = None, None
        self.consume_token('T_For')
        self.consume_token('(')
        if not self.get_next_token().istype(';'):
            init = self.get_expr()
        self.consume_token(';')
        test = self.get_expr()
        self.consume_token(';')
        if not self.get_next_token().istype(')'):
            step = self.get_expr()
        self.consume_token(')')
        return ForStmt(test, self.get_stmt(), init, step)
    
    def get_breakstmt(self):
        token = self.consume_token('T_Break')
        self.consume_token(';')
        return BreakStmt(token)
    
    def get_returnstmt(self):
        token, expr = self.consume_token('T_Return'), None
        if not self.get_next_token().istype(';'):
            expr = self.get_expr()
        self.consume_token(';')
        return ReturnStmt(token, expr)
    
    def get_printstmt(self):
        token = self.consume_token('T_Print')
        self.consume_token('(')
        exprs = [self.get_expr()]
        while not self.get_next_token().istype(')'):
            self.consume_token(',')
            exprs.append(self.get_expr())
        self.consume_token(')')
        self.consume_token(';')
        return PrintStmt(token, exprs)
    
    def get_expr(self):
        '''
        Reference grammar:
        Expr    ::=  LValue = Expr | Constant | LValue | Call | (Expr) |
                     Expr + Expr | Expr - Expr | Expr * Expr | Expr = Expr |
                     Expr % Expr | - Expr | Expr < Expr | Expr <= Expr |
                     Expr > Expr | Expr >= Expr | Expr == Expr | Expr ! = Expr |
                     Expr && Expr | Expr || Expr | ! Expr | ReadInteger() |
                     ReadLine()
        LValue  ::   ident
        Call    ::=  ident ( Actuals )
        Actuals ::=  Expr+, | eps  
                 
        Operator precedence:
        ! -          (unary -, logical not)
        * / %        (multiply, divide, mod)
        + -          (addition, subtraction)
        < <= > >=    (relational)
        == !=        (equality)
        &&           (logical and)
        ||           (logical or)
        =            (assignment)
                 
        Modified grammar:
        Expr    ::=  ident = Eor | Eor
        Eor     ::=  End || Eor | End
        End     ::=  Eeq && End | Eeq
        Eeq     ::=  Ere (==, !=) Eeq | Ere ### Check with professor if associative
        Ere     ::=  Ead (<, <=, >, >=) Ead | Ead
        Ead     ::=  Epr (+, -) Ead | Epr
        Epr     ::=  Eun (*, /, %) Epr | Eun
        Eun     ::=  (!, -) Eun | T
        T       ::=  Constant | ident Q | (Eor) | ReadInteger() | ReadLine()
        Q       ::=  (Expr+,) | eps
        '''
        
        L = self.get_next_token()
        if L.istype('T_Identifier'):
            self.consume_token()
            if self.get_next_token().istype('='):
                # Expr ::= ident = Eor
                op = self.consume_token()
                return AssignExpr(IdentExpr(L), op, self.get_expr_or())               
            else:
                # Expr ::= Eor
                return self.get_expr_or(L)
        else:
            return self.get_expr_or()
                
    def get_expr_or(self, L = None):
        expr = self.get_expr_and(L)
        token = self.get_next_token()
        while token.istype('T_Or'):
            self.consume_token()
            right = self.get_expr_and()
            expr = BinaryExpr('LogicalExpr', expr, token, right)
            token = self.get_next_token()
        return expr
        
    def get_expr_and(self, L = None):
        expr = self.get_expr_eq(L)
        token = self.get_next_token()
        while token.istype('T_And'):
            self.consume_token()
            right = self.get_expr_eq()
            expr = BinaryExpr('LogicalExpr', expr, token, right)
            token = self.get_next_token()
        return expr
    
    def get_expr_eq(self, L = None):
        expr = self.get_expr_rel(L)
        token = self.get_next_token()
        while token.lexeme in ['==', '!=']:
            self.consume_token()
            right = self.get_expr_rel()
            expr = BinaryExpr('EqualityExpr', expr, token, right)
            token = self.get_next_token()
        return expr
    
    def get_expr_rel(self, L = None):
        expr = self.get_expr_add(L)
        token = self.get_next_token()
        while token.lexeme in ['<', '<=', '>', '>=']:
            self.consume_token()
            right = self.get_expr_add() # no associativity
            expr = BinaryExpr('RelationalExpr', expr, token, right)
            token = self.get_next_token()
        return expr
    
    def get_expr_add(self, L = None):
        expr = self.get_expr_prod(L)
        token = self.get_next_token()
        while token.name in ['+', '-']:
            self.consume_token()
            right = self.get_expr_prod()
            expr = BinaryExpr('ArithmeticExpr', expr, token, right)
            token = self.get_next_token()
        return expr
    
    def get_expr_prod(self, L = None):
        expr = self.get_expr_unary(L)
        token = self.get_next_token()
        while token.name in ['*', '/', '%']:
            self.consume_token()
            right = self.get_expr_unary()
            expr = BinaryExpr('ArithmeticExpr', expr, token, right)
            token = self.get_next_token()
        return expr
    
    def get_expr_unary(self, L = None):
        #print('Get unary: remaining tokens')
        #for k, tok in zip(range(self.pos, len(self.tokenlist)), self.tokenlist[self.pos:]):
        #    print('\t', k, ':', tok.name, tok.lexeme)
        if L == None: 
            token = self.get_next_token()             
            if token.name in ['-', '!']:
                self.consume_token()
                return UnaryExpr(token, self.get_expr_unary())
            else:
                return self.get_terminal()
        return self.get_terminal(L)
    
    def get_terminal(self, L = None):
        if L == None:
            token = self.get_next_token()
        else:
            token = L
        if token.istype('T_Identifier'):
            if self.get_next_token().istype('('):
                # ident (Actuals)
                self.consume_token('(')
                expr = CallExpr(L, self.get_actuals())
                self.consume_token(')')

                return expr
            else:
                if L == None:
                    self.consume_token()
                return IdentExpr(token)
        elif token.name in ['T_IntConstant', 'T_DoubleConstant', 'T_StringConstant', 'T_BoolConstant']:
            self.consume_token()
            return ConstantExpr(token)
        elif token.istype('('):
            self.consume_token('(')
            expr = self.get_expr_or()
            self.consume_token(')')
            return expr
        elif token.istype('T_ReadInteger'):
            tok = self.consume_token()
            self.consume_token('(')
            self.consume_token(')')
            return ReadIntegerExpr(tok.line)
        elif token.istype('T_ReadLine'):
            tok = self.consume_token()
            self.consume_token()
            self.consume_token('(')
            self.consume_token(')')
            return ReadLineExpr(tok.line)
        else:
            raise ParseError
        
            
    def get_actuals(self):
        actuals = []
        if not self.get_next_token().istype(')'):
            actuals.append(self.get_expr())
        while not self.get_next_token().istype(')'):
            self.consume_token(',')
            actuals.append(self.get_expr())            
        return actuals
    
    def is_next_token(self, name):
        """
        return True if the next token type matches argumemnt
        if name is not of type str, assume it is an collection of string
        if a collection is passed, then return True if any of them match the next token type
        """
        if isinstance(name, str):
            return self.get_next_token().name == name
        return any([self.get_next_token().name == n for n in name])
            
    def get_next_token(self):
        """
        returns a reference to the next token object
        this does not consume the token
        """
        if self.pos >= len(self.tokenlist):
            return None
        return self.tokenlist[self.pos]
    
    def consume_token(self, name = None):
        """
        consume the next token and advance token pointer
        if a token name is given, then check if the next token matches the given name
        if there is a mismatch, raise exception
        """
        if name != None:
            if not self.is_next_token(name):
                raise ParseError
        self.pos += 1
        return self.tokenlist[self.pos-1]
        
    def print_error(self, token):
        """
        print the corresponding line and highlight the token that caused the error
        """
        print('\n*** Error line {:}.'.format(token.line))
        print(self.textlines[token.line-1])
        print(' '* (token.start - 1) + '^' * (token.end - token.start + 1))
        print('*** syntax error\n')
            
    def parse(self):
        """
        parse the input string
        """
        try:
            program = self.get_program()
        except ParseError:
            self.print_error(self.get_next_token())
            return
        if self.verbose:
            program.print_tree()
        return program


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print('Please enter a filepath as the program argument')
    else:
        path = sys.argv[1]
        try:
            with open(path) as file:
                text = file.read()
                Parser(text, verbose = True).parse()
        except IOError:
            print('Could not read file: {0}'.format(path))