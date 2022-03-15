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
        print('   Program:')
        for decl in self.decls:
            decl.print_tree(tablevel = 1)
        
class Decl(ASTNode):
    pass
        
class VariableDecl(Decl):
    def __init__(self, variable = None):
        if variable:
            # collapse the intermediate node "variable" on the fly
            self.type_ = variable.type_
            self.ident = variable.ident
            
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}{:}{:}VarDecl:'.format(self.type_.line, ' '*3*tablevel, prefix))
        self.type_.print_tree(tablevel+1)
        print('{:>3}{:}Identifier: {:}'.format(self.ident.line, ' '*3*(tablevel+1), self.ident.lexeme))

class Variable():
    pass
    
class FunctionDecl(Decl):
    def print_tree(self, tablevel = 0):
        print('{:>3}{:}FnDecl:'.format(self.type_.line, ' '*3*tablevel))
        self.type_.print_tree(tablevel+1, '(return type) ')
        print('{:>3}{:}Identifier: {:}'.format(self.ident.line, ' '*3*(tablevel+1), self.ident.lexeme))
        for vardecl in self.formals:
            vardecl.print_tree(tablevel + 1, '(formals) ')
        self.stmtblock.print_tree(tablevel + 1, '(body) ')
        
class Type(ASTNode):
    def print_tree(self, tablevel = 0, prefix = ''):
        print('   {:}{:}Type: {:}'.format(' '*3*tablevel, prefix, self.typeval))
            
class StmtBlock(ASTNode):
    def __init__(self):
        self.vardecls = []
        self.stmts = []
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('   {:}{:}StmtBlock:'.format(' '*3*tablevel, prefix))
        for vardecl in self.vardecls:
            vardecl.print_tree(tablevel+1)
        # TODO: print stmts
        for stmt in self.stmts:
            stmt.print_tree(tablevel+1)

class Stmt(ASTNode):
    pass

class IfStmt(Stmt):
    def print_tree(self, tablevel = 0, prefix = ''):
        print('   {:}{:}IfStmt:'.format(' '*3*tablevel, prefix))
        self.test.print_tree(tablevel + 1, '(test) ')
        self.stmt.print_tree(tablevel + 1, '(then) ')
        if self.elsestmt != None:
            self.elsestmt.print_tree(tablevel+1, '(else) ')

class WhileStmt(Stmt):
    def print_tree(self, tablevel = 0, prefix = ''):
        print('   {:}{:}WhileStmt:'.format(' '*3*tablevel, prefix))
        self.test.print_tree(tablevel + 1, '(test) ')
        self.stmt.print_tree(tablevel + 1, '(body) ')

class ForStmt(Stmt):
    def __init__(self):
        self.init = None
        self.test = None
        self.step = None
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('   {:}{:}ForStmt:'.format(' '*3*tablevel, prefix))
        if self.init:
            self.init.print_tree(tablevel + 1, '(init) ')
        self.test.print_tree(tablevel + 1, '(test )')
        if self.step:
            self.step.print_tree(tablevel + 1, '(test) ')
        self.stmt.print_tree(tablevel+1, '(body) ')

class BreakStmt(Stmt):
    def __init__(self, line):
        self.line = line
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}   {:}{:}BreakStmt:'.format(self.line, ' '*3*tablevel, prefix))

class ReturnStmt(Stmt):
    def __init__(self):
        self.expr = None
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}   {:}{:}ReturnStmt:'.format(self.line, ' '*3*tablevel, prefix))
        if self.expr:
            self.expr.print_tree(tablevel + 1)
        else:
            print('   {:}Empty:'.format(' '*3*(tablevel+1)))

class PrintStmt(Stmt):
    def print_tree(self, tablevel = 0, prefix = ''):
        print('   {:}{:}PrintStmt:'.format(' '*3*tablevel, prefix))
        # TODO: print args

class Expr(ASTNode):
    pass

class AssignExpr(Expr):
    def __init__(self, L, R):
        self.L = L
        self.R = R
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('   {:}{:}AssignExpr:'.format(' '*3*tablevel, prefix))
        self.L.print_tree(tablevel + 1)
        print('   {:}Operator: ='.format(' '*3*(tablevel+1)))
        self.R.print_tree(tablevel + 1)

class CallExpr(Expr):
    def __init__(self, ident = None, actuals = []):
        self.ident = ident
        self.actuals = actuals
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('   {:}{:}Call:'.format(' '*3*tablevel, prefix))
        self.ident.print_tree(tablevel + 1)
        for actual in self.actuals:
            actual.print_tree(tablevel+1, '(actuals)')

class BinaryExpr(Expr):
    def __init__(self, exprname, L = None, op = None, R = None):
        self.exprname = exprname
        self.L = L
        self.op = op
        self.R = R
        self.line = self.L.line
    
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}{:}{:}{:}:'.format(self.line,' '*3*tablevel, prefix, self.exprname))
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
        print('{:>3}{:}{:}{:}:'.format(self.line,' '*3*tablevel, prefix, self.exprname))
        print('{:>3}{:}Operator: {:}'.format(self.op.line, ' '*3*(tablevel+1), self.op.name))
        self.R.print_tree(tablevel + 1)

class IdentExpr(Expr):
    def __init__(self, token):
        self.token = token
        self.line = token.line
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}{:}{:}FieldAccess:'.format(self.line, ' '*3*tablevel, prefix))
        print('{:>3}{:}Identifier: {:}'.format(self.line, ' '*3*(tablevel+1), self.token.lexeme))

class ConstantExpr(Expr):
    def __init__(self, token):
        self.token = token
        self.line = token.line
    
    def print_tree(self, tablevel = 0, prefix = ''):
        print('{:>3}{:}{:}{:}: {:}'.format(self.line, ' '*3*tablevel, prefix, self.token.name[2:], self.token.value))

class ReadIntegerExpr(Expr):
    pass

class ReadLineExpr(Expr):
    pass

class Parser:
    
    def __init__(self, text):
        self.text = text
        self.textlines = text.split('\n')
        tokenlist = Scanner().scan(self.text)
        self.tokenlist = [Token(*item) for item in tokenlist]
        self.pos = 0
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
        token = self.get_next_token()
        if token.istype(';') and type_.typeval != 'void':
            decl = VariableDecl()
            decl.type_ = type_
            decl.ident = ident
            decl.line = type_.line
            self.consume_token(';')
            return decl
        elif token.istype('('):
            decl = FunctionDecl()
            decl.type_ = type_
            decl.ident = ident
            decl.line = type_.line
            self.consume_token('(')
            decl.formals = self.get_formals()
            self.consume_token(')')
            decl.stmtblock = self.get_stmtblock()
            return decl
        else:
            raise ParseError
            
    def get_type(self, allow_void = True):
        token = self.get_next_token()
        if token.name not in ['T_Int', 'T_Double', 'T_Bool', 'T_String', 'T_Void']:
            raise ParseError
        if token.istype('T_Void') and not allow_void:
            raise ParseError
        type_ = Type()
        type_.typeval = token.lexeme
        type_.line = token.line
        self.consume_token()
        return type_

    def get_formals(self):
        token = self.get_next_token()
        if token.istype(')'):
            return []
        formals = [VariableDecl(self.get_variable())]
        if self.get_next_token().istype(','):
            self.consume_token()
            formals.extend(self.get_formals())
        return formals
    
    def get_variable(self):
        type_ = self.get_type(allow_void = False)
        ident = self.consume_token('T_Identifier')
        variable = Variable()
        variable.type_ = type_
        variable.ident = ident
        variable.line = type_.line
        return variable
    
    def get_stmtblock(self):
        self.consume_token('{')
        # consume variableDecls
        stmtBlock = StmtBlock()
        token = self.get_next_token()
        while token.name in ['T_Int', 'T_Double', 'T_Bool', 'T_String']:
            stmtBlock.vardecls.append(VariableDecl(self.get_variable()))
            self.consume_token(';')
            token = self.get_next_token()
        # consume Stmts
        while not self.get_next_token().istype('}'):
            stmt = self.get_stmt()
            if stmt != None:
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
        token = self.get_next_token()
        if token.name in stmt_func_map:
            return stmt_func_map[token.name]()
        elif token.istype(';'):
            return None
        else:
            expr = self.get_expr()
            self.consume_token(';')
            return expr
    
    def get_ifstmt(self):
        ifstmt = IfStmt()
        self.consume_token('T_If')
        self.consume_token('(')
        ifstmt.test = self.get_expr()
        self.consume_token(')')
        ifstmt.stmt = self.get_stmt()
        ifstmt.elsestmt = None
        if self.get_next_token().istype('T_Else'):
            self.consume_token('T_Else')
            ifstmt.elsestmt = self.get_stmt()
        return ifstmt
    
    def get_whilestmt(self):
        whilestmt = WhileStmt()
        self.consume_token('T_While')
        self.consume_token('(')
        whilestmt.test = self.get_expr()
        self.consume_token(')')
        whilestmt.stmt = self.get_stmt()
        return whilestmt
    
    def get_forstmt(self):
        forstmt = ForStmt()
        self.consume_token('T_For')
        self.consume_token('(')
        if not self.get_next_token().istype(';'):
            forstmt.init = self.get_expr()
        self.consume_token(';')
        forstmt.test = self.get_expr()
        self.consume_token(';')
        if not self.get_next_token().istype(')'):
            forstmt.step = self.get_expr()
        self.consume_token(')')
        forstmt.stmt = self.get_stmt()
        return forstmt
    
    def get_breakstmt(self):
        token = self.consume_token('T_Break')
        self.consume_token(';')
        return BreakStmt(token.line)
    
    def get_returnstmt(self):
        returnstmt = ReturnStmt()
        token = self.consume_token('T_Return')
        returnstmt.line = token.line
        if not self.get_next_token().istype(';'):
            returnstmt.expr = self.get_expr()
        self.consume_token(';')
        return returnstmt
    
    def get_printstmt(self):
        printStmt = PrintStmt()
        self.consume_token('T_Print')
        self.consume_token('(')
        # TODO: consume arg Exprs
        self.consume_token(')')
        self.consume_token(';')
        return printStmt
    
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
        Eeq     ::=  Ere (==, !=) Eeq | Ere ### Check with prof if associative
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
                self.consume_token()
                return AssignExpr(IdentExpr(L), self.get_expr_or())               
            else:
                # Expr ::= Eor
                return self.get_expr_or(L)
        else:
            return self.get_expr_or()
                
    def get_expr_or(self, L = None):
        expr = self.get_expr_and(L)
        token = self.get_next_token()
        while token.istype('||'):
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
            expr = BinaryExpr('LogicalExpr', expr, token, right)
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
                expr = CallExpr(IdentExpr(L), self.get_actuals())
                self.consume_token(')')
                return expr
            else:
                return IdentExpr(L)
        elif token.name in ['T_IntConstant', 'T_DoubleConstant', 'T_StringConstant', 'T_BoolConstant']:
            self.consume_token()
            return ConstantExpr(token)
        elif token.istype('('):
            self.consume_token('(')
            expr = self.get_expr_or()
            self.consume_token(')')
            return expr
        elif token.istype('T_ReadInteger'):
            self.consume_token()
            return ReadIntegerExpr()
        elif token.istype('T_ReadLine'):
            self.consume_token()
            return ReadLineExpr()
        else:
            raise ParseError
        
            
    def get_actuals(self):
        # TODO: implement
        return []
            
    
    def get_next_token(self):
        if self.pos >= len(self.tokenlist):
            return None
        return self.tokenlist[self.pos]
    
    """ consume the next token and advance pointer """
    """ if a token name is given, then check if the next token matches the given name """
    """ otherwise, raise exception """
    def consume_token(self, name = None):
        token = self.get_next_token()
        if name != None:
            if not token.istype(name):
                raise ParseError
        self.pos += 1
        return token
        
    """ print the corresponding line and highlight the token that caused the error """
    def print_error(self, token):
        line, start, end = token.line-1, token.start-1, token.end-1
        print('*** error line {:}.'.format(line+1))
        print(self.textlines[line])
        print(' '* start + '^' * (end - start + 1))
        print('*** syntax error')
            

    def parse(self):
        try:
            program = self.get_program()
        except ParseError:
            self.print_error(self.get_next_token())
            return
        program.print_tree()
        return    


        
            
            
    
if __name__ == "__main__":
    if len(sys.argv) < 2:
        print('Please enter a filepath as the program argument')
    else:
        path = sys.argv[1]
        try:
            with open(path) as file:
                text = file.read()
                Parser(text).parse()
        except IOError:
            print('Could not read file: {0}'.format(path))