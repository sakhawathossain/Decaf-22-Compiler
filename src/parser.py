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
            
class StmtBlock(ASTNode):
    def __init__(self):
        self.vardecls = []
        self.stmts = []
        
    def print_tree(self, tablevel = 0, prefix = ''):
        print('   {:}{:}StmtBlock:'.format(' '*3*tablevel, prefix))
        for vardecl in self.vardecls:
            vardecl.print_tree(tablevel+1)
        # TODO: print stmts

class Stmt(ASTNode):
    pass

class IfStmt(Stmt):
    pass

class WhileStmt(Stmt):
    pass

class ForStmt(Stmt):
    pass

class BreakStmt(Stmt):
    pass

class ReturnStmt(Stmt):
    pass

class PrintStmt(Stmt):
    pass


    
class Type(ASTNode):
    def print_tree(self, tablevel = 0, prefix = None):
        prefix = prefix + ' ' if prefix != None else ''
        print('   {:}{:}Type: {:}'.format(' '*3*tablevel, prefix, self.typeval))

class Parser:
    
    def __init__(self, text):
        self.text = text
        self.textlines = text.split('\n')
        tokenlist = Scanner().scan(self.text)
        self.tokenlist = [Token(*item) for item in tokenlist]
        self.pos = 0
        return
        
    def parse(self):
        try:
            program = self.get_program()
        except ParseError:
            self.print_error(self.get_next_token())
            return
        program.print_tree()
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
        # TODO: consume stmts
        self.consume_token('}')
        return stmtBlock
    
    def get_stmt(self):
        stmt_func_map = {'T_If': self.get_ifstmt,
                          'T_While': self.get_whilestmt,
                          'T_For': self.get_forstmt,
                          'T_Break': self.get_breakstmt,
                          'T_Return': self.get_returnstmt,
                          'T_Print': self.get_printstmt}
        token = self.get_next_token()
        if token.name not in stmt_func_map:
            raise ParseError
        return stmt_func_map[token.name]()
    
    def get_ifstmt(self):
        pass
    
    def get_whilestmt(self):
        pass
    
    def get_forstmt(self):
        pass
    
    def get_breakstmt(self):
        pass
    
    def get_returnstmt(self):
        pass
    
    def get_printstmt(self):
        pass
    
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