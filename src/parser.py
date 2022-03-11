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
            decl.print_tree(indent = 1)
        
class Decl(ASTNode):
    pass
        
class VariableDecl(Decl):
    def print_tree(self, indent = 0, prefix = None):
        prefix = prefix + ' ' if prefix != None else ''
        print('{:>3}{:}{:}VarDecl:'.format(self.line, ' '*3*indent, prefix))
        self.type_.print_tree(indent+1)
        print('{:>3}{:}Identifier: {:}'.format(self.ident.line, ' '*3*(indent+1), self.ident.lexeme))
    
class FunctionDecl(Decl):
    def __init__(self):
        pass
    def print_tree(self, indent = 0):
        print('{:>3}{:}FnDecl:'.format(self.line, ' '*3*indent))
        self.type_.print_tree(indent+1, '(return type)')
        print('{:>3}{:}Identifier: {:}'.format(self.ident.line, ' '*3*(indent+1), self.ident.lexeme))
        for vardecl in self.formals:
            vardecl.print_tree(indent +1, '(formals)')
            
class StmtBlock(ASTNode):
    def __init__(self):
        self.vardecls = []
        self.stmts = []
    
class Type(ASTNode):
    pass
    def print_tree(self, indent = 0, prefix = None):
        prefix = prefix + ' ' if prefix != None else ''
        print('   {:}{:}Type: {:}'.format(' '*3*indent, prefix, self.typeval))

class Parser:
    def get_program(self):
        program = Program()
        decl = self.get_decl()
        while decl != None:
            program.decls.append(decl)
            decl = self.get_decl()        
        return program
    
    def get_decl(self):
        if self.get_next_token() == None:
            return None
        type_ = self.get_type()
        ident = self.consume_token('T_Identifier')
        token = self.get_next_token()
        if token.istype(';') and type_.typeval != 'T_Void':
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
            
    def get_type(self):
        token = self.get_next_token()
        if token.name not in ['T_Int', 'T_Double', 'T_Bool', 'T_String', 'T_Void']:
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
        vardecl = VariableDecl()
        vardecl.type_ = self.get_type()
        vardecl.line = vardecl.type_.line
        vardecl.ident = self.consume_token('T_Identifier')
        formals = [vardecl]
        if self.get_next_token().istype(','):
            self.consume_token()
            formals.extend(self.get_formals())
        return formals
            
    
    def get_stmtblock(self):
        self.consume_token('{')
        print('ok')
        self.consume_token('}')
        return None
    
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
            
    def __init__(self, text):
        self.text = text
        self.textlines = text.split('\n')
        tokenlist = Scanner().scan(self.text)
        self.tokenlist = [Token(*item) for item in tokenlist]
        self.pos = 0
        
    def parse(self):
        try:
            program = self.get_program()
        except ParseError:
            self.print_error(self.get_next_token())
            return
        program.print_tree()
        # convert to objects for better readability
        
        
        # for token in tokenlist:
        #     print(str(token))
        
            
            
    
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