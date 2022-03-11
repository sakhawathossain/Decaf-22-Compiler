#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Mar  9 16:22:08 2022

@author: shsaimon
"""

import sys
from scanner import Scanner

class Parser:
    
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
        def __init__(self):
            pass
        def print_tree(self, indent = 0):
            print('{:>3}{:}VarDecl:'.format(self.line, ' '*3*indent))
            self.type_.print_tree(indent+1)
            print('{:>3}{:}Identifier: {:}'.format(self.ident.line, ' '*3*(indent+1), self.ident.lexeme))
        
    class Variable(ASTNode):
        def __init__(self):
            pass
        
    class FunctionDecl(Decl):
        def __init__(self):
            pass
        
    class Type():
        pass
        def print_tree(self, indent = 0):
            print('   {:}Type: {:}'.format(' '*3*indent, self.typeval))
        
    
    
    def get_program(self):
        program = self.Program()
        decl = self.get_decl()
        while decl != None:
            program.decls.append(decl)
            decl = self.get_decl()        
        return program
    
    def get_decl(self):
        if self.get_next_token() == None:
            return None
        type_ = self.get_type()
        ident = self.accept_token('T_Identifier')
        token = self.get_next_token()
        if token.istype(';') and type_.typeval != 'T_Void':
            decl = self.VariableDecl()
            decl.type_ = type_
            decl.ident = ident
            decl.line = type_.line
            self.accept_token(';')
            return decl
        elif token.istype('('):
            decl = self.FunctionDecl()
            decl.type_ = type_
            decl.ident = ident
            decl.line = type_.line
            self.accept_token('(')
            decl.formals = self.get_formals()
            self.accept_token(')')
            decl.stmtblock = self.get_stmtblock()
            return decl
        else:
            raise self.ParseError
            
    def get_type(self):
        token = self.get_next_token()
        if token.name not in ['T_Int', 'T_Double', 'T_Bool', 'T_String', 'T_Void']:
            raise self.ParseError
        type_ = self.Type()
        type_.typeval = token.lexeme
        type_.line = token.line
        self.accept_token()
        return type_

    def get_formals(self):
        return None
    
    def get_stmtblock(self):
        return None
    
    def get_next_token(self):
        if self.pos >= len(self.tokenlist):
            return None
        return self.tokenlist[self.pos]
        
    def accept_token(self, name = None):
        token = self.get_next_token()
        if name != None:
            if not token.istype(name):
                raise self.ParseError
        self.pos += 1
        return token
        
    
    def print_error(self, token):
        line, start, end = token.line-1, token.start-1, token.end-1
        print('print_error', line, start, end)
        print(self.textlines[line])
        print(' '* start + '^' * (end - start + 1))
            
    def __init__(self, text):
        self.text = text
        self.textlines = text.split('\n')
        tokenlist = Scanner().scan(self.text)
        self.tokenlist = [self.Token(*item) for item in tokenlist]
        self.pos = 0
        
    def parse(self):
        try:
            program = self.get_program()
        except self.ParseError:
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