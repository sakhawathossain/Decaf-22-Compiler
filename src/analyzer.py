#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr  7 21:22:45 2022

@author: shsaimon
"""

from parser import *

class SymbolTable():
    
    class VarEntry():
        def __init__(self, type_):
            self.type_ = type_
            
    class FuncEntry():
        def __init__(self, return_type, formal_types = []):
            self.return_type = return_type
            self.formal_types = formal_types
    
    def __init__(self, parent = None):
        if parent == None:
            self.level = 0
        else:
            self.level = parent.level + 1
        self.parent = parent
        self.entries = {}
        
    def install(self, decl):
        # TODO: check if declaration already exists
        ident = decl.ident.lexeme
        if isinstance(decl, VariableDecl):
            self.entries[ident] = self.VarEntry(decl.type_)
        else:
            pass
            self.entries[ident] = self.FuncEntry(decl.type_,
                                                 [vardecl.type_.typeval for vardecl in decl.formals])
            
    def lookup(self, ident):
        symbol_table = self
        while symble_table != None:
            if ident in self.entries:
                return self.entries[ident]
            # move up one level
            symbol_table = symbol_table.parent
        return None
    
    def printself(self):
        print('>> SymbolTable [level {}]'.format(self.level))
        for k, v in self.entries.items():
            if isinstance(v, self.VarEntry):
                print('(Var)  {0}\t type: {1}'.format(k, v.type_.typeval))
            else:
                print('(Func) {0}\t type: {1}, sig: {2}'.format(
                    k,
                    v.return_type.typeval,
                    ', '.join(v.formal_types)))
    
class SymbolTableConstructor():
    
    def __init__(self, program):
        self.program = program
        self.program.symbol_table = SymbolTable(None)
        
    def visit_program(self):
        # first pass
        for decl in self.program.decls:
            self.program.symbol_table.install(decl)
        #self.program.symbol_table.printself()
        # second pass
        for decl in self.program.decls:
            if isinstance(decl, FunctionDecl):
                self.visit_function_decl(decl)
                
    def visit_function_decl(self, decl):
        symbol_table = SymbolTable(parent = self.program.symbol_table)
        decl.symbol_table = symbol_table
        for vardecl in decl.formals:
            symbol_table.install(vardecl)
        #symbol_table.printself()
        self.visit_stmtblock(decl.stmtblock, symbol_table)
        
    def visit_stmtblock(self, stmt, parent_st):
        if not isinstance(stmt, StmtBlock):
            return
        symbol_table = SymbolTable(parent = parent_st)
        for decl in stmt.vardecls:
            symbol_table.install(decl)
        for stmt in stmt.stmts:
            if isinstance(stmt, IfStmt):
                self.visit_stmtblock(stmt.stmt, symbol_table)
                if stmt.elsestmt != None:
                    self.visit_stmtblock(stmt.elsestmt, symbol_table)
            elif isinstance(stmt, WhileStmt) or isinstance(stmt, ForStmt):
                self.visit_stmtblock(stmt.stmt, symbol_table)  
        #symbol_table.printself()
        
        
class SyntaxAnalyzer():
    
    def __init__(self, text, verbose = False):
        self.text = text
        self.textlines = text.split('\n')
        self.verbose = verbose
        self.ast = Parser(text, verbose = False).parse()
        
    def analyze(self):
        if self.ast != None:
            SymbolTableConstructor(self.ast).visit_program()
            

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print('Please enter a filepath as the program argument')
    else:
        path = sys.argv[1]
        try:
            with open(path) as file:
                text = file.read()
                SyntaxAnalyzer(text, verbose = True).analyze()
        except IOError:
            print('Could not read file: {0}'.format(path))