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
        #print('lookup', symbol_table)
        #print('lookup', symbol_table != None)
        while symbol_table != None:
            if ident in self.entries:
                return self.entries[ident]
            # move up one level
            symbol_table = symbol_table.parent
            #print('lookup level', symbol_table)
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
        stmt.symbol_table = symbol_table
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
        
class SemanticChecker():
    
    class ErrorType():
        pass
    
    def __init__(self, program, errorfunc):
        self.program = program
        self.errorfunc = errorfunc
        self.visiting_loop = False
        
    def print_error(self, token, message):
        self.errorfunc(token, message)
        
    def visit(self):
        for decl in self.program.decls:
            if isinstance(decl, FunctionDecl):
                self.visit_stmt(decl.stmtblock)
            
    def visit_stmt(self, stmt):
        if stmt == None:
            return
        if isinstance(stmt, Expr):
            self.visit_expr(stmt)
        elif isinstance(stmt, IfStmt):
            self.visit_expr(stmt.test)
            self.visit_stmt(stmt.stmt)
            self.visit_stmt(stmt.elsestmt)
        elif isinstance(stmt, WhileStmt):
            self.visiting_loop = True
            self.visit_expr(stmt.test)
            self.visit_stmt(stmt.stmt)
            self.visiting_loop = False
        elif isinstance(stmt, ForStmt):
            self.visiting_loop = True
            self.visit_expr(stmt.init)
            self.visit_expr(stmt.test)
            self.visit_expr(stmt.step)
            self.visit_stmt(stmt.stmt)
            self.visiting_loop = False
        elif isinstance(stmt, BreakStmt):
            if not self.visiting_loop:
                self.print_error(stmt.token, 'break is only allowed inside a loop')
        elif isinstance(stmt, ReturnStmt):
            self.visit_expr(stmt.expr)
        elif isinstance(stmt, StmtBlock):
            self.symbol_table = stmt.symbol_table
            for s in stmt.stmts:
                self.visit_stmt(s)
            
    def visit_expr(self, expr, enforce_type = None):
        if expr == None:
            return
        if isinstance(expr, AssignExpr) or isinstance(expr, BinaryExpr):
            # TODO:
            self.visit_expr(expr.L)
            self.visit_expr(expr.R)
            is_compat, resulting_type = self.is_compatible(expr.R, expr.op, expr.L)
            if not is_compat:
                self.errorfunc(expr.op, 'Incompatible operands: {0} {1} {2}'.format(
                    expr.L.type_, expr.op.lexeme, expr.R.type_))
            expr.type_ = resulting_type
        elif isinstance(expr, UnaryExpr):
            # TODO:
            self.visit_expr(expr.R)
            is_compat, resulting_type = self.is_compatible(expr.R, expr.op)
            if not is_compat:
                self.errorfunc(expr.op, 'Incompatible operands: {0} {1}'.format(
                    expr.op.lexeme, expr.R.type_))
            expr.type_ = resulting_type
        elif isinstance(expr, IdentExpr):
            ident = expr.token
            entry = self.symbol_table.lookup(ident.lexeme)
            if entry == None:
                self.print_error(expr.token, "No declaration for Variable '{0}' found".format(ident.lexeme))
                # TODO: decide whether to add this to global or immediate symbol table
                self.symbol_table.install(VariableDecl(Variable(Type(Token('Error',
                                                                      'Error',
                                                                      'Error',
                                                                      expr.token.line,
                                                                      expr.token.start,
                                                                      expr.token.end)), ident)))
                expr.type_ = 'Error'
            else:
                expr.type_ = entry.type_.typeval
        elif isinstance(expr, ConstantExpr):
            # remove 'T_' and 'Constant' from the token type
            expr.type_ = expr.token.name.lower()[2:-8]
        
    def is_compatible(self, R, optoken, L = None):
        # print('here')
        op = optoken.lexeme
        # print('op', op)
        if L == None:
            if op == '!' and R.type_ in ['bool', 'Error']:
                return True, R.type_
            elif op == '-' and R.type_ in ['int', 'Error']:
                return True, R.type_
            return False, 'Error'
        compatible = True
        resulting_type = 'Error'
        if L.type_ == 'Error' or R.type_ == 'Error':
            pass
        elif op in ['+', '-', '*', '/', '%']:
            compatible = L.type_ in ['int', 'double'] and R.type_ == L.type_
            resulting_type = L.type_ if compatible else 'Error'
        elif op in ['<', '>', '<=', '>=']:
            compatible = L.type_ in ['int', 'double', 'Error'] and R.type_ == L.type_
            resulting_type = 'bool' if compatible else 'Error'
        elif op in ['!=', '==']:
            compatible = L.type_ == R.type_
            resulting_type = 'bool'
        elif op in ['&&', '||']:
            compatible = L.type_ == 'bool' and R.type_ == L.type_
            resulting_type = 'bool'
        elif op == '=':
            compatible = L.type_ == R.type_
            resulting_type = L.type_
        return compatible, resulting_type
        
    
        
        
class SyntaxAnalyzer():
    
    def __init__(self, text, verbose = False):
        self.text = text
        self.textlines = text.split('\n')
        self.verbose = verbose
        self.ast = Parser(text, verbose = False).parse()
        
    def print_error(self, token, message):
        print('\n*** Error line {:}.'.format(token.line))
        print(self.textlines[token.line-1])
        print(' '* (token.start - 1) + '^' * (token.end - token.start + 1))
        print('*** {0}\n'.format(message))
        
    def analyze(self):
        if self.ast != None:
            SymbolTableConstructor(self.ast).visit_program()
            SemanticChecker(self.ast, self.print_error).visit()
            
            

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