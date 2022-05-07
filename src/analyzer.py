#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Thu Apr  7 21:22:45 2022

@author: shsaimon
"""

# TODO: return type checking
import sys

from parser import Span
from parser import Token
from parser import ParseError

from parser import ASTNode
from parser import Program
from parser import VariableDecl
from parser import Variable
from parser import FunctionDecl
from parser import Type
from parser import Stmt
from parser import StmtBlock
from parser import IfStmt
from parser import WhileStmt
from parser import ForStmt
from parser import BreakStmt
from parser import ReturnStmt
from parser import PrintStmt
from parser import Expr
from parser import AssignExpr
from parser import CallExpr
from parser import BinaryExpr
from parser import UnaryExpr
from parser import IdentExpr
from parser import ConstantExpr
from parser import ReadIntegerExpr
from parser import ReadLineExpr
from parser import Parser

class SymbolTable():
    
    class VarEntry():
        def __init__(self, ident, type_):
            self.ident = ident
            self.type_ = type_
            
    class FuncEntry():
        def __init__(self, ident, type_, formal_types = []):
            self.ident = ident
            self.type_ = type_
            self.formal_types = formal_types
   
    def __init__(self, parent = None):
        self.func = None
        self.level = 0
        if parent != None:
            self.level = parent.level + 1
            self.func = parent.func            
        self.parent = parent
        self.entries = {}
        
    def install(self, ident, type_, formals = None):
        # TODO: check if declaration already exists
        if formals == None:
            self.entries[ident] = self.VarEntry(ident, type_)
        else:
            self.entries[ident] = self.FuncEntry(ident, type_, formals)
            
    def lookup(self, ident):
        symbol_table = self
        while symbol_table != None:
            if ident in symbol_table.entries:
                return symbol_table.entries[ident]
            # move up one level
            symbol_table = symbol_table.parent
        return None
    
    def print_self(self):
        print('SymbolTable >>')
        print('\n'.join([entry + ': ' + self.entries[entry].type_ for entry in self.entries]))
        print()
    
class SymbolTableConstructor():
    
    def __init__(self, program):
        self.program = program
        self.program.symbol_table = SymbolTable()
        
    def visit_program(self):
        # first pass
        for decl in self.program.decls:
            if isinstance(decl, VariableDecl):
                self.program.symbol_table.install(decl.ident, decl.type_.typeval)
            else:
                self.program.symbol_table.install(decl.ident,
                                                  decl.type_.typeval,
                                                  [vardecl.type_.typeval for vardecl in decl.formals])
        # second pass
        for decl in self.program.decls:
            if isinstance(decl, FunctionDecl):
                self.visit_function_decl(decl)
                
    def visit_function_decl(self, decl):
        symbol_table = SymbolTable(parent = self.program.symbol_table)
        # save the current function name for analyzer visit to Return stmts
        symbol_table.func = decl.ident
        # attach symbol table to decl ASTNode
        decl.symbol_table = symbol_table
        for vardecl in decl.formals:
            symbol_table.install(vardecl.ident, vardecl.type_.typeval)
        self.visit_stmtblock(decl.stmtblock, symbol_table)
        
    def visit_stmtblock(self, stmtblock, parent_st):
        if not isinstance(stmtblock, StmtBlock):
            return
        symbol_table = SymbolTable(parent = parent_st)
        stmtblock.symbol_table = symbol_table
        for decl in stmtblock.vardecls:
            symbol_table.install(decl.ident, decl.type_.typeval)
        for stmt in stmtblock.stmts:
            if isinstance(stmt, IfStmt):
                self.visit_stmtblock(stmt.stmt, symbol_table)
                if stmt.elsestmt != None:
                    self.visit_stmtblock(stmt.elsestmt, symbol_table)
            elif isinstance(stmt, WhileStmt) or isinstance(stmt, ForStmt):
                self.visit_stmtblock(stmt.stmt, symbol_table)
            elif isinstance(stmt, StmtBlock):
                self.visit_stmtblock(stmt, symbol_table)
        
class SemanticChecker():
    
    def __init__(self, program, errorfunc):
        self.program = program
        self.errorfunc = errorfunc
        self.loop_level = 0
        self.is_syntax_correct = True
        
    def print_error(self, span, message):
        self.is_syntax_correct = False
        self.errorfunc(span, message)
        
    def visit(self):
        # top level symbol table
        self.symbol_table = self.program.symbol_table
        for decl in self.program.decls:
            if isinstance(decl, FunctionDecl):
                self.symbol_table = decl.symbol_table
                self.visit_stmt(decl.stmtblock)
        return self.is_syntax_correct
            
    def visit_stmt(self, stmt):
        if stmt == None:
            return
        elif isinstance(stmt, IfStmt):
            self.visit_expr(stmt.test)
            if not stmt.test.type_ in ['bool', 'Error']:
                self.print_error(stmt.test.span, 'Test expression must have boolean type')
            self.visit_stmt(stmt.stmt)
            self.visit_stmt(stmt.elsestmt)
        elif isinstance(stmt, WhileStmt):
            self.loop_level += 1
            self.visit_expr(stmt.test)
            if not stmt.test.type_ in ['bool', 'Error']:
                self.print_error(stmt.test.span, 'Test expression must have boolean type')
            self.visit_stmt(stmt.stmt)
            self.loop_level -= 1
        elif isinstance(stmt, ForStmt):
            self.loop_level += 1
            self.visit_expr(stmt.init)
            self.visit_expr(stmt.test)
            if not stmt.test.type_ in ['bool', 'Error']:
                self.print_error(stmt.test.span, 'Test expression must have boolean type')
            self.visit_expr(stmt.step)
            self.visit_stmt(stmt.stmt)
            self.loop_level -= 1
        elif isinstance(stmt, BreakStmt):
            if self.loop_level <= 0:
                self.print_error(stmt.span, 'break is only allowed inside a loop')
        elif isinstance(stmt, ReturnStmt):
            self.visit_expr(stmt.expr)
            entry = self.program.symbol_table.lookup(self.symbol_table.func)
            assert entry != None
            return_expr_type = 'void' if stmt.expr == None else stmt.expr.type_
            if entry.type_ != return_expr_type and \
                not (entry.type_ == 'Error' or return_expr_type == 'Error'):
                    span = stmt.span if stmt.expr == None else stmt.expr.span
                    self.print_error(span,
                                     'Incompatible return: {0} given, {1} expected'.format(
                                         return_expr_type, entry.type_))
        elif isinstance(stmt, PrintStmt):
            for i, expr in enumerate(stmt.exprs):
                self.visit_expr(expr)
                if expr.type_ not in ['int', 'bool', 'string', 'Error']:
                    self.print_error(expr.span,
                                     'Incompatible argument {0}: {1} given, int/bool/string expected'.format(
                                         i+1, expr.type_))
        elif isinstance(stmt, StmtBlock):
            self.symbol_table = stmt.symbol_table
            for s in stmt.stmts:
                self.visit_stmt(s)
            self.symbol_table = self.symbol_table.parent
        elif isinstance(stmt, Expr):
            self.visit_expr(stmt)
            
    def visit_expr(self, expr):
        if expr == None:
            return
        if isinstance(expr, AssignExpr) or isinstance(expr, BinaryExpr):
            self.visit_expr(expr.L)
            self.visit_expr(expr.R)
            is_compat, resulting_type = self.is_compatible(expr.R, expr.op, expr.L)
            if not is_compat:
                self.print_error(expr.op.span, 'Incompatible operands: {0} {1} {2}'.format(
                                     expr.L.type_, expr.op.lexeme, expr.R.type_))
            # TODO: double check with professor if assign-and-evaluate should be allowed
            expr.type_ = resulting_type
        elif isinstance(expr, UnaryExpr):
            # TODO:
            self.visit_expr(expr.R)
            is_compat, resulting_type = self.is_compatible(expr.R, expr.op)
            if not is_compat:
                self.errorfunc(expr.op.span,
                               'Incompatible operand: {0} {1}'.format(
                                   expr.op.lexeme, expr.R.type_))
            expr.type_ = resulting_type
        elif isinstance(expr, IdentExpr):
            ident = expr.ident
            entry = self.symbol_table.lookup(ident)
            if entry == None or isinstance(entry, SymbolTable.FuncEntry):
                self.print_error(expr.span,
                                 "No declaration found for variable '{0}'".format(ident))
                # TODO: decide whether to add this to global or immediate symbol table
                # current behaviour: add to global symbol table so all errors regarding this variable are suppressed
                self.program.symbol_table.install(ident, 'Error')
                expr.type_ = 'Error'
            else:
                expr.type_ = entry.type_
        elif isinstance(expr, ConstantExpr):
            expr.type_ = expr.token.name.lower()[2:-8]
        elif isinstance(expr, ReadIntegerExpr):
            expr.type_ = 'int'
        elif isinstance(expr, ReadLineExpr):
            expr.type_ = 'string'
        elif isinstance(expr, CallExpr):
            ident = expr.ident
            entry = self.symbol_table.lookup(ident)
            if entry == None:
                self.print_error(expr.token.span,
                                 "No declaration found for function '{0}'".format(ident))
                self.program.symbol_table.install(ident, 'Error', [])
                expr.type_ = 'Error'
            elif len(entry.formal_types) != len(expr.actuals):
                for actual in expr.actuals:
                    self.visit_expr(actual)
                if entry.type_ != 'Error':
                    self.print_error(expr.token.span,
                                     "Function '{0}' expects {1} arguments but {2} given".format(
                                         ident, len(entry.formal_types), len(expr.actuals)))
                expr.type_ = 'Error'

            else:
                for i, pair in enumerate(zip(entry.formal_types, expr.actuals)):
                    formal, actual = pair
                    self.visit_expr(actual)
                    if formal != actual.type_:
                        self.print_error(actual.span,
                                         "Incompatible argument {0}: {1} given, {2} expected".format(
                                             i+1, actual.type_, formal))
                        expr.type_ = 'Error'
                        # TODO: check with professor if type checking for the other actuals should be skipped
                expr.type_ = entry.type_

        
    def is_compatible(self, R, optoken, L = None):
        op = optoken.lexeme
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
    
    
class Linker():
    """Check if Main exists"""
    
    def __init__(self, program):
        self.program = program
        
    def link(self):
        if self.program.symbol_table.lookup('main') == None:
            print('\n*** Error.')
            print("*** Linker: function 'main' not defined\n")
            return False
        return True
        
class SyntaxAnalyzer():
    
    def __init__(self, text, verbose = False):
        self.text = text
        self.textlines = text.split('\n')
        self.verbose = verbose
        self.parser = Parser(text, verbose = False)
        self.ast = self.parser.parse()
        
    def analyze(self):
        if self.ast != None:
            SymbolTableConstructor(self.ast).visit_program()
            is_correct =  SemanticChecker(self.ast, self.parser.print_error).visit()
            if is_correct:
                has_main = Linker(self.ast).link()
                if has_main:  
                    return self.ast
            return None
            
            

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