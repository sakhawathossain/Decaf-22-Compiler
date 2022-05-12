#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Apr 22 18:15:55 2022

@author: shsaimon
"""

import sys

from analyzer import SyntaxAnalyzer
from analyzer import SymbolTable

from parser import VariableDecl
from parser import FunctionDecl
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
from parser import Null


class Register:
    def __init__(self):
        self.var = None
        self.isblank = True
        
class MIPSArch:
    
    def __init__(self, emitfunc):
        self.emit = emitfunc
        self.registers = {}
        self.op_binary = {'+': 'add',
                          '-': 'sub',
                          '*': 'mul',
                          '/': 'div',
                          '&&': 'and',
                          '||': 'or',
                          '==': 'seq',
                          '>=': 'sge',
                          '>': 'sge',
                          '<=': 'sle',
                          '<': 'slt',
                          '!=': 'sne'}
        self.op_unary = {'-': 'neg',
                         '!': 'not'}
        for i in range(8):
            self.registers['$t'+str(i)] = Register()

    def get_free_register(self):
        """
        Return a register that is free for use.
        """
        for reg in self.registers:
            if self.registers[reg].isblank:
                self.registers[reg].isblank = False
                return reg
        # TODO: throw error if no register is free
        print('Error! no free registers!')
        return None
    
    def get_used_registers(self):
        """
        Return the list of registers currently in use.
        """
        used = []
        for reg in self.registers:
            if not self.registers[reg].isblank:
                used.append(reg)
        return used
    
    def set_used_registers(self, used):
        """
        Mark each register in list as used.
        """
        for reg in used:
            if reg in self.registers:
                self.registers[reg].isblank = False
                
    def free_registers(self, *reg):
        """
        Mark each register in list as unused.

        """
        for r in reg:
            if r in self.registers:
                self.registers[r].isblank = True
            
    def get_bin_op_instr(self, op):
        """
        Return appropriate assembly instruction for binary operator.
        """

        return self.op_binary[op]
    
    def get_unr_op_instr(self, op):
        """
        Return appropriate assembly instruction for unary operator.
        """
        return self.op_unary[op]
    
    def stack_setup(self):
        self.emit('subu $sp, $sp, 8', 'decrement sp to make space to save ra, fp')
        self.emit('sw $fp, 8($sp)', 'save fp')
        self.emit('sw $ra, 4($sp)', 'save ra')
        self.emit('addiu $fp, $sp, 8', 'set up new fp')
        
    def stack_teardown(self):
        self.emit('move $sp, $fp', 'pop callee frame off stack')
        self.emit('lw $ra, -4($fp)', 'restore saved ra')
        self.emit('lw $fp, 0($fp)', 'restore saved fp')
        self.emit('jr $ra', 'return from function')
        
    def stack_grow(self, size, comment):
        self.emit('subu $sp, $sp, {0}'.format(size),
                 comment)
    
    def stack_shrink(self, size, comment):
        self.emit('add $sp, $sp, {0}'.format(size),
                  'increase sp to remove space registers')
        
    def emit_jump_if_false(self, reg, label):
        self.emit('blez {0}, {1}'.format(reg, label))
        
    def emit_jump(self, label):
        self.emit('b {0}'.format(label))
            
class Label:
    """
    Generate and keep track of labels.
    """
    
    generic_labelcount = -1
    string_labelcount = 0
        
    def next(funclabel = None):
        """
        If funclabel is none, returns a label L<i> where i is the next available number.
        Otherwise, returns label '_main' for the main function, and '_<funclabel>' for others.
        """
        if funclabel != None:
            return '{0}{1}'.format('_' if funclabel != 'main' else '',
                                   funclabel)
        Label.generic_labelcount += 1
        return '_L{0}'.format(Label.generic_labelcount)
    
    def next_str():
        Label.string_labelcount += 1
        return '_string{0}'.format(Label.string_labelcount)
    

class Generator:
    
    def __init__(self, text):
        self.text = text
        self.textlines = text.split('\n')
        self.analyzer = SyntaxAnalyzer(text, verbose = False)
        self.ast = self.analyzer.analyze()
        self.machine = MIPSArch(self.emit)
        self.instructions = []
        
    def emit(self, ins = None, comment = None):
        ins_str = '' if ins == None else '  {0}'.format(ins)
        comment_str = ''
        if comment != None:
            prefix = '' if ins == None else '\t'
            comment_str = '{0}# {1}'.format(prefix, comment.split('\n')[0])
        self.instructions.append('\t{0}{1}'.format(ins_str, comment_str))
        
    def emit_label(self, label):
        self.instructions.append('  {0}:'.format(label))
    
    def generate(self):
        if self.ast == None:
            return
        self.symbol_table = self.ast.symbol_table
        # preamble
        self.emit(comment = 'standard Decaf preamble')
        self.emit('.text')
        self.emit('.align 2')
        self.emit('.globl main')
        # generate program code
        self.visit_program()
        self.write()
        
    def visit_program(self):
        nglobals = 0
        for decl in self.ast.decls:
            if isinstance(decl, VariableDecl):
                entry = self.symbol_table.lookup(decl.ident)
                # assume global memory starts at 0($gp) and goes up, e.g. 4($gp), 8($gp)
                entry.memloc = '{0}($gp)'.format(nglobals*4)
                nglobals += 1
        for decl in self.ast.decls:
            if isinstance(decl, FunctionDecl):
                self.visit_function_decl(decl)
                
    
    def visit_function_decl(self, function_decl):
        # function label
        self.symbol_table = function_decl.symbol_table
        funclabel = Label.next(function_decl.ident)
        self.emit_label(funclabel)
        self.emit(comment = 'BeginFunc')
        
        self.stacksize = self.assign_local_memory(function_decl)
        self.machine.stack_setup()
        self.machine.stack_grow(self.stacksize,
                                'decrement sp to make space for locals/temps')
        
        self.visit_stmt_block(function_decl.stmtblock)
        # handle implicit return
        self.emit(comment = 'EndFunc')
        self.emit(comment = '(below handles reaching end of fn body with no explicit return)')
        self.machine.stack_teardown()
       
    def assign_local_memory(self, function_decl):
        stacksize = 0
        local_var_offset = 8 # first local variable is at -12($fp)
        stack_top_pad = 4 # padding between local vars and pushed params
        def visit_symbol_table(stmtblock):
            nonlocal stacksize
            # assign memory location to the current symbol table's variable 
            for var in stmtblock.vardecls:
                entry = stmtblock.symbol_table.lookup(var.ident)
                if isinstance(entry, SymbolTable.VarEntry):
                    stacksize += 4
                    entry.memloc = '{}($fp)'.format(-stacksize-local_var_offset)
            # recursively visit nested stmtBlocks
            for stmt in stmtblock.stmts:
                if isinstance(stmt, StmtBlock):
                    visit_symbol_table(stmt)
        
        # set memory location of formals to respective stack locations
        # All parameters are in stack
        for i, varDecl in enumerate(function_decl.formals):
            entry = function_decl.symbol_table.lookup(varDecl.ident)
            entry.memloc = '{}($fp)'.format((i+1)*4)
        visit_symbol_table(function_decl.stmtblock)
        return stacksize + stack_top_pad
        
    
    def visit_stmt_block(self, stmtblock):
        self.symbol_table = stmtblock.symbol_table
        for stmt in stmtblock.stmts:
            self.visit_stmt(stmt)
    
    def visit_stmt(self, stmt):
        stmt_func_map = {IfStmt: self.visit_ifstmt,
                         WhileStmt: self.visit_whilestmt,
                         ForStmt: self.visit_forstmt,
                         BreakStmt: self.visit_breakstmt,
                         ReturnStmt: self.visit_returnstmt,
                         PrintStmt: self.visit_printstmt,
                         StmtBlock: self.visit_stmt_block,
                         Expr: self.visit_expr}
        if type(stmt) in stmt_func_map:
            func = stmt_func_map[type(stmt)]
            func(stmt)
        elif isinstance(stmt, Expr):
            self.visit_expr(stmt)
            self.machine.free_registers(stmt.reg)
    
    def visit_ifstmt(self, ifstmt):
        self.visit_expr(ifstmt.test)
        r1 = ifstmt.test.reg
        elselabel = Label.next()
        # self.emit('blez {0}, {1}'.format(r1, elselabel))
        self.machine.emit_jump_if_false(r1, elselabel)
        self.machine.free_registers(r1)
        self.visit_stmt(ifstmt.stmt)
        if ifstmt.elsestmt != None:
            endlabel = Label.next()
            #self.emit('b {0}'.format(endlabel))
            self.machine.emit_jump(endlabel)
        self.emit_label(elselabel)
        if ifstmt.elsestmt != None:
            self.visit_stmt(ifstmt.elsestmt)
            self.emit_label(endlabel)
        #self.machine.free_registers(r1)
            
    
    def visit_whilestmt(self, whilestmt):
        looplabel = Label.next()
        self.emit_label(looplabel)
        self.visit_expr(whilestmt.test)
        r1 = whilestmt.test.reg
        endlabel = Label.next()
        self.loop_endlabel = endlabel # for break stmts
        # self.emit('blez {0}, {1}'.format(r1, endlabel))
        self.machine.emit_jump_if_false(r1, endlabel)
        self.machine.free_registers(r1)
        self.visit_stmt(whilestmt.stmt)
        # self.emit('b {0}'.format(looplabel))
        self.machine.emit_jump(looplabel)
        self.emit_label(endlabel)
        self.machine.free_registers(r1)
    
    def visit_forstmt(self, forstmt):
        self.visit_expr(forstmt.init)
        looplabel = Label.next()
        self.emit_label(looplabel)
        self.visit_expr(forstmt.test)
        r1 = forstmt.test.reg
        endlabel = Label.next()
        self.loop_endlabel = endlabel # for break stmts
        #self.emit('blez {0}, {1}'.format(r1, endlabel))
        self.machine.emit_jump_if_false(r1, endlabel)
        self.visit_stmt(forstmt.stmt)
        self.visit_expr(forstmt.step)
        self.emit('b {0}'.format(looplabel))
        self.emit_label(endlabel)
        self.machine.free_registers(r1)
    
    def visit_breakstmt(self, breakstmt):
        self.emit('b {0}'.format(self.loop_endlabel))
    
    def visit_returnstmt(self, returnstmt):
        if returnstmt.expr != None:
            self.visit_expr(returnstmt.expr)
            r0 = returnstmt.expr.reg
            self.emit('move $v0, {0}'.format(r0), 'assign return value into $v0')
            self.machine.free_registers(r0)
        # self.emit('move $sp, $fp', 'pop callee frame off stack')
        # self.emit('lw $ra, -4($fp)', 'restore saved ra')
        # self.emit('lw $fp, 0($fp)', 'restore saved fp')
        # self.emit('jr $ra', 'return from function')
        self.machine.stack_teardown()
    
    def visit_printstmt(self, printstmt):
        for expr in printstmt.exprs:
            self.visit_expr(expr)
            r0 = expr.reg
            self.emit(comment = 'PushParam')
            #self.emit('subu $sp, $sp, 4', 'decrement sp to make space for param')
            self.machine.stack_grow(4, 'decrement sp to make space for param')
            self.emit('sw {0}, 4($sp)'.format(r0),	'copy param value to stack')
            #print('expr memloc:', self.symbol_table.lookup(expr.ident).memloc)
            if expr.type_ == 'int':
                self.emit(comment = 'LCall _PrintInt')
                self.emit('jal _PrintInt', 'jump to function')
            elif expr.type_ == 'string':
                self.emit(comment = 'LCall _PrintString')
                self.emit('jal _PrintString', 'jump to function')
            elif expr.type_ == 'bool':
                self.emit(comment = 'LCall _PrintBool')
                self.emit('jal _PrintBool', 'jump to function')
            self.machine.free_registers(r0)
            self.emit(comment = 'PopParams')
            #self.emit('add $sp, $sp, 4', 'pop params off stack')
            self.machine.stack_shrink(4, 'pop params off stack')
        
    def visit_expr(self, expr):
        if expr == None:
            return
        if isinstance(expr, AssignExpr):
            self.visit_expr(expr.R)
            ident = expr.L.ident
            r1 = expr.R.reg
            memloc = self.symbol_table.lookup(ident).memloc
            self.emit('sw {0}, {1}'.format(r1, memloc),
                      'spill {0} from {1} to {2}'.format(ident, r1, memloc))
            # TODO: postpone freeing this register because assign and evaluate is allowed
            expr.reg = r1
            self.machine.free_registers(r1)
        elif isinstance(expr, BinaryExpr):
            self.visit_expr(expr.L)
            r1 = expr.L.reg
            self.visit_expr(expr.R)
            r2 = expr.R.reg
            r0 = self.machine.get_free_register()
            op = expr.op.lexeme
            if expr.L.type_ == 'string' and op in ['!=', '==']:
                # special case: use the _StringEqual library function privided
                self.emit(comment = 'PushParam')
                self.machine.stack_grow(8, 'decrement sp to make space for param')
                self.emit('sw {0}, 4($sp)'.format(r1))
                self.emit('sw {0}, 8($sp)'.format(r2))
                # Caution: r1 and r2 are possibly overwritten immediately after call
                # choosing not to save them here because no need
                self.emit(comment = 'LCall _StringEqual')
                self.emit('jal _StringEqual', 'jump to function')
                
                self.machine.stack_shrink(8, 'pop params off stack')
                r0 = self.machine.get_free_register()
                self.emit('move {0}, $v0'.format(r0),
                          'copy function return value from $v0')
            else:
                self.emit('{0} {1}, {2}, {3}'\
                          .format(self.machine.get_bin_op_instr(op),
                                  r0, r1, r2))
            self.machine.free_registers(r1, r2)
            expr.reg = r0
        elif isinstance(expr, UnaryExpr):
            self.visit_expr(expr.R)
            r1 = expr.R.reg
            op = expr.op.lexeme
            r0 = self.machine.get_free_register()
            if op == '!':
                self.emit('nor {0}, {1}, {1}'.format(r0, r1))
            else:
                self.emit('neg {0}, {1}'.format(r0, r1))
            # self.emit('{0} {1}, {2}'.format(self.machine.get_unr_op_instr(op),
            #                                 r0, r1))
            self.machine.free_registers(r1)
            expr.reg = r0
        elif isinstance(expr, CallExpr):
            self.visit_callExpr(expr)
        elif isinstance(expr, IdentExpr):
            ident = expr.ident
            r0 = self.machine.get_free_register()
            memloc = self.symbol_table.lookup(ident).memloc
            self.emit('lw {0}, {1}'.format(r0, memloc),
                      'fill {0} to {1} from {2}'.format(ident, r0, memloc))
            expr.reg = r0
            pass
        elif isinstance(expr, ConstantExpr):
            if expr.const_type == 'int':
                r0 = self.machine.get_free_register()
                self.emit('li {}, {}'.format(r0, expr.value))
                expr.reg = r0
            elif expr.const_type == 'string':
                self.emit('.data', 'create string constant marked with label')
                strlabel = Label.next_str()
                self.emit('{0}: .asciiz "{1}"'.format(strlabel, expr.value))
                self.emit('.text')
                r0 = self.machine.get_free_register()
                self.emit('la {0}, {1}'.format(r0, strlabel))
                expr.reg = r0
            elif expr.const_type == 'bool':
                r0 = self.machine.get_free_register()
                if expr.value == True:
                    self.emit('li {0}, 1'.format(r0),
                              'load constant value 1 (true) into {0}'.format(r0))
                else:
                    self.emit('li {0}, 0'.format(r0),
                              'load constant value 0 (false) into {0}'.format(r0))
                expr.reg = r0
                
        elif isinstance(expr, Null):
            # TODO: implement
            pass
        elif isinstance(expr, ReadIntegerExpr):
            self.emit('jal _ReadInteger')
            r0 = self.machine.get_free_register()
            self.emit('move {0}, $v0'.format(r0))
            expr.reg = r0
        elif isinstance(expr, ReadLineExpr):
            self.emit('jal _ReadLine')
            r0 = self.machine.get_free_register()
            self.emit('move {0}, $v0'.format(r0))
            expr.reg = r0
        
    def visit_callExpr(self, expr):
        self.emit(comment = 'start preparation for function call')
        # SAVE CALLER-SAVED REGISTERS
        used = self.machine.get_used_registers()
        if len(used) > 0:
            size = 4*(len(used) + 1)
            self.emit(comment = "push caller-saved registers")
            # self.emit('subu $sp, $sp, {0}'.format(4*(len(used) + 1)),
            #           'decrement sp to make space for registers')
            self.machine.stack_grow(size,
                                    'decrement sp to make space for registers')
            #print('Used registers:', used)
            for i, reg in enumerate(used):
                self.emit('sw {0}, {1}($sp)'.format(reg, (i+1)*4))
                self.machine.free_registers(reg)
                #print('storing {} in {}($sp)'.format(reg, (i+1)*4))
            
        # EVALUATE AND PUSH PARAMS
        # actuals must be evaluated from left to right, but pushed right to left
        # implementation:
        # first grow stack for pushing params (differs from the sample code)
        # then evaluate each actual and push to stack
        param_memory_size = len(expr.actuals)*4
        # self.emit('subu $sp, $sp, {0}'.format(param_memory_size),
        #           '# decrement sp to make space for param')
        self.machine.stack_grow(param_memory_size,
                                'decrement sp to make space for registers')
        for i, actual in enumerate(expr.actuals):
            self.visit_expr(actual)
            r0 = actual.reg
            # TODO: investigate
            # The sample code for param pushing looks like this:
            # subu $sp, $sp, 4	# decrement sp to make space for param
            # $t0, 4($sp) # copy param value to stack
            # But shouldn't this be 0($sp) instead?
            # One explanation could be that in the samples, initial stack allocation always 
            # allocates extra memory, and param locations start at ebp+4 instead of ebp+8
            
            
            self.emit('sw {0}, {1}($sp)'.format(r0, 4*(i+1)),
                      'copy param value to stack')
            self.machine.free_registers(r0)
            
        # STEP 2: make function call
        funclabel = Label.next(expr.ident)
        self.emit(comment = 'LCall {0}'.format(funclabel))
        self.emit('jal {0}'.format(funclabel))
        
        # STEP 4: pop params
        self.emit(comment = 'PopParams')
        self.emit('add $sp, $sp, {0}'.format(param_memory_size),
                  'pop params off stack')
        
        # POP CALLER-SAVED REGISTERS
        if len(used) > 0:
            self.emit(comment = "pop caller-saved registers")
            for i, reg in enumerate(used):
                self.emit('lw {0}, {1}($sp)'.format(reg, (i+1)*4))
                #print('loading {} from {}($sp)'.format(reg, (i+1)*4))
            self.emit('add $sp, $sp, {0}'.format(4*(len(used)+1)),
                      'increase sp to remove space registers')
            self.machine.set_used_registers(used)
            
        # STEP 3: copy return value
        r0 = self.machine.get_free_register()
        self.emit('move {0}, $v0'.format(r0),
                  'copy function return value from $v0')
        expr.reg = r0

    
    def write(self):
        try:
            with open('samples/testout.s', 'w') as file:
                file.write('\n'.join(self.instructions))
                file.write('\n')
        except IOError as ie:
            print(ie)
        
        
    

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print('Please enter a filepath as the program argument')
    else:
        path = sys.argv[1]
        try:
            with open(path) as file:
                text = file.read()
                Generator(text).generate()
        except IOError:
            print('Could not read file: {0}'.format(path))