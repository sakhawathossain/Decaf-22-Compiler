#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb  8 21:08:15 2022

@author: sakhawathossain
"""

import sys
import re

REGEX_TOKENS = [
        ('T_DoubleConstant', '[0-9]+\.[0-9]*((E[+]?[0-9]+)|(E-[0-9]+))?'),
        ('T_Identifier', '[a-zA-Z]+[a-zA-Z0-9_]*'),
        ('T_IntConstant', '[0-9]+'),
        ('T_StringConstant', '"[^\n"]*"'),
        ('T_Or', '\|\|'),
        ('T_And', '&&'),
        ('T_LessEqual', '<='),
        ('T_GreaterEqual', '>='),
        ('T_Equal', '=='),
        ('+', '\+'),
        ('-', '\-'),
        ('*', '\*'),
        ('/', '/'),
        ('<', '<'),
        ('>', '>'),
        ('=', '='),
        (';', ';'),
        (',', ','),
        ('!', '!'),
        ('{', '{'),
        ('}', '}'),
        ('(', '\('),
        (')', '\)'),
        ('.', '\.')]

KEYWORDS = {'void' : 'T_Void',
            'int': 'T_Int',
            'double': 'T_Double',
            'string': 'T_String',
            'while': 'T_While',
            'if': 'T_If',
            'else': 'T_Else',
            'return': 'T_Return',
            'break': 'T_Break',
            'true': 'T_BoolConstant',
            'false': 'T_BoolConstant'}

REGEX_DELIM = '\s+'
REGEX_MLINE_COMMENT = '/\*(.|\s)*?((\*/)|$)'
REGEX_SLINE_COMMENT = '//[^\n]*\n'
REGEX_IGNORE = '|'.join([REGEX_DELIM, REGEX_MLINE_COMMENT, REGEX_SLINE_COMMENT])
REGEX_BADSTRING = '"[^"\n]*\n'

class Scanner:
    """Regex-based scanner for Decaf-22"""
    
    def __init__(self, verbose = False):
        self.verbose = verbose
    
    def get_val(self, token_type, lexeme):
        """Return the string value as well as correctly typed value for given lexeme and type"""
        
        if token_type == 'T_BoolConstant':
            return lexeme, lexeme == 'true'
        elif token_type == 'T_StringConstant':
            return lexeme, lexeme[1:-1]
        elif token_type in ['T_IntConstant', 'T_DoubleConstant']:
            val = float(lexeme)
            if val - int(val) == 0:
                val = int(val)
            return val, val
        return None, None

    def print_token(self, lexeme, line, start, end, token_type, attr_str = None, msg = None):
        """Print token type, matching lexeme and location information if verbose is set to True"""
        
        if not self.verbose:
            return
        if lexeme in '+-*/<>;=,!{}().':
            token_type = "'{0}'".format(token_type) # wrap operators in single quotes
        print('{0:<12} line {1} cols {2}-{3} is {4} {5}{6}'.format(
            lexeme, line, start, end, token_type,
            '(value = {0})'.format(attr_str) if attr_str != None else '',
            '({0})'.format(msg) if msg != None else ''))
    
    def print_error(self, msg, line):
        print('\n*** Error line {0}.\n*** {1}\n'.format(line, msg))

    def scan(self, txt):
        """Scan string and return a list of tokens"""
        
        tokens = []
        line, col = 1, 1
        while txt != '':
            if re.match(REGEX_IGNORE, txt):
                lexeme = re.match(REGEX_IGNORE, txt).group(0)
                n_lines = lexeme.count('\n')
                line += n_lines
                col = col + len(lexeme) if n_lines == 0 else len(lexeme.split('\n')[-1])+1
                txt = txt[len(lexeme):]
            elif re.match(REGEX_BADSTRING, txt): 
                lexeme = re.match(REGEX_BADSTRING, txt).group(0)
                self.print_error('Unterminated string constant: {0}'.format(lexeme[:-1]), line)
                line += 1
                col = 1
                txt = txt[len(lexeme):]  
            else:
                for token_type, REGEX_TOKEN in REGEX_TOKENS:
                    if re.match(REGEX_TOKEN, txt):
                        lexeme = re.match(REGEX_TOKEN, txt).group(0)
                        start, end = col, col + len(lexeme)-1
                        col = end + 1
                        val, strval, msg = None, None, None
                        txt = txt[len(lexeme):]
                        if token_type == 'T_Identifier':
                            if lexeme in KEYWORDS: # If Identifier matches keyword table entry, update token type
                                token_type = KEYWORDS[lexeme]
                                if token_type == 'T_BoolConstant': # Set BoolConstant value to True or False
                                    strval, val = self.get_val(token_type, lexeme)
                                self.print_token(lexeme, line, start, end, token_type, strval, msg)
                            else:
                                if len(lexeme) > 31: # Truncate identifiers longer than 31 characters
                                    self.print_error('Identifier too long: "{0}"'.format(lexeme), line)
                                    msg = 'truncated to {0}'.format(lexeme[:31])
                                    self.print_token(lexeme, line, start, end, token_type, val, msg = msg)
                                    lexeme = lexeme[:31]
                                else:
                                    self.print_token(lexeme, line, start, end, token_type, val, msg = msg)
                        else:
                            strval, val = self.get_val(token_type, lexeme)
                            self.print_token(lexeme, line, start, end, token_type, strval, msg = msg)
                        tokens.append([token_type, lexeme, val, line, start, end]) # append token to token stream
                        break # no need to check the other regular expressions after a match
                else: # No pattern matches, read a single symbol and print error message
                    lexeme = txt[0]
                    self.print_error("Unrecognized char: '{0}'".format(lexeme), line)
                    txt = txt[1:]
        return tokens 


if __name__ == "__main__":
    if len(sys.argv) < 2:
        print('Please enter a filepath as the program argument')
    else:
        path = sys.argv[1]
        try:
            with open(path) as file:
                Scanner(verbose = True).scan(file.read())
        except IOError:
            print('Could not read file: {0}'.format(path))