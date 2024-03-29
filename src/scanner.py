#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb  8 21:08:15 2022

@author: sakhawathossain
"""

import sys
import re

REGEX_TOKENS = [
        ('T_DoubleConstant', '[0-9]+\.[0-9]*(((E|e)[+]?[0-9]+)|((E|e)-[0-9]+))?'),
        ('T_Identifier', '[a-zA-Z]+[a-zA-Z0-9_]*'),
        ('T_IntConstant', '(((0X)|(0x))[0-9a-fA-F]+)|([0-9]+)'),
        ('T_StringConstant', '"[^\n"]*"'),
        ('T_Or', '\|\|'),
        ('T_And', '&&'),
        ('T_LessEqual', '<='),
        ('T_GreaterEqual', '>='),
        ('T_Equal', '=='),
        ('T_NotEqual', '!='),
        ('+', '\+'),
        ('-', '\-'),
        ('*', '\*'),
        ('/', '/'),
        ('%', '%'),
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
            'null': 'T_Null',
            'for': 'T_For',
            'while': 'T_While',
            'if': 'T_If',
            'else': 'T_Else',
            'return': 'T_Return',
            'break': 'T_Break',
            'bool': 'T_Bool',            
            'true': 'T_BoolConstant',
            'false': 'T_BoolConstant',
            'Print': 'T_Print',
            'ReadInteger': 'T_ReadInteger',
            'ReadLine': 'T_ReadLine'}

REGEX_DELIM = '\s+'
REGEX_MLINE_COMMENT = '/\*(.|\s)*?((\*/)|$)' # non-standard regex, could not solve it any other way
REGEX_SLINE_COMMENT = '//[^\n]*(\n|$)'
REGEX_IGNORE = '|'.join([REGEX_DELIM, REGEX_MLINE_COMMENT, REGEX_SLINE_COMMENT])
REGEX_BADSTRING = '"[^"\n]*(\n|$)'

class Scanner:
    """Regex-based scanner for Decaf-22"""
    
    def __init__(self, verbose = False):
        self.verbose = verbose
    
    def get_val(self, token_type, lexeme):
        """Return the printable value as well as correctly Python-typed value for given lexeme and type"""
        """Printable and correctly Python-typed values are not always the same, e.g. 'true' vs 'True' """
        
        if token_type == 'T_BoolConstant':
            return lexeme, lexeme == 'true'
        elif token_type == 'T_StringConstant':
            return lexeme, lexeme[1:-1] # remove double-quotes from the value
        elif token_type == 'T_IntConstant':
            if lexeme.lower().startswith('0x'): # hexadecimal integer
                val = int(lexeme[2:], 16)
            else:
                val = int(lexeme)
            return val, val
        elif token_type == 'T_DoubleConstant':
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

    def scan(self, text):
        """Scan string and return a list of tokens"""
        
        tokens = [] # will contain token information for downstream processing
        line, col = 1, 1
        while text != '':
            if re.match(REGEX_IGNORE, text):
                lexeme = re.match(REGEX_IGNORE, text).group(0)
                n_lines = lexeme.count('\n')
                line += n_lines
                col = col + len(lexeme) if n_lines == 0 else len(lexeme.split('\n')[-1])+1
                text = text[len(lexeme):]
            elif re.match(REGEX_BADSTRING, text): 
                lexeme = re.match(REGEX_BADSTRING, text).group(0)
                self.print_error('Unterminated string constant: {0}'.format(lexeme.replace('\n', '')), line)
                line += 1
                col = 1
                text = text[len(lexeme):]  
            else:
                for token_type, REGEX_TOKEN in REGEX_TOKENS:
                    if re.match(REGEX_TOKEN, text):
                        lexeme = re.match(REGEX_TOKEN, text).group(0)
                        start, end = col, col + len(lexeme)-1
                        col = end + 1
                        val, strval, msg = None, None, None
                        text = text[len(lexeme):]
                        if token_type == 'T_Identifier':
                            if lexeme in KEYWORDS: # If lexeme matches keyword table entry, update token type
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
                    lexeme = text[0]
                    self.print_error("Unrecognized char: '{0}'".format(lexeme), line)
                    text = text[1:]
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