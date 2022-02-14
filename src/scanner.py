#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Tue Feb  8 21:08:15 2022

@author: shsaimon
"""

import sys
import re

R_TOKENS = [
        ('T_DoubleConstant', '[0-9]+\.[0-9]*((E[+]?[0-9]+)|(E-[0-9]+))?'),
        ('T_Identifier', '[a-zA-Z]+[a-zA-Z0-9_]*'),
        ('T_IntConstant', '[0-9]+'),
        ('T_StringConstant', '"[^\n"]*"'),
        ('T_Or', '\|\|'),
        ('T_And', '&&'),
        ('T_LessEqual', '<='),
        ('T_GreaterEqual', '>='),
        ('T_Equal', '=='),
        ("'+'", '\+'),
        ("'-'", '\-'),
        ("'*'", '\*'),
        ("'/'", '/'),
        ("'<'", '<'),
        ("'>'", '>'),
        ("'='", '='),
        ("';'", ';'),
        ("','", ','),
        ("'!'", '!'),
        ("'{'", '{'),
        ("'}'", '}'),
        ("'('", '\('),
        ("')'", '\)'),
        ("'.'", '\.')
        ]
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
            'false': 'T_BoolConstant'
            }

R_DELIM = '\s+'
R_MCOMMENT = '/\*(.|\s)*?((\*/)|$)'
R_SCOMMENT = '//[^\n]*\n'
R_IGNORE = '|'.join([R_DELIM, R_MCOMMENT, R_SCOMMENT])

R_BADSTRING = '"[^"\n]*\n'

def count_lines(token):
    return token.count('\n')

def get_val(token_type, token):
    if token_type in ['T_BoolConstant', 'T_StringConstant']:
        return token
    elif token_type == 'T_IntConstant':
        return int(token)
    elif token_type == 'T_DoubleConstant':
        val = float(token)
        if val - int(val) == 0:
            val = int(val)
        return val
    return None

def announce_token(token, line, start, end, token_type, attr_str = None, msg = None):
    print('{0:<12} line {1} cols {2}-{3} is {4} {5}{6}'.format(
        token, line, start, end, token_type,
        '(value = {0})'.format(attr_str) if attr_str != None else '',
        '({0})'.format(msg) if msg != None else ''))
    
def announce_error(msg, line):
    print('\n*** Error line {0}.\n*** {1}\n'.format(line, msg))

def scan(txt):
    line, col = 1, 1
    while txt != '':
        if re.match(R_IGNORE, txt):
            token = re.match(R_IGNORE, txt).group(0)
            n_lines = count_lines(token)
            line += n_lines
            col = col + len(token) if n_lines == 0 else len(token.split('\n')[-1])+1
            txt = txt[len(token):]
        elif re.match(R_BADSTRING, txt): 
            token = re.match(R_BADSTRING, txt).group(0)
            announce_error('Unterminated string constant: {0}'.format(token[:-1]), line)
            line += 1
            col = 1
            txt = txt[len(token):]
            
        else:
            for token_type, R_TOKEN in R_TOKENS:
                if re.match(R_TOKEN, txt):
                    token = re.match(R_TOKEN, txt).group(0)
                    start, end = col, col + len(token)-1
                    col = end + 1
                    val, msg = None, None
                    txt = txt[len(token):]
                    if token_type == 'T_Identifier':
                        if token in KEYWORDS:
                            token_type = KEYWORDS[token]
                            if token_type == 'T_BoolConstant':
                                val = token
                            announce_token(token, line, start, end, token_type, val, msg)
                        else:
                            if len(token) > 31:
                                announce_error('Identifier too long: "{0}"'.format(token), line)
                                msg = 'truncated to {0}'.format(token[:31])
                                announce_token(token, line, start, end, token_type, val, msg = msg)
                                token = token[:31]
                            else:
                                announce_token(token, line, start, end, token_type, val, msg = msg)
                    else:
                        val = get_val(token_type, token)
                        announce_token(token, line, start, end, token_type, val, msg = msg)
                    break
            else:
                token = txt[0]
                announce_error("Unrecognized char: '{0}'".format(token), line)
                txt = txt[1:]
                    

if __name__ == "__main__":
    if len(sys.argv) < 2:
        print('Please enter a filepath as the program argument')
    else:
        path = sys.argv[1]
        try:
            with open(path) as file:
                scan(file.read())
        except IOError:
            print('Could not open file: {0}'.format(path))