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

OP_BINARY = {'||': 'T_Or',
             '&&': 'T_And', # missing from the document, adding anyway
             '<=': 'T_LessEqual',
             '>=': 'T_GreaterEqual',
             '==': 'T_Equal'
             }

OP_UNARY = '+-*/<>=;,!{}()'
R_DELIM = '\s+'
R_MCOMMENT = '/\*(.|\s)*?\*/'
R_SCOMMENT = '//[^\n]*\n'

R_BADSTRING = '"[^\n"]*\n'

def count_lines(token):
    return token.count('\n')

def announce_token(token, line, start, end, token_type, attr_str = None, msg = None):
    print('{0:<12} line {1} cols {2}-{3} is {4}{5}{6}'.format(
        token,
        line,
        start,
        end,
        token_type,
        ' (value = {0})'.format(attr_str) if attr_str != None else '',
        ' ({0})'.format(msg) if msg != None else ''))
    
def announce_error(msg, line):
    print('\n*** Error line {0}.'.format(line))
    print('*** ' + msg + '\n')

def scan(txt):
    line = 1
    while txt != '':
        skipped = False
        for R in [R_MCOMMENT, R_SCOMMENT, R_DELIM]:
            if re.match(R, txt):
                skiptoken = re.match(R, txt).group(0)
                line += count_lines(skiptoken)
                txt = txt[len(skiptoken):]
                if R in [R_MCOMMENT]:
                    #print('skipped #{0}#'.format(skiptoken))
                    #print('Remaining:\n', txt[:10])
                    pass
                skipped = True
        if skipped:
            continue
            
        for token_type, R_TOKEN in R_TOKENS:
            if re.match(R_TOKEN, txt):
                match = re.match(R_TOKEN, txt)
                token, start, end = match.group(0), match.start()+1, match.end()
                line += count_lines(token)
                txt = txt[len(token):]
                val = None
                msg = None
                # Token-type specific actions:
                if token_type == 'T_Identifier':
                    if token in KEYWORDS:
                        token_type = KEYWORDS[token]
                        if token_type == 'T_BoolConstant':
                            val = token
                        announce_token(token, line, start, end, token_type, val, msg = msg)
                    else:
                        if len(token) > 31:
                            announce_error('Identifier too long: "{0}"'.format(token), line)
                            msg = 'truncated to {0}'.format(token[:31])
                            announce_token(token, line, start, end, token_type, val, msg = msg)
                            token = token[:31]
                        else:
                            announce_token(token, line, start, end, token_type, val, msg = msg)
                elif token_type == 'T_IntConstant':
                    val = int(token)
                    announce_token(token, line, start, end, token_type, val, msg = msg)
                elif token_type == 'T_DoubleConstant':
                    val = float(token)
                    if val - int(val) == 0:
                        val = int(val)
                    token_type += ' (value = {0})'.format(val)
                    announce_token(token, line, start, end, token_type, val, msg = msg)
                elif token_type == 'T_StringConstant':
                    val = token
                    announce_token(token, line, start, end, token_type, val, msg = msg)
                else:
                    announce_token(token, line, start, end, token_type, val, msg = msg)
                
                break
        else:       
            break
            
if __name__ == "__main__":
    if len(sys.argv) < 2:
        print('Please enter a filepath as the program argument')
    else:
        path = sys.argv[1]
        try:
            file = open(path)
            txt = file.read()
            scan(txt)
            file.close()
        except IOError:
            print('Could not open file: {0}'.format(path))