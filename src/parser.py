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
                    
            
    def __init__(self, text):
        self.text = text
        self.textlines = text.split('\n')
        
    def parse(self):
        tokenlist = Scanner().scan(self.text)
        # convert to objects for better readability
        tokenlist = [self.Token(*item) for item in tokenlist]
        for token in tokenlist:
            print(str(token))
            
            
    
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