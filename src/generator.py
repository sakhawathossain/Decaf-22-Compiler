#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Apr 22 18:15:55 2022

@author: shsaimon
"""

import sys

from analyzer import SyntaxAnalyzer

class Generator:
    
    def __init__(self):
        self.text = text
        self.textlines = text.split('\n')
        self.analyzer = SyntaxAnalyzer(text, verbose = False)
        self.ast = self.SyntaxAnalyzer.analyze()
    
    def generate(self):
        if self.ast == None:
            return
        
    

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