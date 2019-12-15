#! /usr/bin/env python3
import fileinput
import sys
import re

# used to store a parsed TL expressions which are
# constant numbers, constant strings, variable names, and binary expressions
class Expr :
    def __init__(self,op1,operator,op2=None):
        self.op1 = op1
        self.operator = operator
        self.op2 = op2

    def __str__(self):
        if self.op2 == None:
            return self.operator + " " + self.op1.__str__()
        else:
            return self.op1.__str__() + " " + self.operator + " " +  self.op2.__str__()

    # evaluate this expression given the environment of the symTable
    def eval(self, symTable, lineNum):
        if self.operator == "var":
            try:
                return symTable[self.op1]
            except:
                print ("Undefined variable " + self.op1 + " at line " + str(lineNum) + ".")
                sys.exit()
        elif self.operator == "constant":
            return float(self.op1)
        elif self.operator == "+":
            return self.op1.eval(symTable, lineNum) + self.op2.eval(symTable, lineNum)
        elif self.operator == "-":
            return self.op1.eval(symTable, lineNum) - self.op2.eval(symTable, lineNum)
        elif self.operator == "*":
            return self.op1.eval(symTable, lineNum) * self.op2.eval(symTable, lineNum)
        elif self.operator == "/":
            return self.op1.eval(symTable, lineNum) / self.op2.eval(symTable, lineNum)
        elif self.operator == "<":
            return 1.0 if self.op1.eval(symTable, lineNum) < self.op2.eval(symTable, lineNum) else 0.0
        elif self.operator == ">":
            return 1.0 if self.op1.eval(symTable, lineNum) > self.op2.eval(symTable, lineNum) else 0.0
        elif self.operator == ">=":
            return 1.0 if self.op1.eval(symTable, lineNum) >= self.op2.eval(symTable, lineNum) else 0.0
        elif self.operator == "<=":
            return 1.0 if self.op1.eval(symTable, lineNum) <= self.op2.eval(symTable, lineNum) else 0.0
        elif self.operator == "==":
            return 1.0 if self.op1.eval(symTable, lineNum) == self.op2.eval(symTable, lineNum) else 0.0
        elif self.operator == "!=":
            return 1.0 if self.op1.eval(symTable, lineNum) != self.op2.eval(symTable, lineNum) else 0.0
        else:
            print ("Syntax error on line " + str(lineNum) + "." )
            sys.exit()

# used to store a parsed TL statement
class Stmt :
    def __init__(self,keyword,exprs):
        self.keyword = keyword
        self.exprs = exprs         # exprs should be a list of Expr

    def __str__(self):
        others = ""
        for exp in self.exprs:
            others = others + " " + str(exp)
        return self.keyword + others

    # perform/execute this statement given the environment of the symTable
    def perform(self, symTable, lineNum):
        r = None
        if self.keyword == "if":
            if self.exprs[0].eval(symTable, lineNum):
                try:
                    lineNum = symTable[(self.exprs[1]+":")]
                    r = lineNum
                except: 
                    print ("Illegal goto " + self.exprs[1] + " at line " + str(lineNum) + ".")
                    sys.exit()
        elif self.keyword == "let":
            symTable[self.exprs[0]] = self.exprs[1].eval(symTable, lineNum)
        elif self.keyword == "print":
            result = ""
            for each in self.exprs:
                if each.operator == "str":
                    tmp = re.findall('"([^"]*)"', each.op1)
                    result = result + tmp[0] + " "
                else:
                    result = result + str(each.eval(symTable, lineNum)) + " "
                
            print (result)
        elif self.keyword == "input":
            try:
                val = input()
                symTable[self.exprs[0]] = float(val)
            except:
                print ("Illegal or missing input.")
                sys.exit()
        # print ("Doing: " + str(self))
        return r

def parseExpr(expr, pc):
    r = 0
    try:
        if len(expr) == 3:
            r = Expr(parseExpr([expr[0]], pc), expr[1], parseExpr([expr[2]], pc))
        elif expr[0][0].isalpha():
            r = Expr(expr[0], "var")
        elif '\"' in expr[0]:
            r = Expr(expr[0], "str")
        else:
            r = Expr(expr[0], "constant")
    except:
        print ("Syntax error on line " + str(pc) + ".") 
        sys.exit()
    return r

def reConstruct(parsed):
    rs = []
    rebuild = ' '.join(parsed)
    parsed = rebuild.split(',')
    for each in parsed:
        if '\"' in each:
            r = parseExpr([each], pc)
        else:
            r = parseExpr(each.split(), pc)
        rs.append(r)
    return rs

def parseStmt(parsed, pc):
    r = 0
    if parsed[0] == "if":
        r = Stmt(parsed[0], [parseExpr(parsed[1:-2], pc), parsed[-1]])
    elif (parsed[0] == "let" and parsed[2] == "="):
        r = Stmt(parsed[0], [parsed[1], parseExpr(parsed[3:], pc)])
    elif (parsed[0] == "print"):
        r = Stmt(parsed[0], reConstruct(parsed[1:]))
    elif parsed[0] == "input":
        r = Stmt(parsed[0], parsed[1:])
    else:
        print ("Syntax error on line " + str(pc) + ".") 
        sys.exit()
    return r

def parseLine(line, symTable, pc):
    parsed = line.split()         # split by space
    if len(parsed) != 0:
        if (parsed[0][-1]) == ":":    # isLabel
            symTable[parsed[0]] = float(pc)
            return parseStmt(parsed[1:], pc)
        else: 
            return parseStmt(parsed, pc)

if __name__ == '__main__':
    f = open(sys.argv[1])

    stmtAll = []
    symTable = dict()
    pc = 1
    for line in f:
        line_ = line.rstrip('\n')
        stmt = parseLine(line_, symTable, pc)
        stmtAll.append(stmt)
        pc = pc + 1
    # for s in stmtAll:
    #     print (s.__str__())
    lineNum = 1
    while (lineNum <= len(stmtAll)):
        stmt = stmtAll[int(lineNum-1)]
        if stmt != None:
            r = stmt.perform(symTable, lineNum)
        if isinstance(r, float):
            lineNum = r
        else:
            lineNum = lineNum + 1