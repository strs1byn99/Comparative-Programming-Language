import scala.collection.mutable.Map
import scala.io.Source
import scala.io.StdIn.{readDouble}
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex

abstract class Expr
case class Var(name: String) extends Expr
case class Str(name: String) extends Expr
case class Constant(num: Double) extends Expr
case class BinOp(operator: String, left: Expr, right: Expr) extends Expr
case class Error_() extends Expr

abstract class Stmt     // perform match case
case class Let(variable: String, expr: Expr) extends Stmt {
    override def toString: String = s"($variable, $expr)"
}
case class If(expr: Expr, label: String) extends Stmt {
    override def toString: String = s"($expr, $label)"
}
case class Input(variable: String) extends Stmt {
    override def toString: String = s"($variable)"
}
case class Print(exprList: List[Expr]) extends Stmt {
    override def toString: String = s"Print"
}
case class Nop() extends Stmt {
    override def toString: String = s"None"
}
case class Error() extends Stmt  

object TLI {
    def eval(expr: Expr, symTab: Map[String, Double], lineNum: Double): Double = expr match {
        case BinOp("!=",e1,e2) => if (eval(e1,symTab, lineNum) != eval(e2,symTab, lineNum)) 1.0 else 0.0
        case BinOp("==",e1,e2) => if (eval(e1,symTab, lineNum) == eval(e2,symTab, lineNum)) 1.0 else 0.0
        case BinOp("<=",e1,e2) => if (eval(e1,symTab, lineNum) <= eval(e2,symTab, lineNum)) 1.0 else 0.0
        case BinOp(">=",e1,e2) => if (eval(e1,symTab, lineNum) >= eval(e2,symTab, lineNum)) 1.0 else 0.0
        case BinOp("<",e1,e2) => if (eval(e1,symTab, lineNum) < eval(e2,symTab, lineNum)) 1.0 else 0.0
        case BinOp(">",e1,e2) => if (eval(e1,symTab, lineNum) > eval(e2,symTab, lineNum)) 1.0 else 0.0
        case BinOp("/",e1,e2) => eval(e1,symTab, lineNum) / eval(e2,symTab, lineNum) 
        case BinOp("*",e1,e2) => eval(e1,symTab, lineNum) * eval(e2,symTab, lineNum) 
        case BinOp("-",e1,e2) => eval(e1,symTab, lineNum) - eval(e2,symTab, lineNum) 
        case BinOp("+",e1,e2) => eval(e1,symTab, lineNum) + eval(e2,symTab, lineNum) 
        case Var(name) => {
            try {
                symTab(name)
            } catch { 
                case _: Throwable => { printf("Undefined variable %s at line %d.\n", name, lineNum.toInt); System.exit(1); 0.0 }
            }
        }
        case Constant(num) => num
    case _ => {printf("Syntax error on line %d.\n", lineNum.toInt); System.exit(1); 0.0} // should really throw an error
    }

    def parseExpr(expr: Array[String], pc: Double): Expr = {
        try {
            if (expr.length == 3) {
                new BinOp(expr(1), parseExpr(Array(expr(0)), pc), parseExpr(Array(expr(2)), pc))
            } else if (expr(0).contains('\"')) {
                new Str(expr(0))
            } else if (expr(0)(0).isLetter) {
                new Var(expr(0))    
            } else {
                new Constant(expr(0).toDouble)
            }
        } catch {
            case _: Throwable => { printf("Syntax error on line %d.\n", pc.toInt); System.exit(1); new Error_() }
        }
        
    }

    def reConstruct(parsed: Array[String], pc: Double): List[Expr] = {
        var rs = List[Expr]()
        var rebuild = parsed.mkString(" ")
        var reparsed = rebuild.split(",")
        for (each <- reparsed) {
            var x = each.trim()
            if (each.contains("\"")) {
                var e: Array[String] = Array(x)
                rs = rs :+ parseExpr(e, pc)
            } else {
                rs = rs :+ parseExpr(x.split(" "), pc)
            }
        }
        rs
    }

    def parseStmt(parsed: Array[String], pc: Double): Stmt = {
        if (parsed.head == "let" && parsed(2) == "=") {
            new Let(parsed(1), parseExpr(parsed.drop(3), pc))
        } else if (parsed.head == "if" && parsed.takeRight(2).head == "goto" ) {
            var expr = parsed.drop(1).dropRight(2)
            new If(parseExpr(expr, pc), parsed.last)
        } else if (parsed.head == "print") {
            new Print(reConstruct(parsed.drop(1), pc))
        } else if (parsed.head == "input") {
            new Input(parsed(1))
        } else {
            printf("Syntax error on line %d.\n", pc.toInt)
            System.exit(1)
            new Error()
        }
    }

    def parseLine(line: String, symTab: Map[String, Double], pc: Double): Stmt = {
        val parsed = line.split("\\s+")
        if (parsed.length > 1) {
            if (parsed(0).takeRight(1) == ":") {
                symTab += (parsed(0) -> pc)
                parseStmt(parsed.tail, pc)
            } else {
                parseStmt(parsed, pc)
            }
        } else {
            new Nop()
        }
    }

    def toBool(e: Double) = if (e==1) true else false

    def perform(stmt: Stmt, symTab: Map[String, Double], lineNum: Double): Double = stmt match {
        case Let(id, e) => { symTab += (id -> eval(e, symTab, lineNum)); lineNum+1 }
        case If(e, id) => { 
            try {
                if (toBool(eval(e, symTab, lineNum))) symTab(id+":") else lineNum+1
            } catch {
                case _: Throwable => { printf("Illegal goto %s at line %d.\n", id, lineNum.toInt); System.exit(1); 0.0 } 
            }
        }
        case Input(id) => { 
                try {
                    val n = readDouble(); symTab+=(id -> n); lineNum+1
                } catch {
                    case _: Throwable => { println("Illegal or missing input."); System.exit(1); 0.0 }
                } 
            }
        case Print(es) => { 
            var result: String = ""
            for (e <- es) {
                if (e.isInstanceOf[Str]) 
                    result += (e.asInstanceOf[Str].name).split("\"")(1) + " " // get rid of quotation mark
                else
                    result += eval(e, symTab, lineNum).toString + " "
            }
            println(result)
            lineNum+1
        }
        case _ => { lineNum+1 } // Nop() empty line
    }

    def main(args: Array[String]): Unit = {
        val file = Source.fromFile(args(0))
        var pc = 1.0
        var stmtAll = new ListBuffer[Stmt]()
        var symTab = scala.collection.mutable.Map[String, Double]()
        for (line <- file.getLines) {
            var x = line.trim()
            var stmt = parseLine(x, symTab, pc)
            pc = pc + 1.0
            stmtAll += stmt
        }
        // for (stmt <- stmtAll) {
        //     println(stmt)
        // }
        var lineNum = 1.0
        var r = 0.0
        while (lineNum <= stmtAll.length) {
            var stmt = stmtAll((lineNum-1).toInt)
            r = perform(stmt, symTab, lineNum)
            lineNum = r
        }
    }
    // end of main

}