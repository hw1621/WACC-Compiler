package Frontend
import Frontend.AbstractSyntax._
import parsley.Parsley._
import parsley.combinator.{many, sepBy1, sepBy}
import Frontend.Lexer._
import Frontend.Lexer.implicits.implicitSymbol
import Frontend.Errors._
import parsley.Parsley
import parsley.expr._
import parsley.io.ParseFromIO
import parsley.errors.combinator._
import scala.PartialFunction
import java.io.File

object Parser {
    
    implicit val eb: ErrorB = new ErrorB

    def parse(file: File) = parseProgram.parseFromFile(file)
    
    private lazy val parseProgram = fully("begin" *> Program(many(parseFunc), sepBy1(parseStat, ";")) <* "end").label("program")

    private lazy val parseFunc = attempt(Func(parseType, parseIdent, 
                            "(" *> sepBy(parseParam, ",") <* ")", 
                            "is" *> sepBy1(parseStat, ";").filterOut(filterNoReturnFunc) <* "end")).label("function")

    private val parseIdent = Ident(identifier).label("identifier")

    private lazy val parseExpr: Parsley[Expr] = precedence(
        SOps(InfixR)(Or <# "||".label("operator")) +:
        SOps(InfixR)(And <# "&&".label("operator")) +:
        SOps(InfixN)(EQ <# "==".label("operator"), NEQ <# "!=".label("operator")) +:
        SOps(InfixN)(LT <# "<".label("operator"), LE <# "<=".label("operator"), GT <# ">".label("operator"), GE <# ">=".label("operator")) +:
        SOps(InfixL)(Add <# "+".label("operator"), Sub <# "-".label("operator")) +:
        SOps(InfixL)(Mul <# "*".label("operator"), Div <# "/".label("operator"), Mod <# "%".label("operator")) +:
        Ops(Prefix)(Ord <# "ord".label("operator"), Len <# "len".label("operator"), Neg <# NEGATION, Not <# "!".label("operator"), Chr <# "chr".label("operator")) +:
        Atoms(IntLit(INT), BoolLit(boolLit), CharLit(charLit), StrLit(stringLit), Null <# "null", Paren("(" *> parseExpr <* ")"), parseIdentWithArray)
    ).label("expression")

    private lazy val parseParam = Param(parseType, parseIdent).label("function parameter").explain("valid function parameter format: type + indentifier")

    private lazy val parseStat: Parsley[Statements] = ((Skip <# "skip" 
            <|> DeclareIdent(parseType, parseIdent, "=" *> parseRValue)
            <|> AssignVal(parseLValue, "=" *> parseRValue)
            <|> Read("read" *> parseLValue)
            <|> Free("free" *> parseExpr)
            <|> Return("return" *> parseExpr)
            <|> Exit("exit" *> parseExpr)
            <|> Print("print" *> parseExpr)
            <|> Println("println" *> parseExpr)
            <|> If("if" *> parseExpr, "then" *> sepBy1(parseStat, ";"), "else" *> sepBy1(parseStat, ";") <* "fi")                        
            <|> While("while" *> parseExpr, "do" *> sepBy1(parseStat, ";") <* "done"))
            <|> NewScope("begin" *> sepBy1(parseStat, ";") <* "end")).label("statement")

    private lazy val parseArrayElem = ArrayElem("[" *> parseExpr <* "]").label("array_element")

    private lazy val parseIdentWithArray = chain.postfix(parseIdent, parseArrayElem).label("IdentWithArray")

    private lazy val parsePairElem = Fst("fst" *> parseLValue).label("fst") <|> Snd("snd" *> parseLValue).label("snd")

    private lazy val parseLValue: Parsley[lValue] = (parseIdentWithArray <|> parsePairElem).label("lhs_value")

    private lazy val parseRValue: Parsley[rValue] = (parseExpr <|> parseArrayLit 
                                                    <|> NewPair("newpair" *> "(" *> parseExpr <* ",", parseExpr <* ")")
                                                    <|> parsePairElem 
                                                    <|> Call("call" *> parseIdent, "(" *> sepBy(parseExpr, ",") <* ")"))
                                                    .label("rhs_value").explain("Examples: new pair, pair element or call of functions")

    private lazy val parseArrayLit = ArrayLit("[" *> sepBy(parseExpr, ",") <* "]" ).label("array_liter")
    
    private lazy val parseBaseType = ((IntType <# "int") 
                                     <|> (BoolType <# "bool")
                                     <|> (CharType <# "char")
                                     <|> (StringType <# "string")).label("base_type")


    // Array can have nested array
    private lazy val parseType = chain.postfix(parseBaseType <|> parsePairType, ArrayType <# ("[" <* "]")).label("type")

    private lazy val parsePairType = PairType("pair" *> "(" *> parsePairElemType, "," *> parsePairElemType <* ")").label("parse pair-type")

    private lazy val parseArrayType = chain.postfix1(parseBaseType <|> parsePairType, ArrayType <# ("[" <* "]")).label("array_type")

    private lazy val parsePairElemType: Parsley[PairElemType] = (attempt(parseArrayType) <|> parseBaseType <|> (JustPair <# "pair")).label("pair_element_type")

    private val filterNoReturnFunc = new PartialFunction[List[Statements], String] {
        def apply(v1: List[Statements]): String = ""
        def isDefinedAt(stats: List[Statements]): Boolean = {
            stats.last match {
                case Return(_) => false
                case Exit(_)   => false
                case If(expr, thenStat, elseState) => isDefinedAt(thenStat) || isDefinedAt(elseState)
                case While(expr, doStat) => isDefinedAt(doStat)
                case NewScope(stat) => isDefinedAt(stat)
                case _              => true
            }
        }
    }
}
