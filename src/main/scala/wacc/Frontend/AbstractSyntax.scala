package Frontend

import parsley.Parsley
import parsley.position.pos
import parsley.implicits.zipped.{Zipped2, Zipped3, Zipped4}

object AbstractSyntax {
    trait Position {
        val pos: (Int, Int)
    }

    case class Program(funcs: List[Func], statements: List[Statements]) (val pos: (Int, Int)) extends Position
    case class Func(funcType: Type, id: Ident, args: List[Param], body: List[Statements]) (val pos: (Int, Int)) extends Position
    case class Param(paramType: Type, id: Ident) (val pos: (Int, Int)) extends Position

    sealed abstract class Syntax(val pos: (Int, Int))

    //Statements
    sealed abstract class Statements extends Position
    case class Skip()(val pos: (Int, Int)) extends Statements
    case class DeclareIdent(identType: Type, id: Ident, rhs: rValue)(val pos: (Int, Int)) extends Statements
    case class AssignVal(lhs: lValue, rhs: rValue)(val pos: (Int, Int)) extends Statements
    case class Read(lhs: lValue)(val pos: (Int, Int)) extends Statements
    case class Free(expr: Expr)(val pos: (Int, Int)) extends Statements
    case class Return(expr: Expr)(val pos: (Int, Int)) extends Statements
    case class Exit(expr: Expr)(val pos: (Int, Int)) extends Statements
    case class Print(expr: Expr)(val pos: (Int, Int)) extends Statements
    case class Println(expr: Expr)(val pos: (Int, Int)) extends Statements
    case class If(expr: Expr, thenStat: List[Statements], elseStat: List[Statements])(val pos: (Int, Int)) extends Statements
    case class While(expr: Expr, doStat: List[Statements])(val pos: (Int, Int)) extends Statements
    case class NewScope(stat: List[Statements])(val pos: (Int, Int)) extends Statements

    sealed trait lrValue extends Position
    
    //lValue:ident, array-elem. pair-elem
    sealed trait lValue extends lrValue

    //rValue: expr, array-liter, new pair, pair-elem, call
    sealed trait rValue extends lrValue
    case class NewPair(fst: Expr, rhs: Expr)(val pos: (Int, Int)) extends rValue
    case class Call(id: Ident, exprs: List[Expr])(val pos: (Int, Int)) extends rValue

    //pair-elem
    sealed trait PairElem extends lValue with rValue
    case class Fst(value: lValue)(val pos: (Int, Int)) extends PairElem
    case class Snd(value: lValue)(val pos: (Int, Int)) extends PairElem

    //Type
    sealed trait Type extends Position

    case object NoExistType extends Type with PairElemType {override val pos = (-1, -1)}
    case object AnyType extends Type with PairElemType {override val pos = (-1, -1)}

    //base-type 
    sealed trait BaseType extends Type with PairElemType
    case class IntType()(val pos: (Int, Int)) extends BaseType {
        override def toString() = "Int"
    }
    case class BoolType()(val pos: (Int, Int)) extends BaseType {
        override def toString() = "Boolean"
    }
    case class CharType()(val pos: (Int, Int)) extends BaseType {
        override def toString() = "Char"
    }
    case class StringType()(val pos: (Int, Int)) extends BaseType {
        override def toString() = "String"
    }

    //array-type
    case class ArrayType(arrayType: Type)(val pos: (Int, Int)) extends Type with PairElemType
    //pair-type
    case class PairType(lhs: PairElemType, rhs: PairElemType)(val pos: (Int, Int)) extends Type
    //pair elem type
    sealed trait PairElemType extends Type
    case class JustPair()(val pos: (Int, Int)) extends PairElemType

    //Expression
    sealed trait Expr extends rValue with Position
    // case class UnaryOperation(operator: UnOp, expr: Expr) extends Expr {override def getType: Type = operator.getType}
    // case class BinaryOperation(expr1: Expr, operator: BiOp, expr2: Expr) extends Expr {override def getType: Type = operator.getType}

    //Unary Operator
    sealed trait UnOp extends Expr {val expr: Expr}
    case class Not(expr: Expr)(val pos: (Int, Int)) extends UnOp
    case class Neg(expr: Expr)(val pos: (Int, Int)) extends UnOp
    case class Len(expr: Expr)(val pos: (Int, Int)) extends UnOp
    case class Ord(expr: Expr)(val pos: (Int, Int)) extends UnOp
    case class Chr(expr: Expr)(val pos: (Int, Int)) extends UnOp

    //Binary Operator
    sealed trait BiOp extends Expr {val lhs: Expr; val rhs: Expr}
    case class Mul(lhs: Expr, rhs: Expr)(val pos: (Int, Int)) extends BiOp
    case class Div(lhs: Expr, rhs: Expr)(val pos: (Int, Int)) extends BiOp
    case class Mod(lhs: Expr, rhs: Expr)(val pos: (Int, Int)) extends BiOp
    case class Add(lhs: Expr, rhs: Expr)(val pos: (Int, Int)) extends BiOp
    case class Sub(lhs: Expr, rhs: Expr)(val pos: (Int, Int)) extends BiOp
    case class GT(lhs: Expr, rhs: Expr)(val pos: (Int, Int)) extends BiOp
    case class GE(lhs: Expr, rhs: Expr)(val pos: (Int, Int)) extends BiOp
    case class LT(lhs: Expr, rhs: Expr)(val pos: (Int, Int)) extends BiOp
    case class LE(lhs: Expr, rhs: Expr)(val pos: (Int, Int)) extends BiOp
    case class EQ(lhs: Expr, rhs: Expr)(val pos: (Int, Int)) extends BiOp
    case class NEQ(lhs: Expr, rhs: Expr)(val pos: (Int, Int)) extends BiOp
    case class And(lhs: Expr, rhs: Expr)(val pos: (Int, Int)) extends BiOp
    case class Or(lhs: Expr, rhs: Expr)(val pos: (Int, Int)) extends BiOp


    //int-liter
    case class IntLit(value: Int)(val pos: (Int, Int)) extends Expr
    //bool-liter
    case class BoolLit(value: Boolean)(val pos: (Int, Int)) extends Expr
    //char-liter
    case class CharLit(value: Char)(val pos: (Int, Int)) extends Expr
    //str-liter
    case class StrLit(value: String)(val pos: (Int, Int)) extends Expr
    //array-liter
    case class ArrayLit(x: List[Expr])(val pos: (Int, Int)) extends rValue
    //pair-liter
    sealed trait PairLit extends Expr
    case class Null()(val pos: (Int, Int)) extends PairLit
    
    //parenthese
    case class Paren(expr: Expr)(val pos: (Int, Int)) extends Expr
    //array-elem 
    case class ArrayElem(ident: IdentWithArray, expr: Expr)(val pos: (Int, Int)) extends IdentWithArray
    //Identifier
    case class Ident(name: String)(val pos: (Int, Int)) extends IdentWithArray {
        def toStringName = name
    }

    sealed trait IdentWithArray extends lValue with Expr


    //ParsserBridge Template
    trait ParserBridgePos0[A] {
        def apply()(pos: (Int, Int)): A
        def <#(op: Parsley[_]): Parsley[A] = pos.map(p => this.apply()(p)) <* op
    }

    trait ParserBridgePos1[-A, +B] {
        def apply(x: A)(pos: (Int, Int)): B
        def apply(x: Parsley[A]): Parsley[B] = pos <**> x.map(apply(_) _)
        def <#(op: Parsley[_]): Parsley[A => B] = pos.map[A => B](p => this.apply(_)(p)) <* op
    }

    trait ParserBridgePos2[-A, -B, +C] {
        def apply(x: A, y: B)(pos: (Int, Int)): C
        def apply(x: Parsley[A], y: Parsley[B]): Parsley[C] =
            pos <**> (x, y).zipped(this.apply(_, _) _)
        def <#(op: Parsley[_]): Parsley[(A, B) => C] = pos.map[(A, B) => C](p => this.apply(_, _)(p)) <* op
    }

    trait ParserBridgePos3[-A, -B, -C, +D] {
        def apply(x: A, y: B, z: C)(pos: (Int, Int)): D
        def apply(x: Parsley[A], y: Parsley[B], z: Parsley[C]): Parsley[D] =
            pos <**> (x, y, z).zipped(this.apply(_, _, _) _)
        def <#(op: Parsley[_]): Parsley[(A, B, C) => D] = pos.map[(A, B, C) => D](p => this.apply(_, _, _)(p)) <* op
    }

    trait PartialParserBridge[-A, -B, +C] {
        def apply(x: A,  y: B)(pos: (Int, Int)): C
        def apply(x: Parsley[B]): Parsley[A => C] = pos <**> x.map(y => p => this.apply(_,y)(p))
    }

    object Func {
        def apply(funcType: Parsley[Type], id: Parsley[Ident], args: Parsley[List[Param]], body: Parsley[List[Statements]]): Parsley[Func] 
            = pos <**> (funcType, id, args, body).zipped(Func(_,_,_,_) _)
    }
    object Program extends ParserBridgePos2[List[Func], List[Statements], Program]
    object Param extends ParserBridgePos2[Type, Ident, Param]
    object DeclareIdent extends ParserBridgePos3[Type, Ident, rValue, DeclareIdent]
    object Skip extends ParserBridgePos0[Skip]
    object Null extends ParserBridgePos0[Null]
    object AssignVal extends ParserBridgePos2[lValue, rValue, AssignVal]
    object Read extends ParserBridgePos1[lValue, Read]
    object Free extends ParserBridgePos1[Expr, Free]
    object Return extends ParserBridgePos1[Expr, Return]
    object Exit extends ParserBridgePos1[Expr, Exit]
    object Print extends ParserBridgePos1[Expr, Print]
    object Println extends ParserBridgePos1[Expr, Println]
    object If extends ParserBridgePos3[Expr, List[Statements], List[Statements], If]
    object While extends ParserBridgePos2[Expr, List[Statements], While]
    object NewScope extends ParserBridgePos1[List[Statements], NewScope]
    object NewPair extends ParserBridgePos2[Expr, Expr, NewPair]
    object Call extends ParserBridgePos2[Ident, List[Expr], Call]
    object Fst extends ParserBridgePos1[lValue, Fst]
    object Snd extends ParserBridgePos1[lValue, Snd]
    object IntLit extends ParserBridgePos1[Int, IntLit]
    object BoolLit extends ParserBridgePos1[Boolean, BoolLit]
    object CharLit extends ParserBridgePos1[Char, CharLit]
    object StrLit extends ParserBridgePos1[String, StrLit]
    object ArrayLit extends ParserBridgePos1[List[Expr], ArrayLit]    
    object Ident extends ParserBridgePos1[String, Ident]
    object Paren extends ParserBridgePos1[Expr, Paren]
    object PairType extends ParserBridgePos2[PairElemType, PairElemType, PairType]
    object JustPair extends ParserBridgePos0[JustPair]
    object ArrayElem extends PartialParserBridge[IdentWithArray, Expr, ArrayElem]
    
    //Binary operator
    object Mul extends ParserBridgePos2[Expr, Expr, Mul]
    object Div extends ParserBridgePos2[Expr, Expr, Div]
    object Mod extends ParserBridgePos2[Expr, Expr, Mod]
    object Add extends ParserBridgePos2[Expr, Expr, Add]
    object Sub extends ParserBridgePos2[Expr, Expr, Sub]    
    object GT extends ParserBridgePos2[Expr, Expr, GT]
    object GE extends ParserBridgePos2[Expr, Expr, GE] 
    object LT extends ParserBridgePos2[Expr, Expr, LT]  
    object LE extends ParserBridgePos2[Expr, Expr, LE]
    object EQ extends ParserBridgePos2[Expr, Expr, EQ]
    object NEQ extends ParserBridgePos2[Expr, Expr, NEQ]    
    object And extends ParserBridgePos2[Expr, Expr, And]
    object Or extends ParserBridgePos2[Expr, Expr, Or]

    //unary operator
    object Not extends ParserBridgePos1[Expr, Expr]
    object Neg extends ParserBridgePos1[Expr, Expr]
    object Len extends ParserBridgePos1[Expr, Expr]
    object Ord extends ParserBridgePos1[Expr, Expr]
    object Chr extends ParserBridgePos1[Expr, Expr]

    //base-type
    object IntType extends ParserBridgePos0[IntType]
    object CharType extends ParserBridgePos0[CharType]
    object StringType extends ParserBridgePos0[StringType]
    object BoolType extends ParserBridgePos0[BoolType]
    object ArrayType extends ParserBridgePos1[Type, ArrayType]
}
