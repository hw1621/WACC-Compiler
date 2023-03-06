package Frontend

import Frontend.AbstractSyntax._
import scala.io.Source
import java.io.File
import Errors._
import scala.collection.mutable

object Semantic {
  val defaultPos = (-1, -1)
  val waccPrefix = "wacc_"

  def sameType(t1: Type, t2: Type): Boolean = {
    if (t1 == t2) {
      true
    } else (t1, t2) match {
      case (ArrayType(tin1), ArrayType(tin2)) => sameType(tin1, tin2)
      case (PairType(t1l, t1r), PairType(t2l, t2r)) => sameType(t1l, t2l) && sameType(t1r, t2r)
      case (PairType(_, _), JustPair()) => true
      case (JustPair(), PairType(_, _)) => true
      case _ => if (t1 == AnyType || t2 == AnyType) true else false
    }
  }

  def recordSemError (
    msg: String, 
    pos: (Int, Int))
    (implicit
      errors: mutable.ListBuffer[Error],
      source: String,
      fileLines: Array[String]
    ): Unit = {
    errors += SemanticError.genError(msg, pos)
  } 

  def getType(expr: lValue)(implicit varTable: mutable.Map[String, Type]): Type = {
    expr match {
      case Ident(name) => varTable.getOrElse(name, NoExistType)
      case ArrayElem(id, _) => getType(id: Expr) match {
        case ArrayType(inType) => inType
        case _ => NoExistType
      }
      case Fst(value) => getType(value) match {
        case JustPair() => AnyType
        case PairType(inType, _) => inType
        case _ => NoExistType
      }
      case Snd(value) => getType(value) match {
        case JustPair() => AnyType
        case PairType(_, inType) => inType
        case _ => NoExistType
      }
    }
  }

  def getType(expr: rValue)(implicit varTable: mutable.Map[String, Type], funcTable: List[Func]): Type = {
    expr match {
      case expre: Expr => getType(expre)
      case NewPair(e1, e2) => {
        val e1Type: PairElemType = getType(e1) match {
          case PairType(_, _) => JustPair()(defaultPos)
          case pairType: PairElemType => pairType
          case _ => NoExistType
        }
        val e2Type: PairElemType = getType(e2) match {
          case PairType(_, _) => JustPair()(defaultPos)
          case pairType: PairElemType => pairType
          case _ => NoExistType
        }
        PairType(e1Type, e2Type)(defaultPos)
      } 
      case Call(id, params) => funcTable.find(x => x.id.name == id.name) match {
          case Some(value) => value.funcType
          case None => NoExistType
        }
      case Fst(value) =>  getType(value) match {
        case JustPair() => AnyType
        case PairType(inType, _) => inType
        case _ => NoExistType
      }
      case Snd(value) =>  getType(value) match {
        case JustPair() => AnyType
        case PairType(_, inType) => inType
        case _ => NoExistType
      } 
      case ArrayLit(arrayElems) => if (arrayElems.isEmpty) ArrayType(AnyType)(defaultPos) else ArrayType(getType(arrayElems.head))(defaultPos)
    }
  }

  def getType(expr: Expr)(implicit varTable: mutable.Map[String, Type]): Type = {
    expr match {
      case Add(_, _) => IntType()(defaultPos)
      case Sub(_, _) => IntType()(defaultPos)
      case Mul(_, _) => IntType()(defaultPos)
      case Mod(_, _) => IntType()(defaultPos)
      case Div(_, _) => IntType()(defaultPos)
      case And(_, _) => BoolType()(defaultPos)
      case Or(_, _) => BoolType()(defaultPos)
      case EQ(_, _) => BoolType()(defaultPos)
      case NEQ(_, _) => BoolType()(defaultPos)
      case GT(_, _) => BoolType()(defaultPos)
      case GE(_, _) => BoolType()(defaultPos)
      case LT(_, _) => BoolType()(defaultPos)
      case LE(_, _) => BoolType()(defaultPos)
      case Neg(_) => IntType()(defaultPos)
      case Not(_) => BoolType()(defaultPos)
      case Len(_) => IntType()(defaultPos)
      case Ord(_) => IntType()(defaultPos)
      case Chr(_) => CharType()(defaultPos)
      case ArrayElem(id, _) => getType(id: Expr) match {
        case ArrayType(inType) => inType
        case _ => NoExistType
      }
      case BoolLit(_) => BoolType()(defaultPos)
      case IntLit(_) => IntType()(defaultPos)
      case CharLit(_) => CharType()(defaultPos)
      case Ident(name) => varTable.getOrElse(name, NoExistType)
      case StrLit(_) => StringType()(defaultPos)
      case Null() => PairType(AnyType, AnyType)(defaultPos)
      case Paren(inside) => getType(inside)
    }
  }

  def verifyExpr(
    expr: lValue, 
    scopeVar: Map[String, String])
    (implicit
      errors: mutable.ListBuffer[Error], 
      varTable: mutable.Map[String, Type],
      funcTable: List[Func],
      source: String,
      fileLines: Array[String]
    ): lValue = {
    expr match {
      case Ident(name) => {
        if (!scopeVar.contains(name)) {
          recordSemError("Identifier not in scope %s".format(name), expr.pos)
          expr
        } else {
          new Ident(scopeVar.get(name).get)(expr.pos)
        }
      }
      case ArrayElem(id, index) => {
        val newInd = verifyExpr(index: Expr, scopeVar)
        getType(newInd) match {
          case IntType() =>
          case _ => recordSemError("Array index is not int", newInd.pos)
        }
        id match {
          case ArrayElem(_, _) => {
            verifyExpr(id: Expr, scopeVar) match {
              case newid: IdentWithArray => new ArrayElem(newid, newInd)(expr.pos)
              case _ => {
                recordSemError("Array identifier evaluates to wrong type", id.pos)
                expr
              }
            }
          } 
          case Ident(name) => {
            varTable.get(scopeVar.getOrElse(name, "")) match {
              case Some(value) => value match {
                case ArrayType(_) => new ArrayElem(new Ident(scopeVar.get(name).get)(id.pos), newInd)(expr.pos)
                case _ => {
                  recordSemError("Trying to take array element from non-array identifier %s".format(name), id.pos)
                  expr
                } 
              }
              case None => {
                recordSemError("Identifier not in scope %s".format(name), id.pos)
                expr
              }
            }
          }
        }
      }
      case Fst(value) => new Fst(verifyExpr(value, scopeVar))(expr.pos)
      case Snd(value) => new Snd(verifyExpr(value, scopeVar))(expr.pos)
    }
  }

  def verifyExpr(
    expr: rValue, 
    scopeVar: Map[String, String])
    (implicit
      errors: mutable.ListBuffer[Error], 
      varTable: mutable.Map[String, Type],
      funcTable: List[Func],
      source: String,
      fileLines: Array[String]
    ): rValue = {
    expr match {
      case expre: Expr => verifyExpr(expre, scopeVar)
      case NewPair(e1, e2) => {
        val newe1 = verifyExpr(e1, scopeVar)
        val newe2 = verifyExpr(e2, scopeVar)
        getType(newe1) match {
          case PairType(_, _) =>
          case _: PairElemType =>
          case _ => recordSemError("Unallowed element data type in newpair", e1.pos)
        }
        getType(newe2) match {
          case PairType(_, _) =>
          case _: PairElemType =>
          case _ => recordSemError("Unallowed element data type in newpair", e2.pos)
        }
        new NewPair(newe1, newe2)(expr.pos)
      } 
      case Call(id, params) => {
        val newparams = params.map(verifyExpr(_, scopeVar))
        val newid = Ident(waccPrefix + id.name)(id.pos)
        funcTable.find(x => x.id.name == newid.name) match {
          case Some(calledFunc) => {
            if (calledFunc.args.length != newparams.length) {
              recordSemError("Call to function %s has incorrect number of arguments".format(id.name), expr.pos)
            }
            (calledFunc.args zip newparams).foreach({case (x, y) => 
              if (!sameType(x.paramType, getType(y))) {
                recordSemError("Argument %s in the call to function %s has incorrect type, expect %s, found %s".format(x.id.name, id.name, x.paramType, getType(y)), y.pos)
              }
            })
          }
          case None => recordSemError("Unrecognized function identifier", id.pos)
        }
        new Call(newid, newparams)(expr.pos)
      }
      case ArrayLit(arrayElems) => {
        val newelems = arrayElems.map(verifyExpr(_, scopeVar))
        newelems.foreach(x => 
        if (!sameType(getType(x), getType(newelems.head)))
        recordSemError("Elements in array literal has different types", expr.pos))
        new ArrayLit(newelems)(expr.pos)
      }
      case Fst(value) => new Fst(verifyExpr(value, scopeVar))(expr.pos)
      case Snd(value) => new Snd(verifyExpr(value, scopeVar))(expr.pos)
    }
  }

  def verifyExpr(
    expr: Expr, 
    scopeVar: Map[String, String])
    (implicit
      errors: mutable.ListBuffer[Error], 
      varTable: mutable.Map[String, Type],
      funcTable: List[Func],
      source: String,
      fileLines: Array[String]
    ): Expr = {
    expr match {
      case biOp: BiOp => verifyBiOp(biOp, scopeVar)
      case unOp: UnOp => verifyUnOp(unOp, scopeVar)
      case ArrayElem(id, index) => {
        val newInd = verifyExpr(index: Expr, scopeVar)
        getType(newInd) match {
          case IntType() =>
          case _ => recordSemError("Array index is not int", newInd.pos)
        }
        id match {
          case ArrayElem(_, _) => {
            verifyExpr(id: Expr, scopeVar) match {
              case newid: IdentWithArray => new ArrayElem(newid, newInd)(expr.pos)
              case _ => {
                recordSemError("Array identifier evaluates to wrong type", id.pos)
                expr
              }
            }
          } 
          case Ident(name) => {
            varTable.get(scopeVar.getOrElse(name, "")) match {
              case Some(value) => value match {
                case ArrayType(_) => new ArrayElem(new Ident(scopeVar.get(name).get)(id.pos), newInd)(expr.pos)
                case _ => {
                  recordSemError("Trying to take array element from non-array identifier %s".format(name), id.pos)
                  expr
                } 
              }
              case None => {
                recordSemError("Identifier not in scope %s".format(name), id.pos)
                expr
              }
            }
          }
        }
      }
      case Ident(name) => {
        if (!scopeVar.contains(name)) {
          recordSemError("Identifier not in scope %s".format(name), expr.pos)
          expr
        } else {
          new Ident(scopeVar.get(name).get)(expr.pos)
        }
      }
      case Paren(inside) => new Paren(verifyExpr(inside, scopeVar))(expr.pos)
      case _ => expr //println("Do nothing")
    }
  }

  def verifyUnOp(
    expr: UnOp, 
    scopeVar: Map[String, String])
    (implicit
      errors: mutable.ListBuffer[Error], 
      varTable: mutable.Map[String, Type],
      funcTable: List[Func],
      source: String,
      fileLines: Array[String]
    ): UnOp = {
    val inside = verifyExpr(expr.expr, scopeVar)
    def isIntType(operation: String): Unit = {
      getType(inside) match {
        case IntType() =>
        case _ => recordSemError("Argument of %s is not Int".format(operation), inside.pos)
      }
    } 
    def isCharType(operation: String): Unit = {
      getType(inside) match {
        case CharType() =>
        case _ => recordSemError("Argument of %s is not Char".format(operation), inside.pos)
      }
    }
    def isBoolType(operation: String): Unit = {
      getType(inside) match {
        case BoolType() =>
        case _ => recordSemError("Argument of %s is not Bool".format(operation), inside.pos)
      }
    }
    def isSeqType(operation: String): Unit = {
      getType(inside) match {
        case ArrayType(_) => 
        case StringType() =>
        case _ => recordSemError("Argument of %s is not String".format(operation), inside.pos)
      }
    }
    expr match {
      case Chr(_) => {
        isIntType("chr")
        new Chr(inside)(expr.pos)
      }
      case Len(_) => {
        isSeqType("len")
        new Len(inside)(expr.pos)
      }
      case Neg(_) => {
        isIntType("negativity")
        new Neg(inside)(expr.pos)
      }
      case Not(_) => {
        isBoolType("!")
        new Not(inside)(expr.pos)
      }
      case Ord(_) => {
        isCharType("ord")
        new Ord(inside)(expr.pos)
      }
    }
  }

  def verifyBiOp(
    expr: BiOp, 
    scopeVar: Map[String, String])
    (implicit
      errors: mutable.ListBuffer[Error], 
      varTable: mutable.Map[String, Type],
      funcTable: List[Func],
      source: String,
      fileLines: Array[String],
    ): BiOp = {
    val newl = verifyExpr(expr.lhs, scopeVar)
    val newr = verifyExpr(expr.rhs, scopeVar)
    def isIntType(operation: String): Unit = {
      getType(newl) match {
        case IntType() =>
        case _ => recordSemError("Left expression in %s is not Int".format(operation), newl.pos)
      }
      getType(newr) match {
        case IntType() =>
        case _ => recordSemError("Right expression in %s is not Int".format(operation), newr.pos)
      }
    } 
    def isBoolType(operation: String): Unit = {
      getType(newl) match {
        case BoolType() =>
        case _ => recordSemError("Left expression in %s is not Bool".format(operation), newl.pos)
      }
      getType(newr) match {
        case BoolType() =>
        case _ => recordSemError("Right expression in %s is not Bool".format(operation), newr.pos)
      }
    } 
    def isIntOrCharType(operation: String): Unit = {
      val e1t = getType(newl)
      e1t match {
        case IntType() =>
        case CharType() => 
        case _ => recordSemError("Left expression in %s is not Int nor Char".format(operation), newl.pos)
      }
      val e2t = getType(newr)
      e2t match {
        case IntType() =>
        case CharType() => 
        case _ => recordSemError("Right expression in %s is not Int nor Char".format(operation), newr.pos)
      }
      if (!sameType(e1t, e2t)) {
        recordSemError("Two sides of %s has different types, left is %s, right is %s".format(operation, e1t, e2t), expr.pos)
      }
    } 
    def isSameType(operation: String): Unit = {
      val e1t = getType(newl)
      val e2t = getType(newr)
      if (!sameType(e1t, e2t)) {
        recordSemError("Two sides of %s has different types, left is %s, right is %s".format(operation, e1t, e2t), expr.pos)
      }
    }
    expr match {
      case Add(_, _) => {
        isIntType("addition")
        new Add(newl, newr)(expr.pos)
      }
      case Sub(_, _) => {
        isIntType("subtraction")
        new Sub(newl, newr)(expr.pos)
      }
      case Mul(_, _) => {
        isIntType("multiplication")
        new Mul(newl, newr)(expr.pos)
      }
      case Mod(_, _) => {
        isIntType("modding")
        new Mod(newl, newr)(expr.pos)
      }
      case Div(_, _) => {
        isIntType("division")
        new Div(newl, newr)(expr.pos)
      }
      case And(_, _) => {
        isBoolType("and operation")
        new And(newl, newr)(expr.pos)
      }
      case Or(_, _) => {
        isBoolType("or operation")
        new Or(newl, newr)(expr.pos)
      }
      case EQ(_, _) => {
        isSameType("\'==\'")
        new EQ(newl, newr)(expr.pos)
      }
      case NEQ(_, _) => {
        isSameType("\'!=\'")
        new NEQ(newl, newr)(expr.pos)
      }
      case GT(_, _) => {
        isIntOrCharType("\'>\'")
        new GT(newl, newr)(expr.pos)
      }
      case GE(_, _) => {
        isIntOrCharType("\'>=\'")
        new GE(newl, newr)(expr.pos)
      }
      case LT(_, _) => {
        isIntOrCharType("\'<\'")
        new LT(newl, newr)(expr.pos)
      }
      case LE(_, _) => {
        isIntOrCharType("\'<=\'")
        new LE(newl, newr)(expr.pos)
      }
    }
  }

  def verifyState(
    statements: List[Statements], 
    scopeVar: Map[String, String],
    returnType: Type,
    scopePrefix: String)
    (implicit
      errors: mutable.ListBuffer[Error], 
      varTable: mutable.Map[String, Type],
      funcTable: List[Func],
      source: String,
      fileLines: Array[String]
    ): List[Statements] = {
    var localVarTable: Map[String, String] = Map.empty[String, String]
    val newStates: mutable.ListBuffer[Statements] = mutable.ListBuffer.empty[Statements]
    var scopeIndex = 0
    for (statement <- statements) {
      val verifiedState: Statements = statement match {
        case Skip() => statement
        case DeclareIdent(identType, id, initValue) => {
          val newvalue = verifyExpr(initValue, scopeVar ++ localVarTable)
          val newName = scopePrefix ++ id.name
          if (localVarTable.contains(id.name)) {
            recordSemError("Duplicated variable declaration of identifier %s".format(id.name, localVarTable), id.pos)
          } else if (!sameType(identType, getType(newvalue))) {
            recordSemError("Type mismatch in initial value assignment to %s, expect %s, found %s".format(id.name, identType, getType(newvalue)), statement.pos)
          }
          localVarTable = localVarTable + (id.name -> newName)
          varTable += (newName -> identType)
          new DeclareIdent(identType, new Ident(newName)(id.pos), newvalue)(statement.pos)
        }
        case AssignVal(lhs, rhs) => {
          val newl = verifyExpr(lhs, scopeVar ++ localVarTable)
          val newr = verifyExpr(rhs, scopeVar ++ localVarTable)
          if (!sameType(getType(newl), getType(newr))) {
            recordSemError("Type mismatch in assignment, expect %s, found %s".format(getType(newl), getType(newr)), statement.pos)
          } else if (getType(newl) == AnyType && getType(newr) == AnyType) {
            recordSemError("Both sides have unclear type", statement.pos)
          }
          new AssignVal(newl, newr)(statement.pos)
        }
        case Read(expr) => {
          val newexpr = verifyExpr(expr, scopeVar ++ localVarTable)
          getType(newexpr) match {
            case IntType() =>
            case CharType() => 
            case StringType() =>
            case _ => recordSemError("Variable accepting read has wrong type, can only be char, int or string", statement.pos)
          }
          new Read(newexpr)(statement.pos)
        }
        case Free(expr) => {
          val newexpr = verifyExpr(expr, scopeVar ++ localVarTable)
          getType(newexpr) match {
            case PairType(_, _) | ArrayType(_) =>
            case _ => recordSemError("Only Pair and Array can be freed", statement.pos)
          }
          new Free(newexpr)(statement.pos)
        } 
        case Return(expr) => {
          val newexpr = verifyExpr(expr, scopeVar ++ localVarTable)
          if (returnType == null) {
            recordSemError("Return is misused in main program", statement.pos)
          } else if (!sameType(getType(newexpr), returnType)) {
            recordSemError("Return type mismatch, expect %s, found %s".format(returnType, getType(expr)), statement.pos)
          }
          new Return(newexpr)(statement.pos)
        } 
        case Exit(expr) => {
          val newexpr = verifyExpr(expr, scopeVar ++ localVarTable)
          getType(newexpr) match {
            case IntType() =>
            case _ => recordSemError("User defined exit code is not integer", newexpr.pos)
          }
          new Exit(newexpr)(statement.pos)
        } 
        case Print(expr) => new Print(verifyExpr(expr, scopeVar ++ localVarTable))(statement.pos)
        case Println(expr) => new Println(verifyExpr(expr, scopeVar ++ localVarTable))(statement.pos)
        case If(expr, thenBody, elseBody) => {
          val newexpr = verifyExpr(expr, scopeVar ++ localVarTable)
          getType(newexpr) match {
            case BoolType() =>
            case _ => recordSemError("If condition is not Bool", expr.pos)
          }
          val newthen = verifyState(thenBody, scopeVar ++ localVarTable, returnType, "%s%difthen-".format(scopePrefix, scopeIndex))
          val newelse = verifyState(elseBody, scopeVar ++ localVarTable, returnType, "%s%difelse-".format(scopePrefix, scopeIndex))
          scopeIndex += 1
          new If(newexpr, newthen, newelse)(statement.pos)
        }
        case While(expr, body) => {
          val newexpr = verifyExpr(expr, scopeVar ++ localVarTable)
          getType(newexpr) match {
            case BoolType() =>
            case _ => recordSemError("While condition is not Bool", expr.pos)
          }
          val newbody = verifyState(body, scopeVar ++ localVarTable, returnType, "%s%dwhile-".format(scopePrefix, scopeIndex))
          scopeIndex += 1
          new While(newexpr, newbody)(statement.pos)
        }
        case NewScope(body) => {
          val newbody = verifyState(body, scopeVar ++ localVarTable, returnType, "%s%dsubscope-".format(scopePrefix, scopeIndex))
          scopeIndex += 1
          new NewScope(newbody)(statement.pos)
        }
      }
      newStates += verifiedState
    }
    newStates.toList
  }

  def semanticCheck(inProgram: Program, file: String): (List[Error], Program, mutable.Map[String, Type]) = {
    implicit val funcTable = inProgram.funcs.map(x => x match{
      case Func(funcType, id, args, body) => new Func(funcType, Ident(waccPrefix + id.name)(id.pos), args, body)(x.pos)
    })
    implicit val fileName: String = file
    implicit val fileContent: Array[String] = Source.fromFile(new File(file)).getLines().toArray
    implicit val varTable: mutable.Map[String, Type] = mutable.LinkedHashMap[String, Type]()
    implicit val errors: mutable.ListBuffer[Error] = mutable.ListBuffer.empty[Error]
    var tempFuncTable: List[Func] = Nil
    val newFuncs = funcTable.map(x => {
      if (tempFuncTable.exists(y => y.id.name == x.id.name)) {
        recordSemError("Duplicated function declaration: %s".format(x.id.name), x.pos)
      } else {
        tempFuncTable = tempFuncTable :+ x
      }
      var argList: List[Param] = Nil
      x.args.foreach(a => if (argList.exists(b => a.id.name == b.id.name)) {
        recordSemError("Duplicated function argument %s in function %s".format(a.id.name, x.id.name), a.pos)
      } else {
        argList = argList :+ a
      })
      val funScopePrefix = "func-%s-".format(x.id.name)
      x.args.foreach(y => (varTable += (funScopePrefix ++ "param-" ++ y.id.name) -> y.paramType))
      new Func(x.funcType, x.id, x.args, 
      verifyState(x.body, x.args.map(y => (y.id.name -> (funScopePrefix ++ "param-" ++ y.id.name))).toMap, x.funcType, funScopePrefix))(x.pos)
    })
    val newprog = new Program(newFuncs, verifyState(inProgram.statements, Map.empty, null, "main-"))(inProgram.pos)
    (errors.toList, newprog, varTable)
  }
}