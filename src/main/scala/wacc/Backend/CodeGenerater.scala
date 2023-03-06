package Backend

import scala.collection.mutable
import Frontend.AbstractSyntax._
import Frontend.Semantic.getType
import Backend.AbstractInstructions._
import Backend.PredefinedFunctions._

// Register rules
// r0, r1, r2, r3 - Input arguments, (NOT FOR NOW)might be used for variables when it is certain that some of them are not used
// r0 - The return value or the exit code
// r4, r5, r6, r7 - For variables
// r8 - The accumulator, put and take any intermmediate result to/from it
// r9 - Vice accumulator, put any temperory value to it
// r10 - To contain the index of an array
// r12 - To contrain the header address of an array
// Variables that can't fit into registers are pushed into stack, the space must be declared by sub sp, sp, _ before the code body
// Variables in stack are located by relative address below the fp (which marks the upper bound of the stack), since sp could be modified at any time
// Int takes 4 bytes of space, Char and Bool takes 1, String, Array and Pair need to malloc first and store the address
// The size of an array is 4 + [No. of elements] * [Bytes of a single element], the 4 bytes at the beginning are for storing the length of the array
// String is taken as an array of Char, and Pair is taken as an array of 2 elements
// Function arguments that can't fit into registers are pushed into stack
// Arguments in stack are stored by str _, [sp, _]! before jumping to function body. In the function body, they are loaded by the relative address ABOVE the fp
// String literals are stored before the code body, and located via label
// Each generation fucntion must ensure that: 1. The stack remains unchanged after the code 2. All result value stored in r8 or other designated registers

object CodeGenerater {
  val defaultpos = (-1, -1)
  val stringLabelHeader = ".L.str"
  val WORD_SIZE = 4
  val BOOL_TYPE = 1
  val CHAR_TYPE = 1
  val VAR_REG_NUM = 4

  class CodeState(val symTable: mutable.Map[String, IdentSymbol], 
                  val varTypeTable: mutable.Map[String, Type], 
                  val strTable: mutable.Map[String, Label], 
                  val funcStackDepth: mutable.Map[String, (Int, Int)], 
                  var labelCounter: Int,
                  val preFuncs: mutable.Set[List[AssemblyCodeLine]])

  def getIdent(id: String)(implicit codeState: CodeState) = {
    codeState.symTable.get(id).get
  } 
  def toIdent(id: String, src: Register)(implicit codeState: CodeState): Instruction = {
    getIdent(id) match {
      case x: Address => Stra(src, x, typeSize(codeState.varTypeTable.get(id).get))
      case x: Register => Mov(x, src)
    }
  }
  def fromIdent(id: String, dst: Register)(implicit codeState: CodeState): Instruction = {
    getIdent(id) match {
      case x: Address => Ldra(dst, x, typeSize(codeState.varTypeTable.get(id).get))
      case x: Register => Mov(dst, x)
    }
  }
  def typeSize(inType: Type): Int = {
    inType match {
      case BoolType() => BOOL_TYPE
      case CharType() => CHAR_TYPE
      case _ => WORD_SIZE
    }
  }
  def CallMalloc = BranchLink(Label("malloc"))


  def genCode(
    astProgram: Program,
    varTypeTable: mutable.Map[String, Type]): List[AssemblyCodeLine] = {
    implicit val asmCodes: mutable.ListBuffer[AssemblyCodeLine] = mutable.ListBuffer.empty[AssemblyCodeLine]
    val symTable: mutable.Map[String, IdentSymbol] = mutable.LinkedHashMap.empty[String, IdentSymbol]
    val funcStackDepth: mutable.Map[String, (Int, Int)] = mutable.Map.empty[String, (Int, Int)]

    for (func <- astProgram.funcs) {
      // Setting symbols for function variables
      val funcVar = varTypeTable.filter(x => x._1.startsWith(s"func-${func.id.name}") && !x._1.startsWith(s"func-${func.id.name}-param-"))
      var funcDepth = funcVar.map(x => typeSize(x._2)).sum
      var currentDepth = funcDepth
      var funcVarNum = 0
      for ((varName, varType) <- funcVar) {
        if (funcVarNum < VAR_REG_NUM) {
          symTable += (varName -> Register(funcVarNum + WORD_SIZE))
          funcVarNum += 1
          funcDepth -= typeSize(varType)
          currentDepth -= typeSize(varType)
        } else {
          funcVarNum = 5
          val paramSize = typeSize(varType)
          symTable += (varName -> Address(FP, ImmVal(-currentDepth)))
          currentDepth -= paramSize
          funcVarNum += 1
        }
      }
      funcStackDepth += (func.id.name -> (funcVarNum, funcDepth))

      // Setting symbols for function parameters
      var paramNum = 0
      val funcParam = varTypeTable.filter(x => x._1.startsWith(s"func-${func.id.name}-param-"))
      funcVarNum = if (funcVarNum > VAR_REG_NUM) VAR_REG_NUM else funcVarNum
      var paramDepth = funcParam.map(x => typeSize(x._2)).sum + funcVarNum * WORD_SIZE + 24
      for ((varName, varType) <- funcParam) {
        if (paramNum < VAR_REG_NUM) {
          symTable += (varName -> Register(paramNum))
          paramNum += 1
          paramDepth -= typeSize(varType)
        } else {
          val paramSize = typeSize(varType)
          paramDepth -= paramSize
          symTable += (varName -> Address(FP, ImmVal(paramDepth)))
        }
      }
    }

    // Setting symbols for main program variables
    val mainVar = varTypeTable.filter(x => x._1.startsWith("main-"))
    var spDepth = mainVar.map(x => typeSize(x._2)).sum
    var currentDepth = spDepth
    var regNum = 0
    for ((varName, varType) <- mainVar) {
      if (regNum < VAR_REG_NUM) {
        symTable += (varName -> Register(regNum + 4))
        regNum += 1
        spDepth -= typeSize(varType)
        currentDepth -= typeSize(varType)
      } else {
        val paramSize = typeSize(varType)
        symTable += (varName -> Address(FP, ImmVal(-currentDepth)))
        currentDepth -= paramSize
        regNum += 1
      }
    }
    funcStackDepth += ("main" -> (regNum, spDepth))
    implicit val codeState: CodeState = new CodeState(symTable,
                                        varTypeTable,
                                         mutable.LinkedHashMap.empty[String, Label],
                                         funcStackDepth,
                                         0,
                                         mutable.Set.empty[List[AssemblyCodeLine]])
    genProgram(astProgram)
    asmCodes.toList
  }

  def genProgram(
    astProgram: Program
    ) (implicit 
      asmCodes: mutable.ListBuffer[AssemblyCodeLine],
      codeState: CodeState
      ): Unit = {
    val mainCode = genFunction(new Func(AnyType, Ident("main")(defaultpos), Nil, astProgram.statements ++ List(Return(IntLit(0)(defaultpos))(defaultpos)))(defaultpos))
    val funcCodes = astProgram.funcs.map(genFunction)
    // Initialize prestored strings
    asmCodes += Header(".data")
    for ((key -> value) <- codeState.strTable) {
      asmCodes += WordSpace(key.length)
      asmCodes += value
      asmCodes += Asciz(key)
    }
    asmCodes += Header(".text")
    asmCodes += Header(".global main")
    asmCodes ++= mainCode
    asmCodes ++= funcCodes.flatten
    asmCodes ++= codeState.preFuncs.toList.flatten
  }

  def genFunction(
    func: Func
  ) (implicit
    codeState: CodeState
      ): List[AssemblyCodeLine] = {
    implicit val asmCodes: mutable.ListBuffer[AssemblyCodeLine] = mutable.ListBuffer.empty
    implicit val funcName = func.id.name
    val tmp = codeState.funcStackDepth.get(funcName).get
    val idenCount = tmp._1
    val idenSpDepth = tmp._2
    asmCodes += Label(func.id.toStringName)
    asmCodes += Push(List(FP, LR))
    // Push registers about to use
    val pushregs: mutable.ListBuffer[Register] = mutable.ListBuffer.empty[Register]
    var i = 4
    while (idenCount - (i - VAR_REG_NUM) > 0 && i < 8) {
      pushregs += Register(i)
      i += 1
    }
    asmCodes += Push(pushregs.toList ++ List(R8, R9, R10, R12))
    asmCodes += Mov(FP, SP)
    if (idenCount > VAR_REG_NUM) {
      asmCodes += Suba(SP, SP, ImmVal(idenSpDepth))
    }
    val bodyCode = genStatement(func.body)
    asmCodes ++= bodyCode
    asmCodes.toList
  }

  def genStatement(
    stats: List[Statements]
  ) (implicit
    codeState: CodeState,
    funcName: String
  ): List[AssemblyCodeLine] = {
    val asmCodes: mutable.ListBuffer[AssemblyCodeLine] = mutable.ListBuffer.empty[AssemblyCodeLine]
    val idenCount = codeState.funcStackDepth.get(funcName).get._1
    for (stat <- stats) {
      stat match {
        case Skip() =>
        case NewScope(scopeStats) => {
          asmCodes ++= genStatement(scopeStats)
        }
        case Free(expr) => {
          implicit val varTable = codeState.varTypeTable
          asmCodes ++= genExpr(expr)
          getType(expr) match {
            case ArrayType(_) => {
              asmCodes += Suba(R8, R8, ImmVal(WORD_SIZE))
              asmCodes += Mov(R0, R8)
              asmCodes += BranchLink(Label("free"))
            }
            case PairType(_,_) => {
              codeState.preFuncs += FreePair
              asmCodes += Mov(R0, R8)
              asmCodes += BranchLink(Label("_freepair"))              
            }
            case _ => {
              asmCodes += Mov(R0, ImmVal(1))
              asmCodes += BranchLink(Label("exit"))
            }
          }
        }
        case Return(expr) => {
          asmCodes ++= genExpr(expr)
          asmCodes += Mov(R0, R8)
          asmCodes += Mov(SP, FP)
          val pushregs: mutable.ListBuffer[Register] = mutable.ListBuffer.empty[Register]
          var i = 4
          while (idenCount - (i - VAR_REG_NUM) > 0 && i < 8) {
            pushregs += Register(i)
            i += 1
          }
          asmCodes += Pop(pushregs.toList ++ List(R8, R9, R10, R12))
          asmCodes += Pop(List(FP, PC))     
        }
        case Exit(expr) => {
          val result = genExpr(expr)
          for (str <- result) {
            asmCodes += str
          }
          asmCodes += Mov(R0, R8)
          asmCodes += BranchLink(Label("exit"))
        }
        case DeclareIdent(identType, id, rhs) => { 
          asmCodes ++= genRValue(rhs)
          asmCodes += toIdent(id.name, R8)
        }
        case AssignVal(lhs, rhs) => {
          asmCodes ++= genRValue(rhs)
          asmCodes ++= genLValue(lhs)
        }
        case Print(expr) => asmCodes ++= genPrint(expr, false)
        case Println(expr) => asmCodes ++= genPrint(expr, true)
        case Read(lvalue) => {
          lvalue match {
            case inside: IdentWithArray => asmCodes ++= genExpr(inside)
            case inside: PairElem => asmCodes ++= genRValue(inside)
          }
          implicit val varTable = codeState.varTypeTable
          val label = getType(lvalue) match {
            case CharType() => {
              codeState.preFuncs += ReadChar
              "_readc"
            }
            case IntType() => {
              codeState.preFuncs += ReadInt
              "_readi"
            }
            case _ => {
              codeState.preFuncs += ReadChar
              "_readc"
            }  
          }
          asmCodes += Mov(R0, R8)
          asmCodes += BranchLink(Label(label))
          asmCodes += Mov(R8, R0)
          asmCodes ++= genLValue(lvalue)
        }
        case If(expr, thenStats, elseStats) => {
          asmCodes ++= genExpr(expr)
          asmCodes += Comparea(R8, ImmVal(1))
          val thenlabel = s".L${codeState.labelCounter}" 
          codeState.labelCounter += 1 
          asmCodes += Branch(Label(thenlabel), asmEQ)

          asmCodes ++= genStatement(elseStats)
          val endLabel = s".L${codeState.labelCounter}" 
          asmCodes += Branch(Label(endLabel), noCond)
          codeState.labelCounter += 1

          asmCodes += Label(thenlabel)
          asmCodes ++= genStatement(thenStats)
          asmCodes += Label(endLabel)
        } 
        case While(expr, stats) => {
          val condLabel = s".L${codeState.labelCounter}"
          codeState.labelCounter += 1
          val bodyLabel = s".L${codeState.labelCounter}"
          codeState.labelCounter += 1

          asmCodes += Branch(Label(condLabel), noCond)
          asmCodes += Label(bodyLabel)
          asmCodes ++= genStatement(stats)
          asmCodes += Label(condLabel)
          asmCodes ++= genExpr(expr)
          asmCodes += Comparea(R8, ImmVal(1))
          asmCodes += Branch(Label(bodyLabel), asmEQ)
        }
      }       
    }
   asmCodes.toList   
}

  def genPrint(
    expr: Expr, lnFlag: Boolean
    ) (implicit 
       codeState: CodeState
    ): List[AssemblyCodeLine] = {
    val asmCodes: mutable.ListBuffer[AssemblyCodeLine] = mutable.ListBuffer.empty[AssemblyCodeLine]
    implicit val varTable = codeState.varTypeTable
    asmCodes ++= genExpr(expr)
    val label = getType(expr) match {
      case BoolType() => {
        codeState.preFuncs += PrintBool
        "_printb" 
      } 
      case CharType() => {
        codeState.preFuncs += PrintChar
        "_printc"
      }
      case IntType() => {
        codeState.preFuncs += PrintInt
        "_printi"
      }
      case StringType() | ArrayType(CharType()) => {
        codeState.preFuncs += PrintString
        "_prints"
      }
      case _ => {
        codeState.preFuncs += PrintP
        "_printp"
      }
    }
    asmCodes += Push(List(R0))
    asmCodes += Mov(R0, R8)
    asmCodes += BranchLink(Label(label))
    if (lnFlag) {
      codeState.preFuncs += PrintLine
      asmCodes += BranchLink(Label("_println"))
    } 
    asmCodes += Pop(List(R0))
    asmCodes.toList
  }

  def genRValue(
    rhs: rValue
  ) (implicit 
    codeState: CodeState,
    funcName: String
  ):List[AssemblyCodeLine] = {
    val asmCodes: mutable.ListBuffer[AssemblyCodeLine] = mutable.ListBuffer.empty[AssemblyCodeLine]
    implicit val varTable = codeState.varTypeTable
    // Generate ASM code depending on situation and store the final value to R8
    rhs match {
      case Fst(value) => {
        value match {
          case inside: IdentWithArray => asmCodes ++= genExpr(inside)
          case inside: PairElem => asmCodes ++= genRValue(inside)
        }
        asmCodes += Comparea(R8, ImmVal(0))
        asmCodes += BranchLink(Label("_errNull"), asmEQ)
        codeState.preFuncs += ErrNullPtr
        asmCodes += Ldra(R8, Address(R8, ImmVal(0)))
        val pairElemType = getType(value) match {
          case PairType(inType, _) => inType
          case _ => AnyType
        }
        typeSize(pairElemType) match {
          case 1 => asmCodes += Ldra(R8, Address(R8, ImmVal(0)), -1)
          case _ => asmCodes += Ldra(R8, Address(R8, ImmVal(0)))
        }
      }
      case Snd(value) => {
        value match {
          case inside: IdentWithArray => asmCodes ++= genExpr(inside)
          case inside: PairElem => asmCodes ++= genRValue(inside)
        }
        asmCodes += Comparea(R8, ImmVal(0))
        asmCodes += BranchLink(Label("_errNull"), asmEQ)
        codeState.preFuncs += ErrNullPtr
        asmCodes += Ldra(R8, Address(R8, ImmVal(WORD_SIZE)))
        val pairElemType = getType(value) match {
          case PairType(_, inType) => inType
          case _ => AnyType
        }
        typeSize(pairElemType) match {
          case 1 => asmCodes += Ldra(R8, Address(R8, ImmVal(0)), -1)
          case _ => asmCodes += Ldra(R8, Address(R8, ImmVal(0)))
        }
      }
      case NewPair(fst, snd) => {
        asmCodes += Push(List(R12))
        val fstSize = typeSize(getType(fst))
        asmCodes += Mov(R0, ImmVal(fstSize))
        asmCodes += CallMalloc
        asmCodes += Mov(R12, R0)
        asmCodes ++= genExpr(fst)
        asmCodes += Stra(R8, Address(R12, ImmVal(0)), fstSize)
        asmCodes += Mov(R8, R12)
        asmCodes += Push(List(R8))
        val sndSize = typeSize(getType(snd))
        asmCodes += Mov(R0, ImmVal(sndSize))
        asmCodes += CallMalloc
        asmCodes += Mov(R12, R0)
        asmCodes ++= genExpr(snd)
        asmCodes += Stra(R8, Address(R12, ImmVal(0)), sndSize)
        asmCodes += Mov(R8, R12)
        asmCodes += Push(List(R8))
        asmCodes += Mov(R0, ImmVal(8))
        asmCodes += CallMalloc
        asmCodes += Mov(R12, R0)
        asmCodes += Pop(List(R8))
        asmCodes += Stra(R8, Address(R12, ImmVal(WORD_SIZE)))
        asmCodes += Pop(List(R8))
        asmCodes += Stra(R8, Address(R12, ImmVal(0)))
        asmCodes += Mov(R8, R12)
        asmCodes += Pop(List(R12))
      }
      case Call(id, exprs) => {
        val paramCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
        val pushRegs = mutable.ListBuffer[Register](R0)
        var paraSpDepth = 0
        val funcParam = codeState.varTypeTable.filter(x => x._1.startsWith(s"func-${funcName}-param-"))
        var i = 0
        for ((paramName -> _) <- funcParam) {
          if (i < VAR_REG_NUM) {
            codeState.symTable.update(paramName, Address(R12, ImmVal(i * WORD_SIZE)))
          }
          i += 1
        }
        i = 0
        for (param <- exprs) {
          paramCodes ++= genExpr(param)
          if (i < VAR_REG_NUM) {
            paramCodes += Mov(Register(i), R8)
            if (i > 0) pushRegs += Register(i)
          } else {
            val paramSize = typeSize(getType(param))
            paramCodes += Stra(R8, Address(SP, ImmVal(-paramSize), ImmVal(1), PreIndex), paramSize)
            paraSpDepth += paramSize
          }
          i += 1
        }
        if (!pushRegs.isEmpty) {
          asmCodes += Push(pushRegs.toList)
          asmCodes += Mov(R12, SP)
        }
        i = 0
        for ((paramName -> _) <- funcParam) {
          if (i < VAR_REG_NUM) {
            codeState.symTable.update(paramName, Register(i))
          }
          i += 1
        }
        asmCodes ++= paramCodes
        asmCodes += BranchLink(Label(id.name))
        if (paraSpDepth > 0) asmCodes += Adda(SP, SP, ImmVal(paraSpDepth))
        asmCodes += Mov(R8, R0)
        if (!pushRegs.isEmpty) asmCodes += Pop(pushRegs.toList)
      }
      case ArrayLit(x) => {
        val arrayLen = x.length
        val elemSize = if (arrayLen > 0) typeSize(getType(x.head)) else 0
        asmCodes += Push(List(R12))
        asmCodes += Mov(R0, ImmVal(WORD_SIZE + elemSize * arrayLen))
        asmCodes += CallMalloc
        asmCodes += Mov(R12, R0)
        asmCodes += Mov(R8, ImmVal(arrayLen))
        asmCodes += Stra(R8, Address(R12, ImmVal(0)))
        asmCodes += Adda(R12, R12, ImmVal(WORD_SIZE))
        var i = 0
        for (elem <- x) {
          asmCodes ++= genExpr(elem)
          asmCodes += Stra(R8, Address(R12, ImmVal(i)), elemSize)
          i += elemSize
        }
        asmCodes += Mov(R8, R12)
        asmCodes += Pop(List(R12))
      }
      case expr: Expr => asmCodes ++= genExpr(expr) 
    }
    asmCodes.toList
  }

  def checkQuatationMarkInString(str:String): String = {
    if (str.contains('\"')) {
      val x = str.split('\"')
      val last = x.last
      val newx = x.dropRight(1)
      val result = new StringBuilder("")
      for (string <- newx) {
        result ++= string
        result ++= "\\"
        result ++= "\""
      }
      result ++= last
      return result.toString()
    } else {
      return str
    }
  }

  def genExpr(
    expr: Expr
    ) (implicit 
        codeState: CodeState
      ):List[AssemblyCodeLine] = {
    val asmCodes: mutable.ListBuffer[AssemblyCodeLine] = mutable.ListBuffer.empty[AssemblyCodeLine]
    implicit val varTable = codeState.varTypeTable
    // Generate ASM code depending on situation and store the final value to R8
    expr match {
      case BoolLit(value) => if (value) {
        asmCodes += Mov(R8, ImmVal(1))
      } else {
        asmCodes += Mov(R8, ImmVal(0))
      }
      case IntLit(value) => {
        asmCodes += Ldra(R8, Label(s"$value"))
      }
      case CharLit(value) => asmCodes += Mov(R8, ImmVal(value.toInt))
      case StrLit(value) => {
        val checked = checkQuatationMarkInString(value)
        asmCodes += Ldra(R8, codeState.strTable.getOrElse(checked, 
      {
        val stringCounts = codeState.strTable.keys.size
        val newLabel = Label(stringLabelHeader ++ stringCounts.toString)
        codeState.strTable += (checked -> newLabel)
        newLabel
      }))}
      case Ident(name) => {
        asmCodes += fromIdent(name, R8)
      }
      case ArrayElem(id, index) => {
        asmCodes += Push(List(R3))
        asmCodes ++= genExpr(id)
        asmCodes += Push(List(R8))
        asmCodes ++= genExpr(index)
        asmCodes += Pop(List(R3))
        asmCodes += Mov(R10, R8)
        typeSize(getType(expr)) match {
          case 1 => {
            asmCodes += BranchLink(Label("_arrLoadB"))
            codeState.preFuncs += ArrayLoadB
          }
          case _ => {
            asmCodes += BranchLink(Label("_arrLoad"))
            codeState.preFuncs += ArrayLoad
          }
        }
        asmCodes += Mov(R8, R3)
        asmCodes += Pop(List(R3))
      }
      // now start UnOp Evaluation
      case Not(expr: Expr) => {
        asmCodes ++= genExpr(expr)
        asmCodes += Comparea(R8, ImmVal(1))
        asmCodes += Mov(R8, ImmVal(1), asmNE)
        asmCodes += Mov(R8, ImmVal(0), asmEQ)
      }
      case Neg(expr: Expr) => {
        asmCodes ++= genExpr(expr)
        asmCodes += RSub(R8, R8, ImmVal(0), true)
        asmCodes += BranchLink(Label("_errOverflow"), asmVS)
        codeState.preFuncs += ErrOverflow
      }
      case Len(expr: Expr) => {
        asmCodes ++= genExpr(expr)
        asmCodes += Ldra(R8, Address(R8, ImmVal(-4)))
      }
      case Ord(expr: Expr) => {
        asmCodes ++= genExpr(expr)
      }
      case Chr(expr: Expr) => {
        asmCodes ++= genExpr(expr)
        asmCodes += BitAnd(R8, R8, ImmVal(127))
      }
      // now start BiOp Evaluation
      case Mul(lhs: Expr, rhs: Expr) => {
        asmCodes ++= genExpr(lhs)
        asmCodes += Push(List(R8))
        asmCodes ++= genExpr(rhs)
        asmCodes += Pop(List(R9))
        asmCodes += SMull(R8, R9, R8, R9)
        asmCodes += Comparea(R9, R8, Asr(ImmVal(31)))
        asmCodes += BranchLink(Label("_errOverflow"), asmNE)
        codeState.preFuncs += ErrOverflow
      }
      case Div(lhs: Expr, rhs: Expr) => {
        asmCodes += Push(List(R0, R1))
        asmCodes ++= genExpr(lhs)
        asmCodes += Mov(R0, R8)
        asmCodes ++= genExpr(rhs)
        asmCodes += Mov(R1, R8)
        asmCodes += Comparea(R1, ImmVal(0))
        asmCodes += BranchLink(Label("_errDivZero"), asmEQ)
        codeState.preFuncs += ErrDivZero
        asmCodes += BranchLink(Label("__aeabi_idivmod"))
        asmCodes += Mov(R8, R0)
        asmCodes += Pop(List(R0, R1))
      }
      case Mod(lhs: Expr, rhs: Expr) => {
        asmCodes += Push(List(R0, R1))
        asmCodes ++= genExpr(lhs)
        asmCodes += Mov(R0, R8)
        asmCodes ++= genExpr(rhs)
        asmCodes += Mov(R1, R8)
        asmCodes += Comparea(R1, ImmVal(0))
        asmCodes += BranchLink(Label("_errDivZero"), asmEQ)
        codeState.preFuncs += ErrDivZero
        asmCodes += BranchLink(Label("__aeabi_idivmod"))
        asmCodes += Mov(R8, R1)
        asmCodes += Pop(List(R0, R1))
      }
      case Add(lhs: Expr, rhs: Expr) => {
        asmCodes ++= genExpr(lhs)
        asmCodes += Push(List(R8))
        asmCodes ++= genExpr(rhs)
        asmCodes += Pop(List(R9))
        asmCodes += Adda(R8, R8, R9, true)
        asmCodes += BranchLink(Label("_errOverflow"), asmVS)
        codeState.preFuncs += ErrOverflow
      }
      case Sub(lhs: Expr, rhs: Expr) => {
        asmCodes ++= genExpr(lhs)
        asmCodes += Push(List(R8))
        asmCodes ++= genExpr(rhs)
        asmCodes += Pop(List(R9))
        asmCodes += Suba(R8, R9, R8, true)
        asmCodes += BranchLink(Label("_errOverflow"), asmVS)
        codeState.preFuncs += ErrOverflow
      }
      case GT(lhs: Expr, rhs: Expr) =>{
        asmCodes ++= genExpr(lhs)
        asmCodes += Push(List(R8))
        asmCodes ++= genExpr(rhs)
        asmCodes += Pop(List(R9))
        asmCodes += Comparea(R9, R8)
        asmCodes += Mov(R8, ImmVal(1), asmGT)
        asmCodes += Mov(R8, ImmVal(0), asmLE)
      }
      case GE(lhs: Expr, rhs: Expr) => {
        asmCodes ++= genExpr(lhs)
        asmCodes += Push(List(R8))
        asmCodes ++= genExpr(rhs)
        asmCodes += Pop(List(R9))
        asmCodes += Comparea(R9, R8)
        asmCodes += Mov(R8, ImmVal(1), asmGE)
        asmCodes += Mov(R8, ImmVal(0), asmLT)
      }
      case LT(lhs: Expr, rhs: Expr) => {
        asmCodes ++= genExpr(lhs)
        asmCodes += Push(List(R8))
        asmCodes ++= genExpr(rhs)
        asmCodes += Pop(List(R9))
        asmCodes += Comparea(R9, R8)
        asmCodes += Mov(R8, ImmVal(1), asmLT)
        asmCodes += Mov(R8, ImmVal(0), asmGE)
      }
      case LE(lhs: Expr, rhs: Expr) => {
        asmCodes ++= genExpr(lhs)
        asmCodes += Push(List(R8))
        asmCodes ++= genExpr(rhs)
        asmCodes += Pop(List(R9))
        asmCodes += Comparea(R9, R8)
        asmCodes += Mov(R8, ImmVal(1), asmLE)
        asmCodes += Mov(R8, ImmVal(0), asmGT)
      }
      case EQ(lhs: Expr, rhs: Expr) => {
        asmCodes ++= genExpr(lhs)
        asmCodes += Push(List(R8))
        asmCodes ++= genExpr(rhs)
        asmCodes += Pop(List(R9))
        asmCodes += Comparea(R9, R8)
        asmCodes += Mov(R8, ImmVal(1), asmEQ)
        asmCodes += Mov(R8, ImmVal(0), asmNE)
      }
      case NEQ(lhs: Expr, rhs: Expr) => {
        asmCodes ++= genExpr(lhs)
        asmCodes += Push(List(R8))
        asmCodes ++= genExpr(rhs)
        asmCodes += Pop(List(R9))
        asmCodes += Comparea(R9, R8)
        asmCodes += Mov(R8, ImmVal(1), asmNE)
        asmCodes += Mov(R8, ImmVal(0), asmEQ)
      }
      case And(lhs: Expr, rhs: Expr) => {
        val label = Label(s".L${codeState.labelCounter}")
        codeState.labelCounter += 1
        asmCodes ++= genExpr(lhs)
        asmCodes += Comparea(R8, ImmVal(1))
        asmCodes += Branch(label, asmNE)
        asmCodes ++= genExpr(rhs)
        asmCodes += Comparea(R8, ImmVal(1))
        asmCodes += label
        asmCodes += Mov(R8, ImmVal(1), asmEQ)
        asmCodes += Mov(R8, ImmVal(0), asmNE)
      }
      case Or(lhs: Expr, rhs: Expr) => {
        val label = Label(s".L${codeState.labelCounter}")
        codeState.labelCounter += 1
        asmCodes ++= genExpr(lhs)
        asmCodes += Comparea(R8, ImmVal(1))
        asmCodes += Branch(label, asmEQ)
        asmCodes ++= genExpr(rhs)
        asmCodes += Comparea(R8, ImmVal(1))
        asmCodes += label
        asmCodes += Mov(R8, ImmVal(1), asmEQ)
        asmCodes += Mov(R8, ImmVal(0), asmNE)
      }
      case Null() => {
        asmCodes += Mov(R8, ImmVal(0))
      }
      case Paren(inside) => {
        asmCodes ++= genExpr(inside)
      }
    }
    asmCodes.toList    
  }

  def genLValue(
    lhs: lValue
    ) (implicit 
        codeState: CodeState,
        funcName: String
      ):List[AssemblyCodeLine] = {
    val asmCodes: mutable.ListBuffer[AssemblyCodeLine] = mutable.ListBuffer.empty[AssemblyCodeLine]
    implicit val varTable = codeState.varTypeTable
    // Generate ASM code to store the value in R8 to corresponding position indicated by lValue
    lhs match {
      case Fst(value) => {
        asmCodes += Push(List(R8))
        value match {
          case inside: IdentWithArray => asmCodes ++= genExpr(inside)
          case inside: PairElem => asmCodes ++= genRValue(inside)
        }
        asmCodes += Comparea(R8, ImmVal(0))
        asmCodes += BranchLink(Label("_errNull"), asmEQ)
        codeState.preFuncs += ErrNullPtr
        asmCodes += Ldra(R8, Address(R8, ImmVal(0)))
        asmCodes += Pop(List(R9))
        val pairElemType = getType(value) match {
          case PairType(inType, _) => inType
          case _ => AnyType
        }
        typeSize(pairElemType) match {
          case 1 => asmCodes += Stra(R9, Address(R8, ImmVal(0)), 1)
          case _ => asmCodes += Stra(R9, Address(R8, ImmVal(0)))
        }
      }
      case Snd(value) => {
        asmCodes += Push(List(R8))
        value match {
          case inside: IdentWithArray => asmCodes ++= genExpr(inside)
          case inside: PairElem => asmCodes ++= genRValue(inside)
        }
        asmCodes += Comparea(R8, ImmVal(0))
        asmCodes += BranchLink(Label("_errNull"), asmEQ)
        codeState.preFuncs += ErrNullPtr
        asmCodes += Ldra(R8, Address(R8, ImmVal(WORD_SIZE)))
        asmCodes += Pop(List(R9))
        val pairElemType = getType(value) match {
          case PairType(_, inType) => inType
          case _ => AnyType
        }
        typeSize(pairElemType) match {
          case 1 => asmCodes += Stra(R9, Address(R8, ImmVal(0)), 1)
          case _ => asmCodes += Stra(R9, Address(R8, ImmVal(0)))
        }
      }
      case Ident(name) => {
        asmCodes += toIdent(name, R8)
      }
      case ArrayElem(id, index) => {
        asmCodes += Push(List(R3))
        asmCodes += Push(List(R8))
        asmCodes ++= genExpr(id)
        asmCodes += Push(List(R8))
        asmCodes ++= genExpr(index)
        asmCodes += Pop(List(R3))
        asmCodes += Mov(R10, R8)
        asmCodes += Pop(List(R8))
        typeSize(getType(lhs)) match {
          case 1 => {
            asmCodes += BranchLink(Label("_arrStoreB"))
            codeState.preFuncs += ArrayStoreB
          }
          case _ => {
            asmCodes += BranchLink(Label("_arrStore"))
            codeState.preFuncs += ArrayStore
          }
        }
        asmCodes += Pop(List(R3))
      }
    }
    asmCodes.toList
  }
}

