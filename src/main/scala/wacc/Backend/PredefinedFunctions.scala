package Backend

import Backend.AbstractInstructions._
import Backend.CodeGenerater._
import scala.collection.mutable

object PredefinedFunctions {
  val BYTE_SIZE = 1
  val WORD_SIZE = 4
  val SIGNED_BYTE = -1

  // Print Operations
  def PrintString: List[AssemblyCodeLine] = {
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".data")
    asmCodes += WordSpace(WORD_SIZE)
    asmCodes += Label(".L._prints_str0")
    asmCodes += Asciz("%.*s")   
    asmCodes += Header(".text")
    asmCodes += Label("_prints")
    asmCodes += Push(List(R1, R2, R3, LR))
    asmCodes += Mov(R2, R0)
    asmCodes += Ldra(R1, Address(R0, ImmVal(-WORD_SIZE)))
    asmCodes += Ldra(R0, Label(".L._prints_str0"))
    asmCodes += BranchLink(Label("printf"))
    asmCodes += Mov(R0, ImmVal(0))
    asmCodes += BranchLink(Label("fflush"))
    asmCodes += Pop(List(R1, R2, R3, PC))
    asmCodes.toList
  }
  def PrintBool: List[AssemblyCodeLine] = {
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".data")
    asmCodes += WordSpace(5)
    asmCodes += Label(".L._printb_str0")
    asmCodes += Asciz("false") 
    asmCodes += WordSpace(WORD_SIZE)
    asmCodes += Label(".L._printb_str1")
    asmCodes += Asciz("true")
    asmCodes += WordSpace(WORD_SIZE)
    asmCodes += Label(".L._printb_str2")
    asmCodes += Asciz("%.*s")  
    asmCodes += Header(".text")

    asmCodes += Label("_printb")
    asmCodes += Push(List(R1, R2, R3, LR))
    asmCodes += Comparea(R0, ImmVal(0))
    asmCodes += Branch(Label(".L_printb0"), asmNE)
    asmCodes += Ldra(R2, Label(".L._printb_str0"))
    asmCodes += Branch(Label(".L_printb1"), noCond)

    asmCodes += Label(".L_printb0")
    asmCodes += Ldra(R2, Label(".L._printb_str1"))
    asmCodes += Label(".L_printb1")
    asmCodes += Ldra(R1, Address(R2, ImmVal(-WORD_SIZE)))
    asmCodes += Ldra(R0, Label(".L._printb_str2"))
    asmCodes += BranchLink(Label("printf"))
    asmCodes += Mov(R0, ImmVal(0))
    asmCodes += BranchLink(Label("fflush"))
    asmCodes += Pop(List(R1, R2, R3, PC))   
    asmCodes.toList 
  }
  def PrintInt: List[AssemblyCodeLine] = {
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".data")
    asmCodes += WordSpace(2)
    asmCodes += Label(".L._printi_str0")
    asmCodes += Asciz("%d")   
    asmCodes += Header(".text")

    asmCodes += Label("_printi")
    asmCodes += Push(List(R1, R2, R3, LR))
    asmCodes += Mov(R1, R0)
    asmCodes += Ldra(R0, Label(".L._printi_str0"))
    asmCodes += BranchLink(Label("printf"))
    asmCodes += Mov(R0, ImmVal(0))
    asmCodes += BranchLink(Label("fflush"))
    asmCodes += Pop(List(R1, R2, R3, PC)) 
    asmCodes.toList       
  }
  def PrintChar: List[AssemblyCodeLine] = {
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".data")
    asmCodes += WordSpace(2)
    asmCodes += Label(".L._printc_str0")
    asmCodes += Asciz("%c")   
    asmCodes += Header(".text")

    asmCodes += Label("_printc")
    asmCodes += Push(List(R1, R2, R3, LR))
    asmCodes += Mov(R1, R0)
    asmCodes += Ldra(R0, Label(".L._printc_str0"))
    asmCodes += BranchLink(Label("printf"))
    asmCodes += Mov(R0, ImmVal(0))
    asmCodes += BranchLink(Label("fflush"))
    asmCodes += Pop(List(R1, R2, R3, PC))  
    asmCodes.toList          
  }
  def PrintLine: List[AssemblyCodeLine] = {
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".data")
    asmCodes += WordSpace(0)
    asmCodes += Label(".L._println_str0")
    asmCodes += Asciz("")   
    asmCodes += Header(".text")

    asmCodes += Label("_println")
    asmCodes += Push(List(R1, R2, R3, LR))
    asmCodes += Ldra(R0, Label(".L._println_str0"))
    asmCodes += BranchLink(Label("puts"))
    asmCodes += Mov(R0, ImmVal(0))
    asmCodes += BranchLink(Label("fflush"))
    asmCodes += Pop(List(R1, R2, R3, PC))
    asmCodes.toList          
  }

  def PrintP: List[AssemblyCodeLine] = {
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".data")
    asmCodes += WordSpace(2)
    asmCodes += Label(".L._printp_str0")
    asmCodes += Asciz("%p")   
    asmCodes += Header(".text")

    asmCodes += Label("_printp")
    asmCodes += Push(List(R1, R2, R3, LR))
    asmCodes += Mov(R1, R0)
    asmCodes += Ldra(R0, Label(".L._printp_str0"))
    asmCodes += BranchLink(Label("printf"))
    asmCodes += Mov(R0, ImmVal(0))
    asmCodes += BranchLink(Label("fflush"))
    asmCodes += Pop(List(R1, R2, R3, PC))
    asmCodes.toList      
  }

  // Read Operations
  def ReadInt: List[AssemblyCodeLine] = {
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".data")
    asmCodes += WordSpace(2)
    asmCodes += Label(".L._readi_str0")
    asmCodes += Asciz("%d")   
    asmCodes += Header(".text")

    asmCodes += Label("_readi")
    asmCodes += Push(List(R1, R2, R3, LR))
    asmCodes += Stra(R0, Address(SP, ImmVal(-WORD_SIZE), PreIndex))
    asmCodes += Mov(R1, SP)
    asmCodes += Ldra(R0, Label(".L._readi_str0"))
    asmCodes += BranchLink(Label("scanf"))
    asmCodes += Ldra(R0, Address(SP, ImmVal(0)))
    asmCodes += Adda(SP, SP, ImmVal(WORD_SIZE))
    asmCodes += Pop(List(R1, R2, R3, PC))
    asmCodes.toList
  }
  def ReadChar: List[AssemblyCodeLine] = {
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".data")
    asmCodes += WordSpace(3)
    asmCodes += Label(".L._readc_str0")
    asmCodes += Asciz(" %c")   
    asmCodes += Header(".text")

    asmCodes += Label("_readc")
    asmCodes += Push(List(R1, R2, R3, LR))
    asmCodes += Stra(R0, Address(SP, ImmVal(-BYTE_SIZE), PreIndex), BYTE_SIZE)
    asmCodes += Mov(R1, SP)
    asmCodes += Ldra(R0, Label(".L._readc_str0"))
    asmCodes += BranchLink(Label("scanf"))
    asmCodes += Ldra(R0, Address(SP, ImmVal(0)), SIGNED_BYTE)
    asmCodes += Adda(SP, SP, ImmVal(BYTE_SIZE))
    asmCodes += Pop(List(R1, R2, R3, PC))
    asmCodes.toList
  }

  // Array Operations
  def ArrayStore (implicit codeState: CodeState): List[AssemblyCodeLine] = {
    codeState.preFuncs += BoundsCheck
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".text")
    asmCodes += Label("_arrStore")
    asmCodes += Push(List(R1, LR))
    asmCodes += Comparea(R10, ImmVal(0))
    asmCodes += Mov(R1, R10, asmLT)
    asmCodes += BranchLink(Label("_boundsCheck"), asmLT)
    asmCodes += Ldra(LR, Address(R3, ImmVal(-4)))
    asmCodes += Comparea(R10, LR)
    asmCodes += Mov(R1, R10, asmGE)
    asmCodes += BranchLink(Label("_boundsCheck"), asmGE)
    asmCodes += Stra(R8, Address(R3, R10, Lsl(ImmVal(2))))
    asmCodes += Pop(List(R1, PC))
    asmCodes.toList
  }
  def ArrayLoad (implicit codeState: CodeState): List[AssemblyCodeLine] = {
    codeState.preFuncs += BoundsCheck
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".text")
    asmCodes += Label("_arrLoad")
    asmCodes += Push(List(R1, LR))
    asmCodes += Comparea(R10, ImmVal(0))
    asmCodes += Mov(R1, R10, asmLT)
    asmCodes += BranchLink(Label("_boundsCheck"), asmLT)
    asmCodes += Ldra(LR, Address(R3, ImmVal(-4)))
    asmCodes += Comparea(R10, LR)
    asmCodes += Mov(R1, R10, asmGE)
    asmCodes += BranchLink(Label("_boundsCheck"), asmGE)
    asmCodes += Ldra(R3, Address(R3, R10, Lsl(ImmVal(2))))
    asmCodes += Pop(List(R1, PC))
    asmCodes.toList
  }
  def ArrayStoreB (implicit codeState: CodeState): List[AssemblyCodeLine] = {
    codeState.preFuncs += BoundsCheck
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".text")
    asmCodes += Label("_arrStoreB")
    asmCodes += Push(List(R1, LR))
    asmCodes += Comparea(R10, ImmVal(0))
    asmCodes += Mov(R1, R10, asmLT)
    asmCodes += BranchLink(Label("_boundsCheck"), asmLT)
    asmCodes += Ldra(LR, Address(R3, ImmVal(-4)))
    asmCodes += Comparea(R10, LR)
    asmCodes += Mov(R1, R10, asmGE)
    asmCodes += BranchLink(Label("_boundsCheck"), asmGE)
    asmCodes += Stra(R8, Address(R3, R10), 1)
    asmCodes += Pop(List(R1, PC))
    asmCodes.toList
  }
  def ArrayLoadB (implicit codeState: CodeState): List[AssemblyCodeLine] = {
    codeState.preFuncs += BoundsCheck
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".text")
    asmCodes += Label("_arrLoadB")
    asmCodes += Push(List(R1, LR))
    asmCodes += Comparea(R10, ImmVal(0))
    asmCodes += Mov(R1, R10, asmLT)
    asmCodes += BranchLink(Label("_boundsCheck"), asmLT)
    asmCodes += Ldra(LR, Address(R3, ImmVal(-4)))
    asmCodes += Comparea(R10, LR)
    asmCodes += Mov(R1, R10, asmGE)
    asmCodes += BranchLink(Label("_boundsCheck"), asmGE)
    asmCodes += Ldra(R3, Address(R3, R10), 1)
    asmCodes += Pop(List(R1, PC))
    asmCodes.toList
  }

  // Runtime Exceptions
  def BoundsCheck: List[AssemblyCodeLine] = {
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".data")
    asmCodes += WordSpace(42)
    asmCodes += Label(".L._boundsCheck_str0")
    asmCodes += Asciz(raw"fatal error: array index %d out of bounds\n")   
    asmCodes += Header(".text")

    asmCodes += Label("_boundsCheck")
    asmCodes += Ldra(R0, Label(".L._boundsCheck_str0"))
    asmCodes += BranchLink(Label("printf"))
    asmCodes += Mov(R0, ImmVal(0))
    asmCodes += BranchLink(Label("fflush"))
    asmCodes += Mov(R0, ImmVal(255))
    asmCodes += BranchLink(Label("exit"))
    asmCodes.toList
  }
  def ErrOverflow (implicit codeState: CodeState): List[AssemblyCodeLine] = {
    codeState.preFuncs += PrintString
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".data")
    asmCodes += WordSpace(52)
    asmCodes += Label(".L._errOverflow_str0")
    asmCodes += Asciz(raw"fatal error: integer overflow or underflow occurred\n")   
    asmCodes += Header(".text")

    asmCodes += Label("_errOverflow")
    asmCodes += Ldra(R0, Label(".L._errOverflow_str0"))
    asmCodes += BranchLink(Label("_prints"))
    asmCodes += Mov(R0, ImmVal(255))
    asmCodes += BranchLink(Label("exit"))
    asmCodes.toList
  }
  def ErrDivZero (implicit codeState: CodeState): List[AssemblyCodeLine] = {
    codeState.preFuncs += PrintString
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".data")
    asmCodes += WordSpace(40)
    asmCodes += Label(".L._errDivZero_str0")
    asmCodes += Asciz(raw"fatal error: division or modulo by zero\n")   
    asmCodes += Header(".text")

    asmCodes += Label("_errDivZero")
    asmCodes += Ldra(R0, Label(".L._errDivZero_str0"))
    asmCodes += BranchLink(Label("_prints"))
    asmCodes += Mov(R0, ImmVal(255))
    asmCodes += BranchLink(Label("exit"))
    asmCodes.toList
  }
  def ErrNullPtr (implicit codeState: CodeState): List[AssemblyCodeLine] = {
    codeState.preFuncs += PrintString
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Header(".data")
    asmCodes += WordSpace(45)
    asmCodes += Label(".L._errNull_str0")
    asmCodes += Asciz(raw"fatal error: null pair dereferenced or freed\n")   
    asmCodes += Header(".text")

    asmCodes += Label("_errNull")
    asmCodes += Ldra(R0, Label(".L._errNull_str0"))
    asmCodes += BranchLink(Label("_prints"))
    asmCodes += Mov(R0, ImmVal(255))
    asmCodes += BranchLink(Label("exit"))
    asmCodes.toList
  }

  def FreePair (implicit codeState: CodeState): List[AssemblyCodeLine] = {
    codeState.preFuncs += ErrNullPtr
    val asmCodes = mutable.ListBuffer.empty[AssemblyCodeLine]
    asmCodes += Label("_freepair")
    asmCodes += Push(List(LR))
    asmCodes += Mov(R8, R0)
    asmCodes += Comparea(R8, ImmVal(0))
    asmCodes += BranchLink(Label("_errNull"), asmEQ)
    asmCodes += Ldra(R0, Address(R8, ImmVal(0)))
    asmCodes += BranchLink(Label("free"))
    asmCodes += Ldra(R0, Address(R8, ImmVal(4)))
    asmCodes += BranchLink(Label("free"))
    asmCodes += Mov(R0, R8)
    asmCodes += BranchLink(Label("free"))
    asmCodes += Pop(List(PC))
    asmCodes.toList
  }
 }