package Backend

object AbstractInstructions {

  def sizePostfix(size: Int): String = size match {
      case -1 => "sb"
      case 1 => "b"
      case 2 => "h"
      case _ => ""
    }

  sealed trait IdentSymbol
  sealed trait AddressReference {
    def toCodeRef: String
  }

  sealed trait Operand {
    def toCode: String
  }
  case class Register(index: Int) extends Operand with IdentSymbol {
    override def toCode: String = "r" + index.toString
  }
  object R0 extends Register(0)
  object R1 extends Register(1)
  object R2 extends Register(2)
  object R3 extends Register(3)
  object R4 extends Register(4)
  object R5 extends Register(5)
  object R6 extends Register(6)
  object R7 extends Register(7)
  object R8 extends Register(8)
  object R9 extends Register(9)
  object R10 extends Register(10)
  object R12 extends Register(12)
  object FP extends Register(11){
    override def toCode: String = "fp"
  }
  object SP extends Register(13){
    override def toCode: String = "sp"
  }
  object LR extends Register(14){
    override def toCode: String = "lr"
  }
  object PC extends Register(15){
    override def toCode: String = "pc"
  }

  sealed trait AddressMode
  case object Offset extends AddressMode
  case object PreIndex extends AddressMode
  case object PostIndex extends AddressMode

  case class Address(reg: Register, index: Operand, stepSize: ImmVal, indexMode: AddressMode = Offset, shifting: Shifting = noShift) extends IdentSymbol with AddressReference{

    override def toCodeRef: String = indexMode match {
      case Offset => if (stepSize.value == 1) {
        s"[${reg.toCode}, ${index.toCode}${shifting.toCode}]"
      } else {
        s"[${reg.toCode}, ${index.toCode}, ${stepSize.toCode}]"
      }
      case PreIndex => if (stepSize.value == 1) {
        s"[${reg.toCode}, ${index.toCode}${shifting.toCode}]!"
      } else {
        s"[${reg.toCode}, ${index.toCode}, ${stepSize.toCode}]!"
      }
      case PostIndex => if (stepSize.value == 1) {
        s"[${reg.toCode}], ${index.toCode}${shifting.toCode}"
      } else {
        s"[${reg.toCode}], ${index.toCode}, ${stepSize.toCode}"
      }
    }  
  }

  object Address {
    def apply(reg: Register, index: Operand): Address = new Address(reg, index, ImmVal(1), Offset)
    def apply(reg: Register, index: Operand, indexMode: AddressMode): Address = new Address(reg, index, new ImmVal(1), indexMode)
    def apply(reg: Register, index: Operand, shifting: Shifting): Address = new Address(reg, index, new ImmVal(1), Offset, shifting)
  }

  case class ImmVal(value: Int) extends Operand {
    override def toCode: String = "#" + value.toString
  }

  sealed trait AssemblyCodeLine {
    def toCode: String
  }

  case class Header(content: String) extends AssemblyCodeLine {
    override def toCode: String = content
  }

  case class Label(value: String) extends AssemblyCodeLine with AddressReference {
    override def toCode: String = value + ":"
    override def toCodeRef: String = "=" + value
    def toCodeB: String = value
  }

  case class WordSpace(value: Int) extends AssemblyCodeLine {
    override def toCode: String = s".word $value"
  }

  case class Asciz(value: String) extends AssemblyCodeLine {
    override def toCode: String = s".asciz \"$value\""
  }

  case class Push(regs: List[Register]) extends Instruction {
    override def toCode: String = s"push {${regs.map(_.toCode).mkString(", ")}}"
  }

  case class Pop(regs: List[Register]) extends Instruction {
    override def toCode: String = s"pop {${regs.map(_.toCode).mkString(", ")}}"
  }

  sealed trait Instruction extends AssemblyCodeLine

  case class Adda(rd: Register, rn: Operand, rm: Operand, flag: Boolean = false) extends Instruction {
    val postfix = if (flag) "s" else ""
    override def toCode: String = s"add$postfix ${rd.toCode}, ${rn.toCode}, ${rm.toCode}"
  }

  case class Suba(rd: Register, rn: Operand, rm: Operand, flag: Boolean = false) extends Instruction {
    val postfix = if (flag) "s" else ""
    override def toCode: String = s"sub$postfix ${rd.toCode}, ${rn.toCode}, ${rm.toCode}"
  }

  case class RSub(rd: Register, rn: Operand, rm: Operand, flag: Boolean = false) extends Instruction {
    val postfix = if (flag) "s" else ""
    override def toCode: String = s"rsb$postfix ${rd.toCode}, ${rn.toCode}, ${rm.toCode}"
  }

  case class SMull(rdLow: Operand, rdHigh: Operand, rn: Operand, rm: Operand) extends Instruction {
    override def toCode: String = s"smull ${rdLow.toCode}, ${rdHigh.toCode}, ${rn.toCode}, ${rm.toCode}"
  }

  case class Mov(rd: Register, src: Operand, cond: Condition = noCond) extends Instruction {
    override def toCode: String = s"mov${cond.toCode} ${rd.toCode}, ${src.toCode}"
  }

  case class Ldra(reg: Register, addr: AddressReference, size: Int = 4) extends Instruction  {
    val postfix = sizePostfix(size)
    override def toCode: String = s"ldr$postfix ${reg.toCode}, ${addr.toCodeRef}"
  }

  case class Stra(src: Register, addr: AddressReference, size: Int = 4) extends Instruction  {
    val postfix = sizePostfix(size)
    override def toCode: String = s"str$postfix ${src.toCode}, ${addr.toCodeRef}"
  }
  case class BitAnd(rd: Register, rn: Operand, rm: Operand) extends Instruction {
    override def toCode: String = s"and ${rd.toCode}, ${rn.toCode}, ${rm.toCode}"
  }

  case class BranchLink(label: Label, cond: Condition = noCond) extends Instruction {
    override def toCode: String = s"bl${cond.toCode} ${label.toCodeB}"
  }

  case class Branch(label: Label, cond: Condition) extends Instruction {
    override def toCode: String = s"b${cond.toCode} ${label.toCodeB}"
  }

  case class Comparea(oper1: Operand, oper2: Operand, shifting: Shifting = noShift) extends Instruction {
    override def toCode: String = s"cmp ${oper1.toCode}, ${oper2.toCode}${shifting.toCode}"
  }

  sealed trait Condition {
    def toCode: String
  }

  case object noCond extends Condition {
    override def toCode: String = ""
  }

  case object asmVS extends Condition {
    override def toCode: String = "vs"
  }

  case object asmNE extends Condition {
    override def toCode: String = "ne"
  }

  case object asmEQ extends Condition {
    override def toCode: String = "eq"
  }

  case object asmLE extends Condition {
    override def toCode: String = "le"
  }

  case object asmGE extends Condition {
    override def toCode: String = "ge"
  }

  case object asmGT extends Condition {
    override def toCode: String = "gt"
  }  

  case object asmLT extends Condition {
    override def toCode: String = "lt"
  }

  sealed trait Shifting {
    def toCode: String
  }

  case object noShift extends Shifting {
    override def toCode: String = ""
  }

  case class Asr(digits: Operand) extends Shifting {
    override def toCode: String = s", asr ${digits.toCode}"
  }
  case class Lsl(digits: Operand) extends Shifting {
    override def toCode: String = s", lsl ${digits.toCode}"
  }
}
