

import Frontend.Parser._
import Frontend.Semantic.semanticCheck
import Backend.CodeGenerater._
import java.io.File
import scala.util.Success
import scala.util.Failure
import java.io.BufferedWriter
import java.io.FileWriter
import Backend.AbstractInstructions._

object Main {
  final val semanticError = 200
  final val syntaxError = 100
  final val failure = -1
  def main(args: Array[String]): Unit = {
    if (args.length < 1) {
      System.out.println("Source file unspecified")
      System.exit(failure)
    }
    val source = new File(args.head)
    val result = parse(source)
    result match {
      case Success(value) => {
        value match {
          case parsley.Success(x) => {
            semanticCheck(value.get, args.head) match {
              case (errors, newprog, varTable) => {
                if (errors.isEmpty) {
                  //println(newprog)
                  //println(varTable)
                  val armCode = genCode(newprog, varTable)
                  val fileName = source.getName().dropRight(5)
                  writeFile(fileName, armCode)
                  sys.exit(0)
                } else {
                  println(errors.mkString("\n"))
                  sys.exit(semanticError)
                }
              }
            }
          }
          case parsley.Failure(msg) => {
            println(msg)
            System.exit(syntaxError)
          }
        }
      }
      case Failure(msg) => {
        println(msg) 
        System.exit(failure)
      }
    }

  }

  def writeFile(filename: String, content: List[AssemblyCodeLine]): Unit = {
    val filepath = (s"./$filename.s")
    val file = new File(filepath)
    file.createNewFile()
    val bw = new BufferedWriter(new FileWriter(file))
    val insSeq: Seq[AssemblyCodeLine] = content
    for (intr <- insSeq) {
      val insStr = intr.toCode
      //println(insStr)
      intr match {
        case _: Label => bw.write("\t")
        case _: Header => bw.write("\t")
        case _ => bw.write("\t\t")
      }
      bw.write(insStr)
      bw.write("\n")
    }
    bw.close()
  }
}

