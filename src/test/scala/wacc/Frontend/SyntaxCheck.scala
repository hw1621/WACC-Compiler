package wacc.Frontend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import java.io.File
import Frontend.Parser._
import Frontend.Semantic._

class SyntaxCheck extends AnyFlatSpec {
	
	def getListOfFilesRecursively(dir: String): Array[String] = {
		val d = new File(dir)
		if (d.exists && d.isDirectory) {
     		val result : List[String] = d.listFiles.filter(_.isDirectory)
					.toList.flatMap(x => getListOfFilesRecursively(x.toString()))
			d.listFiles.filter(_.isFile).map(_.toString()).concat(result)
		} else {
			Array[String]()
		}
	}

 
	def allSuccess(srcPath: String) = {
		val allValidProgramPaths = getListOfFilesRecursively(srcPath)
		for (path <- allValidProgramPaths) {
      		behavior of "Valid programs " + path
      		it should "pass syntax check" in { parse(new File(path)).get should matchPattern {case parsley.Success(_) => }}
          it should "pass semantic check" in {
            parse(new File(path)).get match {
              case parsley.Success(program) => assert(semanticCheck(program, path)._1.isEmpty)
              case parsley.Failure(_) => info("Semantic check not process due to syntax error")
            }
          }
		}
	}

	def allSynFail(srcPath: String) = {
		val allInValidProgramPaths = getListOfFilesRecursively(srcPath)
		for (path <- allInValidProgramPaths) {
      		behavior of "Syntactically Invalid programs " + path
      		it should "failed syntax check" in {parse(new File(path)).get should matchPattern {case parsley.Failure(_) => }}
		}
	}

  val examplePath = "wacc_examples"
  info("Testing all valid program")
  allSuccess(new File(examplePath, "valid").getPath)
  info("Testing all syntactically invalid program")
  allSynFail(new File(examplePath, "invalid/syntaxErr").getPath)

}
