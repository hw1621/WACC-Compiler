package wacc.Backend

import org.scalatest.flatspec.AnyFlatSpec
import java.io.File
import scala.io.Source
import scala.sys.process._
import java.io.ByteArrayOutputStream
import org.scalatest.TryValues._
import scala.util.{Try, Success, Failure}
import java.{util => ju}
import parsley.internal.deepembedding.singletons.Empty
import java.io.PrintWriter
import scala.util.control.Breaks._
import scala.collection.mutable.ListBuffer

class AssemblyTest extends AnyFlatSpec {

    implicit var pathMap:Map[String, String] = Map()

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

    def getAssemblyFromRoot(): Array[String] = {
        val dic = new File(".")
        if (dic.exists && dic.isDirectory) {
            var result = Array[String]()
            result = dic.listFiles.filter(x => x.getName.endsWith(".s")).map(_.toString()).concat(result)
            return result
        } else {
            Array[String]()
        }
    }

    def compileAllExampleFiles(srcPath: String) = {
        //make the compiler
        val allExampleProgramPaths = getListOfFilesRecursively(srcPath)
        // for (path <- allExampleProgramPaths) {
        //     println(path)
        // }
        //compile all the example files
        for (path <- allExampleProgramPaths) {
            val filename = new File(path).getName().dropRight(5)
            try {
                s"./compile $path".!
                println(s"Successfully compiled $path")
            } catch {
                case _ : Throwable => //println (s"Failed to compiled $path")
            }
            pathMap += (filename -> path)
        }
        //println(getAssemblyFromRoot().mkString)
    }

    def compileAndCompareAssemblyCode() = {
        val allExampleProgramPaths = getAssemblyFromRoot() 
        for (path <- allExampleProgramPaths) { //path is the exact path of the program example/xxx.s
            val outputPath:String = path.dropRight(2) // eg: example/xxx
            s"arm-linux-gnueabi-gcc -mcpu=arm1176jzf-s -mtune=arm1176jzf-s $path  -o $outputPath".!
            val output = new ByteArrayOutputStream
            val error = new ByteArrayOutputStream
            val errorWritter = new PrintWriter(error)
            val outputWritter = new PrintWriter(output)
            val exitCode = (Seq("qemu-arm", "-L", "/usr/arm-linux-gnueabi/", outputPath)).!(ProcessLogger(outputWritter.println, errorWritter.println))
            errorWritter.close()
            outputWritter.close()
            var finalOutput = output.toString().filter(x => !x.equals('\n') && !x.equals(' '))
            //println(s"Our output is: $rawOutput")

            val currFileName = new File(outputPath).getName() // eg: xxx
            //println(currFileName)
            val originalPath = pathMap(currFileName)
            val lines = Source.fromFile(originalPath).getLines
            var standardOutput = new ListBuffer[String] ()
            for (line <- lines) {
                standardOutput += line
            }
            //println(standardOutput.mkString)
            val stdo1 = standardOutput.dropWhile(x => !x.equals("# Output:"))
            val stdo3 = stdo1.drop(1)
            //println(standardOutput.mkString)
            val stdo4 = stdo3.takeWhile(x => !x.equals(""))
            val stdo2 = stdo4.map(x => x.drop(2))
            println(stdo2.mkString)
            var stdOutput = ""
            for (line <- stdo2.toList) {
                stdOutput += line
            }
            stdOutput = stdOutput.filter(x => !x.equals(' ')) 
            //println(s"standard output is: $stdOutput")

            behavior of "The output of our program " + path 
            it should "equal to the output provided" in {
                assert(finalOutput == stdOutput)
            }
            s"rm -f $outputPath".!
            s"rm -f $path".!
        }
    }

    val examplePath = "wacc_examples/valid/while"
    info("Compile program")
    compileAllExampleFiles(new File(examplePath).getPath)
    info("Testing all compiled program")
    compileAndCompareAssemblyCode()
    
}