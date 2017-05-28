package oxidation

import java.io.File

import oxidation.tool.Compile
import utest._

import scala.sys.process.{Process, ProcessLogger}

object CompiledTestSuite extends TestSuite {

  val tests = apply {
    val files = new File("compiled-test").listFiles(_.getName.endsWith(".ox"))
    assert(files.nonEmpty)
    val Right((exeFile, stats)) = Compile.compile(Compile.Options(
      infiles = files,
      validate = true
    ))
    val builder = new StringBuilder
    val log = new ProcessLogger {
      override def buffer[T](f: => T) = f

      override def out(s: => String) =
        builder ++= s += '\n'

      override def err(s: => String) =
        builder ++= s += '\n'
    }
    val res = Process(exeFile.getAbsolutePath) ! log
    if(res != 0) throw AssertionError(s"Compiled test failed: ${builder.toString} with error code $res", Seq.empty)
    println(s" ---------- spills = ${stats.spillCount}")
  }

}
