package oxidation

import java.io.File

import oxidation.tool.Compile
import utest._

import scala.sys.process.{Process, ProcessLogger}

object CompiledTestSuite extends TestSuite {

  def exec(noConstProp: Boolean): Unit = {
    val files = new File("compiled-test").listFiles(_.getName.endsWith(".ox"))
    assert(files.nonEmpty)
    val outName = "compiledtests" ++ (if(noConstProp) "-ncp" else "") ++ ".exe"
    val buildDir = new File(new File("compiled-test"), "build")
    buildDir.mkdirs()
    val outFile = new File(buildDir, outName)
    val Right((exeFile, stats)) = Compile.compile(Compile.Options(
      infiles = files,
      output = Some(outFile),
      validate = true,
      withoutConstPropagation = noConstProp
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

  val tests = apply {
    "full" - {
      exec(noConstProp = false)
    }
    "without constant propagation" - {
      exec(noConstProp = true)
    }
  }

}
