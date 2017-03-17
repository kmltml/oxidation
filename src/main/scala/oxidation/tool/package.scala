package oxidation

package object tool {

  trait LogOptions {

    def verbose: Boolean

  }

  def phase[A](name: String)(body: => A)(implicit opt: LogOptions): A = {
    val startMillis = System.currentTimeMillis()
    if(opt.verbose) {
      Console.err.println(s"Starting phase: $name")
    }
    val a = body
    if(opt.verbose) {
      Console.err.println(s"Phase finished: $name ${System.currentTimeMillis() - startMillis}ms")
    }
    a
  }

}
