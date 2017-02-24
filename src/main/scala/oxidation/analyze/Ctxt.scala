package oxidation.analyze

/**
  * Created by Kamil on 24.02.2017.
  */
class Ctxt(val terms: Map[String, Type], val types: Map[String, Type]) {

  def withTerms(ts: Map[String, Type]): Ctxt = new Ctxt(terms ++ ts, types)

  def withTypes(ts: Map[String, Type]): Ctxt = new Ctxt(terms, types ++ ts)

}

object Ctxt {

  val empty = new Ctxt(Map.empty, Map.empty)

  def terms(ts: (String, Type)*): Ctxt = empty.withTerms(Map(ts: _*))
  def types(ts: (String, Type)*): Ctxt = empty.withTypes(Map(ts: _*))

}