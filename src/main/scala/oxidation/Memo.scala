package oxidation

import scala.collection.mutable

class Memo[I, O](private val f: I => O) {

  private val store: mutable.WeakHashMap[I, O] = mutable.WeakHashMap.empty

  def apply(i: I): O = {
    store.getOrElseUpdate(i, f(i))
  }

}

object Memo {

  def apply[I, O](f: I => O): Memo[I, O] = new Memo(f)

}