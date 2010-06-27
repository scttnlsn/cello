package btree

import scala.collection.mutable.HashMap

class Writer()(implicit val pager: Pager) {

  val lock: AnyRef = new Object()

  def save(root: Root): Long = {
    lock.synchronized {
      val snapshot = Snapshot()
      Footer(snapshot.version + 1, root.save()).save()
    }
  }

}

object Writer {

  private val lock: AnyRef = new Object()
  private val writers = HashMap[String, Writer]()

  def apply()(implicit pager: Pager): Writer = {
    lock.synchronized {
      if (!writers.contains(pager.path)) {
        writers.update(pager.path, new Writer())
      }
      writers.get(pager.path).get
    }
  }

}
