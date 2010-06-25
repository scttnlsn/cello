package btree

import scala.collection.mutable.HashMap

class Writer()(implicit val pager: Pager) {

  val lock: AnyRef = new Object()

  def delete(key: String): Long = {
    perform {
      root => root.delete(key)
    }
  }

  def delete(keys: Seq[String]): Long = {
    perform {
      root => keys.foreach(key => root.delete(key))
    }
  }

  def set(key: String, value: String): Long = {
    perform {
      root => root.set(key, value)
    }
  }

  def set(kvs: Seq[(String, String)]): Long = {
    perform {
      root => kvs.foreach(kv => root.set(kv._1, kv._2))
    }
  }

  private def perform(action: Root => Unit): Long = {
    lock.synchronized {
      val snapshot = Snapshot()
      val root = Root(snapshot.root)
      action(root)
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
