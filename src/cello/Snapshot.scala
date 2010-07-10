package cello

class Snapshot(val version: Long, val root: Swappable) {

  def get(key: String): Option[String] = {
    root.load().get(key)
  }

}

object Snapshot {

  def apply()(implicit pager: Pager): Snapshot = {
    Footer.search(pager.pages() - 1) match {
      case None => new Snapshot(1, ~LeafNode())
      case Some(x) => new Snapshot(x.version, Paged(x.rootPage))
    }
  }

}
