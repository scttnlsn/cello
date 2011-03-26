package com.scttnlsn.cello

case class Compactor()(implicit val pager: Pager) {

  val compacted = Pager(pager.path + ".compact")

  def compact(root: Swappable): Long = {
    Writer().lock.synchronized {
      val page = copy(root.load())
      val footer = Footer(1, page)(compacted).save()
      pager.replace(compacted.path)
      footer
    }
  }

  private def copy(node: Node): Long = {
    node match {
      case (x: LeafNode) => {
        (~LeafNode(x.map)(compacted)).dump()
      }
      case (x: InnerNode) => {
        val map = x.map.map(y => (y._1, Paged(copy(y._2.load()))))
        val last = Paged(copy(x.last.load()))
        (~InnerNode(map, last)(compacted)).dump()
      }
    }
  }

}
