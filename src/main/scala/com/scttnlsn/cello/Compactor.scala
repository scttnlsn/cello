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
        val pages = x.children.map(c => Paged(copy(c.load())))
        (~InnerNode(x.keys, pages)(compacted)).dump()
      }
    }
  }

}
