package cello

case class Compactor()(implicit val pager: Pager) {

  val compact = Pager(pager.path + ".compact")

  def compact(root: Swappable): Long = {
    Writer().lock.synchronized {
      val page = copy(root.load())
      Footer(1, page)(compact).save()
    }
  }

  private def copy(node: Node): Long = {
    node match {
      case (x: LeafNode) => {
        (~LeafNode(x.keys, x.values)(compact)).dump()
      }
      case (x: InnerNode) => {
        val pages = x.children.map(c => Paged(copy(c.load())))
        (~InnerNode(x.keys, pages)(compact)).dump()
      }
    }
  }

}
