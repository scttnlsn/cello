package btree

case class Root(
  private var root: Swappable)(
  implicit val pager: Pager) {

  def delete(key: String): Unit = {
    val node = root.load()
    node.delete(key)
    root = ~node
  }

  def get(key: String): Option[String] = {
    root.load().get(key)
  }

  def save(): Long = {
    root.dump()
  }

  def set(key: String, value: String): Unit = {
    val node = root.load()
    node.set(key, value)
    if (node.full) {
      val (pivot, left, right) = node.split
      root = ~InnerNode(pivot, left, right)
    } else {
      root = ~node
    }
  }

}
