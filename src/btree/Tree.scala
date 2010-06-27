package btree

case class Tree(val path: String) {

  implicit val pager = Pager(path)

  private var snapshot = Snapshot()
  private var root = Root(snapshot.root)

  def delete(key: String): Unit = {
    root.delete(key)
  }

  def get(key: String): Option[String] = {
    root.get(key)
  }

  def save(): Long = {
    Writer().save(root)
  }

  def set(key: String, value: String): Unit = {
    root.set(key, value)
  }

  def sync(): Unit = {
    snapshot = Snapshot()
    root = Root(snapshot.root)
  }

}
