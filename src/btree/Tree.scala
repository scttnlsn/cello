package btree

case class Tree(val path: String) {

  implicit val pager = Pager(path)

  def delete(key: String): Long = {
    Writer().delete(key)
  }

  def delete(keys: Seq[String]): Long = {
    Writer().delete(keys)
  }

  def set(key: String, value: String): Long = {
    Writer().set(key, value)
  }

  def set(kvs: Seq[(String, String)]): Long = {
    Writer().set(kvs)
  }

  def snapshot(): Snapshot = {
    Snapshot()
  }

}
