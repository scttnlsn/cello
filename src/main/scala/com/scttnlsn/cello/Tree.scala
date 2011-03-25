package com.scttnlsn.cello

case class Tree(val path: String) {

  implicit val pager = Pager(path)

  private var root = Snapshot().root

  def compact(): Long = {
    val page = Compactor().compact(root)
    sync()
    page
  }

  def delete(key: String): Unit = {
    val node = root.load()
    node.delete(key)
    root = ~node
  }

  def get(key: String): Option[String] = {
    root.load().get(key)
  }

  def save(): Long = {
    Writer().save(root)
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

  def sync(): Unit = {
    root = Snapshot().root
  }

}
