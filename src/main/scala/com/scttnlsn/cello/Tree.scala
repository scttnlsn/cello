package com.scttnlsn.cello

case class Tree(val path: String) {

  implicit val pager = Pager(path)

  private var root = Snapshot().root

  def get(key: String): Option[String] = {
    root.load().get(key)
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
  
  def delete(key: String): Unit = {
    val node = root.load()
    node.delete(key)
    root = ~node
  }

  def save(): Long = {
    Writer() !? Save(root) match {
      case page: Long => page
    }
  }

  def sync(): Unit = {
    root = Snapshot().root
  }
  
  def compact(): Long = {
    Writer() !? Compact(root) match {
      case page: Long => page
    }
  }

}
