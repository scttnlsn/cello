package com.scttnlsn.cello.btree

case class Tree[A, B](val page: Option[Long])(implicit val meta: Meta[A, B]) {

  private var root: Swappable[A, B] = page match {
    case Some(x) => Paged[A, B](x)
    case None => ~LeafNode[A, B]()
  }

  def get(key: A): Option[B] = {
    root.load().get(key)
  }
  
  def set(key: A, value: B): Unit = {
    val node = root.load()
    node.set(key, value)
    if (node.full) {
      val (pivot, left, right) = node.split
      root = ~InnerNode(pivot, left, right)
    } else {
      root = ~node
    }
  }
  
  def delete(key: A): Unit = {
    val node = root.load()
    node.delete(key)
    root = ~node
  }
  
  def save(): Long = {
    root.dump
  }

}
