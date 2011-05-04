package com.scttnlsn.cello.btree

import com.scttnlsn.cello.Pager
import scala.collection.immutable.TreeMap

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
  
  def copy(pager: Pager): Long = {
    implicit val m = meta.replace(pager)
    implicit val ordering = m.ordering
    def copyNode(node: Node[A, B]): Long = node match {
      case LeafNode(map) => {
        (~LeafNode(map)(m)).dump
      }
      case InnerNode(map, last) => {
        val pairs = map.map(x => (x._1, paged(copyNode(x._2.load), m))).toArray;
        (~InnerNode(TreeMap(pairs:_*), paged(copyNode(last.load), m))(m)).dump
      }
    }
    copyNode(root.load)
  }
  
  private def paged(page: Long, meta: Meta[A, B]): Child[A, B] = {
    Paged[A, B](page)(meta)
  }

}
