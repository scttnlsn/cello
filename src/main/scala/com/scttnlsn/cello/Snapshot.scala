package com.scttnlsn.cello

import com.scttnlsn.cello.Binary._
import com.scttnlsn.cello.btree.Meta
import com.scttnlsn.cello.btree.Tree

class Snapshot(val version: Long, val page: Option[Long], val p: Pager) {
  
  implicit val meta = new Meta[String, String] {
    implicit val pager = p
    implicit val ordering = implicitly[Ordering[String]]
    implicit val keyFormat = implicitly[BinaryFormat[String]]
    implicit val valueFormat = implicitly[BinaryFormat[String]]
  }
  
  def get(key: String): Option[String] = {
    tree.get(key)
  }
  
  def tree(): Tree[String, String] = {
    Tree[String, String](page)
  }
  
}

object Snapshot {

  def apply(pager: Pager): Snapshot = {
    pager.reinitialize()
    Footer.search(pager.pages() - 1, pager) match {
      case None => new Snapshot(0, None, pager)
      case Some(x) => new Snapshot(x.version, Some(x.page), pager)
    }
  }

}
