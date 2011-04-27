package com.scttnlsn.cello

import com.scttnlsn.cello.Binary._

class Snapshot(val version: Long, val root: Swappable[String, String])

object Snapshot {
  
  implicit val format = StringFormat

  def apply()(implicit pager: Pager): Snapshot = {
    pager.reinitialize()
    Footer.search(pager.pages() - 1) match {
      case None => new Snapshot(1, ~LeafNode[String, String]())
      case Some(x) => new Snapshot(x.version, Paged[String, String](x.rootPage))
    }
  }

}
