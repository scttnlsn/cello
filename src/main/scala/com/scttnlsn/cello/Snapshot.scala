package com.scttnlsn.cello

import com.scttnlsn.cello.Binary._

class Snapshot[A, B](val version: Long, val root: Swappable[A, B])

object Snapshot {

  def apply[A, B]()(implicit meta: Meta[A, B]): Snapshot[A, B] = {
    meta.pager.reinitialize()
    Footer.search(meta.pager.pages() - 1, meta.pager) match {
      case None => new Snapshot(1, ~LeafNode[A, B]())
      case Some(x) => new Snapshot(x.version, Paged[A, B](x.page))
    }
  }

}
