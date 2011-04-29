package com.scttnlsn.cello

class Snapshot(val version: Long, val page: Option[Long])

object Snapshot {

  def apply(pager: Pager): Snapshot = {
    pager.reinitialize()
    Footer.search(pager.pages() - 1, pager) match {
      case None => new Snapshot(1, None)
      case Some(x) => new Snapshot(x.version, Some(x.page))
    }
  }

}
