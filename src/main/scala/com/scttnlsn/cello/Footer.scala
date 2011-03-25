package com.scttnlsn.cello

import java.nio.ByteBuffer

class Footer(
  val version: Long,
  val rootPage: Long)(
  implicit val pager: Pager) {

  def save(): Long = {
    val buffer = ByteBuffer.allocate(Pager.PAGESIZE)
    buffer.putLong(version)
    buffer.putLong(rootPage)
    buffer.putLong(version)
    buffer.putLong(rootPage)
    pager.append(buffer)
  }

}

object Footer {

  def apply(version: Long, rootPage: Long)(implicit pager: Pager): Footer = {
    new Footer(version, rootPage)
  }

  def get(page: Long)(implicit pager: Pager): Option[Footer] = {
    val buffer = pager.read(page)
    val version = buffer.getLong()
    val rootPage = buffer.getLong()
    if (version == buffer.getLong() && rootPage == buffer.getLong()) {
      Some(Footer(version, rootPage))
    } else {
      None
    }
  }

  def search(page: Long)(implicit pager: Pager): Option[Footer] = {
    page match {
      case -1 => None
      case x => get(x) match {
        case None => search(page - 1)
        case y => y
      }
    }
  }

}
