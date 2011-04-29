package com.scttnlsn.cello

import com.scttnlsn.cello.Binary._
import java.nio.ByteBuffer

class Footer(val version: Long, val page: Long, val pager: Pager) {

  def save(): Long = {
    val buffer = ByteBuffer.allocate(Pager.PAGESIZE)
    dump[Long](buffer, List(version, page, version, page))
    pager.append(buffer)
  }

}

object Footer {

  def apply(version: Long, page: Long, pager: Pager): Footer = {
    new Footer(version, page, pager)
  }

  def get(page: Long, pager: Pager): Option[Footer] = {
    val buffer = pager.read(page)
    val v1::p1::v2::p2::xs = load[Long](buffer, 4)
    if (v1 == v2 && p1 == p2) {
      Some(Footer(v1, p1, pager))
    } else {
      None
    }
  }

  def search(page: Long, pager: Pager): Option[Footer] = {
    page match {
      case -1 => None
      case x => get(x, pager) match {
        case None => search(page - 1, pager)
        case y => y
      }
    }
  }

}
