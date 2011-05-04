package com.scttnlsn.cello.btree

import com.scttnlsn.cello.Binary
import com.scttnlsn.cello.Binary._
import com.scttnlsn.cello.Pager
import java.nio.ByteBuffer

abstract sealed class Swappable[A, B] {

  def dump(): Long

  def load(): Node[A, B]

}

case class Paged[A, B](val page: Long)(implicit val meta: Meta[A, B]) extends Swappable[A, B] {

  def dump(): Long = {
    page
  }

  def load(): Node[A, B] = {
    val buffer = meta.pager.read(page)
    Binary.load[Byte](buffer) match {
      case Swappable.BYTE_LEAF => LeafNode(buffer)
      case Swappable.BYTE_INNER => InnerNode(buffer)
    }
  }

}

case class Volatile[A, B](val node: Node[A, B])(implicit val meta: Meta[A, B]) extends Swappable[A, B] {

  def dump(): Long = {
    val buffer = ByteBuffer.allocate(Pager.PAGESIZE)
    node.pack(buffer)
    meta.pager.append(buffer)
  }

  def load(): Node[A, B] = {
    node
  }

}

object Swappable {

  val BYTE_LEAF = 0.toByte
  val BYTE_INNER = 1.toByte

}
