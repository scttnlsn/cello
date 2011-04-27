package com.scttnlsn.cello

import com.scttnlsn.cello.Binary._
import java.nio.ByteBuffer

abstract sealed class Swappable[A, B] {

  def dump(): Long

  def load(): Node[A, B]

}

case class Paged[A, B](val page: Long)(implicit val pager: Pager, val ordering: Ordering[A], keyFormat: BinaryFormat[A], valueFormat: BinaryFormat[B]) extends Swappable[A, B] {

  def dump(): Long = {
    page
  }

  def load(): Node[A, B] = {
    val buffer = pager.read(page)
    Binary.load[Byte](buffer) match {
      case Swappable.BYTE_LEAF => LeafNode(buffer)
      case Swappable.BYTE_INNER => InnerNode(buffer)
    }
  }

}

case class Volatile[A, B](val node: Node[A, B])(implicit val pager: Pager, val ordering: Ordering[A], keyFormat: BinaryFormat[A], valueFormat: BinaryFormat[B]) extends Swappable[A, B] {

  def dump(): Long = {
    val buffer = ByteBuffer.allocate(Pager.PAGESIZE)
    node.pack(buffer)
    pager.append(buffer)
  }

  def load(): Node[A, B] = {
    node
  }

}

object Swappable {

  val BYTE_LEAF = 0.toByte
  val BYTE_INNER = 1.toByte

}
