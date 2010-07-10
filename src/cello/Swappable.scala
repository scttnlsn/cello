package cello

import java.nio.ByteBuffer

abstract sealed class Swappable {

  def dump(): Long

  def load(): Node

}

case class Paged(val page: Long)(implicit val pager: Pager) extends Swappable {

  def dump(): Long = {
    page
  }

  def load(): Node = {
    val buffer = pager.read(page)
    buffer.get() match {
      case Swappable.BYTE_LEAF => LeafNode(buffer)
      case Swappable.BYTE_INNER => InnerNode(buffer)
    }
  }

}

case class Volatile(val node: Node)(implicit val pager: Pager) extends Swappable {

  def dump(): Long = {
    val buffer = ByteBuffer.allocate(Pager.PAGESIZE)
    node.pack(buffer)
    pager.append(buffer)
  }

  def load(): Node = {
    node
  }

}

object Swappable {

  val BYTE_LEAF = 0.toByte
  val BYTE_INNER = 1.toByte

}
