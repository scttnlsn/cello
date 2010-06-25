package btree

import java.nio.ByteBuffer
import scala.collection.mutable.ListBuffer

abstract class Node {

  implicit val pager: Pager

  val keys: ListBuffer[String]

  def delete(key: String): Unit

  def full(): Boolean

  def get(key: String): Option[String]

  def set(key: String, value: String): Unit

  def split(): (String, Node, Node)

  def pack(buffer: ByteBuffer): Unit

  def unary_~(): Swappable = {
    Volatile(this)
  }

  protected def where(key: String): Option[Int] = {
    for (i <- 0 to keys.length - 1) {
      if (key <= keys(i)) {
        return Some(i)
      }
    }
    None
  }

}
