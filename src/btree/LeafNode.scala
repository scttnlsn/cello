package btree

import java.nio.ByteBuffer
import scala.collection.mutable.ListBuffer

class LeafNode(
  val keys: ListBuffer[String],
  val values: ListBuffer[String])(
  implicit val pager: Pager) extends Node {

  def delete(key: String):Unit = {
    where(key) match {
      case None => ()
      case Some(i) => {
        if (key == keys(i)) {
          keys.remove(i)
          values.remove(i)
        }
      }
    }
  }

  def full(): Boolean = {
    var sum = 5
    keys.foreach(key => sum += key.length + 1)
    values.foreach(value => sum += value.length + 1)
    sum > Pager.PAGESIZE
  }

  def get(key: String): Option[String] = {
    where(key) match {
      case None => None
      case Some(i) => {
        if (key == keys(i)) {
          Some(values(i))
        } else {
          None
        }
      }
    }
  }

  def set(key: String, value: String): Unit = {
    where(key) match {
      case None => {
        keys.append(key)
        values.append(value)
      }
      case Some(i) => {
        if (key == keys(i)) {
          values.update(i, value)
        } else {
          keys.insert(i, key)
          values.insert(i, value)
        }
      }
    }
  }

  def split(): (String, LeafNode, LeafNode) = {
    val m = keys.size / 2
    val left = LeafNode(keys.take(m), values.take(m))
    val right = LeafNode(keys.drop(m), values.drop(m))
    (keys(m - 1), left, right)
  }

  def pack(buffer: ByteBuffer): Unit = {
    buffer.put(Swappable.BYTE_LEAF)
    buffer.putInt(keys.length)
    keys.foreach(key => Utils.putString(buffer, key))
    values.foreach(value => Utils.putString(buffer, value))
  }

}

object LeafNode {

  def apply(keys: Seq[String], values: Seq[String])(implicit pager: Pager): LeafNode = {
    val keysBuffer = new ListBuffer[String]()
    val valuesBuffer = new ListBuffer[String]()
    keysBuffer.appendAll(keys)
    valuesBuffer.appendAll(values)
    new LeafNode(keysBuffer, valuesBuffer)
  }

  def apply()(implicit pager: Pager): LeafNode = {
    LeafNode(List(), List())
  }

  def apply(buffer: ByteBuffer)(implicit pager: Pager): LeafNode = {
    val n = buffer.getInt()
    val keys = (1 to n).map(_ => Utils.getString(buffer))
    val values = (1 to n).map(_ => Utils.getString(buffer))
    LeafNode(keys, values)
  }

}
