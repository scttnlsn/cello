package cello

import java.nio.ByteBuffer
import scala.collection.mutable.ListBuffer

class InnerNode(
  val keys: ListBuffer[String],
  val children: ListBuffer[Swappable])(
  implicit val pager: Pager) extends Node {

  def delete(key: String): Unit = {
    val (i, child) = where(key) match {
      case None => (children.length - 1, children.last.load())
      case Some(i) => (i, children(i).load())
    }
    child.delete(key)
    children.update(i, ~child)
  }

  def full(): Boolean = {
    var sum = 5 + children.length * 8
    keys.foreach(key => sum += key.length + 1)
    sum > Pager.PAGESIZE
  }

  def get(key: String): Option[String] = {
    where(key) match {
      case None => children.last.load().get(key)
      case Some(i) => children(i).load().get(key)
    }
  }

  def set(key: String, value: String): Unit = {
    val position = where(key)
    val child = position match {
      case None => children.last.load()
      case Some(i) => children(i).load()
    }
    child.set(key, value)
    if (child.full) {
      val (pivot, left, right) = child.split
      position match {
        case None => {
          keys.append(pivot)
          children.update(children.length - 1, ~left)
          children.append(~right)
        }
        case Some(i) => {
          keys.insert(i, pivot)
          children.insert(i, ~left)
          children.update(i + 1, ~right)
        }
      }
    } else {
      position match {
        case None => {
          children.update(children.length - 1, ~child)
        }
        case Some(i) => {
          children.update(i, ~child)
        }
      }
    }
  }

  def split(): (String, InnerNode, InnerNode) = {
    val m = keys.size / 2
    val left = InnerNode(keys.take(m), children.take(m+1))
    val right = InnerNode(keys.drop(m+1), children.drop(m+1))
    (keys(m), left, right)
  }

  def pack(buffer: ByteBuffer): Unit = {
    buffer.put(Swappable.BYTE_INNER)
    buffer.putInt(keys.length)
    keys.foreach(key => Utils.putString(buffer, key))
    children.foreach(child => buffer.putLong(child.dump()))
  }

}

object InnerNode {

  def apply(keys: Seq[String], children: Seq[Swappable])(implicit pager: Pager): InnerNode = {
    val keysBuffer = new ListBuffer[String]()
    val childrenBuffer = new ListBuffer[Swappable]()
    keysBuffer.appendAll(keys)
    childrenBuffer.appendAll(children)
    new InnerNode(keysBuffer, childrenBuffer)
  }

  def apply(key: String, left: Node, right: Node)(implicit pager: Pager): InnerNode = {
    InnerNode(List(key), List(~left, ~right))
  }

  def apply(buffer: ByteBuffer)(implicit pager: Pager): InnerNode = {
    val n = buffer.getInt()
    val keys = (1 to n).map(_ => Utils.getString(buffer))
    val children = (1 to n + 1).map(_ => Paged(buffer.getLong()))
    InnerNode(keys, children)
  }

}
