package com.scttnlsn.cello

import java.nio.ByteBuffer

abstract class Node[A, B] {
  
  implicit val meta: Meta[A, B]
  
  implicit val pager = meta.pager
  
  implicit val ordering = meta.ordering
  
  implicit val keyFormat = meta.keyFormat
  
  implicit val valueFormat = meta.valueFormat
  
  implicit def ordered[A : Ordering](x : A) = new Ordered[A] {
    def compare(y : A) = implicitly[Ordering[A]].compare(x, y)
  }
  
  def get(key: A): Option[B]
  
  def set(key: A, value: B): Unit

  def delete(key: A): Unit
  
  def split(): (A, Node[A, B], Node[A, B])

  def full(): Boolean

  def pack(buffer: ByteBuffer): Unit

  def unary_~(): Swappable[A, B] = {
    Volatile(this)
  }

}
