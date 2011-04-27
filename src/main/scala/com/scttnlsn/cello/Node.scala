package com.scttnlsn.cello

import java.nio.ByteBuffer

abstract class Node[A, B] {
  
  implicit val keyFormat: BinaryFormat[A]
  
  implicit val valueFormat: BinaryFormat[B]
  
  implicit val ordering: Ordering[A]
  
  implicit def ordering2order[A : Ordering](x : A) = new Ordered[A] {
    def compare(y : A) = implicitly[Ordering[A]].compare(x, y)
  }

  implicit val pager: Pager
  
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
