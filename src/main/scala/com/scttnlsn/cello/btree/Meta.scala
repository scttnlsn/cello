package com.scttnlsn.cello.btree

import com.scttnlsn.cello.BinaryFormat
import com.scttnlsn.cello.Pager

trait Meta[A, B] {
  
  val pager: Pager
  
  val ordering: Ordering[A]
  
  val keyFormat: BinaryFormat[A]
  
  val valueFormat: BinaryFormat[B]
  
  def replace(replacement: Pager): Meta[A, B] = {
    new Meta[A, B] {
      implicit val pager = replacement
      implicit val ordering: Ordering[A] = Meta.this.ordering
      implicit val keyFormat: BinaryFormat[A] = Meta.this.keyFormat
      implicit val valueFormat: BinaryFormat[B] = Meta.this.valueFormat
    }
  }
  
}