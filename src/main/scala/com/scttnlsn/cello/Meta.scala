package com.scttnlsn.cello

trait Meta[A, B] {
  
  val pager: Pager
  
  val ordering: Ordering[A]
  
  val keyFormat: BinaryFormat[A]
  
  val valueFormat: BinaryFormat[B]
  
}