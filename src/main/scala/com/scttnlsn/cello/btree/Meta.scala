package com.scttnlsn.cello.btree

import com.scttnlsn.cello.BinaryFormat
import com.scttnlsn.cello.Pager

trait Meta[A, B] {
  
  val pager: Pager
  
  val ordering: Ordering[A]
  
  val keyFormat: BinaryFormat[A]
  
  val valueFormat: BinaryFormat[B]
  
}