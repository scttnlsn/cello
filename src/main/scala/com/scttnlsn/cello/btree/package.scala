package com.scttnlsn.cello

import scala.collection.SortedMap

package object btree {
    
    type Child[A, B] = Swappable[A, B]
    
    type ChildMap[A, B] = SortedMap[A, Child[A, B]]
    
}