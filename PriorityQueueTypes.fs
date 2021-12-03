[<AutoOpen>]
module PriorityQueueTypes
open Mini

type DEQ<'a> = // Die Warteschlange
    { frontLength: Nat
      front: List<'a>
      rearLength: Nat
      rear: List<'a>
    }