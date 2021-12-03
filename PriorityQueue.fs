module PriorityQueue
open Mini
open PriorityQueueTypes

// Beispiele vom Übungsblatt
let exQueue1 =
    { frontLength = 3N
    ; front = [1N; 2N; 3N]
    ; rearLength = 0N
    ; rear = []
    }

let exQueue2 =
    { frontLength = 3N
    ; front = [9N; 42N; 4711N]
    ; rearLength = 3N
    ; rear = [5N; 7N; 11N]
    }

// a)
let isEmpty<'a> (q: DEQ<'a>): Bool =
    failwith "TODO"

// b)
let rec check<'a> (q: DEQ<'a>): DEQ<'a> =
    failwith "TODO"

// c)
let rec snoc<'a> (x: 'a) (q: DEQ<'a>): DEQ<'a> =
    failwith "TODO"

// d)
let head<'a> (q: DEQ<'a>): Option<'a> =
    failwith "TODO"

let tail<'a> (q: DEQ<'a>): Option<DEQ<'a>> =
    failwith "TODO"
