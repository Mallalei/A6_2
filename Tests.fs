module Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open FsCheck
open Swensen.Unquote
open Mini
open PriorityQueueTypes

type ValidDEQ<'a>     = Valid     of DEQ<'a> * List<'a> // queue, all elements as list
type RearHeavyDEQ<'a> = RearHeavy of DEQ<'a> * List<'a> // queue, all elements as list

type ArbitraryModifiers =
    static member Nat() =
        Arb.from<bigint>
        |> Arb.filter (fun i -> i >= 0I)
        |> Arb.convert (Nat.Make) (fun n -> n.ToBigInteger())

    static member ValidDEQ<'a>() =
        Arb.fromGen << Gen.sized <| fun size ->
            gen {
                let! sizeF = Gen.choose(0, size)
                let! sizeR = Gen.choose(0, min sizeF (size - sizeF))
                let! front = Arb.generate<'a> |> Gen.listOfLength sizeF
                let! rear  = Arb.generate<'a> |> Gen.listOfLength sizeR
                return Valid ({ frontLength = Nat.Make sizeF ; front = front ; rearLength = Nat.Make sizeR ; rear = rear }, (front @ List.rev rear))
            }

    static member RearHeavyDEQ<'a>() =
        Arb.fromGen << Gen.sized <| fun size ->
            gen {
                let! sizeR = Gen.choose(1, size)
                let! sizeF = Gen.choose(0, min 0 (sizeR - 1))
                let! front = Arb.generate<'a> |> Gen.listOfLength sizeF
                let! rear  = Arb.generate<'a> |> Gen.listOfLength sizeR
                return RearHeavy ({ frontLength = Nat.Make sizeF ; front = front ; rearLength = Nat.Make sizeR ; rear = rear }, (front @ List.rev rear))
            }

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

let exEmpty: DEQ<Nat> = {frontLength=0N; front=[]; rearLength=0N; rear=[]}

let isValidDEQ<'a> (q: DEQ<'a>): Bool =
    q.rearLength <= q.frontLength           &&
    q.rearLength  = Nat.Make q.rear.Length  &&
    q.frontLength = Nat.Make q.front.Length


[<TestClass>]
type Tests() =
    do Arb.register<ArbitraryModifiers>() |> ignore

    // ------------------------------------------------------------------------
    // a)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``a) isEmpty Beispiele`` (): unit =
        test <@ PriorityQueue.isEmpty exEmpty  = true @>
        test <@ PriorityQueue.isEmpty exQueue1 = false @>
        test <@ PriorityQueue.isEmpty exQueue2 = false @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``a) isEmpty Zufallstest`` (): unit =
        Check.QuickThrowOnFailure(fun (Valid (q, xs): ValidDEQ<Nat>) ->
            Assert.AreEqual(
                List.isEmpty xs,
                PriorityQueue.isEmpty q
            )
        )

    // ------------------------------------------------------------------------
    // b)

    [<TestMethod>] [<Timeout(5000)>]
    member this.``b) check Beispiele`` (): unit =
        test <@ PriorityQueue.check exEmpty = exEmpty @>
        test <@ PriorityQueue.check exQueue1 = exQueue1 @>
        test <@ PriorityQueue.check exQueue2 = exQueue2 @>
        let exQueueInvalid = { frontLength = 2N ; front = [42N; 4711N] ; rearLength = 3N ; rear = [5N; 7N; 11N] }
        test <@ isValidDEQ (PriorityQueue.check exQueueInvalid) = true @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``b) check Zufallstest (valid queue)`` (): unit =
        Check.QuickThrowOnFailure(fun (Valid (q, _): ValidDEQ<Nat>) ->
            Assert.AreEqual(
                q,
                PriorityQueue.check q
            )
        )

    [<TestMethod>] [<Timeout(5000)>]
    member this.``b) check Zufallstest (rear heavy queue)`` (): unit =
        Check.QuickThrowOnFailure(fun (RearHeavy (q, xs): RearHeavyDEQ<Nat>) ->
            Assert.AreNotEqual(
                q,
                PriorityQueue.check q
            )
            Assert.IsTrue(PriorityQueue.check q |> isValidDEQ)
        )

    // ------------------------------------------------------------------------
    // c)

    [<TestMethod>] [<Timeout(1000)>]
    member this.``c) snoc Beispiele`` (): unit =
        test <@ (PriorityQueue.snoc 42N exEmpty).front = [42N] @>

        let exQueue1' = PriorityQueue.snoc 42N exQueue1
        test <@ exQueue1'.front = exQueue1.front @>
        test <@ exQueue1'.rear = [42N] @>
        test <@ exQueue1'.frontLength = exQueue1.frontLength @>
        test <@ isValidDEQ exQueue1' = true @>

        let exQueue2' = PriorityQueue.snoc 1N exQueue2
        test <@ exQueue2'.front = exQueue2.front @ List.rev (1N :: exQueue2.rear) @>
        test <@ exQueue2'.rear = [] @>
        test <@ isValidDEQ exQueue2' = true @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``c) snoc Zufallstest`` (): unit =
        Check.QuickThrowOnFailure(fun (x: Nat, Valid (q, xs): ValidDEQ<Nat>) ->
            let actualQ = PriorityQueue.snoc x q
            Assert.IsTrue(isValidDEQ actualQ)
            Assert.AreEqual(
                xs @ [x],
                actualQ.front @ List.rev actualQ.rear
            )
        )

    // ------------------------------------------------------------------------
    // d)

    [<TestMethod>] [<Timeout(5000)>]
    member this.``d) head Beispiele`` (): unit =
        test <@ PriorityQueue.head exEmpty = None @>
        test <@ PriorityQueue.head exQueue1 = Some 1N @>
        test <@ PriorityQueue.head exQueue2 = Some 9N @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``d) tail Beispiele`` (): unit =
        test <@ PriorityQueue.tail exEmpty = None @>
        let exQueue1Tail = { frontLength = 2N; front = [2N; 3N] ; rearLength = 0N; rear = [] }
        test <@ PriorityQueue.tail exQueue1 = Some exQueue1Tail @>
        let exQueue2Tail = { frontLength = 5N ; front = [42N; 4711N; 11N; 7N; 5N;] ; rearLength = 0N ; rear = [] }
        test <@ PriorityQueue.tail exQueue2 = Some exQueue2Tail @>

    [<TestMethod>] [<Timeout(5000)>]
    member this.``d) head Zufallstest`` (): unit =
        Check.QuickThrowOnFailure(fun (Valid (q, xs): ValidDEQ<Nat>) ->
            let actual = PriorityQueue.head q
            match xs with
            | []    -> Assert.AreEqual(None, actual)
            | y::ys -> Assert.AreEqual(Some y, actual)
        )

    [<TestMethod>] [<Timeout(5000)>]
    member this.``d) tail Zufallstest`` (): unit =
        Check.QuickThrowOnFailure(fun (Valid (q, xs): ValidDEQ<Nat>) ->
            let actual = PriorityQueue.tail q
            match (xs, actual) with
            | [], _              -> Assert.AreEqual(None, actual)
            | y::ys, Some actual ->
                Assert.IsTrue(isValidDEQ actual)
                Assert.AreEqual(ys, actual.front @ List.rev actual.rear)
            | _::_ , None -> Assert.Fail "tail gibt None zurück, obwohl Queue nicht leer war."
        )
