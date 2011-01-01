﻿#r @"D:\Documents\xnautils\xnautils\bin\release\XNAUtils.dll"

open XNAUtils.Heap

type Eventually<'R> =
    | Completed of 'R
    | Blocked of float32 * (unit -> Eventually<'R>)
    | Running of (unit -> Eventually<'R>)
    | Yield of (unit -> Eventually<'R>)

let rec bind k e =
    match e with
    | Completed(r) -> Running(fun () -> k r)
    | Running(work) -> Running(fun () -> bind k (work()))
    | Blocked(dt, work) -> Blocked(dt, fun () -> bind k (work()))
    | Yield(work) -> Yield(fun () -> work() |> bind k)

let result r = Completed(r)

let delay f = Running f

type OkOrException<'T> =
    | Ok of 'T
    | Exception of System.Exception

let rec catch e =
    let newWork work =
        let res = try Ok(work()) with e -> Exception e
        match res with
        | Ok cont -> catch cont
        | Exception e -> result(Exception e)
    match e with
    | Completed r -> result(Ok r)
    | Blocked(dt, work) -> Blocked(dt, fun () -> newWork work)
    | Running work -> Running(fun() -> newWork work)
    | Yield work -> Yield(fun() -> newWork work)

(* Boiler-plate *)
let tryFinally e compensation =
    catch e
    |> bind (fun res ->
        compensation()
        match res with
        | Ok x -> result x
        | Exception e -> raise e)

let tryWith e handler =
    catch e
    |> bind (function
        | Ok x -> result x
        | Exception e -> handler e)

let rec whileLoop cond body =
    if cond() then
        body
        |> bind (fun () -> whileLoop cond body)
    else
        result()

let combine e1 e2 =
    e1 |> bind (fun () -> e2)

let using (r : #System.IDisposable) f =
    tryFinally (f r) (fun () -> r.Dispose())

let forLoop (xs : 'T seq) f =
    using (xs.GetEnumerator()) (fun it ->
            whileLoop (fun () -> it.MoveNext())
                (delay (fun () -> it.Current |> f))
        )

(* The builder type *)
type TaskBuilder() =
    member x.Bind(e, f) = bind f e
    member x.Return(r) = result r
    member x.ReturnFrom(r) = r
    member x.Combine(e1, e2) = combine e1 e2
    member x.Delay(f) = delay f
    member x.Zero() = result ()
    member x.TryWith(e, handler) = tryWith e handler
    member x.TryFinally(e, compensation) = tryFinally e compensation
    member x.While(e, f) = whileLoop e f
    member x.For(e, f) = forLoop e f
    member x.Using(e, f) = using e f

(* Task API *)
let task = TaskBuilder()

let wait dt =
    Blocked(dt, fun () -> Completed())

let next () =
    Yield(fun() -> Completed())

let waitCond f = task {
    while not (f()) do
        do! next()
}

type Lock() =
    let mutable locked = false;

    member this.Grab() =
        task {
            do! waitCond (fun() -> not locked)
            locked <- true
        }

    member this.Release() =
        task {
            locked <- false
        }

type BlockingChannel<'M>() =
    let mutable content = None
    let mutable flag_send = false
    let mutable flag_receive = false

    member this.Send(m : 'M) =
        task {
            do! waitCond (fun () -> not flag_send)
            content <- Some m
            flag_send <- true
            do! waitCond (fun () -> flag_receive)
            flag_send <- false
            content <- None
        }

    member this.Read() =
        task {
            do! waitCond (fun () -> flag_send)
            let ret = content.Value
            flag_receive <- true
            do! waitCond (fun () -> not flag_send)
            flag_receive <- false
            return ret
        }

        
(* Simulation *)
let step dt = function
    | Completed(r) as v -> (v, dt)
    | Blocked(w, f) ->
        if dt >= w then
            (f(), dt - w)
        else
            (Blocked(w-dt, f), 0.0f)
    | Running(f) ->
        (f(), dt)
    | Yield(f) ->
        (Running(f), dt)

let hasCompleted = function
    | Completed _ -> true
    | _ -> false

let isBlocked = function
    | Blocked _ | Yield _ -> true
    | _ -> false

let isRunning = function
    | Running _ -> true
    | _ -> false

(* Execution *)

type Scheduler() =
    let tasks : Heap<Eventually<unit>> = newHeap 4

    let cmp (ev1, ev2) =
        match ev1, ev2 with
        | Running _, Running _ -> false
        | Running _, _ -> true
        | Yield _, Running _ | Yield _, Yield _ -> false
        | Yield _, _ -> true
        | Blocked _, Running _ | Blocked _, Yield _ -> false
        | Blocked (t1, _), Blocked (t2, _) -> t1 < t2
        | Completed _, _ -> false
        | _, Completed _ -> true

    member x.AddTask(t) =
        insert cmp tasks t
    
    member x.HasLiveTasks =
        tasks.count > 0
        &&
        tasks.arr.[0..tasks.count-1]
        |> Array.exists (function Completed _ -> false | _ -> true)

    member x.RunFor(dt) =
        let decrBlockingTimes delta =
            for i in 0..tasks.count-1 do
                tasks.arr.[i] <-
                    match tasks.arr.[i] with
                    | Yield _ | Running _ -> failwith "All tasks should be blocked."
                    | Blocked (t, f) -> Blocked (t-delta, f)
                    | Completed () as v -> v

        let rec work dt =
            if dt > 0.0f && tasks.count > 0 then
                let t = take cmp tasks
                match t with
                | Yield f | Running f ->
                    f()
                    |> insert cmp tasks
                    work dt
                | Completed () ->
                    work dt
                | Blocked (t, f) when t < dt ->
                    decrBlockingTimes t
                    f()
                    |> insert cmp tasks
                    work (dt - t)
                | Blocked _ ->
                    insert cmp tasks t
                    decrBlockingTimes dt
                    ()
                    
        work dt

let stepUntilBlocked dt ev =
    let ev, dt = step dt ev
    let mutable ev = ev
    let mutable dt = dt
    while not (isBlocked(ev) || hasCompleted(ev)) do
        let ev', dt' = step dt ev
        dt <- dt'
        ev <- ev'
    ev

let runAllCompressed evs =
    let mutable state = evs
    printfn "%A" state
    while not (state |> Array.forall hasCompleted) do
        let max_time =
            state
            |> Seq.map(function Blocked (t, _) -> t | _ -> 0.0f)
            |> Seq.min
        printfn "Start sweep (%f)" max_time
        state <- state |> Array.mapi (fun i x -> printfn "Task %d>" i; step max_time x |> fst)
        printfn "%A" state
    state
    |> Array.map (function Completed r -> r | _ -> failwith "Unreachable")

let runAllFixed dt evs =
    let scheduler = new Scheduler()
    
    evs
    |> Array.iter (fun t -> scheduler.AddTask(t))

    while scheduler.HasLiveTasks do
        scheduler.RunFor(dt)

let toEventuallyObj ev = task {
    let! res = ev
    return box res
}

(* tests *)
let test1() =
    let t1 = task {
        for i in 0..3 do
            do! wait 3.0f
        printfn "Hello"
    }
    runAllFixed 1.0f [|t1|]

let test2() =
    let b = ref false

    let sender = task {
        b := false
        do! next()
        printfn "Sending"
        b := true
        printfn "Sent"
        do! next()
        printfn "Resetting"
        b := false
        printfn "Reset"
    }

    let receiver = task {
        printfn "Waiting"
        do! waitCond (fun () -> !b)
        printfn "Received"
    }

    let sys = [| sender ; receiver |]
    runAllFixed 0.01f sys

