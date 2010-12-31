type Eventually<'R> =
    | Completed of 'R
    | Blocked of float32 * (unit -> Eventually<'R>)
    | Running of (unit -> Eventually<'R>)

let rec bind k e =
    match e with
    | Completed(r) -> Running(fun () -> k r)
    | Running(work) -> Running(fun () -> bind k (work()))
    | Blocked(dt, work) -> Blocked(dt, fun () -> bind k (work()))

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

let wait dt =
    Blocked(dt, fun () -> Completed())

let step dt = function
    | Completed(r) as v -> (v, dt)
    | Blocked(w, f) ->
        if dt >= w then
            (f(), dt - w)
        else
            (Blocked(w-dt, f), 0.0f)
    | Running(f) ->
        (f(), dt)

let hasCompleted = function
    | Completed _ -> true
    | _ -> false

let isBlocked = function
    | Blocked _ -> true
    | _ -> false
    
let stepUntilBlocked dt ev =
    let ev, dt = step dt ev
    let mutable ev = ev
    let mutable dt = dt
    while not (isBlocked(ev) || hasCompleted(ev)) do
        let ev', dt' = step dt ev
        dt <- dt'
        ev <- ev'
    ev

let runAll dt evs =
    let mutable state = evs
    printfn "%A" state
    while not (state |> Seq.forall hasCompleted) do
        state <- state |> Seq.mapi (fun i x -> printfn "Task %d>" i; stepUntilBlocked dt x)
        printfn "%A" state
    state
    |> Seq.map (function Completed r -> r | _ -> failwith "Unreachable")


let task = TaskBuilder()

let toEventuallyObj ev = task {
    let! res = ev
    return box res
}

let next() = task {
    printfn "Entered: next"
    do! wait 0.0f
    printfn "Left: next"
}

let waitCond b = task {
    printfn "Entered: waitCond"
    while not (!b) do
        do! wait 0.0f
    printfn "Left: waitCond"
}

let test1() =
    let t1 = task {
        for i in 0..10 do
            do! wait 0.016f
        printfn "Hello"
    }
    runAll 0.02f [t1]

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
        do! waitCond b
        printfn "Received"
    }

    let sys = [ sender ; receiver ]
    let mutable state = sys
    printfn "%A" state
    while not (state |> List.forall hasCompleted) do
        state <- state |> List.mapi (fun i x -> printfn ">%d:" i; stepUntilBlocked 0.016f x)
        printfn "%A" state
