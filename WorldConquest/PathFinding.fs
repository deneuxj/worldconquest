module PathFinding

open System.Collections.Generic
open XNAUtils.Heap

// F#-friendly extension method for dictionaries.
type Dictionary<'K, 'V> with
    member x.GetValue(key) =
        match x.TryGetValue(key) with
        | true, v -> Some v
        | false, _ -> None

let find (h : 'Coords -> float32) // Estimate of distance to the goal. Must satisfy h(x) = 0.0f iff x is a goal.
         (neighboursOf : 'Coords -> 'Coords list)
         (dist : 'Coords * 'Coords -> float32) // Distance between two neighbours. Typically always 1.0f, but can vary if movement costs are used.
         (start : 'Coords) =
    // Distance from start along optimal path
    let g_score = new Dictionary<'Coords, float32>()

    // Used to reconstruct the path
    let came_from = new Dictionary<'Coords, 'Coords>()

    // Set of visited nodes
    let visited = new HashSet<'Coords>()

    // Set of open nodes. Ordered by g_score + estimate
    let working = newHeap<float32 * 'Coords> 16
    let isInWorking = new HashSet<'Coords>() // for quick membership test.
    let insert, take =
        let cmp(a, b) = fst a <= fst b in
        (fun score coords ->
            insert cmp working (score, coords)
            isInWorking.Add(coords) |> ignore),
        (fun () ->
            let coords = take cmp working |> snd
            isInWorking.Remove(coords) |> ignore
            coords)

    // Initially: working = {start}, g_score[start] = estimate to goal (distance from start is 0).
    let h_start = h(start)
    let g_start = 0.0f
    insert (g_start + h_start) start
    g_score.[start] <- g_start

    // Keeps track of which goal we found a path to (if any).
    // Note that this function works with multiple goals too.
    let mutable goal =
        if h_start = 0.0f then
            Some start
        else
            None

    while not <| isEmpty working && goal.IsNone do
        let x = take()
        if not <| visited.Contains(x) then
            let h_x = h(x)
            if h_x = 0.0f then
                goal <- Some x
            else
                visited.Add(x) |> ignore

                let unvisited_neighbours =
                    neighboursOf x
                    |> List.filter (fun x' -> not <| visited.Contains(x'))
                
                for x' in unvisited_neighbours do
                    let g = g_score.[x] + dist(x, x')
                
                    let update() =
                        came_from.[x'] <- x
                        g_score.[x'] <- g
                        insert (g + h(x')) x'

                    match g_score.GetValue(x') with
                    | None -> update()
                    | Some old_g when g < old_g -> update()
                    | Some _ -> ()
    
    match goal with
    | Some goal ->
        let rec reconstruct x path =
            match came_from.GetValue(x) with
            | Some x' -> reconstruct x' (x::path)
            | None -> path
        reconstruct goal [goal]
        |> Some
    | None ->
        None