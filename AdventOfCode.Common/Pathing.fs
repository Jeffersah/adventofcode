namespace AdventOfCode.Common

open System.Collections.Generic
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<Struct; IsReadOnly>]
type PathNode<'node, 'step, 'cost when 'node : comparison> = {
    node: 'node
    parents: Map<'node, 'step>
    pathCost: 'cost
    heuristicCost: 'cost
    totalCost: 'cost
}

type PathEntry<'node, 'step> = {
    node: 'node
    next: ('step * PathEntry<'node, 'step>) option
}

type PathResult<'node, 'step, 'cost when 'node : comparison> = {
    path: PathEntry<'node, 'step> option
    visited: Map<'node, PathNode<'node, 'step, 'cost>>
}

module Pathing =
    let inline findPath<'node, 'step, 'cost
        when 'node: comparison
        and 'cost: comparison
        and 'cost : (static member (+) : ('cost * 'cost) -> 'cost)
        and ('cost or int) : (static member op_Explicit : int -> 'cost)>
        (initial: 'node list)
        (getSteps: 'node -> struct('cost * 'step * 'node) seq)
        (getHeuristic: 'node -> 'cost)
        (isComplete: 'node -> bool) =
        
        // This implementation is not very F#-like, but I'm using mutability for performance reasons
        let closedSet = Dictionary<'node, PathNode<'node, 'step, 'cost>>()
        let openSet = PriorityQueue<PathNode<'node, 'step, 'cost>, 'cost>()
        let zero = GenericMath.ofInt<'cost> 0
        
        for node in initial do
            let hcost = getHeuristic node
            openSet.Enqueue({ node = node; parents = Map.empty; pathCost = zero; heuristicCost = hcost; totalCost = hcost }, hcost)
        
        let rec run (openSet: PriorityQueue<PathNode<'node, 'step, 'cost>, 'cost>) (closedSet: Dictionary<'node, PathNode<'node, 'step, 'cost>>) =
            match openSet.TryDequeue () with
            | (false, _, _) ->
                { path = None; visited = closedSet |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq }
            | (true, node, totalCost) ->
                match closedSet.TryGetValue(node.node) with
                | (true, pathNode) ->
                    if pathNode.totalCost = totalCost then
                        let unionParents =
                            node.parents
                            |> Map.fold (fun map k v -> Map.add k v map) pathNode.parents
                        closedSet[node.node] <- { pathNode with parents = unionParents }
                    run openSet closedSet
                | _ ->
                    closedSet[node.node] <- node
                    if isComplete node.node then
                        let rec buildPath (node: PathNode<'node, 'step, 'cost>) (next: ('step * PathEntry<'node, 'step>) option) =
                            let self = { node = node.node; next = next }
                            match node.parents |> Map.toSeq |> List.ofSeq with
                            | [] -> self
                            | (p, step)::_ ->
                                let previous = closedSet[p]
                                buildPath previous (Some(step, self))
                        let path = buildPath node None                        
                        { path = Some path; visited = closedSet |> Seq.map (fun kvp -> kvp.Key, kvp.Value) |> Map.ofSeq }
                    else
                        for step in getSteps node.node do
                            let (struct(stepCost, step, nextNode)) = step
                            let newStepCost = 'cost.(+) (stepCost, node.pathCost)
                            let heuristic = getHeuristic nextNode
                            let newEntry = { node = nextNode; parents = Map.add node.node step Map.empty; pathCost = newStepCost; heuristicCost = heuristic; totalCost = 'cost.(+) (newStepCost, heuristic) }
                            let shouldConsider =
                                match closedSet.TryGetValue(newEntry.node) with
                                | (true, pathNode) when pathNode.totalCost = newEntry.totalCost -> true
                                | (false, _) -> true
                                | _ -> false
                            if shouldConsider then
                                openSet.Enqueue(newEntry, newEntry.totalCost)
                        run openSet closedSet
        run openSet closedSet