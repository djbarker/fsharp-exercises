let the_list = [2; 4; 6; 8; 10]

(*
    Generates the powerset of a given list using the correspondence between
    elements of the powerset and binary numbers. This only works for lists
    whose lengths are 32 or less.
 *)  
let power_set x = 
    let x' = Array.ofList x

    let rec choose y n =
        let mutable out = []
        for i in 0..Array.length y do
            if (pown 2 i)&&&n>0 then
                out <- out@[y.[i]]
        out

    let N = (pown 2 (Array.length x'))-1
    seq {0..N} |> Seq.map (fun i -> choose x' i) |> Seq.sortBy (fun el -> List.length el)

// generate the powerset
let the_answer = power_set the_list

// sanity check
assert (Seq.length the_answer = (pown 2 (List.length the_list)))

// print the powerset to the screen and a file   
the_answer |> Seq.iter (fun el -> printfn "%A" el)
let lines = Seq.map (fun el -> sprintf "%A" el) the_answer
let fout = System.IO.File.WriteAllLines ("powerset.txt", lines)