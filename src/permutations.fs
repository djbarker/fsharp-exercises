let the_list = [ 2; 4; 6; 8; 10]

let rec permute x =
    if List.length x = 2 then
        [ x; List.rev x ]
    else
        // remove item i from list y
        let rem y i =
            y |> List.filter (fun j -> j<>i)

        // prepend the item i to each element of the a' list list
        let pre y i =
            y |> List.map (fun y' -> i::y')

        // calculate the permutations of each sublist then add the removed item
        x |> List.collect (fun i -> pre (permute (rem x i)) i )

let permutations = permute the_list

// sanity check
let factorial n = seq{1..n} |> Seq.reduce (*)
assert (List.length permutations = (the_list |> List.length |> factorial))

// print
permutations |> Seq.iter (printfn "%A")
