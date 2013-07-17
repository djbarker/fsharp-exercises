let the_list = [ 2; 4; 6; 8; 10]

let rec permute x =
    if List.length x = 2 then
        [ x; List.rev x ]
    else
        // remove item i from list y
        let rem y i =
            y |> List.filter (fun j -> j<>i)

        // calculate the permutations of each sublist
        let sub_perms = x |> List.map (fun i -> permute (rem x i))

        // for each sublist append the removed element to the permuations
        let perms = List.map2 (fun y i -> (y |> List.map(fun j -> i::j))) sub_perms x

        // flatten the list structure to a list of each permutation
        let rec flatten y = 
            match y with
            | head :: tail -> head @ flatten(tail)
            | [] -> []

        // return the list of permutations
        flatten(perms)

// print
(permute the_list) |> Seq.iter (printfn "%A")
