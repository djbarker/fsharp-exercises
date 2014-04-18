open System

let rng = new Random ()

module Grid = 
    // type alias
    type Grid = int [,]
    let rows (G: Grid) = G.GetLength 0
    let cols (G: Grid) = G.GetLength 1

    // get a list of all indices for the grid
    let all_indices (G: Grid) = seq {0..(rows G)-1} |> Seq.collect (fun i -> seq {0..(cols G)-1} |> Seq.map(fun j -> (i,j))) |> List.ofSeq

    // get a list of indices for the empty squares
    let empty_indices (G: Grid) = seq {0..(rows G)-1} |> Seq.collect (fun i -> seq {0..(cols G)-1} |> Seq.map(fun j -> (i,j)) |> Seq.filter(fun (i,j)->G.[i,j]=0)) |> List.ofSeq

    // print the grid to screen
    let print_grid (G: Grid) = 
        printf "-----------------------------\n\n"
        for i in  0..(rows G)-1 do
            for j in 0..(cols G)-1 do
                match G.[i,j] with
                | 0 -> printf ".\t"
                | _ -> printf "%O\t" G.[i,j]
            if i<(rows G)-1 then
                printfn "\n\n"
        printf "\n\n-----------------------------\n"

    // is the board full or not
    let board_full G = (List.length (empty_indices G))=0

    // adds a tile to a random empty grid space
    let add_tile (G': Grid) =
        let G = Array2D.copy G'
        let empty = empty_indices G
        let num_empty = List.length empty
        let (i,j) = empty.[rng.Next num_empty]
        G.[i,j] <- if (rng.NextDouble ())<0.1 then 4 else 2
        G

    // get rows and columns as lists
    let get_row n G = seq {0..(cols G)-1} |> List.ofSeq |> List.collect (fun j -> [G.[n,j]])
    let get_col n G = seq {0..(rows G)-1} |> List.ofSeq |> List.collect (fun i -> [G.[i,n]])

    // strip and append zeros to row lists
    let strip_zeros = List.filter (fun v -> v<>0) 
    let rec app_zeros n x =
        if List.length x<n then
            app_zeros n (0::x)
        else
            x 

    // combines a row or column list
    let rec combine x = 
        match x with
        | h1 :: h2 :: tail -> 
            if h1=h2 then
                2*h1::(combine tail)
            else
                h1::(combine (h2::tail))
        | _ -> x

    type Direction =
        | Left
        | Right
        | Up
        | Down

    // performs the actual move in the specified direction
    let do_move (G': Grid) dir =
   
        let G = Array2D.copy G'

        let set_row i (x: int list) = seq {0..(cols G)-1} |> Seq.iter (fun j -> G.[i,j] <- x.[j] )
        let set_col j (x: int list) = seq {0..(rows G)-1} |> Seq.iter (fun i -> G.[i,j] <- x.[i] )

        match dir with
        | Left -> for i in 0..(rows G)-1 do
                    let new_row = (get_row i G) |> strip_zeros |> combine |> List.rev |> (app_zeros (cols G)) |> List.rev
                    set_row i new_row
        | Right -> for i in 0..(rows G)-1 do
                    let new_row = (get_row i G) |> strip_zeros |> List.rev |> combine |> List.rev |> (app_zeros (cols G))
                    set_row i new_row
        | Up -> for i in 0..(cols G)-1 do
                    let new_col = (get_col i G) |> strip_zeros |> combine |> List.rev |> (app_zeros (rows G)) |> List.rev
                    set_col i new_col
        | Down -> for i in 0..(cols G)-1 do
                    let new_col = (get_col i G) |> strip_zeros |> List.rev |> combine |> List.rev |> (app_zeros (rows G))
                    set_col i new_col
        G

    // counts the number of possible moves available - function could be cleverer
    let count_moves (G: Grid) =
        let G_l = do_move G Left
        let G_r = do_move G Right
        let G_u = do_move G Up
        let G_d = do_move G Down
        let toint x = if x then 1 else 0
        List.sum [toint (G_l<>G); toint (G_r<>G); toint (G_u<>G); toint (G_d<>G)]

open Grid

// gets a move direction from the user - lets hope they don't press a non cursor key too many times
let get_dirn () = 
    let cursors = [ConsoleKey.LeftArrow; ConsoleKey.RightArrow; ConsoleKey.UpArrow; ConsoleKey.DownArrow]
    let rec get_key K = 
        if not (cursors |> List.exists (fun c -> K=c)) then
            get_key (Console.ReadKey ()).Key
        else 
            K
    
    match get_key (Console.ReadKey ()).Key with
    | ConsoleKey.LeftArrow  -> Left
    | ConsoleKey.RightArrow -> Right
    | ConsoleKey.UpArrow    -> Up
    | ConsoleKey.DownArrow  -> Down
    | _ -> failwith "Invalid key returned!"


[<EntryPoint>]
let main argv = 
  
    // initial setup
    let mutable the_grid = Array2D.create 4 4 0

    the_grid <- add_tile the_grid
    the_grid <- add_tile the_grid

    print_grid the_grid

    // play the game
    while (count_moves the_grid)>0 do
        let grid' = do_move the_grid (get_dirn ())

        if grid'<>the_grid && not (board_full grid') then
            the_grid <- add_tile grid'

        printf "\n\n\n\n\n\n\n\n\n"
        print_grid the_grid
   
    printfn "Game over!"

    0 // return an integer exit code