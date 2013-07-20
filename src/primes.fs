let nmax = 50000

// very inefficient implementation of sieve of Eratosthenes
let remove_multiples x m = 
    x |> List.filter( fun i -> i%m<>0 )

let rec sieve x = 
    match x with
    | head :: tail -> 
        if head<2 then
            sieve tail
        else
            head :: sieve (remove_multiples tail head)
    | [] -> []

let primes = ((seq {1..nmax}) |> List.ofSeq |> sieve)

primes |> List.iter( fun i -> printf "%d\t" i )

printfn ""


