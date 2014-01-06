let nmax = 50000

// improved implementation of sieve of Eratosthenes
let sieve N = 

    let remove_multiples x m = 
        x |> List.filter( fun i -> i%m<>0 )

    let rec sieve_impl x out = 
        match x with
        | head :: tail -> sieve_impl (remove_multiples tail head) (head::out)
        | [] -> out

    (sieve_impl (seq {2..nmax} |> List.ofSeq) []) |> List.rev

// returns the execution time of 'func arg' in milliseconds and the result in a pair.
let duration func arg = 
    let T = new System.Diagnostics.Stopwatch ()
    T.Start ()
    (T.ElapsedMilliseconds, func arg)

// calculate and store the primes
let time, primes = duration sieve nmax

primes |> List.iter( fun i -> printf "%d\t" i )

printfn ""
printfn "Duration: %d ms" time
