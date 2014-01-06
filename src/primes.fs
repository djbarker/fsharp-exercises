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

(*
    Returns the execution time of 'func' applied to 'arg1' in milliseconds and
    the result in a pair. For functions with more than one argument partial
    application can be used, e.g. (duration f x) y.
 *)
let duration func arg1 = 
    let T = new System.Diagnostics.Stopwatch ()
    T.Start ()
    (T.ElapsedMilliseconds, func arg1)

// calculate and store the primes
let time, primes = duration sieve nmax

primes |> List.iter( fun i -> printf "%d\t" i )

printfn ""
printfn "Duration: %d ms" time
