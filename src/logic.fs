// A simple program which takes a set of axioms and
// and draws conclusions from them. If the axioms are
// not self consistent the program will not crash but
// will return contradicting conclusions

//
// Convenience functions
//

let print_set x = 
    let max_el = x |> Set.maxElement
    printf "{"
    (x-(Set.singleton max_el)) |> Set.iter(fun el -> printf "%O, " el)
    printfn "%O}" max_el

// repeatedly apply a function to data until the output does not change
let rec repeat func (data : Set<'a>) =
    let out = func data

    if out=data then
        out
    else
        func (data + out)

//
// The main program
//

type Predicate = { Name : string; Desc : string }

// assume things are either plants or animals
let pred_A = { Name="H"; Desc="a human" }
let pred_B = { Name="P"; Desc="a plant" }
let pred_C = { Name="A"; Desc="an animal" }
let pred_D = { Name="S"; Desc="a sunflower" }

type Statement = 
    | Assert of Predicate // for asserting a predicate is true, i.e. turning a predicate into a statement
    | Not of Statement
    | Implies of Statement * Statement
    | Null
    override this.ToString() = 
        match this with
        | Assert P -> P.Name
        | Not S -> "¬" + S.ToString()
        | Implies (S1,S2) -> S1.ToString() + " => " + S2.ToString()
        | Null -> "Ø"
    member this.ToSentence() = 
        match this with
        | Assert P -> P.Desc
        | Not S -> "not " + S.ToSentence()
        | Implies (S1,S2) -> S1.ToSentence() + " implies " + S2.ToSentence()
        | Null -> "Null"

// removes double negations
let rec simplify S = 
    match S with
    | Not(Not S') -> simplify S'
    | _ -> S

// constructs the contrapositive of an implication
let contrapositive S =
    match S with
    | Implies (S1,S2) -> Implies (simplify (Not S2), simplify (Not S1))
    | _ -> Null

// constructs the inverse of an implication
let inverse S = 
    match S with
    | Implies (S1,S2) -> Implies (simplify (Not S1), simplify (Not S2))
    | _ -> Null

// constructs the converse of an implication
let converse S = 
    match S with
    | Implies (S1,S2) -> Implies (simplify S2, simplify S1)
    | _ -> Null

// TODO: finish
let contradiction s1 s2 =
    match s1,s2 with
    | Assert(p1),Not(Assert(p2)) when p1=p2 -> true
    | _ -> false

//let st1 = Implies(Assert(pred_A),Not(Assert(pred_B)))
//printfn "The contrapositive of the statement \"%O\" is \"%O\"." st1 (contrapositive st1)
//printfn "The inverse of the statement \"%O\" is \"%O\"." st1 (inverse st1)
//printfn "The converse of the statement \"%O\" is \"%O\"." st1 (converse st1)

let the_axioms = Set.ofList [
                   Assert(pred_A);
                   Implies(Assert(pred_A),Not(Assert(pred_B)))
                   Implies(Not(Assert(pred_B)),Assert(pred_C))
                   Implies (Assert pred_D, Assert pred_B)
                 ]

printfn "The axioms: "
print_set the_axioms

// modus ponens, i.e.  ((P=>Q) and P) => Q
let modus_ponens statement axioms =
    match statement with
    | Implies (S1,S2) -> 
        match axioms |> Set.exists(fun axiom -> axiom=S1) with
        | true -> S2
        | false -> Null
    | _ -> Null

// modus tollens, i.e. (P=>Q) and ¬Q) => ¬P
let modus_tollens statement axioms = 
    match statement with 
    | Implies (S1,S2) ->
        match axioms |> Set.exists(fun axiom -> (simplify axiom) = (simplify (Not S2))) with
        | true -> simplify (Not S1)
        | false -> Null
    | _ -> Null

// apply a rule to a set of axioms to draw conclusions
let rec apply_rule rule axioms =
    axioms |> Set.map( fun axiom -> rule axiom axioms )

// apply modus_tollens then modus_ponens
let all_rules axioms = 
    apply_rule modus_ponens axioms+(apply_rule modus_tollens axioms)

// draw conclusions and print them
let conclusions = repeat all_rules the_axioms |> Set.filter(fun el -> el <> Null)

printfn "The conclusions: "
print_set conclusions

printfn "\n-- Human readable --"
printfn "The axioms: "
the_axioms |> Set.iter(fun el -> printfn "  %O" (el.ToSentence()) )
printfn "The conclusions: "
conclusions |> Set.iter(fun el -> printfn "  %O" (el.ToSentence()) )


// stop the program exiting until keydown
//System.Console.ReadKey() |> ignore
