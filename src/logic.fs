(* 
 * A simple program which takes a set of axioms and draws conclusions from them
 * If the axioms are not self consistent the program will not crash but it will
 * return contradicting conclusions. If the contradictory axioms are not used 
 * in drawing conclusions no warning will be presented.
 *) 

(*
 * Convenience functions
 *)

let print_set (x: Set<'a>) = 
    if x.Count>0 then
        let max_el = x |> Set.maxElement
        printf "{"
        (x-(Set.singleton max_el)) |> Set.iter(fun el -> printf "%O, " el)
        printfn "%O}" max_el
    else
        printfn "Ø"

// repeatedly apply a function to data until the output does not change
let rec repeat func (data : Set<'a>) =
    let out = func data

    if (data+out)=data then
        out
    else
        repeat func (data + out)

(*
 * The main program
 *)

type Predicate = { Name : string; Desc : string }

type Statement = 
    | Assert of Predicate // for asserting a predicate is true, i.e. turning a predicate into a statement
    | Not of Statement
    | And of Statement * Statement
    | Or of Statement * Statement // inclusive or
    | Implies of Statement * Statement
    | Equivalent of Statement * Statement // i.e. implies and is implied by
    | Null
    override this.ToString() = 
        match this with
        | Assert P -> P.Name
        | Not S -> "¬" + S.ToString()
        | And (S1,S2) -> "(" + S1.ToString() + " & " + S2.ToString() + ")"
        | Or (S1,S2) -> "(" + S1.ToString() + " | " + S2.ToString() + ")"
        | Implies (S1,S2) -> S1.ToString() + " => " + S2.ToString()
        | Equivalent (S1,S2) -> S1.ToString() + " <=> " + S2.ToString()
        | Null -> "Ø"

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

// expands equivalencies and "or"s to two implications, i.e. {A<=>B} -> {A=>B,B=>A}
let expand statements =
    let equivalencies = statements |> Set.filter( fun s -> match s with | Equivalent(s1,s2) -> true | _ -> false )
    let others = statements - equivalencies // set diff
    others + ( Set.fold( fun acc eq -> acc + (match eq with | Equivalent(s1,s2) -> Set.ofList [Implies(s1,s2);Implies(s2,s1)] | _ -> raise (System.Exception "Non-equivalency in equivalency list!"))) Set.empty equivalencies)

(*
 * The predicates and axioms
 *)

let pred_A = { Name="A"; Desc="x is a Human" }
let pred_B = { Name="B"; Desc="x is a Man" }
let pred_C = { Name="C"; Desc="x is a Woman" }
let pred_D = { Name="D"; Desc="x is a Mammal"}
let pred_E = { Name="E"; Desc="x is a Male"}

let my_axioms = Set.ofList [
                            Assert(pred_B);
                            //Assert(pred_A); Assert(pred_E);
                            Equivalent (And(Assert pred_A, Assert pred_E), Assert pred_B); // Human Male <=> Man
                            Implies (Assert pred_A, Assert pred_D) ; // Human => Mammal
                            Implies (Or(Assert pred_B, Assert pred_C), Assert pred_A); // Man or Woman => Human
                            Implies (Assert pred_B, Not (Assert pred_C)); // Man => ¬Woman
                            Implies (Assert pred_C, Not (Assert pred_B))  // Woman => ¬Man
                            ]

printfn "The axioms:"
print_set my_axioms

// evaulate a statement under the given axioms
let rec eval axioms statement =
    let eval_ = eval axioms // partially apply
    match statement with
    | Assert pred -> axioms |> Set.exists (fun ax -> ax=statement)
    | Not st -> not (eval_ st)
    | And (s1,s2) -> (eval_ s1) && (eval_ s2)
    | Or (s1,s2) -> (eval_ s1) || (eval_ s2)
    | _ -> raise (System.Exception "huh")

// modus ponens, i.e.  ((P=>Q) and P) => Q
let modus_ponens statement axioms =
    match statement with
    | Implies (S1,S2) -> 
        if eval axioms S1 then
            if not (axioms |> Set.exists(fun ax-> ax=S2)) then
                printfn "[MP] (%O) and %O therefore %O" statement S1 S2
            S2
        else
            Null
    | _ -> Null

// modus tollens, i.e. (P=>Q) and ¬Q) => ¬P
let modus_tollens statement axioms = 
    match statement with 
    | Implies (S1,S2) ->
        if axioms |> Set.exists(fun axiom -> (simplify axiom) = (simplify (Not S2))) then
            if not (axioms |> Set.exists(fun ax-> (simplify ax)=simplify (Not S1))) then
                printfn "[MT] (%O) and %O therefore %O" statement (simplify (Not S2)) (simplify (Not S1))
            simplify (Not S1)
        else
            Null
    | _ -> Null

// apply a rule to a set of axioms to draw conclusions
let rec apply_rule rule axioms =
    axioms |> Set.map( fun axiom -> rule axiom axioms )

// apply modus_tollens then modus_ponens
let all_rules axioms = 
    apply_rule modus_ponens axioms+(apply_rule modus_tollens axioms)

// draw conclusions and print them
printfn "\nThe steps:"

let conclusions = (repeat all_rules (expand my_axioms)) |> Set.filter(fun el -> el <> Null)

printfn "\nThe conclusions:"
print_set conclusions

// checks for "zero order" contradictions, i.e. the set contains A and ¬A.
let zero_order_self_consistent statements =
    statements |> Set.forall( fun s1 -> not (statements |> Set.exists( fun s2 -> match simplify s2 with | Not(s3) when s3=(simplify s1) -> true | _ -> false )))

if not (zero_order_self_consistent conclusions) then
    printfn "Warning; the conclusions are not self-consistent.\nThis implies the axioms are contradictory somehow."

// stop the program exiting until keydown
System.Console.ReadKey() |> ignore
