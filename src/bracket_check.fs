
let valid_example = "([<{}>()])"
let invalid_example = "([)]"

exception ListException

(*
 * List operations
 *)
let front x = 
    match x with
    | [head] -> head
    | head :: tail -> head
    | _ -> raise ListException

let pop x =
    match x with
    | [tail] -> []
    | head :: tail -> tail
    | _ -> raise ListException

(*
 * Bracket types
 *) 
type Brackets = 
    | Paren  = 0
    | Curly = 1
    | Square = 2
    | Angle  = 3

(*
 * Checks if the brackets are properly matched and nested in a string.
 *)
let is_valid str =
    let stack = ref [] // mutable ref - so it can be captured by closure

    let handle_bracket open_char close_char token char =
        if char=open_char then
            stack := token :: !stack
        elif char=close_char then
            if (front !stack)=token then
                stack := pop !stack
            else
                raise ListException
        char

    try
        str |> Seq.iter (fun char -> 
                char 
                |> handle_bracket '(' ')' Brackets.Paren
                |> handle_bracket '{' '}' Brackets.Curly
                |> handle_bracket '[' ']' Brackets.Square
                |> handle_bracket '<' '>' Brackets.Angle
                |> ignore
            )

        if List.length(!stack)=0 then
            true
        else
            false
    with
    | ListException -> false

// sanity checks
assert (is_valid valid_example)
assert not (is_valid invalid_example)
    
// an example
let example = "template<> double test<double>(std::vector<int[3]>& x, std::function<int(int[3])>& y);"
printfn "%A" (is_valid example)
