namespace CalcPELibrary

module Symbolic = 

    //Lots of info pulled from http://www.codeproject.com/Articles/87294/Symbolic-Calculation-in-F

    //This is called a "discriminated union," and my best understanding is that it using type inference to describe syntax
    type expr = 
        | X                         //allows for a variable
        | Const of float           //describes numerical constants as doubles
        | Add of expr * expr
        | Sub of expr * expr
        | Mul of expr * expr
        | Div of expr * expr


    //These "active patterns" match the syntax constructs
    let (|Op|_|) (x : expr) = 
        match x with
        | Add(e1, e2) -> Some(Add, e1, e2)
        | Sub(e1, e2) -> Some(Sub, e1, e2)
        | Mul(e1, e2) -> Some(Mul, e1, e2)
        | Div(e1, e2) -> Some(Div, e1, e2)
        | _ -> None



    let rec Simplify x : expr = 
        match x with
        | Add(Const(n1), Const(n2)) -> Const(n1 + n2)
        | Sub(Const(n1), Const(n2)) -> Const(n1 - n2)
        | Mul(Const(n1), Const(n2)) -> Const(n1 * n2)
        | Div(Const(n1), Const(n2)) -> Const(n1 / n2)
    //    | Neg(Const(0.)) -> Const(0.)
    //    | Neg(Neg(e)) -> e |> Simplify
        | Add(e, Const(0.)) -> e |> Simplify
        | Add(Const(0.), e) -> e |> Simplify
        | Add(Const(n), e) -> Add(e, Const(n)) |> Simplify
    //    | Add(e1, Neg(e2)) -> Sub(e1, e2) |> Simplify
    //    | Add(Neg(e1), e2) -> Sub(e2, e1) |> Simplify
        | Sub(e, Const(0.)) -> e |> Simplify
    //    | Sub(Const(0.), e) -> Neg(e) |> Simplify
        | Mul(e, Const(1.)) -> e |> Simplify
        | Mul(Const(1.), e) -> e |> Simplify
        | Mul(e, Const(0.)) -> Const(0.)
        | Mul(Const(0.), e) -> Const(0.)
        | Mul(e, Const(n)) -> Mul(Const(n), e) |> Simplify
        | Mul(Div(Const(n), e1), e2) -> Mul(Const(n), Div(e2, e1)) |> Simplify
        | Mul(e1, Div(Const(n), e2)) -> Mul(Const(n), Div(e1, e2)) |> Simplify
    //    | Mul(Neg(e1), e2) -> Neg(Mul(e1, e2)) |> Simplify
    //    | Mul(e1, Neg(e2)) -> Neg(Mul(e1, e2)) |> Simplify
        | Div(Const(0.), e) -> Const(0.)
        | Div(e, Const(1.)) -> e |> Simplify
    //    | Div(Neg(e1), e2) -> Neg(Div(e1, e2)) |> Simplify
    //    | Div(e1, Neg(e2)) -> Neg(Div(e1, e2)) |> Simplify
    //    | Pow(Const(0.), e) -> Const(0.)
    //    | Pow(Const(1.), e) -> Const(1.)
    //    | Pow(e, Const(0.)) -> Const(1.)
    //    | Pow(e, Const(1.)) -> e |> Simplify
        | Op (op, e1, e2)
            ->
            let e1s = Simplify e1
            let e2s = Simplify e2
            if e1s <> e1 || e2s <> e2 then
                op(Simplify e1, Simplify e2) |> Simplify
            else
                op(e1, e2)
        | _ -> x


    let OpName (e : expr) : string = 
        match e with
        | Add(e1, e2) -> "+"
        | Sub(e1, e2) -> "-"
        | Mul(e1, e2) -> "*"
        | Div(e1, e2) -> "/"
//        | Pow(e1, e2) -> "^"
        | _ -> failwith(sprintf "Unrecognized operator [%A]" e)


    let rec FormatExpression (inner : expr) : string = 
        match inner with
        | X -> "x"
        | Const(n) -> sprintf "%f" n
        | Add(e1, e2) -> "(" + FormatExpression(e1) + " " + OpName(inner) + " " + FormatExpression(e2) + ")"
//        | Neg x -> sprintf "-%s" (FormatExpression(x))
        | Op(op, e1, e2) -> "(" + FormatExpression(e1) + " " + OpName(inner) + " " + FormatExpression(e2) + ")"
//        | Func(f, e) -> FuncName(inner) (FormatExpression(e))





