module TinyMod

type identifier = Id of string

type expression =
     | Num of int
     | Boolean of bool
     | Plus of expression * expression
     | Equ of expression * expression
     | Not of expression
     | Read
     | Ide of identifier

type command = 
     | Assign of identifier * expression
     | Output of expression
     | If of expression * commandlist * commandlist
     | While of expression * commandlist 
and commandlist = command list

type valueOfExpr = 
    | Boolean of bool
    | Num of int

type memoryState = System.Collections.Generic.Dictionary<string, valueOfExpr>

let doReadFunction _ = 
    printf "< "
    let w = stdin.ReadLine()
    try
        let b = System.Convert.ToBoolean(w)
        Boolean (b) : valueOfExpr
    with
    | :? System.FormatException -> 
        try
            let i = System.Convert.ToInt32(w)
            Num(i) : valueOfExpr
        with
        | :? System.FormatException ->
            raise (new System.ArgumentException("Input text was not in correct format"))

let doOutputFunction (w:valueOfExpr) =
    printf "> "
    match w with
    | Boolean b -> printfn "%b" b
    | Num i -> printfn "%d" i

let rec evalExpression (w:expression) (m:memoryState) =
    match w with
    | expression.Num(i) -> (valueOfExpr.Num(i), m) 
    | expression.Boolean(b) -> (valueOfExpr.Boolean(b), m)
    | Plus (e1, e2) -> 
        let (w1, m1) = evalExpression e1 m
        let (w2, m2) = evalExpression e2 m1
        match (w1, w2) with
        | Num(intNum1), Num(intNum2) -> ((Num(intNum1 + intNum2)), m2)
        | _ -> raise (new System.ArgumentException("Binary \"+\" not used on two numbers"))
    | Equ (e1, e2) ->
        let (w1, m1) = evalExpression e1 m
        let (w2, m2) = evalExpression e2 m1
        match (w1, w2) with
        | Num(intNum1), Num(intNum2) -> ((Boolean(intNum1 = intNum2)), m2)
        | Boolean(boolV1), Boolean(boolV2) -> ((Boolean(boolV1 = boolV2)), m2)
        | _ -> raise (new System.ArgumentException("Binary \"=\" not used on two the same types"))
    | Not expr ->
        let (w1, m1) = evalExpression expr m
        match w1 with
        | Boolean(true) -> (Boolean(false), m1)
        | Boolean(false) -> (Boolean(true), m1)
        | _ -> raise (new System.ArgumentException("Unary \"not\" used on non-boolean value"))
    | Read -> (doReadFunction (), m)
    | Ide (Id(nazwa)) -> if m.ContainsKey(nazwa) then (m.[nazwa], m)
                         else raise (new System.ArgumentException("Identifier \"" + nazwa + "\" not found in memory"))

let rec interpetCommand cmd (m:memoryState) =
    match cmd with
    | Assign(Id(nazwa), wyr) -> 
        let (w1, m1) = evalExpression wyr m
        if m1.ContainsKey(nazwa) then m1.[nazwa] <- w1 ; m1
        else m1.Add(nazwa, w1) ; m1
    | Output ( wyr ) -> 
        let (w1, m1) = evalExpression wyr m
        doOutputFunction w1
        m1
    | If ( wyr, cmdThen, cmdElse) ->
        let (w1, m1) = evalExpression wyr m
        match w1 with
        | Boolean(w) -> if w then processList cmdThen m1 else processList cmdElse m1
        | _ -> raise (new System.ArgumentException("if instruction not applied on boolean value"))
    | While (wyr, cmdList) ->
        let (w1, m1) = evalExpression wyr m
        match w1 with
        | Boolean (true) -> 
            let m2 = processList cmdList m1
            interpetCommand cmd m2
        | Boolean (false) -> m1
        | _ -> raise (new System.ArgumentException("while condition not applied on boolean value"))

and processList cmdList (m:memoryState) =
    match cmdList with
    | [] -> m
    | h::t -> let m1 = interpetCommand h m
              processList t m1

let dumpMemoryState (m:memoryState) =
    printfn "Actual memory state"
    for i in m do 
        printfn "%s = %A" i.Key i.Value
