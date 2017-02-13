module TinyMod

type identifier = Id of string

type expression =
     | Num of int
     | Boolean of bool
     | Read
     | Ide of identifier
     | Not of expression
     | Equ of expression * expression
     | Plus of expression * expression

type command = 
     | Assign of identifier * expression
     | Output of expression
     | If of expression * commandlist * commandlist
     | While of expression * commandlist 
and commandlist = command list

type valueOfExpr = 
    | Boolean of bool
    | Num of int

type memoryState = System.Collections.Generic.
                      Dictionary<string, valueOfExpr>

let doReadFunction _ = 
  printf "< "
  let w = stdin.ReadLine()
  try
    let b = System.Convert.ToBoolean(w)
    valueOfExpr.Boolean(b)
  with
  | :? System.FormatException -> 
    try
      let i = System.Convert.ToInt32(w)
      if i >= 0 then valueOfExpr.Num(i)
      else failwith "Input text not in correct format"
    with
    | :? System.FormatException ->
      failwith "Input text not in correct format"

let doOutputFunction (w:valueOfExpr) =
    printf "> "
    match w with
    | Boolean b -> printfn "%b" b
    | Num i -> printfn "%d" i

let rec evalExpression (e:expression) (s:memoryState) =
    match e with
    | expression.Num(i) -> (valueOfExpr.Num(i), s) 
    | expression.Boolean(b) -> (valueOfExpr.Boolean(b), s)
    | Read -> (doReadFunction (), s)
    | Ide (Id(name)) -> 
        if s.ContainsKey(name) 
           then (s.[name], s)
           else failwith ("Identifier \"" + name + 
                  "\" not found in memory")    
    | Not expr ->
        let (v, s1) = evalExpression expr s
        match v with
        | Boolean(true) -> (Boolean(false), s1)
        | Boolean(false) -> (Boolean(true), s1)
        | _ -> failwith ("Unary \"not\" used on" +
                         " non-Boolean value")
    | Equ (expr1, expr2) ->
        let (v1, s1) = evalExpression expr1 s
        let (v2, s2) = evalExpression expr2 s1
        match (v1, v2) with
        | Num(intNum1), Num(intNum2) -> 
           ((Boolean(intNum1 = intNum2)), s2)
        | Boolean(boolV1), Boolean(boolV2) -> 
           ((Boolean(boolV1 = boolV2)), s2)
        | _ -> failwith("Binary \"=\" not used on" +
                 " two the same types")
    | Plus (expr1, expr2) -> 
        let (v1, s1) = evalExpression expr1 s
        let (v2, s2) = evalExpression expr2 s1
        match (v1, v2) with
        | Num(intNum1), Num(intNum2) -> 
          ((Num(intNum1 + intNum2)), s2)
        | _ -> failwith("Binary \"+\" not used on" +
                 " two numbers")
//end of evalExpression

let rec interpretCommand cmd (s:memoryState) =
    match cmd with
    | Assign(Id(name), expr) -> 
        let (v, s1) = evalExpression expr s
        if s1.ContainsKey(name) then s1.[name] <- v ; s1
        else s1.Add(name, v) ; s1
    | Output ( expr ) -> 
        let (v, s1) = evalExpression expr s
        doOutputFunction v
        s1
       | If (expr, cmdThen, cmdElse) ->
       let (v, s1) = evalExpression expr s
       match v with
       | Boolean (w) -> if w then processList cmdThen s1 
                             else processList cmdElse s1
       | _ -> failwith "if condition not Boolean value"
    | While (expr, cmdList) ->
        let (v, s1) = evalExpression expr s
        match v with
        | Boolean (false) -> s1
        | Boolean (true) -> 
             let s2 = processList cmdList s1
             interpretCommand cmd s2
        | _ -> failwith "while condition not Boolean value"

and processList cmdList (s:memoryState) =
    match cmdList with
    | [] -> s
    | h::t -> let s1 = interpretCommand h s
              processList t s1
//end of interpretCommand


let dumpMemoryState (m:memoryState) =
    printfn "Actual memory state"
    for i in m do 
        printfn "%s = %A" i.Key i.Value
