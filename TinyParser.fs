// Implementation file for parser generated by fsyacc
module TinyParser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
//# 0 ""

# 8 "TinyParser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | ID of (string)
  | BOOL of (bool)
  | NUM of (int)
  | WHILE
  | DO
  | OD
  | SEMI
  | EOF
  | NOT
  | EQU
  | PLUS
  | ASSIGN
  | READ
  | OUTPUT
  | IF
  | THEN
  | ELSE
  | FI
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_ID
    | TOKEN_BOOL
    | TOKEN_NUM
    | TOKEN_WHILE
    | TOKEN_DO
    | TOKEN_OD
    | TOKEN_SEMI
    | TOKEN_EOF
    | TOKEN_NOT
    | TOKEN_EQU
    | TOKEN_PLUS
    | TOKEN_ASSIGN
    | TOKEN_READ
    | TOKEN_OUTPUT
    | TOKEN_IF
    | TOKEN_THEN
    | TOKEN_ELSE
    | TOKEN_FI
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startprogram
    | NONTERM_program
    | NONTERM_cmdlist
    | NONTERM_cmd
    | NONTERM_expr

// This function maps tokens to integers indexes
let tagOfToken (t:token) = 
  match t with
  | ID _ -> 0 
  | BOOL _ -> 1 
  | NUM _ -> 2 
  | WHILE  -> 3 
  | DO  -> 4 
  | OD  -> 5 
  | SEMI  -> 6 
  | EOF  -> 7 
  | NOT  -> 8 
  | EQU  -> 9 
  | PLUS  -> 10 
  | ASSIGN  -> 11 
  | READ  -> 12 
  | OUTPUT  -> 13 
  | IF  -> 14 
  | THEN  -> 15 
  | ELSE  -> 16 
  | FI  -> 17 

// This function maps integers indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_ID 
  | 1 -> TOKEN_BOOL 
  | 2 -> TOKEN_NUM 
  | 3 -> TOKEN_WHILE 
  | 4 -> TOKEN_DO 
  | 5 -> TOKEN_OD 
  | 6 -> TOKEN_SEMI 
  | 7 -> TOKEN_EOF 
  | 8 -> TOKEN_NOT 
  | 9 -> TOKEN_EQU 
  | 10 -> TOKEN_PLUS 
  | 11 -> TOKEN_ASSIGN 
  | 12 -> TOKEN_READ 
  | 13 -> TOKEN_OUTPUT 
  | 14 -> TOKEN_IF 
  | 15 -> TOKEN_THEN 
  | 16 -> TOKEN_ELSE 
  | 17 -> TOKEN_FI 
  | 20 -> TOKEN_end_of_input
  | 18 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startprogram 
    | 1 -> NONTERM_program 
    | 2 -> NONTERM_cmdlist 
    | 3 -> NONTERM_cmdlist 
    | 4 -> NONTERM_cmd 
    | 5 -> NONTERM_cmd 
    | 6 -> NONTERM_cmd 
    | 7 -> NONTERM_cmd 
    | 8 -> NONTERM_expr 
    | 9 -> NONTERM_expr 
    | 10 -> NONTERM_expr 
    | 11 -> NONTERM_expr 
    | 12 -> NONTERM_expr 
    | 13 -> NONTERM_expr 
    | 14 -> NONTERM_expr 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 20 
let _fsyacc_tagOfErrorTerminal = 18

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | ID _ -> "ID" 
  | BOOL _ -> "BOOL" 
  | NUM _ -> "NUM" 
  | WHILE  -> "WHILE" 
  | DO  -> "DO" 
  | OD  -> "OD" 
  | SEMI  -> "SEMI" 
  | EOF  -> "EOF" 
  | NOT  -> "NOT" 
  | EQU  -> "EQU" 
  | PLUS  -> "PLUS" 
  | ASSIGN  -> "ASSIGN" 
  | READ  -> "READ" 
  | OUTPUT  -> "OUTPUT" 
  | IF  -> "IF" 
  | THEN  -> "THEN" 
  | ELSE  -> "ELSE" 
  | FI  -> "FI" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | ID _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | BOOL _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NUM _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | WHILE  -> (null : System.Object) 
  | DO  -> (null : System.Object) 
  | OD  -> (null : System.Object) 
  | SEMI  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | NOT  -> (null : System.Object) 
  | EQU  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | ASSIGN  -> (null : System.Object) 
  | READ  -> (null : System.Object) 
  | OUTPUT  -> (null : System.Object) 
  | IF  -> (null : System.Object) 
  | THEN  -> (null : System.Object) 
  | ELSE  -> (null : System.Object) 
  | FI  -> (null : System.Object) 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 5us; 65535us; 0us; 2us; 5us; 6us; 14us; 15us; 16us; 17us; 21us; 22us; 5us; 65535us; 0us; 4us; 5us; 4us; 14us; 4us; 16us; 4us; 21us; 4us; 7us; 65535us; 8us; 9us; 10us; 11us; 12us; 13us; 19us; 20us; 28us; 29us; 32us; 30us; 33us; 31us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 9us; 15us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 2us; 2us; 3us; 1us; 3us; 1us; 3us; 1us; 4us; 1us; 4us; 3us; 4us; 13us; 14us; 1us; 5us; 3us; 5us; 13us; 14us; 1us; 6us; 3us; 6us; 13us; 14us; 1us; 6us; 1us; 6us; 1us; 6us; 1us; 6us; 1us; 6us; 1us; 7us; 3us; 7us; 13us; 14us; 1us; 7us; 1us; 7us; 1us; 7us; 1us; 8us; 1us; 9us; 1us; 10us; 1us; 11us; 1us; 12us; 3us; 12us; 13us; 14us; 3us; 13us; 13us; 14us; 3us; 13us; 14us; 14us; 1us; 13us; 1us; 14us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 11us; 13us; 15us; 17us; 19us; 23us; 25us; 29us; 31us; 35us; 37us; 39us; 41us; 43us; 45us; 47us; 51us; 53us; 55us; 57us; 59us; 61us; 63us; 65us; 67us; 71us; 75us; 79us; 81us; |]
let _fsyacc_action_rows = 34
let _fsyacc_actionTableElements = [|4us; 32768us; 0us; 7us; 3us; 19us; 13us; 10us; 14us; 12us; 0us; 49152us; 1us; 32768us; 7us; 3us; 0us; 16385us; 1us; 16386us; 6us; 5us; 4us; 32768us; 0us; 7us; 3us; 19us; 13us; 10us; 14us; 12us; 0us; 16387us; 1us; 32768us; 11us; 8us; 5us; 32768us; 0us; 27us; 1us; 25us; 2us; 24us; 8us; 28us; 12us; 26us; 2us; 16388us; 9us; 32us; 10us; 33us; 5us; 32768us; 0us; 27us; 1us; 25us; 2us; 24us; 8us; 28us; 12us; 26us; 2us; 16389us; 9us; 32us; 10us; 33us; 5us; 32768us; 0us; 27us; 1us; 25us; 2us; 24us; 8us; 28us; 12us; 26us; 3us; 32768us; 9us; 32us; 10us; 33us; 15us; 14us; 4us; 32768us; 0us; 7us; 3us; 19us; 13us; 10us; 14us; 12us; 1us; 32768us; 16us; 16us; 4us; 32768us; 0us; 7us; 3us; 19us; 13us; 10us; 14us; 12us; 1us; 32768us; 17us; 18us; 0us; 16390us; 5us; 32768us; 0us; 27us; 1us; 25us; 2us; 24us; 8us; 28us; 12us; 26us; 3us; 32768us; 4us; 21us; 9us; 32us; 10us; 33us; 4us; 32768us; 0us; 7us; 3us; 19us; 13us; 10us; 14us; 12us; 1us; 32768us; 5us; 23us; 0us; 16391us; 0us; 16392us; 0us; 16393us; 0us; 16394us; 0us; 16395us; 5us; 32768us; 0us; 27us; 1us; 25us; 2us; 24us; 8us; 28us; 12us; 26us; 0us; 16396us; 1us; 16397us; 10us; 33us; 0us; 16398us; 5us; 32768us; 0us; 27us; 1us; 25us; 2us; 24us; 8us; 28us; 12us; 26us; 5us; 32768us; 0us; 27us; 1us; 25us; 2us; 24us; 8us; 28us; 12us; 26us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 5us; 6us; 8us; 9us; 11us; 16us; 17us; 19us; 25us; 28us; 34us; 37us; 43us; 47us; 52us; 54us; 59us; 61us; 62us; 68us; 72us; 77us; 79us; 80us; 81us; 82us; 83us; 84us; 90us; 91us; 93us; 94us; 100us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 1us; 3us; 3us; 2us; 7us; 5us; 1us; 1us; 1us; 1us; 2us; 3us; 3us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 3us; 3us; 3us; 3us; 4us; 4us; 4us; 4us; 4us; 4us; 4us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 16387us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; 16390us; 65535us; 65535us; 65535us; 65535us; 16391us; 16392us; 16393us; 16394us; 16395us; 65535us; 65535us; 65535us; 65535us; 65535us; 65535us; |]
let _fsyacc_reductions ()  =    [| 
# 183 "TinyParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : TinyMod.commandlist)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startprogram));
# 192 "TinyParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'cmdlist)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 14 "TinyParser.fsp"
                                              _1 
                   )
# 14 "TinyParser.fsp"
                 : TinyMod.commandlist));
# 203 "TinyParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'cmd)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 16 "TinyParser.fsp"
                                     [_1] 
                   )
# 16 "TinyParser.fsp"
                 : 'cmdlist));
# 214 "TinyParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'cmd)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'cmdlist)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 17 "TinyParser.fsp"
                                            [_1] @ _3 
                   )
# 17 "TinyParser.fsp"
                 : 'cmdlist));
# 226 "TinyParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 20 "TinyParser.fsp"
                                          TinyMod.Assign(TinyMod.Id(_1), _3) 
                   )
# 20 "TinyParser.fsp"
                 : 'cmd));
# 238 "TinyParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 21 "TinyParser.fsp"
                                       TinyMod.Output(_2) 
                   )
# 21 "TinyParser.fsp"
                 : 'cmd));
# 249 "TinyParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'cmdlist)) in
            let _6 = (let data = parseState.GetInput(6) in (Microsoft.FSharp.Core.Operators.unbox data : 'cmdlist)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 23 "TinyParser.fsp"
                                                 TinyMod.If(_2, _4, _6) 
                   )
# 23 "TinyParser.fsp"
                 : 'cmd));
# 262 "TinyParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : 'cmdlist)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 24 "TinyParser.fsp"
                                                    TinyMod.While(_2, _4) 
                   )
# 24 "TinyParser.fsp"
                 : 'cmd));
# 274 "TinyParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 27 "TinyParser.fsp"
                                TinyMod.Num(_1) 
                   )
# 27 "TinyParser.fsp"
                 : 'expr));
# 285 "TinyParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : bool)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 "TinyParser.fsp"
                                TinyMod.Boolean(_1) 
                   )
# 28 "TinyParser.fsp"
                 : 'expr));
# 296 "TinyParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 "TinyParser.fsp"
                                TinyMod.Read 
                   )
# 29 "TinyParser.fsp"
                 : 'expr));
# 306 "TinyParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 30 "TinyParser.fsp"
                              TinyMod.Ide(TinyMod.Id(_1)) 
                   )
# 30 "TinyParser.fsp"
                 : 'expr));
# 317 "TinyParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 "TinyParser.fsp"
                                    TinyMod.Not(_2) 
                   )
# 31 "TinyParser.fsp"
                 : 'expr));
# 328 "TinyParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 "TinyParser.fsp"
                                         TinyMod.Equ(_1, _3) 
                   )
# 32 "TinyParser.fsp"
                 : 'expr));
# 340 "TinyParser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'expr)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 33 "TinyParser.fsp"
                                          TinyMod.Plus( _1, _3) 
                   )
# 33 "TinyParser.fsp"
                 : 'expr));
|]
# 353 "TinyParser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 21;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let program lexer lexbuf : TinyMod.commandlist =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
