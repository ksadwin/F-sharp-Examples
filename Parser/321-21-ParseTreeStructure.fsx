type Token = 
    //keywords
    |IF | WHILE | ELSE | PRINT | SCAN | TYPE_IDENT
    //single-char symbols
    | COMMA 
    //others
    |OPEN_P | CLOSE_P | END_IF | END_WHILE | END_LINE
    |LIST_MUTATOR | LIST_ACCESSOR
    |IDENT | LITERAL | ASSIGN
    |BIN1_OP | BIN2_OP | BIN3_OP | UN_OP
    |EMPTY
    //anything invalid
    |ERROR

//the data structure to represent our parse tree
type ParseTree = 
    |Nonterminal of string * ParseTree list
    |Terminal of Token * string


let parseTreeToString root = 
    let rec childrenToString nodeList indent = 
        match nodeList with
        |[] -> failwith "Nonterminal without terminals"
        |head::[] -> nodeToString head indent
        |head::tail -> (nodeToString head indent) + childrenToString tail indent

    and nodeToString root indent =
        match root with
        |Terminal(token, lexeme) ->  indent + "|"+ (sprintf "%A : " token) + lexeme + "\n" 
        |Nonterminal (name, children) ->
            indent+ "|"+name+"\n" + childrenToString children (indent+"|  ")
    nodeToString root "" 

    
let rec exp_piece lexlist =
    match lexlist with
    |[] -> failwith "exp_piece error: unexpected end of program"
    |head::tail ->
        let token, lexeme = head
        match token with
        |LITERAL|IDENT ->
            let terminal = Terminal(token, lexeme)
            Nonterminal("<exp_piece>", [terminal]), tail
        |UN_OP ->
            let terminalUn = Terminal(token, lexeme)
            match tail with
            |[] -> failwith "exp_piece error: unexpected end of program"
            |_ ->
                let subtree, rest = exp_piece (tail)
                Nonterminal("<exp_piece>", [terminalUn; subtree]), rest
        |_ -> failwithf "exp_piece failed: expected LITERAL, IDENT, or UN_OP; found %A" token




let rec exp1_piece lexlist =
    let subtreeExppiece, rest = exp_piece lexlist
    let subtreeRest, rest2 = exp1_rest rest
    Nonterminal("<exp1_piece>", [subtreeExppiece; subtreeRest]), rest2
and exp1_rest lexlist =
    match lexlist with
    |[] -> Nonterminal("<exp1_rest>", [Terminal(EMPTY, "")]), lexlist
    |head::tail ->
        let token, lexeme = head
        match token with
        |BIN1_OP ->
            let terminalBin1 = Terminal(List.head lexlist)
            match tail with
            |[] -> failwith "exp1_rest error: unexpected end of program"
            |_ ->
                let subtreeExp, rest = exp1_piece (List.tail lexlist)
                Nonterminal("<exp1_rest>", [terminalBin1; subtreeExp]), rest
        |_ -> Nonterminal("<exp1_rest>", [Terminal(EMPTY, "")]), lexlist



let rec exp2_piece lexlist =
    let subtreeExp1, rest = exp1_piece lexlist
    let subtreeRest, rest2 = exp2_rest rest
    Nonterminal("<exp2_piece>", [subtreeExp1; subtreeRest]), rest2
and exp2_rest lexlist =
    match lexlist with
    |[] -> Nonterminal("<exp2_rest>", [Terminal(EMPTY, "")]), lexlist
    |head::tail ->
        let token, lexeme = head
        match token with
        |BIN2_OP ->
            let terminalBin2 = Terminal(List.head lexlist)
            match tail with
            |[] -> failwith "exp2_rest error: unexpected end of program"
            |_ ->
                let subtreeExp, rest = exp2_piece (List.tail lexlist)
                Nonterminal("<exp2_rest>", [terminalBin2; subtreeExp]), rest
        |_ -> Nonterminal("<exp2_rest>", [Terminal(EMPTY, "")]), lexlist



let rec exp lexlist =
    let subtreeExp2, rest = exp2_piece lexlist
    let subtreeRest, rest2 = exp_rest rest
    Nonterminal("<exp>", [subtreeExp2; subtreeRest]), rest2
and exp_rest lexlist =
    match lexlist with
    |[] -> Nonterminal("<exp_rest>", [Terminal(EMPTY, "")]), lexlist
    |head::tail ->
        let token, lexeme = head
        match token with
        |BIN3_OP ->
            let terminalBin3 = Terminal(List.head lexlist)
            match tail with
            |[] -> failwith "exp_rest error: unexpected end of program"
            |_ ->
                let subtreeExp, rest = exp (List.tail lexlist)
                Nonterminal("<exp_rest>", [terminalBin3; subtreeExp]), rest
        |_ -> Nonterminal("<exp_rest>", [Terminal(EMPTY, "")]), lexlist


let list_mut_stmt lexlist =
    let terminalMutator = Terminal(List.head lexlist)
    match List.tail lexlist with
    |[] -> failwith "list_mut_stmt error: unexpected end of program"
    |head::tail ->
        let tokenOpen, lexemeOpen = head
        match tokenOpen with
        |OPEN_P ->
            let terminalOpen = Terminal(head)
            let subtree, rest = exp tail
            match rest with
            |[] -> failwith "list_mut_stmt error: unexpected end of program"
            |head2::tail2 ->
                let tokenClose, lexemeClose = head2
                match tokenClose with
                |CLOSE_P ->
                    let terminalClose = Terminal(head2)
                    match tail2 with
                    |[] -> failwith "list_mut_stmt error: unexpected end of program"
                    |head3::tail3 ->
                        let tokenEnd, lexemeEnd = head3
                        match tokenEnd with
                        |END_LINE ->
                            let terminalEnd = Terminal(head3)
                            Nonterminal("<list_mut_stmt>", [terminalMutator; terminalOpen; subtree; terminalClose; terminalEnd]), tail3
                        |_ -> failwithf "list_mut_stmt error: expected END, found %A" tokenEnd
                |_ -> failwithf "list_mut_stmt error: expected CLOSE_P, found %A" tokenClose
        |_ -> failwithf "list_mut_stmt error: expected OPEN_P, found %A" tokenOpen


let assn_stmt lexlist =
    let terminalAssign = Terminal(List.head lexlist)
    let subtree, rest = exp (List.tail lexlist)
    match rest with
    |[] -> failwith "assn_stmt error: unexpected end of program"
    |head::tail ->
        let terminalEnd = Terminal(head)
        Nonterminal("<assn_stmt>", [terminalAssign; subtree; terminalEnd]), tail


let choose_assn_or_list lexlist =
    match lexlist with
    |[] -> failwith "choose_assn_or_list error: unexpected end of program"
    |head::tail ->
        let token, lexeme = head
        match token with
        |ASSIGN ->
            let subtreeAssn, rest = assn_stmt lexlist
            Nonterminal("<choose_assn_or_list>", [subtreeAssn]), rest
        |LIST_MUTATOR ->
            let subtreeMut, rest = list_mut_stmt lexlist
            Nonterminal("<choose_assn_or_list>", [subtreeMut]), rest
        |_ -> failwithf "choose_assn_or_list error: expected ASSIGN or LIST_MUTATOR, found %A" token


let assn_or_list lexlist =
    let terminalIdent = Terminal(List.head lexlist)
    let subtreeChoose, rest = choose_assn_or_list (List.tail lexlist)
    Nonterminal("<assn_or_list>", [terminalIdent; subtreeChoose]), rest   


let assn_mod lexlist =
    let token, lexeme = List.head lexlist
    match token with
    |ASSIGN ->
        let terminalAssn = Terminal(List.head lexlist)
        match List.tail lexlist with
        |[] -> failwith "assn_mod error: unexpected end of program"
        |_ ->
            let subtreeExp, rest = exp (List.tail lexlist)
            Nonterminal("<assn_mod>", [terminalAssn; subtreeExp]), rest
    |_ ->
        Nonterminal("<assn_mod>", [Terminal(EMPTY, "")]), lexlist


let declare_stmt lexlist = 
    let terminalType = Terminal(List.head lexlist)
    match List.tail lexlist with
    |[] -> failwith "declare_stmt error: unexpected end of program"
    |head::tail ->
        let tokenIdent, lexemeIdent = head
        match tokenIdent with
        |IDENT ->
            let terminalIdent = Terminal(head)
            match tail with
            |[] -> failwith "declare_stmt error: unexpected end of program"
            |_ ->
                let subtreeMod, rest = assn_mod tail
                match rest with
                |[] -> failwith "declare_stmt error: unexpected end of program"
                |head2::tail2 ->
                    let tokenEnd, lexemeEnd = head2
                    match tokenEnd with
                    |END_LINE ->
                        let terminalEnd = Terminal(head2)
                        Nonterminal("<declare_stmt>", [terminalType; terminalIdent; subtreeMod; terminalEnd]), tail2
                    |_ -> failwithf "declare_stmt error: expected END_LINE, found %A" tokenEnd
        |_ -> failwithf "declare_stmt error: expected IDENT, found %A" tokenIdent


let rec to_print_rest lexlist =
    let token, lexeme = List.head lexlist
    match token with
    |COMMA ->
        let terminalComma = Terminal(token, lexeme)
        match List.tail lexlist with
        |[] -> failwith "to_print_rest error: unexpected end of program"
        |_ ->
            let subtreeExp, rest = exp (List.tail lexlist)
            match rest with
            |[] -> failwith "to_print_rest error: unexpected end of program"
            |_ ->
                let subtreeRest, rest2 = to_print_rest rest
                Nonterminal("<to_print_rest>", [terminalComma; subtreeExp; subtreeRest]), rest2
    |_ ->
        Nonterminal("<to_print_rest>", [Terminal(EMPTY, "")]), lexlist


let to_print lexlist =
    match lexlist with
    |[] -> failwith "to_print error: unexpected end of program"
    |_ ->
        let subtreeExp, rest = exp lexlist
        match rest with
        |[] -> failwith "to_print error: unexpected end of program"
        |head::tail ->
            let token, lexeme = head
            match token with
            |COMMA ->
                let subtreeRest, rest2 = to_print_rest rest
                Nonterminal("<to_print>", [subtreeExp;subtreeRest]), rest2
            |_ ->
                Nonterminal("<to_print>", [subtreeExp]), rest


let print_stmt lexlist =
    let terminalPrint = Terminal(List.head lexlist)
    match List.tail lexlist with
    |[] -> failwith "print_stmt error: unexpected end of program"
    |head::tail ->
        let tokenOpen, lexemeOpen = head
        match tokenOpen with
        |OPEN_P -> 
            let terminalOpen = Terminal(head)
            match tail with
            |[] -> failwith "print_stmt error: unexpected end of program"
            |_ ->
                //to_print
                let subtreePrint, rest = to_print tail
                match rest with
                |[] -> failwith "print_stmt error: unexpected end of program"
                |head2::tail2 ->
                    let tokenClose, lexemeClose = head2
                    match tokenClose with
                    |CLOSE_P ->
                        let terminalClose = Terminal(head2)
                        match tail2 with
                        |[] -> failwith "print_stmt error: unexpected end of program"
                        |head3::tail3 ->
                            let tokenEnd, lexemeEnd = head3
                            match tokenEnd with
                            |END_LINE ->
                                let terminalEnd = Terminal(head3)
                                Nonterminal("<print_stmt>", [terminalPrint; terminalOpen; subtreePrint; terminalClose; terminalEnd]), tail3
                            |_ -> failwithf "print_stmt error: expected END_LINE, found %A" tokenEnd
                    |_ -> failwithf "print_stmt error: expected CLOSE_P, found %A" tokenClose
        |_ -> failwithf "print_stmt error: expected OPEN_P, found %A" tokenOpen


let scan_stmt lexlist =
    let terminalScan = Terminal(List.head lexlist)
    match List.tail lexlist with
    |[] -> failwith "scan_stmt error: unexpected end of program"
    |headOpen::tailOpen ->
        let tokenOpen, lexemeOpen = headOpen
        match tokenOpen with
        |OPEN_P ->
            let terminalOpen = Terminal(headOpen)
            match tailOpen with
            |[] -> failwith "scan_stmt error: unexpected end of program"
            |headIdent::tailIdent ->
                let tokenIdent, lexemeIdent = headIdent
                match tokenIdent with
                |IDENT ->
                    let terminalIdent = Terminal(headIdent)
                    match tailIdent with
                    |[] -> failwith "scan_stmt error: unexpected end of program"
                    |headClose::tailClose ->
                        let tokenClose, lexemeClose = headClose
                        match tokenClose with
                        |CLOSE_P ->
                            let terminalClose = Terminal(headClose)
                            match tailClose with
                            |[] -> failwith "scan_stmt error: unexpected end of program"
                            |headEnd::tailEnd ->
                                let tokenEnd, lexemeEnd = headEnd
                                match tokenEnd with
                                |END_LINE ->
                                    let terminalEnd = Terminal(headEnd)
                                    Nonterminal("<scan_stmt>", [terminalScan; terminalOpen; terminalIdent; terminalClose; terminalEnd]), tailEnd
                                |_ -> failwithf "scan_stmt error: expected END_LINE, found %A" tokenEnd
                        |_ -> failwithf "scan_stmt error: expected CLOSE_P, found %A" tokenClose
                |_ -> failwithf "scan_stmt error: expected IDENT, found %A" tokenIdent
        |_ -> failwithf "scan_stmt error: expected OPEN_P, found %A" tokenOpen


let rec stmts lexlist =
    //LOOK FOR THE RIGHT TOKENS BEFORE GOING TO STMT, STMTS COULD BE EMPTY!!!!!
    match lexlist with
    |[] -> Nonterminal("<stmts>", [Terminal(EMPTY, "")]), lexlist
    |head::tail ->
        let token, lexeme = head
        match token with
        |TYPE_IDENT|IDENT|IF|WHILE|PRINT|SCAN ->
            let subtreeStmt, rest = stmt lexlist
            let subtreeStmts, rest2 = stmts rest
            Nonterminal("<stmts>", [subtreeStmt; subtreeStmts]), rest2
        |_ -> Nonterminal("<stmts>", [Terminal(EMPTY, "")]), lexlist
and stmt lexlist =
    let token, lexeme = List.head lexlist
    match token with
    |TYPE_IDENT ->
        let subtree, rest = declare_stmt lexlist
        Nonterminal("<stmt>", [subtree]), rest
    |IDENT ->
        let subtree, rest = assn_or_list lexlist
        Nonterminal("<stmt>", [subtree]), rest
    |IF ->
        let subtree, rest = if_stmt lexlist
        Nonterminal("<stmt>", [subtree]), rest
    |WHILE -> 
        let subtree, rest = while_loop lexlist
        Nonterminal("<stmt>", [subtree]), rest
    |PRINT ->
        let subtree, rest = print_stmt lexlist
        Nonterminal("<stmt>", [subtree]), rest
    |SCAN -> 
        let subtree, rest = scan_stmt lexlist
        Nonterminal("<stmt>", [subtree]), rest
    |_ -> failwithf "stmt failed: expected TYPE_IDENT, IDENT, IF, WHILE, PRINT, or SCAN; found %A" token
and if_stmt lexlist =
    let terminalIf = Terminal(List.head lexlist)
    match List.tail lexlist with
    |[] -> failwith "if_stmt error: unexpected end of program"
    |_ ->
        let subtreeExp, rest = exp (List.tail lexlist)
        match rest with
        |[] -> failwith "if_stmt error: unexpected end of program"
        |_ ->
            let subtreeStmts, rest2 = stmts rest
            let subtreeRest, rest3 = if_rest rest2
            Nonterminal("<if_stmt>", [terminalIf; subtreeExp; subtreeStmts; subtreeRest]), rest3
and if_rest lexlist =
    match lexlist with
    |[] -> failwith "if_rest error: unexpected end of program"
    |head::tail ->
        let token, lexeme = head
        match token with
        |END_IF ->
            let terminalEnd = Terminal(head)
            Nonterminal("<if_rest>", [terminalEnd]), tail
        |ELSE ->
            let terminalElse = Terminal(head)
            let subtreeStmts, rest = stmts tail
            match rest with
            |[] -> failwith "if_rest error: unexpected end of program"
            |headEnd::tailEnd ->
                let tokenEnd, lexemeEnd = headEnd
                match tokenEnd with
                |END_IF ->
                    let terminalEnd = Terminal(headEnd)
                    Nonterminal("<if_rest>", [terminalElse; subtreeStmts; terminalEnd]), tailEnd
                |_ -> failwithf "if_rest error: expected END_IF, found %A" (tokenEnd)
        |_ -> failwithf "if_rest error: expected END_IF or ELSE, found %A" (token)
and while_loop lexlist =
    let terminalWhile = Terminal(List.head lexlist)
    match List.tail lexlist with
    |[] -> failwith "while_loop error: unexpected end of program"
    |_ ->
        let subtreeExp, restExp = exp (List.tail lexlist)
        match restExp with
        |[] -> failwith "while_loop error: unexpected end of program"
        |_ ->
            let subtreeStmts, restStmts = stmts restExp
            match restStmts with
            |[] -> failwith "while_loop error: unexpected end of program"
            |head::tail ->
                let token, lexeme = head
                match token with
                |END_WHILE ->
                    let terminalEnd = Terminal(head)
                    Nonterminal("<while_loop>", [terminalWhile; subtreeExp; subtreeStmts; terminalEnd]), tail
                |_ -> failwithf "while_loop error: expected END_WHILE, found %A" token


//finish lexing a string literal 
let rec stringLiteral lexSoFar (str : string) =
    match str with
    |"" -> ERROR, lexSoFar, str
    | _ ->
        match str.[0] with
        |'"' -> LITERAL, lexSoFar+str.[0..0], str.[1..]
        | _ -> stringLiteral (lexSoFar+str.[0..0]) str.[1..]

//lex an ident - has already observed the # (lexSoFar)
let rec ident lexSoFar (str : string) =
    match ((str.[0] >= 'A' && str.[0] <= 'Z')
         ||(str.[0] >= 'a' && str.[0] <= 'z')
         ||(str.[0] >= '0' && str.[0] <= '9')) with
    |false ->
        match str.[0..0] with
        |"_" -> ident (lexSoFar+str.[0..0]) str.[1..]
        |_ -> IDENT, lexSoFar, str
    |true -> ident (lexSoFar+str.[0..0]) str.[1..]

//see if next three letters are one of our keywords. thanks to team spoookyC for this shortcut
let keywordLookup threeLetterLex rest =
    match threeLetterLex with
    |"and" | "oor" -> BIN3_OP, threeLetterLex, rest
    |"boo" | "dub" | "flo" | "int" | "lst" | "str" -> TYPE_IDENT, threeLetterLex, rest
    |"els" -> ELSE, threeLetterLex, rest
    |"fls" | "tru" -> LITERAL, threeLetterLex, rest
    |"iff" -> IF, threeLetterLex, rest
    |"not" -> UN_OP, threeLetterLex, rest
    |"prt" -> PRINT, threeLetterLex, rest
    |"skn" -> SCAN, threeLetterLex, rest
    |"wyl" -> WHILE, threeLetterLex, rest
    | _ -> ERROR, threeLetterLex.[0..0], (threeLetterLex.[1..2]+rest)

//match everything after a : (lexSoFar) with one of the ending tokens
let endLookup lexSoFar rest =
    match rest with
    |"" -> ERROR, lexSoFar, rest
    |_ ->
        match rest.[0..0] with
        |")" -> CLOSE_P, ":)", rest.[1..]
        |"'" ->
            match rest.[1..1] with
            |"{" -> END_LINE, ":'{", rest.[2..]
            |_ -> ERROR, lexSoFar, rest
        |"^" -> 
            match rest.[1..1] with
            |"|" -> END_IF, ":^|", rest.[2..]
            |"{" -> END_WHILE, ":^{", rest.[2..]
            |_ -> ERROR, lexSoFar, rest
        |_ -> ERROR, lexSoFar, rest

//referenced by numberizer and listOperator, identifies both ints and floats using flag
let rec numberizerer lexSoFar (rest : string) hasFloatingPoint =
        match rest.[0..0] with
        |"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9" -> numberizerer (lexSoFar+rest.[0..0]) rest.[1..] hasFloatingPoint
        |"." ->
            match hasFloatingPoint with
            |true -> ERROR, (lexSoFar+rest.[0..0]), rest.[1..]
            |false -> numberizerer (lexSoFar+rest.[0..0]) rest.[1..] true
        |_ -> LITERAL, lexSoFar, rest

//stub function for numberizerer
let numberizer lexSoFar rest = 
    numberizerer lexSoFar rest false

//see if lexeme following . (lexSoFar) is float or list operator or neither
let listOperator lexSoFar (rest : string) =
    match rest.[0..0] with
    |"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9" ->
        //make sure it is not actually a float!
        numberizerer (lexSoFar+rest.[0..0]) rest.[1..] true
    |_ ->
        //ensure that there are three more chars to check
        try
            match rest.[0..2] with
                |"app" | "rmv" -> LIST_MUTATOR, lexSoFar+rest.[0..2], rest.[3..]
                |"len" | "get" -> LIST_ACCESSOR, lexSoFar+rest.[0..2], rest.[3..]
                |_ -> ERROR, lexSoFar, rest
        with
        | :?System.ArgumentOutOfRangeException -> ERROR, lexSoFar, rest


//the big deal. the one. the LEXER
let rec nextLex progStr =
    match progStr with
    |"" -> failwith "Can't lex an empty string"
    |_ ->
        let currStr = string progStr.[0]
        let rest = progStr.[1..]
        try
            //find which arrow to follow in the FSM...
            match currStr with
            |" " | "\n" | "\r" | "\t" -> nextLex rest //ignore whitespace. RECURSIVE!!
            |"\"" -> stringLiteral currStr rest
            |"#" -> ident currStr rest
            |"+" -> BIN2_OP, currStr, rest
            | "*" | "/" -> BIN1_OP, currStr, rest
            |"<" | ">" ->
                match rest.[0..0] with
                |"=" -> BIN3_OP, currStr+rest.[0..0], rest.[1..]
                |_ -> BIN3_OP, currStr, rest
            |"=" ->
                match rest.[0..0] with
                |"=" -> BIN3_OP, currStr+rest.[0..0], rest.[1..]
                |_ -> ERROR, currStr, rest
            |"-" -> UN_OP, currStr, rest
            |"," -> COMMA, currStr, rest
            |"a" | "b" | "d" | "e" | "f" | "i" | "l" | "n" | "o" | "p" | "s" | "t" | "w" ->
                keywordLookup (currStr+rest.[0..1]) rest.[2..]
                //possible first letters of keywords
            |"(" -> //check if OPEN_P
                match rest.[0..0] with
                |":" -> OPEN_P, "(:", rest.[1..]
                |_ -> ERROR, currStr, rest
            |":" -> endLookup currStr rest
            |"X" -> //check if ASSIGN
                match rest.[0..1] with
                |"^O" -> ASSIGN, "X^O", rest.[2..]
                |_ -> ERROR, currStr, rest
            |"." -> listOperator currStr rest
            |"0"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"9" -> numberizer currStr rest
            |_ -> ERROR, currStr, rest
        with
        | :?System.ArgumentOutOfRangeException -> ERROR, currStr, rest


let testLexer program = 
    let rec testLexerHelper remainingProgram currLexList = 
        let token, lexeme, rest = nextLex remainingProgram
        //printf "%A : %s" token lexeme
        match rest with
            |"" ->List.rev ((token, lexeme)::currLexList)
            |_ -> testLexerHelper rest ((token, lexeme)::currLexList)
    testLexerHelper program []

let buildTree lexlist =
    match lexlist with
    |[] -> failwith "why did you try to parse an empty list"
    |_ ->
        let tree, unnecessary = stmts lexlist
        tree


//direct path to file
let progString = System.IO.File.ReadAllText "C:\Users\Kelly\Documents\Visual Studio 2013\Projects\Lexer\LexSampleProgram.txt"

let lexlist = testLexer progString
(*
let lexlist = [(TYPE_IDENT, "int"); (IDENT, "#donald_trump"); (END_LINE, ":'{");
               (SCAN, "skn"); (OPEN_P, "(:"); (IDENT, "#donaldTrump"); (CLOSE_P, ":)");
               (END_LINE, ":'{"); (TYPE_IDENT, "flo"); (IDENT, "#years"); (ASSIGN, "X^O");
               (LITERAL, "0.01"); (END_LINE, ":'{"); (TYPE_IDENT, "lst");
               (IDENT, "#yearList"); (END_LINE, ":'{"); (IF, "iff");
               (IDENT, "#donald_trump"); (BIN3_OP, "=="); (LITERAL, "2016");
               (BIN3_OP, "and"); (IDENT, "#years"); (BIN3_OP, "<="); (LITERAL, "4.01");
               (WHILE, "wyl"); (IDENT, "#years"); (BIN3_OP, "<"); (LITERAL, "4.01");
               (PRINT, "prt"); (OPEN_P, "(:"); (LITERAL, "\"Year \""); (COMMA, ",");
               (IDENT, "#years"); (BIN2_OP, "+"); (IDENT, "#donaldTrump"); (COMMA, ",");
               (LITERAL, "\"Still the Donald\""); (CLOSE_P, ":)"); (END_LINE, ":'{");
               (IDENT, "#yearList"); (LIST_MUTATOR, ".app"); (OPEN_P, "(:");
               (IDENT, "#years"); (BIN2_OP, "+"); (IDENT, "#donaldTrump"); (CLOSE_P, ":)");
               (END_LINE, ":'{"); (IDENT, "#years"); (ASSIGN, "X^O"); (IDENT, "#years");
               (BIN2_OP, "+"); (LITERAL, "1"); (END_LINE, ":'{"); (END_WHILE, ":^{");
               (PRINT, "prt"); (OPEN_P, "(:"); (LITERAL, "\"All the years:\""); (COMMA, ",");
               (IDENT, "#yearList"); (CLOSE_P, ":)"); (END_LINE, ":'{"); (ELSE, "els");
               (IDENT, "#years"); (ASSIGN, "X^O"); (IDENT, "#years"); (BIN2_OP, "+");
               (UN_OP, "-"); (LITERAL, "15"); (END_LINE, ":'{"); (TYPE_IDENT, "boo");
               (IDENT, "#2much"); (ASSIGN, "X^O"); (UN_OP, "not"); (LITERAL, "fls");
               (BIN3_OP, "oor"); (LITERAL, "tru"); (END_LINE, ":'{"); (PRINT, "prt");
               (OPEN_P, "(:"); (LITERAL, "\"Not even counting…\""); (CLOSE_P, ":)");
               (END_LINE, ":'{"); (END_IF, ":^|")]
*)
let tree = buildTree lexlist
let str = parseTreeToString tree
printf "%s" str

