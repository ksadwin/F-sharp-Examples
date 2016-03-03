type Token = 
    |IF | WHILE | ELSE | PRINT | SCAN | TYPE_IDENT
    |OPEN_P | CLOSE_P | END_IF | END_WHILE | END_LINE
    |LIST_MUTATOR | LIST_ACCESSOR
    |IDENT | LITERAL | ASSIGN
    |BIN1_OP | BIN2_OP | BIN3_OP | UN_OP  | COMMA
    //anything invalid
    |ERROR

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

//direct path to file
let progString = System.IO.File.ReadAllText "C:\Users\Kelly\Documents\Visual Studio 2013\Projects\Lexer\LexSampleProgram.txt"

let ans = testLexer progString
printfn "%A" ans