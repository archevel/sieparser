namespace sieparser

open System
open System.Globalization
open FParsec


module SIEParser =
    type Account = int
    type Amount = decimal
    type Date = System.DateTime
    type DimNumber = int
    type ObjectNumber = int
    type DimObjectPair = DimNumber * ObjectNumber
    type ObjectListResult = Result<DimObjectPair list, string>
    type Quantity = int
    type TransactionKind = TRANS | BTRANS | RTRANS 
    type Transaction = {Kind: TransactionKind; Account: Account; ObjectList: ObjectListResult;  Amount: Amount; Optionals: (Date option * string option * Quantity option * string option)}
    type VerNumber = int
    type Serie = string
    type Verification = {Serie: Serie; VerNumber: VerNumber; VerDate: Date; VerText: string; Transactions: Transaction list; RegDate: Date option; Sign: string option}

    let makeTransaction kind acc ol am opts = { Kind = kind; Account = acc; ObjectList = ol; Amount = am; Optionals = opts }

    let sieField p = p <|> between (pchar '"') (pchar '"') p 
    let sieDate =
        let eightDigits = manyMinMaxSatisfy 8 8 isDigit
        sieField eightDigits >>= fun str ->            
           try preturn (DateTime.ParseExact(str, "yyyyMMdd", CultureInfo.InvariantCulture): Date)               
           with _ -> fail "Date format error"

    let parseSieDate = run sieDate

    let sieAmount =
        let amountLiteralOpts = NumberLiteralOptions.AllowFraction ||| NumberLiteralOptions.AllowMinusSign
        let pamount = numberLiteral amountLiteralOpts "amount" 
        sieField pamount |>> fun a -> (decimal a.String): Amount
    let parseSieAmount = run sieAmount

    
    let sieAccount = 
        let pAccount = numberLiteral NumberLiteralOptions.None "account"
        sieField pAccount |>> fun a -> int(a.String): Account
    let parseSieAccount = run sieAccount

    let sieObjektLista = 
        let pnum = numberLiteral NumberLiteralOptions.None "dimOrObj"
        let pobj = (sieField pnum) |>> fun x -> (int x.String): ObjectNumber 
        let pdim = (sieField pnum) |>> fun x -> (int x.String): DimNumber

        let pdimObj = (pdim .>> spaces1) .>>. (pobj .>> spaces )|>> DimObjectPair

        let toValidResult = ObjectListResult.Ok 

        let pdimObjs = many pdimObj
        let validList = spaces >>. pdimObjs .>> spaces .>> (pchar '}') |>> toValidResult

        let toInvalidResult(str:string)  = 
            printfn "trying %s" str
            match str.Trim(' ')  with 
            | "" -> ObjectListResult.Ok []
            | _ -> ObjectListResult.Error str

        let invalidList = many1Chars (noneOf "}") .>> (pchar '}') |>> toInvalidResult
        let pcontent =  (attempt validList) <|> invalidList
        (pchar '{') >>.  pcontent


        // pObjektLista >>= fun x -> 
        //     List.map fun (d, o) -> DimObjektPair (d, o) 

    let parseSieObjektLista = run sieObjektLista

    let emptyLine = newline >>. spaces
    let stringLiteral =
        let str s = pstring s
        let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
        let escapedChar = str "\\" >>. (anyOf "\\\"nrt" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c   -> string c)
        between (str "\"") (str "\"")
                (stringsSepBy normalCharSnippet escapedChar)

    let nonBlank(s:string) = if String.IsNullOrWhiteSpace(s) then None else Some s
    // "	#TRANS 1930 {} -1043.00 20200101 "någontext" -3 ttr"
    let transactionLike(kind: TransactionKind) = 
        let ptransLabel = spaces >>. pstring ("#" + kind.ToString()) 
        let transAccount = spaces >>. sieAccount
        let transObjektLista = spaces >>. sieObjektLista
        let transAmount = spaces >>. sieAmount
        
        let transDate = spaces1 >>. sieDate
        let transText = spaces1 >>. stringLiteral 
        let transQuant = spaces1 >>. sieField (numberLiteral NumberLiteralOptions.AllowMinusSign "account") |>> fun a -> int(a.String): Quantity
        let transSign = spaces1 >>. stringLiteral <|> restOfLine false
        let transOptionalsPresent = tuple4 (opt transDate) (opt transText) (opt transQuant) (opt transSign) 
        let fallback = restOfLine false |>> fun text -> (None, nonBlank text, None, None): Date option * string option* Quantity option * string option
        let transOptionals = (attempt transOptionalsPresent) <|> fallback

        ptransLabel >>. (pipe4 transAccount transObjektLista transAmount transOptionals (makeTransaction kind))
    
    let sieTransaction = (attempt <| transactionLike TRANS ) <|> (attempt <| transactionLike BTRANS)  <|> (attempt <| transactionLike RTRANS)
    let parseSieTransaction = run sieTransaction

    let sieTransactions = 
        let transactions = (many (sieTransaction .>> (many emptyLine))) .>> (pchar '}') //|>> List.reduce (@)
        let noTransactions _ :list<Transaction> = [] 
        pstring "{" >>. (many emptyLine) >>. spaces >>. (attempt transactions <|> ((pchar '}') |>> noTransactions) ) 

    let parseSieTransactions = run sieTransactions 

    let sieVerification = 
        // #VER "V" "42" 20190423 "Payment V41:1043,00" 20210301
        let label = pstring "#VER" 
        
        let serie = spaces1 >>. ((attempt stringLiteral) <|> ((many1 <| noneOf "\t ") |>> String.Concat)) |>> fun s -> s:Serie
        let num = numberLiteral NumberLiteralOptions.None "vernum"
        let vernum = spaces1 >>. sieField num |>> fun a -> int(a.String): VerNumber
        let verDate = spaces1 >>. sieDate
        let verText = spaces1 >>. ((attempt stringLiteral) <|> ((many1 <| noneOf "\t ") |>> String.Concat))
        let regDate = opt verDate
        let sign = restOfLine true
        
        let verOptionals = (regDate .>>. sign)
        let verMandatories = (label >>. tuple4 serie vernum verDate verText)
        let ver = (tuple3 verMandatories verOptionals ((many emptyLine) >>. sieTransactions))

        ver |>> fun (((s, vn, vd, vt), (rd, sign), trans)) -> {
            Serie = s
            VerNumber = vn
            VerDate = DateTime.Now
            VerText = vt
            Transactions = trans
            RegDate = rd
            Sign = if String.IsNullOrWhiteSpace sign then None else Some sign
        }

        
        
        //  |>> fun _ -> 

    let parseSieVerification = run sieVerification