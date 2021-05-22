namespace sieparser.Tests

open Newtonsoft.Json
open FsCheck
open System
open sieparser.SIEParser
open FParsec

module CheckHelpers =

    let wrap i w = w + i + w

    let genMaybeWrapped gen = 
        let genWrappedAccounts = gen |> Gen.map (fun a -> wrap a "\"")
        Gen.oneof [ gen; genWrappedAccounts ]

    let maybeGen g = Gen.oneof [ Gen.constant ""; g ] 
    let makeAccount = fun ((a,b,c,d)) -> (string a) + (string b) + (string c) + (string d)
    let genAccounts = Gen.choose (0, 9) |> Gen.four |> Gen.map makeAccount

    let numGens = Gen.elements [0..99999] |> Gen.map (fun i -> i.ToString()) |> genMaybeWrapped |> Gen.two
    // generates a string of spaces and tabs
    let spaceGen = Gen.oneof [ gen {return " "}; gen {return "\t"}] 
    let nonEmptySpaceGen = spaceGen |> Gen.nonEmptyListOf |> Gen.map System.String.Concat
    let maybeEmptySpaceGen = spaceGen |> Gen.listOf |> Gen.map System.String.Concat


    let validAsciiChar = Gen.choose(0, 255) |> Gen.map char |> Gen.filter (fun c -> c = '\n' || c = '\r' || c = '\t' || not(Char.IsControl(c)) )
    let validAsciiStr = Gen.listOf validAsciiChar |> Gen.map String.Concat |> Gen.map (fun s -> s.Replace("\"", "\\\""))
    let validNonEmptyAsciiStr = Gen.nonEmptyListOf validAsciiChar |> Gen.map String.Concat
    let validNonWhiteSpaceAsciiStr = validAsciiStr |> Gen.map (fun s -> s.Replace("\t", "").Replace(" ", "").Replace("\n", "").Replace("\r", "")) |> Gen.filter (fun s -> s.Length <> 0)
    let escapedStringGen = validNonEmptyAsciiStr |> Gen.map JsonConvert.SerializeObject
    // generates a dimension-objekt pair (separated by random number of whitespaces)
    let makeDimObjektPair (d, o) s = "" + d + s + o
    let dimObjGen = Gen.map2 makeDimObjektPair numGens nonEmptySpaceGen

    // generates the objektlista string, e.g. "{1 \"89\"  \t   3000\t93\t\t}"
    let dimObjsGen = Gen.map2 (fun dimObj s -> dimObj + s) dimObjGen spaceGen |> Gen.listOf |> Gen.map System.String.Concat
    let objectListGen = Gen.map3 (fun s1 dos s2 -> "{" + s1 + dos + s2 + "}") maybeEmptySpaceGen dimObjsGen maybeEmptySpaceGen

    let decimalToRoundedString(d: decimal) = Math.Round(d, 2).ToString()

    let formatDate(d: DateTime) = d.ToString "yyyyMMdd"
    let dateGen = Arb.generate<DateTime> |> Gen.map formatDate



    let toStringGen = Gen.map (fun x -> x.ToString()) 

    let acc = genMaybeWrapped genAccounts
    let amount = Arb.generate<decimal> |> Gen.map decimalToRoundedString
    let maybeDate = maybeGen dateGen
    let quantGen = toStringGen Arb.generate<int> 
    let textGen = Gen.oneof [validNonWhiteSpaceAsciiStr |> Gen.map (fun x -> x.ToString()) ; escapedStringGen; Gen.constant ""; Gen.constant "\"\""] 
            
    let optionalsJoiner (s1, s2, s3) td tt tq ts = "" + td + s1 + tt + s2 + tq + s3 + ts
    let optionalPaddings = Gen.three nonEmptySpaceGen
    let optionalsGen = Gen.map5 optionalsJoiner optionalPaddings maybeDate textGen quantGen textGen

    let transJoiner (s1, s2, s3, s4) s0 td tt tq ts = "" + s0 + "#TRANS" + s1 + td + s2 + tt + s3 + tq + s4 + ts
    let transPaddings = Gen.four nonEmptySpaceGen
    let transGen = Gen.map6 transJoiner transPaddings spaceGen acc objectListGen amount optionalsGen

    let newlineGen = Gen.oneof [ Gen.constant "\n"; Gen.constant "\r\n"; Gen.constant "\n \n"; Gen.constant "\n\t\n"]
    let newlinesGen = Gen.nonEmptyListOf newlineGen |> Gen.map System.String.Concat 
    let makeTransactionsString transaction newlines = "" + transaction + newlines
    let transListGen = Gen.listOf <| Gen.map2 makeTransactionsString transGen newlinesGen 

    let joiner prelines postlines contents = "{" + prelines + contents + postlines + "}"
    let transactionsString = Gen.map3 joiner newlinesGen newlinesGen (transListGen |> Gen.map System.String.Concat)

    type ValidAccount = 
        static member String() =         
            Arb.fromGen (genMaybeWrapped genAccounts)

    type ValidObjectList = 
        static member String() =         
            Arb.fromGen objectListGen 

    type ParsableTransaction =
        static member String() = 
            Arb.fromGen transGen

    type ParsableTransactionList =
        static member String() = 
            Arb.fromGen transactionsString