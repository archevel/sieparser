namespace sieparser.Tests

open System
open System.Globalization

open Newtonsoft.Json
open NUnit.Framework
open sieparser
open FParsec
open FsCheck
open FsCheck.NUnit
open CheckHelpers

module PrimitiveParserTests =
    [<SetUp>]
    let Setup () =
        ()

    let expectSuccess p str = 
        match run p str with
        | Success(result, _, _)   -> Assert.Pass "expected success and got it"
        | Failure(errorMsg, _, _) -> Assert.Fail errorMsg

    let doParse p str = 
        match run p str with
        | Failure(eos, _, _)        -> 
            printfn "from %s eos: %s" str eos
            None
        | Success(result, _, _)   -> Some(result)

    [<Test>]
    let SieDateParseSucceedsFor ([<Values("00010101", "20190423", "20201223", "99991231")>] x, [<Values("","\"")>] w) =
        let p = SIEParser.sieDate
        let input = wrap x w  
        doParse p input |> Option.isSome |> Assert.IsTrue
        

    [<Test>]
    let SieDateParseFailsFor ([<Values("", "00000101", "20201232", "20201301", "a")>] x: string) =
        let p = SIEParser.sieDate
        doParse p x |> Option.isSome |> Assert.IsFalse


    [<Test>]
    let should_parse_all_dates_with_format_yyyyMMdd([<Values("","\"")>] w) = 
        let dateTest (d: DateTime) =
            let input = wrap (d.ToString("yyyyMMdd", CultureInfo.InvariantCulture)) w
            let expected = new DateTime(d.Year, d.Month, d.Day)
            match SIEParser.parseSieDate input with 
            | Success(actual, _, _) -> actual = expected
            |  _ -> false
        
        Check.QuickThrowOnFailure dateTest

    [<Test>]
    let should_parse_amounts_witha_precision_of_2_places([<Values("","\"")>] w) =
        let amountTest (d: decimal) = 
            let expected = Math.Round(d, 2)
            let input = wrap (expected.ToString()) w
            match SIEParser.parseSieAmount input with 
            | Success(actual, _, _) -> actual = expected
            |  _ -> false
        Check.QuickThrowOnFailure amountTest

    [<Property(Arbitrary=[| typeof<ValidAccount> |])>]
    let should_parses_accounts_from_0000_to_9999 accString =
        let cleaned = ("" + accString).Trim('"')
        let expected = int cleaned
        printfn "Got accString %s" accString
        let input = accString
        match SIEParser.parseSieAccount input with 
        | Success(actual, _, _) -> actual = expected
        |  _ -> false
        
    [<Test>]
    let should_parse_sample_list() = 
        let x = "{       }"
        let y = """{ a}"""
     //   let y = """{ 53735        "7785"  86231   "36732"         }"""
        let p = SIEParser.sieObjektLista
        let res = doParse p y 
        match res with 
        | Some(Result.Ok(ol)) -> Assert.Pass()
        | Some(Result.Error(bad)) -> Assert.Pass()
        | _ -> Assert.Fail "weird!!"
        
        //res |> Option.isSome |> Assert.IsTrue    


    [<Property(Arbitrary=[| typeof<ValidObjectList> |])>]
    let should_parse_valid_objektlista ol = 
        let res = SIEParser.parseSieObjektLista ol 
        match res with 
        | Success(Result.Ok(l), _, _) -> true
        | _ -> false

    [<Property>]
    let should_parse_invalid_objektlista(invalid: NonNull<string>) =
        let clean = invalid.Get.Replace("}", "").Replace("\n", "").Replace("\r", "").Trim('{')
        let res = SIEParser.parseSieObjektLista ("{" + clean + "}")
        match res with 
        | Success(Result.Error(str), _, _) -> clean.Length = str.Length
        | Success(Result.Ok(_), _, _) -> true
        | _ -> false

    [<Test>]
    let should_parse_sample_transaction_i_like() = 
        //let input = "	#TRANS 1930 {} -1043.00 20200101 någontext -3 ttr"
        let input = """{
    	#TRANS 1930 {} -1043.00 20200101 någontext -3 ttr
            #TRANS  4831 {} 0               "!" 0

    #TRANS 1930 {} -1043.00 20200101 någontext -3 ttr        
            #TRANS  4831 {} 0               "!" 0



            #TRANS  4831 {} 0               "!" 0



    }"""

        let p = SIEParser.sieTransactions
        doParse p input |> Option.isSome  |> Assert.IsTrue


    [<Property(Arbitrary=[| typeof<ParsableTransaction> |])>]
    let should_parse_valid_transaction t = 
        let res = SIEParser.parseSieTransaction t 
        match res with 
        | Success(tr, _, _) -> true
        | _ -> false


    [<Property(Arbitrary=[| typeof<ParsableTransactionList> |])>]
    let should_parse_valid_transactionList ts = 
        let res = SIEParser.parseSieTransactions ts 
        match res with 
        | Success(tr, _, _) -> true
        | _ -> false

    [<Test>]
    let should_parse_sample_transaction_list([<Values("{}","{ }","{\t}","{\n}","{\r\n}","{\n }","{\n \n}")>] samp) = 
        let res = SIEParser.parseSieTransactions samp 
        match res with 
        | Success(tr, _, _) -> Assert.IsEmpty(tr)
        | Failure(e, _, _) -> Assert.Fail("couldn't parse sample: " + e)

    [<Test>]
    let should_parse_sample_verification() =
        let samp = """#VER "V" "42" 20190423 "Payment V41:1043,00" 20210301

{
	#TRANS 1930 {} -1043.00
	#TRANS 2440 {} 1043.00
    #RTRANS 2440 {} 1043.00
    #TRANS 2440 {} 1043.00
    #BTRANS 2440 {} 1043.00
}"""
        let res = SIEParser.parseSieVerification samp 
        match res with 
        | Success(v, _, _) -> 
            Assert.AreEqual(5, v.Transactions.Length)
            Assert.AreEqual("Payment V41:1043,00", v.VerText)
            Assert.AreEqual(Some(new DateTime(2021, 03, 01)), v.RegDate)
        | Failure(e, _, _) -> Assert.Fail("couldn't parse sample: " + e)