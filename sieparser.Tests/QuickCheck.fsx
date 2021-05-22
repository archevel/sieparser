#r "nuget: FsCheck"
#r "nuget: FParsec"
#r "nuget: Newtonsoft.Json"
#load "../sieparser/Library.fs"
#load "CheckHelpers.fs"

open FsCheck
open sieparser.SIEParser
open sieparser.Tests.CheckHelpers
open FParsec
open Newtonsoft.Json

// let revIsOrig (xs:list<int>) = List.rev xs = xs
// Check.Quick revIsOrig
let arbVal = ParsableTransaction.String()

let test = 
    Prop.forAll arbVal (fun (x:string) -> 
        match parseSieTransaction x with  
        | Success(result, _, _) -> true
        | _ -> false
        )

Check.Quick test

let arbTransactions = ParsableTransactionList.String()
Check.Quick (Prop.forAll arbTransactions (fun (x:string) -> 
        match parseSieTransactions x with  
        | Success(result, _, _) -> true
        | Failure(e,_,_) -> printfn  "GOT ERRROR:\n%s\n%s" (JsonConvert.SerializeObject e) e; false
        )) 
// Prop.
// Check.One({} fun (p: string) -> 
//     printfn "%s" p
//     true