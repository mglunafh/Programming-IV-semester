open System
open System.IO
open System.Net
open Microsoft.FSharp.Control.WebExtensions
open System.Text.RegularExpressions

let asyncFetch (url : string) = 
    async{
        let request = WebRequest.Create(url)
        let! response = request.AsyncGetResponse()
        let stream = response.GetResponseStream()
        let reader = new StreamReader(stream)
        let! html = reader.AsyncReadToEnd()
        return html}

let downloadAllLinks url = 
    async {
        let! page = asyncFetch url
        let urlRegExpr = new Regex(@"href=""http://?(\w|((?!\s|'|"")\W))*""")
        let matches = urlRegExpr.Matches(page)
        let tasks = [| for ref in matches ->  ref.Value.Substring(6, ref.Value.Length - 7) |> asyncFetch |]
        let refs = [|for ref in matches -> ref.Value.Substring(6)|]
        let! results = Async.Parallel tasks 

        for i = 0 to results.Length - 1
            do printfn "%s _contains_ %d" refs.[i] results.[i].Length}

let showInfo  = Async.RunSynchronously << downloadAllLinks 