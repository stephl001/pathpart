#r "packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
open Newtonsoft.Json.Linq
open System
open System.Text.RegularExpressions

type PathValue = string*Object

module PathPart = 
    let private (|CollectionProperty|_|) part =
        let m = Regex.Match(part, @"^([^\[]+)\[0\]$")
        if m.Success then Some m.Groups.[1].Value else None
    let private (|PropertyName|_|) (part:string) =
        if Regex.IsMatch(part, @"^[a-zA-Z][a-zA-Z0-9_]*$")
        then Some part
        else None

    let private jsonProperty name (value:Object) =
        new JObject(new JProperty(name, value))

    let private jsonCollection name (value:Object) =
        jsonProperty name (new JArray([|value|]))

    let rec private toJson (value:Object)  = function
    | [CollectionProperty p] -> jsonCollection p value
    | [PropertyName p] -> jsonProperty p value
    | (CollectionProperty p)::rest -> toJson value rest |> jsonCollection p 
    | (PropertyName p)::rest -> toJson value rest |> jsonProperty p
    | _ -> failwith "Oups!"

    let private toStringParts (path:string) = path.Split [|'.'|] |> List.ofArray
    let private convertPath (p,v) = p |> toStringParts |> toJson v

    let private merge (o1:JObject) (o2:JObject) = 
        o1.Merge(o2, new JsonMergeSettings(MergeArrayHandling = MergeArrayHandling.Merge))
        o1

    let private getDuplicatePaths = 
        List.groupBy fst >> List.filter (snd >> List.length >> (<>) 1) >> List.map fst
    let private validatePathValues (pv : PathValue list) = 
        let duplicates = getDuplicatePaths pv
        match duplicates with
        | [] -> pv
        | _ -> failwith (sprintf "The following keys were duplicated with different values: %A" duplicates)

    let private mergeAll = new JObject() |> List.fold merge
    let handlePathValueList = validatePathValues >> List.map convertPath >> mergeAll