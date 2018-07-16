#r "C:/Users/ratat/Projects/pathpart/packages/Newtonsoft.Json/lib/net45/Newtonsoft.Json.dll"
open Newtonsoft.Json.Linq

open System
open System.Text.RegularExpressions

module PathPart = 
    let (|CollectionProperty|_|) part =
        let m = Regex.Match(part, @"^([^\[]+)\[0\]$")
        if m.Success then Some m.Groups.[1].Value else None
    let (|PropertyName|_|) (part:string) =
        if Regex.IsMatch(part, @"^[a-zA-Z][a-zA-Z0-9_]*$")
        then Some part
        else None

    let split (s:string) = s.Split('.') |> List.ofArray

    let private jsonProperty name (value:Object) : JToken =
        new JObject(new JProperty(name, value)) :> _

    let private jsonCollection name (value:Object) : JToken =
        new JArray(jsonProperty name value) :> _

    let rec toJson (value:Object)  = function
    | [CollectionProperty p] -> jsonCollection p value
    | [PropertyName p] -> jsonProperty p value
    | (CollectionProperty p)::rest -> toJson value rest |> jsonCollection p 
    | (PropertyName p)::rest -> toJson value rest |> jsonProperty p
    | _ -> failwith "Oups!"

    let convertPath value = split >> toJson value

    let merge (o1:JObject) (o2:JObject) = o1.Merge(o2); o1

    let mergeAll (objs:JObject list) =
        objs |> List.fold merge (new JObject())
