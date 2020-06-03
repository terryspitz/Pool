module Program

open System.IO


let [<EntryPoint>] main _ =
    let svg = Pool.poolHtml 0.0
    printfn "%A" svg
    File.WriteAllLines("output.svg", svg) |> ignore
    0
