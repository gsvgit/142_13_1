open Bubble
open Qsort
open FSharp.Charting
open System.Windows.Forms
open System.Drawing
let time f =
    let start = System.DateTime.Now
    f() |> ignore
    ((System.DateTime.Now - start).TotalMilliseconds)
let testBubble n = Bubble.main n
let testQsort n = Qsort.main n
let testSQsort n = Array.sort n
let pointBubble = Array.zeroCreate 291
let pointQsort = Array.zeroCreate 291
let pointSQsort = Array.zeroCreate 291
let rand () = 
    for k in 0..290 do
        let arr = Array.zeroCreate (System.Random().Next(5 * k + 100))
        let ran = new System.Random()
        for i in 0..arr.Length - 1 do
            arr.[i] <- ran.Next()
        let fb () = testBubble (arr)
        let fq () = testQsort (arr)
        let fsq () = testSQsort (arr)
        pointBubble.[k] <- (k, time fb)
        pointQsort.[k] <- (k, time fq)
        pointSQsort.[k] <- (k, time fsq)
rand ()    
(([Chart.Line(pointBubble, Name = "Bubble");
 Chart.Line(pointQsort, Name = "Qsort");
 Chart.Line(pointSQsort, Name = "SQsort")]) |> Chart.Combine).ShowChart()
let f = new System.Windows.Forms.Form()
Application.Run(f)