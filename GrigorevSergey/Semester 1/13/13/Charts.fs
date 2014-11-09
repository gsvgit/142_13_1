namespace Problem

open Fibonacci
open System.Drawing
open Timing
open FSharp.Charting
open System.Windows.Forms

module Charts =
    type LineConfig = {
        Name : string
        Color : Color
        FunctionIndex : int
    }
    
    type ChartSet = {
        Name : string
        Functions : list<LineConfig>
        Start : int
        Stop : int
        Step : int
    }

    type PointSet = {
        Start : int
        Stop : int
        Step : int
        Index : int
        Set : array<int * int>
    }

    let functions = [
        fun i () -> fib7 i |> ignore;
        fun i () -> fib8 i |> ignore;
        fun i () -> fib9 i |> ignore;
        fun i () -> fib10 i |> ignore;
        fun i () -> fib11 i |> ignore;
        fun i () -> fib12 i |> ignore
    ]

    let mutable pointSets : list<PointSet> = [
        
    ]
        
    let pointSet n start step stop =
        let n = n - 7
        let rec iter i =
            if i = pointSets.Length
            then None
            elif pointSets.[i].Index = n && pointSets.[i].Start = start && pointSets.[i].Stop = stop && pointSets.[i].Step = step
            then Some (pointSets.[i].Set)
            else iter (i + 1)
        match iter 0 with
        | Some a -> a
        | None ->
            let tmp = measureTime start step stop functions.[n]
            pointSets <- {Start = start; Step = step; Stop = stop; Index = n; Set = tmp} :: pointSets
            tmp
            
    let charts = [
        {
            Name = "2^n and linear algorythms";
            Start = 0;
            Stop = 40;
            Step = 5;
            Functions =
            [
                {Name = "2^n"; Color = Color.Red; FunctionIndex = 7};
                {Name = "Linear"; Color = Color.Green; FunctionIndex = 9};
            ]
        };
        {
            Name = "Iterative and recursive algorythms";
            Start = 0;
            Stop = 1000000;
            Step = 50000;
            Functions =
            [
                {Name = "Iterative"; Color = Color.Red; FunctionIndex = 8};
                {Name = "Recursive"; Color = Color.Green; FunctionIndex = 9};
            ]
        };
        {
            Name = "Matrix algorythms";
            Start = 0;
            Stop = 1000000;
            Step = 50000;
            Functions =
            [
                {Name = "Linear"; Color = Color.Red; FunctionIndex = 10};
                {Name = "Log"; Color = Color.Green; FunctionIndex = 11};
            ]
        };
        {
            Name = "Fast algorythms";
            Start = 0;
            Stop = 1000000;
            Step = 50000;
            Functions =
            [
                {Name = "Iterative"; Color = Color.Red; FunctionIndex = 8};
                {Name = "Linear"; Color = Color.Orange; FunctionIndex = 10};
                {Name = "Recursive"; Color = Color.Blue; FunctionIndex = 9};
                {Name = "Log"; Color = Color.Green; FunctionIndex = 11};
            ]
        };
        {
            Name = "N-th and first N algorythms";
            Start = 0;
            Stop = 100000000;
            Step = 5000000;
            Functions =
            [
                {Name = "First N"; Color = Color.Red; FunctionIndex = 12};
                {Name = "N-th"; Color = Color.Green; FunctionIndex = 9};
            ]
        }
    ]

    let computeChart chart =
        let l = chart.Functions.Length
        let rec iter lst acc =
            match lst with
            | h :: t ->
                let tmp = pointSet h.FunctionIndex chart.Start chart.Step chart.Stop
                let c = Chart.Line (tmp, Name = h.Name, Color = h.Color)
                c :: acc |> iter t
            | [] -> acc |> List.rev
        let cc = new ChartTypes.ChartControl (Chart.Combine(iter chart.Functions []).WithXAxis(Title = "n").WithYAxis(Title = "t, ms").WithTitle(Text = chart.Name).WithLegend())
        cc.Dock <- DockStyle.Fill
        cc
        

    let getChartControls () =
        let l = charts.Length
        let arr = Array.zeroCreate l
        let rec iter i lst =
            match lst with
            | h :: t ->
                arr.[i] <- (h.Name, computeChart h)
                iter (i + 1) t
            | [] -> arr
        iter 0 charts

