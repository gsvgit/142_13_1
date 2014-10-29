namespace Problem

open System

module Timing =
    let time f =
        let start = DateTime.Now
        f ()
        DateTime.Now - start
    
    let measureTime start step stop f =
        let l = (stop - start) / step + 1
        let arr = Array.zeroCreate l
        let j = ref 0
        for i in start .. step .. stop do 
            arr.[!j] <- i, (time (f i)).Milliseconds
            j := !j + 1
        arr

    let measureOne start stop f =
        measureTime start 1 stop f 

