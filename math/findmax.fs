open System
let average a b =
    (a+b) /2.0

let derive f (dx : float)=
    fun x -> (f (x + dx) - f x)/ dx



//given a function, and a point a where the derivative is >0
//a point b where it is <0 and tolerancethis f approximates a local max of f
// (the dx is arbitrary
let rec findMax f (a:float) (b:float) tolerance =
    let midpoint = average a b
    let derivative =
        (derive f 0.0001) midpoint
    match abs(derivative)< tolerance with
    |true -> midpoint
    |false ->
        match derivative with
        |n when n > 0.0 ->
            findMax f midpoint b tolerance
        |n when n < 0.0 ->
            findMax f a midpoint tolerance
        |_ -> midpoint



let main() =
    //Console.Write("What's your name? ")
    //let name = Console.ReadLine()
    let max = findMax (fun x -> sin x) 0.0 2.0 0.0001
    printfn "The max is situated at %f" max
    
main()