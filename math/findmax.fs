let average a b =
    (a+b) /2.0

let derive f (dx : float)=
    fun x -> (f (x + dx) + f x)/ dx

let closeenough a b (tolerance:float) =
    abs(a-b) < tolerance

let rec findMax f (a:float) (b:float) tolerance =
    let midpoint = average a b
    let derivative =
        (derive f 0.000001) midpoint
    match abs(derivative)< tolerance with
    |true -> midpoint
    |false ->
        match derivative with
        |n when n>0.0 ->
            findMax f midpoint b
        |n when n<0.0 ->
            findMax f a midpoint
	|_ -> midpoint