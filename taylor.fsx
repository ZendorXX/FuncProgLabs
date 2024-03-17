// Print a table of a given function f, computed by taylor series

// function: x(3 - x) / (1 - x)^3
// series: 3x + 8x^2 + ... + n * (n + 2) * x^n
// a = 0.0
// b = 0.5
let eps = 1e-5

// function to compute
let f x = x * (3.0 - x) / (1.0 - x) ** 3.0

let a = 0.0
let b = 0.5
let n = 10

// Define a function to compute f using naive taylor series method
let taylor_naive x = 
    let rec taylor_naive_calc n acc = 
        let curr = float n * (float n + 2.0) * x ** float n
        if abs curr > eps then taylor_naive_calc (n + 1) (acc + curr)
        else (acc, n)
    taylor_naive_calc 1 0.0


// (n + 1) * (n + 1 + 2) * x^(n + 1)) / (n * (n + 2) * x^n) = x * (n + 1)*(n + 3) / (n * (n + 2))
// Define a function to do the same in a more efficient way
let taylor x = 
    let rec taylor_calc n prev acc = 
        let curr = prev * x * (float n + 1.0) * (float n + 3.0) / float n / (float n + 2.0)
        if abs curr > eps then taylor_calc (n + 1) curr (acc + curr)
        else (acc, n)
    taylor_calc 1 (3.0 * x) (3.0 * x)

let main =
    printfn "|  x  |  Builtin  | Smart Taylor | # terms | Dumb Taylor | # terms |"
    printfn "|-----|-----------|--------------|---------|-------------|---------|"
    for i = 0 to n do
        let x = a + (float i) / (float n) * (b - a)
        let (smartTaylor_value, smartTaylor_terms) = taylor x
        let (dumbTaylor_value, dumbTaylor_terms) = taylor_naive x
        printfn "|%5.2f| %10.6f | %10.6f | %5d | %10.6f | %5d |" x (f x) smartTaylor_value smartTaylor_terms dumbTaylor_value dumbTaylor_terms
// make sure to improve this t4able to include the required number of iterations
// for each of the methods

main

