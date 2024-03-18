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

let rec whileLoop condition body state = 
    if condition state then
        let newState = body state
        whileLoop condition body newState
    else
        state

let pown a n = 
    let condition (_, i, n) = i <= n
    let body (acc, i, _) = (acc * a, i + 1, n)
    let (acc, _, _) = whileLoop condition body (1.0, 1, n)
    acc

// Define a function to compute f using naive taylor series method
let taylor_naive x = 
    let curr n x = float n * (float n + 2.0) * (pown x n)

    let condition (_, n) = curr n x > eps
    let body (acc, n) = ((curr n x) + acc, n + 1)

    let (acc, n) = whileLoop condition body (0.0, 1)
    (acc, n)


// (n + 1) * (n + 1 + 2) * x^(n + 1)) / (n * (n + 2) * x^n) = x * (n + 1)*(n + 3) / (n * (n + 2))
// Define a function to do the same in a more efficient way
let taylor x = 
    let curr prev n x = float prev * x * (float n + 1.0) * (float n + 3.0) / float n / (float n + 2.0)

    let condition (_, n, prev) = curr prev n x > eps
    let body (acc, n, prev) = (acc + curr prev n x, n + 1, curr prev n x)

    let (acc, n, _) = whileLoop condition body (3.0 * x, 1, 3.0 * x)
    (acc, n)

let main =
    printfn "|  x  |   Builtin  | Smart Taylor | # terms | Dumb Taylor | # terms |"
    printfn "|-----|------------|--------------|---------|-------------|---------|"
    for i = 0 to n do
        let x = a + (float i) / (float n) * (b - a)
        let (smartTaylor_value, smartTaylor_terms) = taylor x
        let (dumbTaylor_value, dumbTaylor_terms) = taylor_naive x
        printfn "|%5.2f| %10.6f |  %10.6f  | %5d   | %10.6f  | %5d   |" x (f x) smartTaylor_value smartTaylor_terms dumbTaylor_value dumbTaylor_terms
// make sure to improve this t4able to include the required number of iterations
// for each of the methods

main

