// Определите функции для решение алгебраических уравнений

// 3*ln(x)^2 + 6*ln(x) - 5 = 0  [1, 3] x = 1.8832
// 0.6 * 3^x - 2.3x - 3 = 0     [2, 3] x = 2.4200
// x^2 - ln(1+x) - 3 = 0        [2, 3] x = 2.0267

let dichotomy f a b eps =
    let rec loop a b =
        if b - a < eps then (a + b) / 2.0
        else
            let c = (a + b) / 2.0
            if f a * f c < 0.0 then loop a c
            else loop c b
    loop a b

let iterations phi x0 eps =
    let rec loop x =
        let x' = phi x
        if abs (x' - x) < eps then x'
        else loop x'
    loop x0

let newthon f f' x0 eps =
    let rec loop x =
        let x' = x - f x / f' x
        if abs (x' - x) < eps then x'
        else loop x'
    loop x0

// Решите 3 уравнения (начиная со своего номера варианта) с использованием 3-х методов
let f1 x = 3.0 * (log x) ** 2.0 + 6.0 * (log x) - 5.0
let f2 x = 0.6 * 3.0 ** x - 2.3 * x - 3.0
let f3 x = x ** 2.0 - (log (1.0 + x)) - 3.0

let f1' x = (6.0 * (log x) + 6.0) / x 
let f2' x = 0.6 * (log 3.0) * 3.0 ** x - 2.3
let f3' x = 2.0 * x - 1.0 / (1.0 + x)

let phi1 x = x - f1 x / f1' x
let phi2 x = x - f2 x / f2' x
let phi3 x = x - f3 x / f3' x

let main = 
    let eps = 1e-4
    printfn "%10.4f  %10.4f  %10.4f" (dichotomy f1 1.0 3.0 eps) (iterations phi1 1.0 eps) (newthon f1 f1' 1.0 eps)
    printfn "%10.4f  %10.4f  %10.4f" (dichotomy f2 2.0 3.0 eps) (iterations phi2 2.0 eps) (newthon f2 f2' 2.0 eps)
    printfn "%10.4f  %10.4f  %10.4f" (dichotomy f3 2.0 3.0 eps) (iterations phi3 2.0 eps) (newthon f3 f3' 2.0 eps)

main