// Определите функции для решение алгебраических уравнений

// 3*ln(x)^2 + 6*ln(x) - 5 = 0  [1, 3] x = 1.8832
// 0.6 * 3^x - 2.3x - 3 = 0     [2, 3] x = 2.4200
// x^2 - ln(1+x) - 3 = 0        [2, 3] x = 2.0267

let eps = 1e-5

let dichotomy f a b = 
    let rec loop a b =
        let c = (a + b) / 2.0
        if b - a < eps then c
        else if f a * f c > 0.0 then loop c b
        else loop a c
    loop a b

let iterations phi x0 = 
    let rec loop x = 
        if abs ((phi x) - x) < eps then phi x
        else loop (phi x)
    loop x0

let newthon f f' x0 = 
    let phi x = x - f x / f' x
    iterations phi x0
// используйте функцию 'iterations'

// Решите 3 уравнения (начиная со своего номера варианта) с использованием 3-х методов
let f1 x = 3.0 * (System.Math.Log x) ** 2.0 + 6.0 * (System.Math.Log x) - 5.0
let f2 x = 0.6 * 3.0 ** x - 2.3 * x - 3.0
let f3 x = x ** 2.0 - (System.Math.Log (1.0 + x)) - 3.0

let f1' x = (6.0 * (System.Math.Log x) + 6.0) / x 
let f2' x = 0.6 * (System.Math.Log 3.0) * 3.0 ** x - 2.3
let f3' x = 2.0 * x - 1.0 / (1.0 + x)

let phi1 x = x - f1 x / f1' x
let phi2 x = x - f2 x / f2' x
let phi3 x = x - f3 x / f3' x

let main = 
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f1 1.0 3.0) (iterations phi1 ((1.0 + 3.0) / 2.0)) (newthon f1 f1' ((1.0 + 3.0) / 2.0))
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f2 2.0 3.0) (iterations phi2 ((2.0 + 3.0) / 2.0)) (newthon f2 f2' ((2.0 + 3.0) / 2.0))
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f3 2.0 3.0) (iterations phi3 ((2.0 + 3.0) / 2.0)) (newthon f3 f3' ((2.0 + 3.0) / 2.0))

 