def main(): Unit {
    let xs = (5 cons (nil : [Int])) in
    caseL xs of {
          nil : [Int] -> print("nil")
        | (y cons ys) : [Int] -> print(intToString(y))
    }
}

main()
