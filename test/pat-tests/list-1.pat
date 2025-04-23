def main() : Unit {
    let xs = (5 cons (nil : [Int])) in
    caseL xs : [Int] of {
          nil -> print("nil")
        | (y cons ys) -> print(intToString(y))
    }
}

main()
