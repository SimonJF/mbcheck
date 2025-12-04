def main() : Unit {
    let xs = (5 :: (nil : [Int])) in
    caseL xs : [Int] of {
          nil -> print("nil")
        | (y :: ys) -> print(intToString(y))
    }
}

main()
