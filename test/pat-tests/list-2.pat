def snoc(xs : [Int], x : Int): [Int] {
    caseL xs of {
        nil : [Int] -> (x cons (nil : [Int]))
      | (y cons ys) : [Int] -> (y cons snoc(ys, x))
    }
}

def reverse(xs : [Int]): [Int] {
    caseL xs of {
          nil : [Int] -> nil
        | (y cons ys) : [Int] -> snoc(reverse(ys), y)
    }
}

def main(): Unit {
    let xs = (1 cons (2 cons (3 cons (nil : [Int])))) in
    caseL reverse(xs) of {
        nil : [Int] -> print("nil")
      | (y cons ys) : [Int] -> print(intToString(y))
  }
}

main()
