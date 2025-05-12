def snoc(xs : [Int], x : Int): [Int] {
    caseL xs : [Int] of {
        nil -> (x cons (nil : [Int]))
      | (y cons ys) -> (y cons snoc(ys, x))
    }
}

def reverse(xs : [Int]): [Int] {
    caseL xs : [Int] of {
          nil -> nil
        | (y cons ys) -> snoc(reverse(ys), y)
    }
}

def main(): Unit {
    let xs = (1 cons (2 cons (3 cons (nil : [Int])))) in
    caseL reverse(xs) : [Int] of {
        nil -> print("nil")
      | (y cons ys) -> print(intToString(y))
  }
}

main()
