def snoc(xs : [Int], x : Int): [Int] {
    caseL xs : [Int] of {
        nil -> (x :: (nil : [Int]))
      | (y :: ys) -> (y :: snoc(ys, x))
    }
}

def reverse(xs : [Int]): [Int] {
    caseL xs : [Int] of {
          nil -> nil
        | (y :: ys) -> snoc(reverse(ys), y)
    }
}

def main(): Unit {
    let xs = (1 :: (2 :: (3 :: (nil : [Int])))) in
    caseL reverse(xs) : [Int] of {
        nil -> print("nil")
      | (y :: ys) -> print(intToString(y))
  }
}

main()
