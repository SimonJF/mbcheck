def main(): Unit {
    let x : (Int + Bool) = inl(5) in
    case x of {
          inl(x): Int -> print(intToString(x))
        | inr(y): Bool -> print("right branch")
    }
}

main()
