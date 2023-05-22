def main(): Unit {
    let x = (inl(5) : (Int + Bool)) in
    case x of {
          inl(x): Int -> print(intToString(x))
        | inr(y): Bool -> print("right branch")
    }
}

main()
