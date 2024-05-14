interface MyMailbox1 { }

def synth_bad2(z: MyMailbox1![R]): Unit {
  let (x, y) = ((), z) in
  x
}
