def main(x : Int): Int {
    let q = x + another(5) in
    let f = (fun (t: Int): Int { t+200 })(5) in
    if (f < 10){
        new1(q)
    } else {
        new2(q)
    }
}

def another(z :Int): Int {
    let y = z in y
}

def new1(r:Int): Int {
    let k = 100 in k + r
}

def new2(q:Int): Int {
    let d = 100 in d + q + 20000
}

main(100)


