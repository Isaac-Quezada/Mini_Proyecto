// 1/3 SIMPSON NORMAL 

def simspon(a : Int, b: Int, f :Double=> Double):Double = {
    var x = (a + b) / 2
    val nd = (f(a) + 4 * f(x) + f(b)) / 6
    (b - a) * nd

}

// 1/3 SIMPSON Compuesta 

def simpsonCompuesta(a : Int, b: Int, num : Int ,f :Double=> Double):Double = {
    val h = ( b - a )/num.toDouble 
    val xj = ( j : Double) => (a + j*h)

    val funciones = (j : Double) => f(xj(2 * j - 2)) + (4 * f(xj(2 * j - 1))) + (f(xj(
        2 * j)))

    ((1 to num/2).map(funciones(_)).sum)* h/3 
}


