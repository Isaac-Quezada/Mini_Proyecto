/*
    Mini_Proyecto
    Isaac Quezada
    Programacion Funcional
*/


//Simpson 1/3 Normal

def simspon(a : Int, b: Int, f :Double=> Double):Double = {
    var x = (a + b) / 2
    val nd = (f(a) + 4 * f(x) + f(b)) / 6
    (b - a) * nd

}

//Simpson 1/3 Compuesta

def simpsonCompuesta(a : Int, b: Int, num : Int ,f :Double=> Double):Double = {
    val h = ( b - a )/num.toDouble 
    val xj = ( j : Double) => (a + j*h)

    val funciones = (j : Double) => f(xj(2 * j - 2)) + (4 * f(xj(2 * j - 1))) + (f(xj(
        2 * j)))

    ((1 to num/2).map(funciones(_)).sum)* h/3 
}

//Simpson 1/3 Extendida

def simpsonExtendida(a : Int, b: Int,f :Double=> Double):Double = {
    val num = 2 * (b -a) 
    val h = ( b - a )/num.toDouble 
    val x = ( n : Double) => f(a + n*h)

    val funciones = f(a) + 4 * (1 to num-1 by 2).map(x(_)).sum + 2 *(2 to num-2 by 2).map(x(_)).sum +f(b)
    h/3 * funciones
}

//Ejercicios
/*
EJERCICIO
Simpson 1/3 Normal
Simpson 1/3 Compuesta
Simpson 1/3 Extendida
*/


//Ejemplo 1
val ejemplo1 = (x : Double) => (-math.pow(x,2) + 8*x  - 12)
simspon(3,5,ejemplo1)
simpsonCompuesta(3 , 5 , 10 ,ejemplo1)  
simpsonExtendida(3 , 5 ,ejemplo1)  


//Ejemplo 2
val ejemplo2 = ((x : Double) => (3*(math.pow(x,2))))
simspon(0,2,ejemplo2)
simpsonCompuesta(0 , 2 , 30 ,ejemplo2)  
simpsonExtendida(0 , 2 ,ejemplo2) 


//Ejemplo 3
val ejemplo3 = ((x : Double) => (x+2*(math.pow(x,2))+(math.pow(x,3))+5*(math.pow(x,4))))
simspon(-1,1,ejemplo3)
simpsonCompuesta(1 , -1 , 20 ,ejemplo3)  
simpsonExtendida(1 , -1 ,ejemplo3) 


//Ejemplo 4
val ejemplo4 = ((x : Double) => (2*x+1)/((math.pow(x,2))+x)) 
simspon(1,2,ejemplo4)
simpsonCompuesta(1 ,2 , 40 ,ejemplo4)  
simpsonExtendida(1 , 2 ,ejemplo4) 


//Ejemplo 5
val ejemplo5 = ((x : Double) => math.pow((math.E),x))
simspon(1,0,ejemplo5)
simpsonCompuesta(1 , 0 , 50 ,ejemplo5)  
simpsonExtendida(1 , 0 ,ejemplo5) 


