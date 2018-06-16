object Main extends App {

  // Se crea dominio vacío
  val dominioVacio = Dominio(List())

  // Se comprueba que funciona el metodo asociado a comprobar la condición de dominio vacío
  println("Comprobación de vacío sobre dominio vacío: " + dominioVacio.vacio)

  // Se crean 4 variables
  val X1 = Variable("X1",3)
  val X2 = Variable("X2",4)
  val X3 = Variable("X3",2)
  val X4 = Variable("X4",2)

  // Se crea un dominio con las variables creadas antes
  val dominioNoVacio=Dominio(List(X1,X2,X3,X4))

  // Este dominio ya no esta vacío
  println("Comprobación de vacío sobre dominio no vacío: " + dominioNoVacio.vacio)

  // Se obtiene la longitud del dominio
  val longitud=dominioNoVacio.longitud
  println("Longitud del dominio no vacío (debe ser 4): " + longitud)

  // Se muestra el objeto usando toString
  println(dominioNoVacio)

  // Se suma una variable al dominioNoVacio
  val X5 = new Variable("X5",5)
  val dominioSuma = dominioNoVacio + X5

  println("Dominio suma (+X5): " + dominioSuma)

  // Se crea un dominio sobre X1, X2, X5 y X6
  val X6 = new Variable("X6", 3)
  val dominioNoVacio2 = new Dominio(List(X1, X2, X5, X6))

  // Se genera ahora la suma de los dos dominios no vacíos
  val dominioSuma2 = dominioNoVacio + dominioNoVacio2
  println("Suma de dominios: " + dominioSuma2)
}
