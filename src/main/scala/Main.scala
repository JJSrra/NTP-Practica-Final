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
}
