object Main extends App {

  println("\t\t\t\t\tDOMINIO")
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
  val dominioNoVacio2 = Dominio(List(X1, X2, X5, X6))

  // Se genera ahora la suma de los dos dominios no vacíos
  val dominioSuma2 = dominioNoVacio + dominioNoVacio2
  println("Suma de dominios: " + dominioSuma2)

  // Se resta el elemento X2 al dominio anterior
  val dominioSinX2 = dominioSuma2 - X2
  println("Dominio anterior sin X2: " + dominioSinX2)

  // Se resta el dominioNoVacio2
  val dominioResta = dominioSinX2 - dominioNoVacio2
  println("Dominio anterior menos (X1,X2,X5,X6): " + dominioResta)

  // Prueba de cálculo del máximo índice
  val maximoIndice = dominioSuma2.maximoIndice
  println("Máximo índice de dominio de suma: " + maximoIndice)

  println("========================================================================")

  println("\t\t\t\t\tASIGNACIÓN")

  // Se crea asignación vacía y se comprueba su chequeo
  val asignacionVacia = Asignacion(Dominio(List()), List())
  println("Comprobación vacío asignación vacía: " + asignacionVacia.vacia)

  // Se crea asignación, dando valores 2, 3, 1 y 0 a las variables
  val asignacion1 = Asignacion(Dominio(List(X1, X2, X3, X4)), List(2,3,1,0))
  println("Comprobación vacío sobre asignación no vacía: " + asignacion1.vacia)
  println("Se muestra la asignación: " + asignacion1)

  // Se comprueba el valor de la variable X2 en la asignación anterior
  val valorVariableX2 = asignacion1.obtenerValorVariable(X2)
  println("Comprobación de valor de X2: " + valorVariableX2)

  // Se añade el par X6 - 0 a la asignación anterior
  val asignacion2 = asignacion1 + (X6,0)
  println("Asignación anterior sumándole X6 con valor 0: " + asignacion2)

  // Cálculo del índice asociado a la asignación (debe ser 46)
  val indice1 = asignacion1.calcularIndice
  println("indice1 (debe ser 46): " + indice1)

  // Proyectar X2 y X6 sobre la asignación anterior
  val asignacionProyectada = asignacion2.proyectar(Dominio(List(X2,X6)))
  println("Asignación anterior proyectada sobre X2 y X6: " + asignacionProyectada)

  // Crear una asignación nueva solamente con un dominio
  val asignacion0 = Asignacion(Dominio(List(X1,X3,X5)))
  println("Asignacion solamente con dominio: " + asignacion0)

  // A partir del índice obtenemos la asignación
  val asignacionDeIndice = Asignacion(asignacion1.dominio, indice1)

  // Se muestra la asignación obtenida: debe ser X1=2, X2=3, X3=1, X4=0
  println("Asignación obtenida a partir de índice: " + asignacionDeIndice)

  println("========================================================================")

  println("\t\t\t\t\tVALORES ARRAY")

  // Crear un objeto ValoresArray
  val valoresArray1 = ValoresArray(Dominio(List(X3,X4)), List(0.2, 0.8, 0.6, 0.4))
  println("Variables de ValoresArray: " + valoresArray1.obtenerVariables)

  println("Imprimir el objeto ValoresArray anterior:\n" + valoresArray1)

  val var1 = Variable("X1",2)
  val var2 = Variable("X2",2)
  val var3 = Variable("X3",2)

  val valores1 = ValoresArray(Dominio(List(var1,var2)), List(0.3, 0.7, 0.6, 0.4))
  val valores2 = ValoresArray(Dominio(List(var2,var3)), List(0.9, 0.1, 1.0, 0.0))

  val valoresCombinado = valores1.combinar(valores2)
  println("Valores combinados:\n" + valoresCombinado)

  val valoresRestringido = valoresCombinado.restringir(var1, 0)
  println("Valores restringidos:\n" + valoresRestringido)

  println("========================================================================")

  println("\t\t\t\t\tVALORES ARBOL")

  val arbol1 = ValoresArbol(Dominio(List(var1,var2,var3)), List(0.27, 0.03, 0.63, 0.07, 0.6, 0, 0.4, 0))
}
