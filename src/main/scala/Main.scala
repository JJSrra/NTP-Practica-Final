object Main extends App {

  val x1 = Variable("X1", 3)
  val x2 = Variable("X2", 2)
  val x3 = Variable("X3", 10)

  val dom = new Dominio(List(x1, x2, x3))
  val indice = dom.indiceVariables(x3)
  println(indice)
}
