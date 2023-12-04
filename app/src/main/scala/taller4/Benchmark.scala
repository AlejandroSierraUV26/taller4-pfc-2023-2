package taller4

object Benchmark {
  type Matriz = Vector[Vector[Int]]

  def medirTiempo(funcion: => Unit): Double = {
    val inicio = System.nanoTime()
    funcion
    val fin = System.nanoTime()
    (fin - inicio) / 1e6 // Tiempo en milisegundos
  }
  def compararAlgoritmos(algoritmo1: (Matriz, Matriz) => Matriz, algoritmo2: (Matriz, Matriz) => Matriz)(m1: Matriz, m2: Matriz): (Double, Double, Double) = {
    val tiempoAlgoritmo1 = medirTiempo(algoritmo1(m1, m2))
    val tiempoAlgoritmo2 = medirTiempo(algoritmo2(m1, m2))
    val relacionRendimiento = tiempoAlgoritmo1 / tiempoAlgoritmo2
    (tiempoAlgoritmo1, tiempoAlgoritmo2, relacionRendimiento)
  }
}