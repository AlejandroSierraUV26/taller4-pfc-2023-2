package taller4

import taller4.Operacion.{prodPunto, prodPuntoPar, vectorAlAzar}

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

  def compararProdPunto(tamanoVector: Int): (Double, Double, Double) = {
    val vector1 = vectorAlAzar(tamanoVector,10)
    val vector2 = vectorAlAzar(tamanoVector,10)

    val tiempoInicioSecuencial = System.nanoTime()
    val resultadoSecuencial = prodPunto(vector1, vector2)
    val tiempoSecuencial = (System.nanoTime() - tiempoInicioSecuencial) / 1e6

    val tiempoInicioParalelo = System.nanoTime()
    val resultadoParalelo = prodPuntoPar(vector1, vector2)
    val tiempoParalelo = (System.nanoTime() - tiempoInicioParalelo) / 1e6

    val aceleracion = tiempoSecuencial / tiempoParalelo

    (tiempoSecuencial, tiempoParalelo, aceleracion)
  }
}