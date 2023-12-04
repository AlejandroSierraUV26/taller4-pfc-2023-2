package taller4
import common.task
import taller4.Benchmark._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}
import scala.util.Random


object Operacion {
  type Matriz = Vector[Vector[Int]]
  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    val v = Vector.fill(long,long)(Random.nextInt(vals))
    v
  }
  def vectorAlAzar(long: Int, vals: Int): Vector[Int] ={
    val v = Vector.fill(long)(Random.nextInt(vals))
    v
  }
  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({case (x,y) => x*y}).sum
  }
  /*
  def prodPuntoParD(v1: ParVector[Int], v2: ParVector[Int]): Int = {
    (v1 zip v2).map({ case (x, y) => x * y }).sum
  }
  */
  def traspuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l,l)((i,j) => m(j)(i))
  }
  // Version Estandar Secuencial
  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
    val mt2 = traspuesta(m2)
    Vector.tabulate(m1.length, mt2.length){(i,j) => prodPunto(m1(i), mt2(j))}
  }
  // Version Paralela
  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val mt2 = traspuesta(m2)
    Vector.tabulate(m1.length, mt2.length) ((i,j) => task(prodPunto(m1(i), mt2(j))).join())
  }
  // Multiplicacion recursiva de matrices
  def subMatriz(m: Matriz, fila: Int, col: Int, tam: Int): Matriz = {
    Vector.tabulate(tam, tam)((i, j) => m(i + fila)(j + col))
  }
  def sumMatriz(m1: Matriz, m2:Matriz): Matriz = {
    Vector.tabulate(m1.length, m1.length)((i, j) => m1(i)(j) + m2(i)(j))
  }
  def multMatrizRecPar(a: Matriz, b: Matriz): Matriz = {

    val rowsA = a.length
    val colsA = a.head.length
    val colsB = b.head.length

    val futures: Vector[Future[Vector[Int]]] = Vector.tabulate(rowsA) { i =>
      Future {
        Vector.tabulate(colsB) { j =>
          (0 until colsA).map(k => a(i)(k) * b(k)(j)).sum
        }
      }
    }
    val resultFuture: Future[Vector[Vector[Int]]] = Future.sequence(futures)

    Await.result(resultFuture, 5.seconds)
  }
  def multMatrizRec(a: Matriz, b: Matriz): Matriz = {
    val rowsA = a.length
    val colsA = a.head.length
    val colsB = b.head.length

    val result: Matriz = Vector.tabulate(rowsA) { i =>
      Vector.tabulate(colsB) { j =>
        (0 until colsA).map(k => a(i)(k) * b(k)(j)).sum
      }
    }
    result
  }
  def restarMatriz(m1: Matriz, m2: Matriz): Matriz = {
    Vector.tabulate(m1.length, m1.length)((i, j) => m1(i)(j) - m2(i)(j))
  }
  def multStrassen(m1: Matriz, m2: Matriz): Matriz = {
    // Recibe m1 y m2 matrices cuadradas de la misma dimensión (potencia de 2)
    // y devuelve la multiplicación de las dos matrices usando el algoritmo de Strassen
    val n = m1.head.count(_=> true)
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val m = n / 2
      val a11 = subMatriz(m1, 0, 0, m)
      val a12 = subMatriz(m1, 0, m, m)
      val a21 = subMatriz(m1, m, 0, m)
      val a22 = subMatriz(m1, m, m, m)
      val b11 = subMatriz(m2, 0, 0, m)
      val b12 = subMatriz(m2, 0, m, m)
      val b21 = subMatriz(m2, m, 0, m)
      val b22 = subMatriz(m2, m, m, m)
      val p1 = multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22))
      val p2 = multStrassen(sumMatriz(a21, a22), b11)
      val p3 = multStrassen(a11, restarMatriz(b12, b22))
      val p4 = multStrassen(a22, restarMatriz(b21, b11))
      val p5 = multStrassen(sumMatriz(a11, a12), b22)
      val p6 = multStrassen(restarMatriz(a21, a11), sumMatriz(b11, b12))
      val p7 = multStrassen(restarMatriz(a12, a22), sumMatriz(b21, b22))
      val c11 = restarMatriz(sumMatriz(sumMatriz(p1, p4), p7), p5)
      val c12 = sumMatriz(p3, p5)
      val c21 = sumMatriz(p2, p4)
      val c22 = restarMatriz(sumMatriz(sumMatriz(p1,p3),p6), p2)

      Vector.tabulate(n, n) { (i, j) =>
        if (i < m && j < m) c11(i)(j)
        else if (i < m && j >= m) c12(i)(j - m)
        else if (i >= m && j < m) c21(i - m)(j)
        else c22(i - m)(j - m)
      }
    }
  }
  def multStrassenPar(m1: Matriz, m2: Matriz): Matriz = {
    val n = m1.head.count(_ => true)
    if (n == 1) {
      Vector(Vector(m1(0)(0) * m2(0)(0)))
    } else {
      val m = n / 2
      val a11 = subMatriz(m1, 0, 0, m)
      val a12 = subMatriz(m1, 0, m, m)
      val a21 = subMatriz(m1, m, 0, m)
      val a22 = subMatriz(m1, m, m, m)
      val b11 = subMatriz(m2, 0, 0, m)
      val b12 = subMatriz(m2, 0, m, m)
      val b21 = subMatriz(m2, m, 0, m)
      val b22 = subMatriz(m2, m, m, m)
      val p1 = multStrassen(sumMatriz(a11, a22), sumMatriz(b11, b22))
      val p2 = multStrassen(sumMatriz(a21, a22), b11)
      val p3 = multStrassen(a11, restarMatriz(b12, b22))
      val p4 = multStrassen(a22, restarMatriz(b21, b11))
      val p5 = multStrassen(sumMatriz(a11, a12), b22)
      val p6 = multStrassen(restarMatriz(a21, a11), sumMatriz(b11, b12))
      val p7 = multStrassen(restarMatriz(a12, a22), sumMatriz(b21, b22))
      val c11 = restarMatriz(sumMatriz(sumMatriz(p1, p4), p7), p5)
      val c12 = sumMatriz(p3, p5)
      val c21 = sumMatriz(p2, p4)
      val c22 = restarMatriz(sumMatriz(sumMatriz(p1, p3), p6), p2)

      Vector.tabulate(n, n) { (i, j) =>
        task(
          if (i < m && j < m) c11(i)(j)
          else if (i < m && j >= m) c12(i)(j - m)
          else if (i >= m && j < m) c21(i - m)(j)
          else c22(i - m)(j - m)).join()
      }
    }
  }

  def main(args: Array[String]): Unit = {
  for {
    i<- 1 to 8
    m1 = matrizAlAzar(math.pow(2,i).toInt, 2)
    m2 = matrizAlAzar(math.pow(2,i).toInt, 2)
  }yield (println(compararAlgoritmos(multMatrizRec, multMatrizRecPar)(m1, m2), math.pow(2,i).toInt))
  }
}


