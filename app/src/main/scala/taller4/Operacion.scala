package taller4

import common.task

import scala.util.Random
object Operacion {

  type Matriz = Vector[Vector[Int]]
  def matrizAlAzar(long: Int, vals: Int): Matriz = {
    val v = Vector.fill(long,long)(Random.nextInt(vals))
    println(v)
    v
  }
  def vectorAlAzar(long: Int, vals: Int): Vector[Int] ={
    val v = Vector.fill(long)(Random.nextInt(vals))
    println(v)
    v
  }
  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map({case (x,y) => x*y}).sum
  }
  def traspuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l,l)((i,j) => m(j)(i))
  }
  // Version Estandar Secuencial
    def multMatriz(m1: Matriz, m2: Matriz): Matriz = {
      val mt2 = traspuesta(m2)
      Vector.tabulate(m1.length, mt2.length) { (i, j) =>
        prodPunto(m1(i), mt2(j))
      }
    }
  // Version Paralela
    def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
      Vector(Vector(0))
    }

  // Multiplicacion recursiva de matrices
  def subMatriz(m: Matriz, fila: Int, col: Int, tam: Int): Matriz = {
    Vector.tabulate(tam, tam)((i, j) => m(i + fila)(j + col))
  }
  def sumMatriz(m1: Matriz, m2:Matriz): Matriz = {
    Vector.tabulate(m1.length, m1.length)((i, j) => m1(i)(j) + m2(i)(j))
  }

  def multMatrizRec(m1: Matriz, m2: Matriz) : Matriz = {
    Vector(Vector(0))
  }
  def multMatrizRecPar(m1: Matriz, m2: Matriz) : Matriz = {
    Vector(Vector(0))
  }

  /// Multiplicaci´on de matrices usando el algoritmo de Strassen

  def restarMatriz(m1: Matriz, m2: Matriz): Matriz = {
    Vector.tabulate(m1.length, m1.length)((i, j) => m1(i)(j) - m2(i)(j))
  }

  def multStassen(m1:Matriz, m2 : Matriz) : Matriz ={
    Vector(Vector(0))
  }
  def multStassenPar(m1:Matriz, m2 : Matriz) : Matriz ={
    Vector(Vector(0))
  }




}
