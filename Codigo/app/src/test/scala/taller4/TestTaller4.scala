/**
 * Plantilla para pruebas
* @author Carlos Delgado
* @version 1.0
* @note 22 de Noviembre de 2023 
 */
package taller4

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestTaller4 extends AnyFunSuite{
    test("testTaller4 I") {
        val m1 = Vector(
            Vector(1, 2),
            Vector(3, 4))
        val m2 = Vector(
            Vector(5, 6),
            Vector(7, 8))
        assert(Operacion.multMatriz(m1, m2) == Vector(Vector(19, 22), Vector(43, 50)))
        assert(Operacion.multMatrizPar(m1, m2) == Vector(Vector(19, 22), Vector(43, 50)))
        assert(Operacion.multStrassen(m1, m2) == Vector(Vector(19, 22), Vector(43, 50)))
        assert(Operacion.multStrassenPar(m1, m2) == Vector(Vector(19, 22), Vector(43, 50)))
        assert(Operacion.multMatrizRec(m1, m2) == Vector(Vector(19, 22), Vector(43, 50)))
        assert(Operacion.multMatrizRecPar(m1, m2) == Vector(Vector(19, 22), Vector(43, 50)))
    }
    test("testTaller4 II") {
        val m1 = Vector(Vector(1,0,1,0),Vector(0,1,0,1),Vector(1,0,1,0),Vector(0,1,0,1))
        val m2 = Vector(Vector(0,1,0,1),Vector(1,0,1,0),Vector(0,1,0,1),Vector(1,0,1,0))
        assert(Operacion.multMatriz(m1, m2) == Vector(Vector(0,2,0,2),Vector(2,0,2,0),Vector(0,2,0,2),Vector(2,0,2,0)))
        assert(Operacion.multMatrizPar(m1, m2) == Vector(Vector(0,2,0,2),Vector(2,0,2,0),Vector(0,2,0,2),Vector(2,0,2,0)))
        assert(Operacion.multMatrizRec(m1, m2) == Vector(Vector(0,2,0,2),Vector(2,0,2,0),Vector(0,2,0,2),Vector(2,0,2,0)))
        assert(Operacion.multMatrizRecPar(m1, m2) == Vector(Vector(0,2,0,2),Vector(2,0,2,0),Vector(0,2,0,2),Vector(2,0,2,0)))
        assert(Operacion.multStrassen(m1, m2) == Vector(Vector(0,2,0,2),Vector(2,0,2,0),Vector(0,2,0,2),Vector(2,0,2,0)))
        assert(Operacion.multStrassenPar(m1, m2) == Vector(Vector(0,2,0,2),Vector(2,0,2,0),Vector(0,2,0,2),Vector(2,0,2,0)))

    }
    test("Multiplicacion Estandar de Matrices"){
        val m1 = Operacion.matrizAlAzar(2,2)
        val m2 = Operacion.matrizAlAzar(2,2)
        assert(Operacion.multMatriz(m1,m2) == Operacion.multMatrizPar(m1,m2))
        val m3 = Operacion.matrizAlAzar(4, 4)
        val m4 = Operacion.matrizAlAzar(4, 4)
        assert(Operacion.multMatriz(m3, m4) == Operacion.multMatrizPar(m3, m4))
    }
    test ("Multiplicacion Recursiva de Matrices"){
        val m1 = Operacion.matrizAlAzar(2, 2)
        val m2 = Operacion.matrizAlAzar(2, 2)
        assert(Operacion.multMatrizRec(m1,m2) == Operacion.multMatrizRecPar(m1,m2))
        val m3 = Operacion.matrizAlAzar(4, 4)
        val m4 = Operacion.matrizAlAzar(4, 4)
        assert(Operacion.multMatrizRec(m3, m4) == Operacion.multMatrizRecPar(m3, m4))
    }
    test ("Multiplicacion Metodo : Strassen De Matrices"){
        val m1 = Operacion.matrizAlAzar(2, 2)
        val m2 = Operacion.matrizAlAzar(2, 2)
        assert(Operacion.multStrassen(m1,m2) == Operacion.multStrassenPar(m1,m2))
        val m3 = Operacion.matrizAlAzar(4, 4)
        val m4 = Operacion.matrizAlAzar(4, 4)
        assert(Operacion.multStrassen(m3, m4) == Operacion.multStrassenPar(m3, m4))
    }
    test("Producto Punto de Matrices"){
        val m1 = Operacion.vectorAlAzar(10,2)
        val m2 = Operacion.vectorAlAzar(10,2)
        assert(Operacion.prodPunto(m1,m2) == Operacion.prodPuntoPar(m1,m2))
    }
}
