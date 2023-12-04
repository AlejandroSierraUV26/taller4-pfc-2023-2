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
    test("testTaller4 I"){
        val m1 = Operacion.matrizAlAzar(2, 2)
        val m2 = Operacion.matrizAlAzar(2, 2)
        assert(Operacion.multMatriz(m1,m2) == Operacion.multMatrizPar(m1,m2))
    }
}
