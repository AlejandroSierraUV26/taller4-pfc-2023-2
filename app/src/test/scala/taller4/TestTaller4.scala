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
        assert(Operacion.multMatriz(m1,m2) ==Operacion.multMatrizPar(m1,m2))
    }
    test("testTaller4 II"){
        val m1 = Operacion.matrizAlAzar(2, 2)
        val m2 = Operacion.matrizAlAzar(2, 2)
        val t = System.currentTimeMillis()
        val mt1 = Operacion.multMatrizRec(m1,m2)
        val tt1 = System.currentTimeMillis() - t
        val t2 = System.currentTimeMillis()
        val mt2 = Operacion.multMatrizRecPar(m1,m2)
        val tt2 = System.currentTimeMillis() - t2
        assert(tt2 < tt1)
    }


}
