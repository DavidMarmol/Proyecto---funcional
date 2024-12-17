package taller

import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalameter._
import taller.CalculosRiego
import taller.CalculosRiegoPar

@RunWith(classOf[JUnitRunner])
class EvaluarComparacionTest extends AnyFunSuite {
  val calculosRiego = new CalculosRiego()
  val calculosRiegoPar = new CalculosRiego()

  test("Medir tiempo secuencial y paralelo") {
    println(f"${"Tamaño de la finca (tablones)"}%-30s ${"Versión secuencial (ms)"}%-30s ${"Versión paralela (ms)"}%-30s ${"Aceleración (%)"}%-20s")
    println("-" * 110)

    val tamanos = Seq(1, 3, 5, 6 , 9) // Tamaños de finca a evaluar

    tamanos.foreach { tamano =>
      val finca = calculosRiego.fincaAlAzar(tamano)
      val distancia = calculosRiego.distanciaAlAzar(tamano)

      val tiempoSecuencial = measure {
        calculosRiego.programacionRiegoOptimo(finca, distancia)
      }.value

      val tiempoParalelo = measure {
        calculosRiegoPar.programacionRiegoOptimo(finca, distancia)
      }.value

      val aceleracion = ((tiempoSecuencial - tiempoParalelo) / tiempoSecuencial) * 100

      println(f"$tamano%-30d $tiempoSecuencial%-30.2f $tiempoParalelo%-30.2f $aceleracion%-20.2f")
    }
  }
}