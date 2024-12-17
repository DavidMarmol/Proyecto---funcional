package taller

import org.junit.runner.RunWith
import org.scalatest.funsuite.AnyFunSuite
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class CalculosRiegoTest extends AnyFunSuite {

  val calculosRiego = new CalculosRiego

  // Type aliases from CalculosRiego
  type Tablon = (Int, Int, Int)
  type Finca = Vector[Tablon]
  type Distancia = Vector[Vector[Int]]

  // Predefined Finca of 3 tablones
  val finca: Finca = Vector((5, 2, 1), (6, 3, 2), (4, 1, 3))
  val distancia: Distancia = Vector(
    Vector(0, 2, 3),
    Vector(2, 0, 1),
    Vector(3, 1, 0)
  )

  test("generarProgramacionesRiego should generate all permutations") {
    val programaciones = calculosRiego.generarProgramacionesRiego(finca)
    assert(programaciones.size == 6) // 3! = 6 permutaciones
  }

  test("programacionRiegoOptimo should return the optimal irrigation schedule") {
    val (progRiego, costo) = calculosRiego.programacionRiegoOptimo(finca, distancia)
    assert(costo == 8) // Ejemplo de aserción
  }

  test("costoRiegoFinca should calculate the correct irrigation cost") {
    val progRiego = Vector(0, 1, 2) // Ejemplo de programación de riego
    val costo = calculosRiego.costoRiegoFinca(finca, progRiego)
    assert(costo >= 0) // Ejemplo de aserción
  }

  test("costoMovilidad should calculate the correct mobility cost") {
    val progRiego = Vector(0, 1, 2) // Ejemplo de programación de riego
    val costo = calculosRiego.costoMovilidad(finca, progRiego, distancia)
    assert(costo == 3) // Ejemplo de aserción
  }
}