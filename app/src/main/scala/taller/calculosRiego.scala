package taller
import scala.util.Random
import scala.collection.immutable.Vector
class CalculosRiego {
  // Definición de tipos
  // Un tablon es una tripleta con tiempo de supervivencia, tiempo de riego y prioridad
  type Tablon = (Int, Int, Int)
  // Una finca es un vector de tablones
  type Finca = Vector[Tablon]
  // Una matriz de distancias entre tablones
  type Distancia = Vector[Vector[Int]]
  // Una programación de riego es un vector que asocia cada tablón con su turno de riego
  type ProgRiego = Vector[Int]
  // Tiempo de inicio de riego de cada tablón
  type TiempoInicioRiego = Vector[Int]

  val random = new Random()

  // Genera una finca aleatoria
  def fincaAlAzar(long: Int): Finca = {
    Vector.fill(long)((random.nextInt(long * 2) + 1, random.nextInt(long) + 1, random.nextInt(4) + 1))
  }

  // Genera una matriz de distancias aleatoria
  def distanciaAlAzar(long: Int): Distancia = {
    val v = Vector.fill(long, long)(random.nextInt(long * 3) + 1)
    Vector.tabulate(long, long) { (i, j) =>
      if (i < j) v(i)(j)
      else if (i == j) 0
      else v(j)(i)
    }
  }

  // Funciones para explorar la finca
  def tsup(f: Finca, i: Int): Int = f(i)._1

  def treg(f: Finca, i: Int): Int = f(i)._2

  def prio(f: Finca, i: Int): Int = f(i)._3

  // Calcula el tiempo de inicio de riego
  def tIR(f: Finca, pi: ProgRiego): TiempoInicioRiego = {
    val tiempos = Array.fill(f.length)(0)
    for (j <- 1 until pi.length) {
      val prevTablon = pi(j - 1)
      val currTablon = pi(j)
      tiempos(currTablon) = tiempos(prevTablon) + treg(f, prevTablon)
    }
    tiempos.toVector
  }

  // Calcula el costo de riego para un tablón
  def costoRiegoTablon(i: Int, f: Finca, pi: ProgRiego): Int = {
    val tiempoInicio = tIR(f, pi)(i)
    val tiempoFinal = tiempoInicio + treg(f, i)
    if (tsup(f, i) >= tiempoFinal) {
      tsup(f, i) - tiempoFinal
    } else {
      prio(f, i) * (tiempoFinal - tsup(f, i))
    }
  }

  // Calcula el costo de riego para toda la finca
  def costoRiegoFinca(f: Finca, pi: ProgRiego): Int = {
    (0 until f.length).map(i => costoRiegoTablon(i, f, pi)).sum
  }

  // Calcula el costo de movilidad
  def costoMovilidad(f: Finca, pi: ProgRiego, d: Distancia): Int = {
    (0 until pi.length - 1).map(j => d(pi(j))(pi(j + 1))).sum
  }

  // Genera todas las posibles programaciones de riego
  def generarProgramacionesRiego(f: Finca): Vector[ProgRiego] = {
    val indices = (0 until f.length).toVector
    indices.permutations.toVector
  }

  // Encuentra la programación óptima de riego
  def programacionRiegoOptimo(f: Finca, d: Distancia): (ProgRiego, Int) = {
    val programaciones = generarProgramacionesRiego(f)
    val costos = programaciones.map(pi =>
      (pi, costoRiegoFinca(f, pi) + costoMovilidad(f, pi, d))
    )
    costos.minBy(_._2) // Retorna la programación con el menor costo
  }

  // Pruebas
  def runTests(): Unit = {
    val finca = fincaAlAzar(10)
    val distancia = distanciaAlAzar(10)

    println("Finca generada:")
    println(finca.mkString(", "))

    println("\nMatriz de distancias:")
    distancia.foreach(row => println(row.mkString(", ")))

    val (mejorProg, costoOptimo) = programacionRiegoOptimo(finca, distancia)

    println("\nMejor programación de riego:")
    println(mejorProg.mkString(", "))

    println(s"\nCosto óptimo: $costoOptimo")
  }
}