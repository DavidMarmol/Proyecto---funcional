package taller
import scala.util.Random
import scala.collection.immutable.Vector
import scala.collection.parallel.CollectionConverters._


class CalculosRiegoPar {


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
    Vector.tabulate(long, long) { (i, j) =>
      if (i < j) random.nextInt(long * 3) + 1
      else if (i == j) 0
      else random.nextInt(long * 3) + 1
    }
  }

  // Funciones para explorar la finca
  def tsup(f: Finca, i: Int): Int = f(i)._1
  def treg(f: Finca, i: Int): Int = f(i)._2
  def prio(f: Finca, i: Int): Int = f(i)._3

  // Calcula el tiempo de inicio de riego de forma funcional
  def tIR(f: Finca, pi: ProgRiego): TiempoInicioRiego = {
    pi.scanLeft(0)((tiempo, currTablon) => tiempo + treg(f, currTablon)).drop(1)
  }

  // Calcula el costo de riego para un tablón
  def costoRiegoTablon(i: Int, f: Finca, pi: ProgRiego): Int = {
    val tiempos = tIR(f, pi)
    val tiempoInicio = tiempos(i)
    val tiempoFinal = tiempoInicio + treg(f, i)
    if (tsup(f, i) >= tiempoFinal) {
      tsup(f, i) - tiempoFinal
    } else {
      prio(f, i) * (tiempoFinal - tsup(f, i))
    }
  }

  // Calcula el costo de riego para toda la finca en paralelo
  def costoRiegoFincaPar(f: Finca, pi: ProgRiego): Int = {
    (0 until f.length).par.map(i => costoRiegoTablon(i, f, pi)).sum
  }

  // Calcula el costo de movilidad de forma funcional y paralela
  def costoMovilidadPar(f: Finca, pi: ProgRiego, d: Distancia): Int = {
    (0 until pi.length - 1).par.map(j => d(pi(j))(pi(j + 1))).sum
  }

  // Genera todas las posibles programaciones de riego de forma paralela
  def generarProgramacionesRiegoPar(f: Finca): Vector[ProgRiego] = {
    val indices = (0 until f.length).toVector
    indices.permutations.toVector.par.toVector
  }

  // Encuentra la programación óptima de riego en paralelo
  def programacionRiegoOptimoPar(f: Finca, d: Distancia): (ProgRiego, Int) = {
    val programaciones = generarProgramacionesRiegoPar(f)
    val costos = programaciones.par.map(pi =>
      (pi, costoRiegoFincaPar(f, pi) + costoMovilidadPar(f, pi, d))
    )
    costos.minBy(_._2)
  }

  // Pruebas
  val finca = fincaAlAzar(10) // Genera una finca con 5 tablones
  val distancia = distanciaAlAzar(10) // Genera la matriz de distancias

  println("Finca generada:")
  println(finca.mkString(", "))

  println("\nMatriz de distancias:")
  distancia.foreach(row => println(row.mkString(", ")))

  val (mejorProg, costoOptimo) = programacionRiegoOptimoPar(finca, distancia)

  println("\nMejor programación de riego:")
  println(mejorProg.mkString(", "))

  println(s"\nCosto óptimo: $costoOptimo")
}
