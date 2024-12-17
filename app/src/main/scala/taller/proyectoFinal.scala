package taller
import org.scalameter._
import scala.util.Random


type Tablon = (Int,Int,Int) //tiempo de supervivencia, tiempo de riego, prioridad del tablon
type Finca = Vector[Tablon] // una finca en un vector de tablones
type Distancia = Vector[Vector[Int]]
type ProgRiego = Vector[Int] // 0 es el primer turno, n-1 el ultimo
type TiempoInicioRiego = Vector[Int] 

//Generacion entradas aleatorias
val random = new random()

def fincaAlAzar(long:Int): Finca = {
    val v = Vector.fill(long)(
        random.nextInt(long*2)+ 1, //para el tiempo
        random.nextInt(long)+1, //supervivencia
        random.nextInt(4)+1 //regado
    )
    v
}

def distanciaAlAzar(long: Int): Distancia ={
    val v = Vector.fill(long,long)(random.nextInt(long*3)+1)
    Vector.tabulate(long,long)((i,j) =>
    if (1<j) v(i)(j)
    else if (i==j)0
    else v(j)(i))
}

//Exploracion de entradas

def tsup(f: Finca, i: Int): Int ={ 
    f(i)._1
}
def treg(f: Finca, i:Int): Int ={
    f(i)._2
}
def prio(f:Finca, i:Int): Int ={
    f(i)._3
}

//calculando el tiempo de inicio de riego

def tIR(f:Finca, pi: ProgRiego): TiempoInicioRiego={
    val tiempos=Array.fill(f.length)(0)
    for (j<- 1 until pi.length){
        val prevTablon = pi(j - 1)
        val currTablon = pi(j)
        tiempos(currTablon)= tiempos(prevTablon)+ treg(f, prevTablon)
    }
    tiempos.toVector
}

//calculando costos

def costoRiegoTablon(i:Int,f:Finca,pi: ProgRiego): Int ={
    val tiempoInicio = tIR(f,pi)(i)
    val tiempoFinal = tiempoInicio + treg(f,i)
    if (tsup(f,i)- treg(f,i) >= tiempoInicio){
        tsup(f,i)*(tiempoFinal - tsup(f,i))
    }
}

def costoRiegoFinca(f:Finca,pi: ProgRiego): Int={
    (0 until f.length).map(i => costoRiegoTablon(1,f,pi)).sum
}

def costoMovilidad(f:Finca, pi: ProgRiego, d: Distancia): Int={
    (0 until pi.length -1).map(j => d(pi(j))(pi(j+1))).sum
}


// Generando programaciones de riego

def generarProgramacionesRiego(f:Finca): Vector[ProgRiego]={
    val indices = (0 until f.length).toVector
    indices.permutations.toVector
}

//calculando una programacion de riego optima

def ProgramacionRiegoOptimo(f:Finca, d:Distancia): (ProgRiego,Int)={
    val programaciones = generarProgramacionesRiego(f)
    val costos = programaciones.map(pi =>
        (pi, costoRiegoFinca(f,pi)+ costoMovilidad(f,pi,d)))
    costos.mindBy(_._2)
}

// Paralelizando el calculo de los costos de riego y de movilidad

def costoRiegoFincaPar(f: Finca, pi: ProgRiego): Int ={
    (0 until f.length).par.map(i => costoRiegoTablon(i,f,pi)).sum
}

def costoMovilidadPar(f:Finca, pi:ProgRiego, d: Distancia): Int={
    (0 until pi.length -1).par.map(j => d(pi(j))(pi(j+1))).sum
}

// paralelizando la generacion de programaciones de riego

def generarProgramacionesRiegoPar(f:Finca): Vector[ProgRiego]={
    val indices = (0 until f.length).toVector
    indices.permutations.toVector.par.toVector
}

// Paralelizando la programacion de riego optima

def ProgramacionRiegoOptimoPar(f:Finca, d:Distancia): (ProgRiego,Int)={
    val programaciones = generarProgramacionesRiegoPar(f)
    val costos = programaciones.par.map(pi =>(pi, costoRiegoFincaPar(f,pi)+ costoMovilidadPar(f,pi,d))
    )
    costos.mindBy(_._2)
}











