package taller
import scala.util.Random
import common._
import scala.annotation.tailrec

class Matrices {
  // Tipo de matriz definido como alias
  type Matriz = Vector[Vector[Int]]

  // Función para generar matriz al azar
  def MatrizAlAzar(long: Int, vals: Int): Matriz = {
    // Usar Random.nextInt en lugar de random sin definir
    val random = new Random()
    Vector.fill(long, long)(random.nextInt(vals))
  }

  def vectorAlAzar ( long : Int , vals : Int ) : Vector [ Int ] = {
    //Crea un vector de enteros de longitud long ,
    // con valores aleatorios entre 0 y vals
    val v = Vector.fill(long)(Random.nextInt(vals))
    v
  }

  // Función para calcular producto punto
  def ProdPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map { case (i, j) => i * j }.sum
  }

  // Función para calcular matriz transpuesta
  def transpuesta(m: Matriz): Matriz = {
    val l = m.length
    Vector.tabulate(l, l) { (i, j) => m(j)(i) }
  }

  def multMatriz(m1: Matriz, m2: Matriz): Matriz = {

    val length = m1.length
    val transp_m2 = transpuesta(m2)
    assert(m1.length == m2.length)
    Vector.tabulate(length, length) { (i, j) => ProdPunto(m1(i), transp_m2(j))}
  }

  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val length = m1.length
    val transp_m2 = transpuesta(m2)
    assert(m1.length == m2.length)
    // val tab: Vector[(Int, Int)] = Vector.tabulate(length*length) {(i: Int) => (i/length,i%length)}
    val joins = Vector.tabulate(length, length) {(i, j) => task(ProdPunto(m1(i), transp_m2(j)))}
    Vector.tabulate(length, length) {(i, j) => joins(i)(j).join()}
  }

  def subMatriz(m: Matriz, i: Int, j: Int, l: Int) : Matriz = {
    assert(i+l-1 < m.length && j+l-1 < m.length)
    Vector.tabulate(l,l) {(x, y) => m(i+x)(j+y)}
  }
  
  def sumMatriz(m1: Matriz, m2: Matriz) :Matriz = {
    assert(isPowerOfTwo(m1.length) && isPowerOfTwo(m2.length) && m1.length == m2.length)
    val length : Int = m1.length
    Vector.tabulate(length, length) {(i, j) => m1(i)(j) + m2(i)(j)}
  }
  
  def isPowerOfTwo(n: Int) : Boolean = {
    @tailrec
    def iPOT(n: Int): Boolean = {
      if (n==1) true
      else if (n%2 != 0) false
      else iPOT(n/2)
    }
    iPOT(n)
  }

  def multMatrizRec(m1: Matriz, m2: Matriz) : Matriz = {
    assert(isPowerOfTwo(m1.length) && isPowerOfTwo(m2.length) && m1.length == m2.length) {println("Las matrices de tener tamaño 2^k")}
    val length = m1.length
    if (length <= 2) multMatriz(m1, m2)
    else {
      val subMl = length/2
      val subMats = Vector.tabulate(2, 2)((i , j) => 
          sumMatriz(multMatrizRec(subMatriz(m1, i*subMl, 0, subMl), subMatriz(m2, 0, j*subMl, subMl)),
                    multMatrizRec(subMatriz(m1, i*subMl, subMl, subMl), subMatriz(m2, subMl, j*subMl, subMl))
          ))
      val result: Vector[Vector[Int]] = Vector(
        subMats(0)(0)(0) ++ subMats(0)(1)(0),
        subMats(0)(0)(1) ++ subMats(0)(1)(1),
        subMats(1)(0)(0) ++ subMats(1)(1)(0),
        subMats(1)(0)(1) ++ subMats(1)(1)(1))
      result
    }
  }

  def multMatrizRecPar(m1: Matriz, m2: Matriz, maxProf: Int, prof: Int) : Matriz = {
    assert(isPowerOfTwo(m1.length) && isPowerOfTwo(m2.length) && m1.length == m2.length) {println("Las matrices de tener tamaño 2^k")}
    val length = m1.length
    if (length <= 2) multMatriz(m1, m2)
    else {
      val subMl = length/2
      if (prof < maxProf) {
        val subMatTasks = Vector.tabulate(2, 2)((i, j) => 
            task(sumMatriz(
              multMatrizRecPar(subMatriz(m1, i*subMl, 0, subMl), subMatriz(m2, 0, j*subMl, subMl), maxProf, prof+1),
              multMatrizRecPar(subMatriz(m1, i*subMl, subMl, subMl), subMatriz(m2, subMl, j*subMl, subMl), maxProf, prof+1)
            )))

        val subMats = Vector.tabulate(2,2) {(i, j) => subMatTasks(i)(j).join()}

        val result: Vector[Vector[Int]] = Vector(
          subMats(0)(0)(0) ++ subMats(0)(1)(0),
          subMats(0)(0)(1) ++ subMats(0)(1)(1),
          subMats(1)(0)(0) ++ subMats(1)(1)(0),
          subMats(1)(0)(1) ++ subMats(1)(1)(1))

        result
      } else{
        multMatriz(m1, m2) // Por alguna razon el multMatriz es 40 veces mas rapido que multMatrizRec asi que usamos ese
      }                    // Igualmente multMatrizRecPar es el doble de rapida que multMatrizRec cuando usamos multMatrizRec
    }                      // en lugar de multMatriz
  }
  //Función para restar dos matrices de igual tamaño
  def restaMatriz(m1: Matriz , m2: Matriz ) : Matriz ={
    assert(m1.length == m2.length)
    val length = m1.length
    Vector.tabulate(length, length) {(i, j) => m1(i)(j) - m2(i)(j)}
  }
}

