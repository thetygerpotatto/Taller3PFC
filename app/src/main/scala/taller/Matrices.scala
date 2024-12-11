package taller
import scala.util.Random
import common._
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable._
import scala.collection.parallel._

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
  def prodPunto(v1: Vector[Int], v2: Vector[Int]): Int = {
    (v1 zip v2).map { case (i, j) => i * j }.sum
  }

  def prodPuntoParD(v1: ParVector[Int], v2: ParVector[Int]): Int = {
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
    Vector.tabulate(length, length) { (i, j) => prodPunto(m1(i), transp_m2(j))}
  }

  def multMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
    val length = m1.length
    val transp_m2 = transpuesta(m2)
    assert(m1.length == m2.length)
    // val tab: Vector[(Int, Int)] = Vector.tabulate(length*length) {(i: Int) => (i/length,i%length)}
    val joins = Vector.tabulate(length, length) {(i, j) => task(prodPunto(m1(i), transp_m2(j)))}
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
    assert(isPowerOfTwo(m1.length) && isPowerOfTwo(m2.length) && m1.length == m2.length) 
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
    assert(isPowerOfTwo(m1.length) && isPowerOfTwo(m2.length) && m1.length == m2.length) 
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
        multMatrizRec(m1, m2) // Por alguna razon el multMatriz es 40 veces mas rapido que multMatrizRec asi que usamos ese
      }                    // Igualmente multMatrizRecPar es el doble de rapida que multMatrizRec cuando usamos multMatrizRec
    }                      // en lugar de multMatriz
  }
  //Función para restar dos matrices de igual tamaño
  def restMatriz(m1: Matriz , m2: Matriz ) : Matriz ={
    assert(m1.length == m2.length)
    val length = m1.length
    Vector.tabulate(length, length) {(i, j) => m1(i)(j) - m2(i)(j)}
  }


  def strassensAlg(m1: Matriz, m2: Matriz):Matriz = {
    assert(m1.length == m2.length)
    
    val p1 = (m1(0)(0)) * (m2(0)(1) - m2(1)(1))
    val p2 = (m1(0)(0) + m1(0)(1)) * (m2(1)(1))
    val p3 = (m1(1)(0) + m1(1)(1)) * (m2(0)(0))
    val p4 = (m1(1)(1)) * (m2(1)(0) - m2(0)(0))
    val p5 = (m1(0)(0) + m1(1)(1)) * (m2(0)(0) + m2(1)(1))
    val p6 = (m1(0)(1) - m1(1)(1)) * (m2(1)(0) + m2(1)(1))
    val p7 = (m1(0)(0) - m1(1)(0)) * (m2(0)(0) + m2(0)(1))

    val c11 = p5 + p4 - p2 + p6
    val c12 = p1 + p2
    val c21 = p3 + p4
    val c22 = p5 + p1 - p3 - p7
    val result: Vector[Vector[Int]] = Vector(
      Vector(c11, c12),
      Vector(c21, c22))
    
    result
  }

  def strassensMatrixMult(m1: Matriz, m2: Matriz) : Matriz = {
    assert(isPowerOfTwo(m1.length) && isPowerOfTwo(m2.length) && m1.length == m2.length) 
    val length = m1.length
    if (length <= 2) strassensAlg(m1, m2)
    else {
      val subMl = length/2
      val subMats = Vector.tabulate(2, 2)((i , j) => 
        strassensMatrixMult(
          strassensMatrixMult(subMatriz(m1, i*subMl, 0, subMl), subMatriz(m2, 0, j*subMl, subMl)),
          strassensMatrixMult(subMatriz(m1, i*subMl, subMl, subMl), subMatriz(m2, subMl, j*subMl, subMl))
          )
          )
        
      val result: Vector[Vector[Int]] = Vector(
        subMats(0)(0)(0) ++ subMats(0)(1)(0),
        subMats(0)(0)(1) ++ subMats(0)(1)(1),
        subMats(1)(0)(0) ++ subMats(1)(1)(0),
        subMats(1)(0)(1) ++ subMats(1)(1)(1))
      result
    }
  }
}

