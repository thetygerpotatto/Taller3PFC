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

  def MultMatriz(m1: Matriz, m2: Matriz): Matriz = {

    val length = m1.length
    val transp_m2 = transpuesta(m2)
    assert(m1.length == m2.length)
    Vector.tabulate(length, length) { (i, j) => ProdPunto(m1(i), transp_m2(j))}
  }

  def MultMatrizPar(m1: Matriz, m2: Matriz): Matriz = {
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

  def MultMatrizRec(m1: Matriz, m2: Matriz) : Matriz = {
    assert(isPowerOfTwo(m1.length) && isPowerOfTwo(m2.length) && m1.length == m2.length)
    val length = m1.length
    if (length <= 2) MultMatriz(m1, m2)
    else {
      val subMl = length/2
      val sm1 = sumMatriz(MultMatrizRec(subMatriz(m1, 0, 0, subMl), subMatriz(m2, 0, 0, subMl)),
                MultMatrizRec(subMatriz(m1, 0, subMl, subMl), subMatriz(m2, subMl, 0, subMl)))

      val sm2 = sumMatriz(MultMatrizRec(subMatriz(m1, 0, 0, subMl), subMatriz(m2, 0, subMl, subMl)),
                MultMatrizRec(subMatriz(m1, 0, subMl, subMl), subMatriz(m2, subMl, subMl, subMl)))

      val sm3 = sumMatriz(MultMatrizRec(subMatriz(m1, subMl, 0, subMl), subMatriz(m2, 0, 0, subMl)),
                MultMatrizRec(subMatriz(m1, subMl, subMl, subMl), subMatriz(m2, subMl, 0, subMl)))

      val sm4 = sumMatriz(MultMatrizRec(subMatriz(m1, subMl, 0, subMl), subMatriz(m2, 0, subMl, subMl)),
                MultMatrizRec(subMatriz(m1, subMl, subMl, subMl), subMatriz(m2, subMl, subMl, subMl)))

      val result: Vector[Vector[Int]] = Vector(sm1(0) ++ sm2(0),
            sm1(1) ++ sm2(1),
            sm3(0) ++ sm4(0),
            sm3(1) ++ sm4(1))
      result
    }
  }
}
