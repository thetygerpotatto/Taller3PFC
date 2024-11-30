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
    length : Ing = m1.length
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
}
