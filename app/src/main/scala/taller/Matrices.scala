package taller
import scala.util.Random

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
    if (m1.length == m2.length) {
      Vector.tabulate(length, length) { (i, j) => ProdPunto(m1(i), transp_m2(j))}
    } else m1
  }
}