package common
import org.scalameter._
import taller.Matrices
import scala.collection.parallel.immutable._
import scala.collection.parallel._
import scala.util.Random
import scala.collection.parallel.CollectionConverters._

class Benchmark {
  type Matriz = Vector[Vector[Int]]
  def compararAlgoritmos(f1: (Matriz, Matriz) => Matriz, f2: (Matriz, Matriz, Int, Int) => Matriz)(m1: Matriz, m2: Matriz): (Quantity[Double], Quantity[Double], Double) = {
    val alg1 = withWarmer(new Warmer.Default) measure {f1(m1, m2)}
    val alg2 = withWarmer(new Warmer.Default) measure {f2(m1, m2, 1, 0)}
    val acc = alg1.value/alg2.value
    (alg1, alg2, acc)
  }

  // def compararProdPunto(n: Int): (Quantity[Double], Quantity[Double], Double) = {
  //   // val matObj = new Matrices()
  //   // val v1 = matObj.vectorAlAzar(n, 100)
  //   // val v2 = matObj.vectorAlAzar(n, 100)
  //   // val p1 = withWarmer(new Warmer.Default) measure {matObj.prodPunto(v1, v2)}
  //   // //val p2 = withWarmer(new Warmer.Default) measure {matObj.prodPuntoParD(v1.par, v2.par)}
  //   // val diff = p1.value / p2.value 
  //   // (p1, p2, diff)
  // }


}

