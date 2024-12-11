package taller

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MatricesTest extends AnyFunSuite {
  type Matriz = Vector[Vector[Int]]
  val testMatrix = new Matrices()

  val m1 = Vector(
  Vector(2, 5, 3, 8),
  Vector(7, 1, 4, 6),
  Vector(9, 3, 2, 1),
  Vector(5, 8, 7, 4)
  )

  val m2 = Vector(
    Vector(4, 7, 1, 3),
    Vector(8, 2, 5, 9),
    Vector(6, 3, 4, 7),
    Vector(2, 1, 8, 5)
  )
  val expectedResult: Matriz = Vector(
    Vector(82, 41, 103, 112), 
    Vector(72, 69, 76, 88), 
    Vector(74, 76, 40, 73), 
    Vector(134, 76, 105, 156)
  
  )
  val result: Matriz = testMatrix.multMatrizRec(m1, m2)
  
  test("testMatrixMultiplication"){
    assert(result.isInstanceOf[Vector[Vector[Int]]], "Debe ser de tipo:  Vector[Vector[Int]]")

    assert(result.equals(expectedResult), s"INCORRECTO/ RESULTADO: $expectedResult")
  }

  val strassensResult: Matriz = testMatrix.strassensMatrixMult(m1, m2)///el mas importante
  test("Strassen's Matrix Multiplication"){
    assert(strassensResult.isInstanceOf[Vector[Vector[Int]]], "Debe ser de tipo:  Vector[Vector[Int]]")

    assert(strassensResult.equals(expectedResult), s"INCORRECTO/ RESULTADO: $expectedResult")
  }
}