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
  
  //Multmatriz tests
  test("Multmatriz 1") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(0, 2), Vector(1, 2))
    val v2 = Vector(Vector(1, 1), Vector(1, 2))
    val res = matObj.multMatriz(v1, v2)
    assert(res == Vector(Vector(2, 4), Vector(3, 5)))
  }

  test("Multmatriz 2") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(8, 1, 0), Vector(6, 0, 3), Vector(9, 4, 0))
    val v2 = Vector(Vector(5, 4, 0), Vector(4, 9, 2), Vector(0, 6, 3))
    val res = Vector(Vector(44, 41, 2), Vector(30, 42, 9), Vector(61, 72, 8))
    assert(matObj.multMatriz(v1, v2) == res)
  }

  test("Multmatriz 3") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(1, 1, 0, 3, 7, 5), Vector(7, 7, 8, 2, 7, 4), Vector(3, 5, 5, 2, 3, 3), Vector(4, 2, 4, 0, 8, 2), Vector(1, 0, 5, 5, 3, 4), Vector(5, 9, 3, 8, 4, 0))
    val v2 = Vector(Vector(4, 4, 7, 3, 5, 9), Vector(6, 1, 0, 6, 1, 5), Vector(4, 5, 6, 6, 5, 3), Vector(2, 4, 7, 3, 2, 5), Vector(0, 8, 3, 6, 1, 9), Vector(3, 5, 3, 7, 3, 9))
    val res = Vector(Vector(31, 98, 64, 95, 34, 137), Vector(118, 159, 144, 187, 105, 231), Vector(75, 89, 83, 114, 61, 131), Vector(50, 112, 82, 110, 56, 148), Vector(46, 93, 93, 94, 55, 112), Vector(102, 108, 121, 135, 69, 175))
    assert(matObj.multMatriz(v1, v2) == res)
  }

  test("Multmatriz 4") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(7, 5, 6, 8, 0, 5, 1, 7), Vector(1, 6, 9, 8, 5, 3, 6, 6), Vector(0, 4, 6, 0, 3, 1, 2, 9), Vector(1, 4, 6, 3, 6, 8, 7, 6), Vector(9, 6, 3, 3, 7, 0, 0, 3), Vector(7, 1, 5, 5, 3, 0, 3, 3), Vector(8, 9, 3, 1, 9, 8, 8, 3), Vector(0, 8, 9, 6, 7, 6, 5, 5))
    val v2 = Vector(Vector(7, 8, 0, 1, 1, 0, 2, 7), Vector(2, 5, 6, 4, 6, 0, 8, 2), Vector(6, 3, 6, 0, 0, 1, 1, 4), Vector(3, 1, 5, 3, 3, 4, 0, 6), Vector(2, 8, 2, 8, 6, 4, 9, 6), Vector(8, 9, 5, 2, 2, 1, 8, 0), Vector(9, 9, 5, 0, 8, 7, 8, 5), Vector(0, 2, 1, 4, 2, 7, 1, 5))
    val res = Vector(Vector(168, 175, 143, 89, 93, 99, 115, 171), Vector(185, 206, 191, 119, 157, 148, 182, 193), Vector(76, 107, 90, 78, 78, 96, 98, 105), Vector(199, 244, 168, 114, 154, 141, 220, 158), Vector(116, 176, 86, 110, 102, 64, 135, 162), Vector(129, 138, 85, 62, 76, 79, 81, 149), Vector(249, 341, 178, 147, 205, 128, 303, 201), Vector(195, 238, 206, 138, 170, 137, 229, 180))
    assert(matObj.multMatriz(v1, v2) == res)
  }

  test("Multmatriz 5") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(8, 2, 3, 1, 5, 6, 9, 4, 5, 2), Vector(9, 3, 6, 4, 4, 3, 1, 0, 9, 7), Vector(4, 2, 3, 6, 1, 6, 3, 9, 4, 9), Vector(2, 0, 4, 6, 6, 0, 4, 5, 3, 7), Vector(5, 4, 8, 3, 7, 7, 8, 8, 4, 8), Vector(1, 7, 2, 9, 6, 6, 2, 5, 0, 0), Vector(9, 6, 9, 5, 8, 3, 3, 0, 3, 9), Vector(8, 1, 1, 2, 9, 7, 8, 9, 4, 7), Vector(7, 2, 5, 9, 4, 5, 5, 2, 7, 3), Vector(8, 4, 8, 1, 5, 5, 3, 0, 2, 8))
    val v2 = Vector(Vector(6, 0, 3, 0, 7, 3, 7, 8, 6, 3), Vector(8, 8, 7, 6, 6, 1, 3, 2, 8, 0), Vector(1, 7, 2, 0, 9, 4, 1, 5, 8, 0), Vector(9, 2, 3, 8, 7, 6, 8, 6, 1, 0), Vector(7, 2, 6, 1, 5, 6, 0, 9, 3, 6), Vector(9, 3, 8, 4, 1, 3, 0, 9, 4, 1), Vector(4, 1, 0, 7, 5, 2, 8, 5, 1, 7), Vector(7, 3, 4, 5, 0, 3, 9, 2, 3, 4), Vector(1, 0, 0, 7, 2, 6, 5, 2, 6, 4), Vector(7, 3, 6, 1, 1, 7, 9, 3, 2, 8))
    val res = Vector(Vector(248, 94, 153, 169, 190, 166, 224, 257, 183, 175), Vector(237, 113, 162, 143, 216, 216, 226, 239, 223, 153), Vector(300, 126, 194, 188, 152, 206, 291, 218, 169, 169), Vector(215, 92, 130, 135, 155, 184, 221, 183, 119, 158), Vector(357, 185, 246, 215, 250, 252, 307, 320, 250, 232), Vector(284, 135, 187, 183, 176, 145, 163, 214, 146, 79), Vector(317, 176, 228, 147, 288, 243, 250, 306, 254, 183), Vector(349, 114, 227, 195, 192, 232, 304, 314, 194, 249), Vector(279, 112, 162, 205, 236, 215, 252, 272, 198, 145), Vector(247, 142, 189, 100, 216, 185, 190, 251, 211, 152))
    assert(matObj.multMatriz(v1, v2) == res)
  }


  // tests MultmatrizPar
  test("MultmatrizPAr 1") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(0, 2), Vector(1, 2))
    val v2 = Vector(Vector(1, 1), Vector(1, 2))
    val res = matObj.multMatriz(v1, v2)
    assert(res == Vector(Vector(2, 4), Vector(3, 5)))
  }

  test("MultmatrizPar 2") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(8, 1, 0), Vector(6, 0, 3), Vector(9, 4, 0))
    val v2 = Vector(Vector(5, 4, 0), Vector(4, 9, 2), Vector(0, 6, 3))
    val res = Vector(Vector(44, 41, 2), Vector(30, 42, 9), Vector(61, 72, 8))
    assert(matObj.multMatrizPar(v1, v2) == res)
  }

  test("MultmatrizPar 3") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(1, 1, 0, 3, 7, 5), Vector(7, 7, 8, 2, 7, 4), Vector(3, 5, 5, 2, 3, 3), Vector(4, 2, 4, 0, 8, 2), Vector(1, 0, 5, 5, 3, 4), Vector(5, 9, 3, 8, 4, 0))
    val v2 = Vector(Vector(4, 4, 7, 3, 5, 9), Vector(6, 1, 0, 6, 1, 5), Vector(4, 5, 6, 6, 5, 3), Vector(2, 4, 7, 3, 2, 5), Vector(0, 8, 3, 6, 1, 9), Vector(3, 5, 3, 7, 3, 9))
    val res = Vector(Vector(31, 98, 64, 95, 34, 137), Vector(118, 159, 144, 187, 105, 231), Vector(75, 89, 83, 114, 61, 131), Vector(50, 112, 82, 110, 56, 148), Vector(46, 93, 93, 94, 55, 112), Vector(102, 108, 121, 135, 69, 175))
    assert(matObj.multMatrizPar(v1, v2) == res)
  }

  test("MultmatrizPar 4") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(7, 5, 6, 8, 0, 5, 1, 7), Vector(1, 6, 9, 8, 5, 3, 6, 6), Vector(0, 4, 6, 0, 3, 1, 2, 9), Vector(1, 4, 6, 3, 6, 8, 7, 6), Vector(9, 6, 3, 3, 7, 0, 0, 3), Vector(7, 1, 5, 5, 3, 0, 3, 3), Vector(8, 9, 3, 1, 9, 8, 8, 3), Vector(0, 8, 9, 6, 7, 6, 5, 5))
    val v2 = Vector(Vector(7, 8, 0, 1, 1, 0, 2, 7), Vector(2, 5, 6, 4, 6, 0, 8, 2), Vector(6, 3, 6, 0, 0, 1, 1, 4), Vector(3, 1, 5, 3, 3, 4, 0, 6), Vector(2, 8, 2, 8, 6, 4, 9, 6), Vector(8, 9, 5, 2, 2, 1, 8, 0), Vector(9, 9, 5, 0, 8, 7, 8, 5), Vector(0, 2, 1, 4, 2, 7, 1, 5))
    val res = Vector(Vector(168, 175, 143, 89, 93, 99, 115, 171), Vector(185, 206, 191, 119, 157, 148, 182, 193), Vector(76, 107, 90, 78, 78, 96, 98, 105), Vector(199, 244, 168, 114, 154, 141, 220, 158), Vector(116, 176, 86, 110, 102, 64, 135, 162), Vector(129, 138, 85, 62, 76, 79, 81, 149), Vector(249, 341, 178, 147, 205, 128, 303, 201), Vector(195, 238, 206, 138, 170, 137, 229, 180))
    assert(matObj.multMatrizPar(v1, v2) == res)
  }

  test("MultmatrizPar 5") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(8, 2, 3, 1, 5, 6, 9, 4, 5, 2), Vector(9, 3, 6, 4, 4, 3, 1, 0, 9, 7), Vector(4, 2, 3, 6, 1, 6, 3, 9, 4, 9), Vector(2, 0, 4, 6, 6, 0, 4, 5, 3, 7), Vector(5, 4, 8, 3, 7, 7, 8, 8, 4, 8), Vector(1, 7, 2, 9, 6, 6, 2, 5, 0, 0), Vector(9, 6, 9, 5, 8, 3, 3, 0, 3, 9), Vector(8, 1, 1, 2, 9, 7, 8, 9, 4, 7), Vector(7, 2, 5, 9, 4, 5, 5, 2, 7, 3), Vector(8, 4, 8, 1, 5, 5, 3, 0, 2, 8))
    val v2 = Vector(Vector(6, 0, 3, 0, 7, 3, 7, 8, 6, 3), Vector(8, 8, 7, 6, 6, 1, 3, 2, 8, 0), Vector(1, 7, 2, 0, 9, 4, 1, 5, 8, 0), Vector(9, 2, 3, 8, 7, 6, 8, 6, 1, 0), Vector(7, 2, 6, 1, 5, 6, 0, 9, 3, 6), Vector(9, 3, 8, 4, 1, 3, 0, 9, 4, 1), Vector(4, 1, 0, 7, 5, 2, 8, 5, 1, 7), Vector(7, 3, 4, 5, 0, 3, 9, 2, 3, 4), Vector(1, 0, 0, 7, 2, 6, 5, 2, 6, 4), Vector(7, 3, 6, 1, 1, 7, 9, 3, 2, 8))
    val res = Vector(Vector(248, 94, 153, 169, 190, 166, 224, 257, 183, 175), Vector(237, 113, 162, 143, 216, 216, 226, 239, 223, 153), Vector(300, 126, 194, 188, 152, 206, 291, 218, 169, 169), Vector(215, 92, 130, 135, 155, 184, 221, 183, 119, 158), Vector(357, 185, 246, 215, 250, 252, 307, 320, 250, 232), Vector(284, 135, 187, 183, 176, 145, 163, 214, 146, 79), Vector(317, 176, 228, 147, 288, 243, 250, 306, 254, 183), Vector(349, 114, 227, 195, 192, 232, 304, 314, 194, 249), Vector(279, 112, 162, 205, 236, 215, 252, 272, 198, 145), Vector(247, 142, 189, 100, 216, 185, 190, 251, 211, 152))
    assert(matObj.multMatrizPar(v1, v2) == res)
  }
  // subMatriz
  test("subMatriz 1") {
    val matObj = new Matrices()
    val v1 = Vector( Vector(2, 7, 9, 1), 
                     Vector(9, 7, 8, 4), 
                     Vector(1, 7, 7, 8), 
                     Vector(2, 3, 6, 6))
    val res = Vector(Vector(7, 8), Vector(6, 6))
    assert(matObj.subMatriz(v1, 2, 2, 2) == res)
  }

  test("subMatriz 2") {
    val matObj = new Matrices()
    val v1 = Vector( Vector(2, 7, 9, 1), 
                     Vector(9, 7, 8, 4), 
                     Vector(1, 7, 7, 8), 
                     Vector(2, 3, 6, 6))
    val res = Vector(Vector(7, 8), Vector(7, 7))
    assert(matObj.subMatriz(v1, 1, 1, 2) == res)
  }

  test("subMatriz 3") {
    val matObj = new Matrices()
    val v1 = Vector( Vector(2, 7, 9, 1), 
                     Vector(9, 7, 8, 4), 
                     Vector(1, 7, 7, 8), 
                     Vector(2, 3, 6, 6))
    assert(matObj.subMatriz(v1, 0, 0, 4) == v1)
  }

  test("subMatriz 4") {
    val matObj = new Matrices()
    val v1 = Vector( Vector(2, 7, 9, 1), 
                     Vector(9, 7, 8, 4), 
                     Vector(1, 7, 7, 8), 
                     Vector(2, 3, 6, 6))
    val res = Vector(Vector(7, 8, 4), Vector(7, 7, 8), Vector(3, 6, 6))
    assert(matObj.subMatriz(v1, 1, 1, 3) == res)
  }
  
  test("subMatriz 5") {
    val matObj = new Matrices()
    val v1 = Vector( Vector(2, 7, 9, 1), 
                     Vector(9, 7, 8, 4), 
                     Vector(1, 7, 7, 8), 
                     Vector(2, 3, 6, 6))
    val res = Vector(Vector(9, 7), Vector(1, 7))
    assert(matObj.subMatriz(v1, 1, 0, 2) == res)
  }

      //sumMatriz
  test("sumMatriz 1") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(3, 3), Vector(4, 2))
    val v2 = Vector(Vector(8, 1), Vector(5, 5))
    val res = Vector(Vector(11, 4), Vector(9, 7))
    assert(matObj.sumMatriz(v1, v2) == res)
  }

  test("sumMatriz 2") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(0, 8, 7, 6), Vector(2, 8, 8, 9), Vector(2, 1, 2, 4), Vector(1, 2, 5, 0))
    val v2 = Vector(Vector(3, 4, 4, 3), Vector(2, 2, 7, 9), Vector(1, 6, 0, 5), Vector(4, 4, 5, 1))
    val res = Vector(Vector(3, 12, 11, 9), Vector(4, 10, 15, 18), Vector(3, 7, 2, 9), Vector(5, 6, 10, 1))
    assert(matObj.sumMatriz(v1, v2) == res)
  }

  test("sumMatriz 3") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(7, 5, 8, 3, 7, 6, 8, 2), Vector(1, 8, 5, 1, 9, 1, 0, 1), Vector(0, 5, 9, 5, 1, 7, 4, 4), Vector(8, 4, 4, 9, 5, 8, 5, 5), Vector(7, 6, 8, 6, 9, 5, 3, 0), Vector(8, 6, 5, 7, 0, 0, 0, 3), Vector(4, 2, 0, 5, 4, 4, 9, 9), Vector(7, 3, 1, 9, 7, 2, 1, 1))
    val v2 = Vector(Vector(5, 4, 6, 9, 9, 0, 4, 8), Vector(2, 7, 2, 5, 9, 2, 7, 5), Vector(3, 8, 8, 2, 8, 3, 9, 9), Vector(7, 0, 5, 8, 9, 0, 0, 2), Vector(0, 2, 7, 2, 7, 7, 5, 7), Vector(3, 5, 8, 1, 4, 5, 4, 1), Vector(4, 3, 0, 1, 4, 9, 6, 8), Vector(9, 8, 0, 8, 2, 9, 1, 7))
    val res = Vector(Vector(12, 9, 14, 12, 16, 6, 12, 10), Vector(3, 15, 7, 6, 18, 3, 7, 6), Vector(3, 13, 17, 7, 9, 10, 13, 13), Vector(15, 4, 9, 17, 14, 8, 5, 7), Vector(7, 8, 15, 8, 16, 12, 8, 7), Vector(11, 11, 13, 8, 4, 5, 4, 4), Vector(8, 5, 0, 6, 8, 13, 15, 17), Vector(16, 11, 1, 17, 9, 11, 2, 8))       
    assert(matObj.sumMatriz(v1, v2) == res)
  }

  test("sumMatriz 4"){
    val matObj = new Matrices()
    val v1 = Vector(Vector(5))
    val v2 = Vector(Vector(4))
    val res = Vector(Vector(9))
    assert(matObj.sumMatriz(v1, v2) == res)
  }

  test("sumMatriz 5"){
    val matObj = new Matrices()
    val v1 = Vector(Vector(9, 9, 2, 3), Vector(1, 4, 1, 6), Vector(3, 0, 4, 9), Vector(8, 9, 5, 7))
    val v2 = Vector(Vector(5, 1, 1, 5), Vector(3, 8, 4, 1), Vector(8, 7, 9, 8), Vector(5, 1, 0, 2))
    val res = Vector(Vector(14, 10, 3, 8), Vector(4, 12, 5, 7), Vector(11, 7, 13, 17), Vector(13, 10, 5, 9))
    assert(matObj.sumMatriz(v1, v2) == res)
  }

  //restMatriz 
 test("restMatriz 1") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(7, 3, 0, 9), Vector(0, 7, 5, 9), Vector(7, 7, 3, 6), Vector(4, 1, 5, 1))
    val v2 = Vector(Vector(7, 6, 5, 5), Vector(3, 3, 3, 0), Vector(7, 1, 3, 2), Vector(9, 8, 3, 7))
    val res = Vector(Vector(0, -3, -5, 4), Vector(-3, 4, 2, 9), Vector(0, 6, 0, 4), Vector(-5, -7, 2, -6))
    assert(matObj.restMatriz(v1, v2) == res)
 }

 test("restMatriz 2") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(0, 5, 3, 6), Vector(3, 0, 9, 7), Vector(4, 7, 5, 9), Vector(9, 3, 4, 8))
    val v2 = Vector(Vector(8, 7, 2, 6), Vector(1, 7, 5, 8), Vector(6, 9, 1, 6), Vector(5, 2, 8, 8))
    val res = Vector(Vector(-8, -2, 1, 0), Vector(2, -7, 4, -1), Vector(-2, -2, 4, 3), Vector(4, 1, -4, 0))
    assert(matObj.restMatriz(v1, v2) == res)
 }

 test("restMatriz 3") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(2))
     val v2 = Vector(Vector(8))
     val res = Vector(Vector(-6))
     assert(matObj.restMatriz(v1, v2) == res)
 }

 test("restMatriz 4") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(9, 3), Vector(3, 3))
     val v2 = Vector(Vector(1, 6), Vector(0, 0))
     val res = Vector(Vector(8, -3), Vector(3, 3))
     assert(matObj.restMatriz(v1, v2) == res)
 }

 test("restMatriz 5") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(4, 1, 4, 4, 8, 2, 6, 8), Vector(5, 8, 7, 2, 0, 3, 7, 3), Vector(8, 7, 8, 9, 4, 6, 7, 8), Vector(1, 0, 0, 9, 1, 3, 6, 8), Vector(8, 2, 7, 7, 5, 6, 6, 4), Vector(2, 9, 1, 7, 7, 0, 1, 8), Vector(5, 7, 0, 2, 8, 0, 7, 6), Vector(2, 5, 9, 3, 5, 7, 7, 1))
     val v2 = Vector(Vector(2, 5, 9, 8, 1, 8, 7, 8), Vector(3, 8, 5, 3, 5, 8, 7, 2), Vector(8, 3, 1, 4, 4, 4, 2, 0), Vector(2, 5, 2, 8, 2, 8, 0, 5), Vector(6, 7, 8, 0, 6, 2, 9, 8), Vector(6, 3, 6, 1, 8, 7, 4, 0), Vector(0, 8, 9, 5, 6, 9, 9, 6), Vector(4, 3, 0, 9, 2, 5, 4, 4))
     val res = Vector(Vector(2, -4, -5, -4, 7, -6, -1, 0), Vector(2, 0, 2, -1, -5, -5, 0, 1), Vector(0, 4, 7, 5, 0, 2, 5, 8), Vector(-1, -5, -2, 1, -1, -5, 6, 3), Vector(2, -5, -1, 7, -1, 4, -3, -4), Vector(-4, 6, -5, 6, -1, -7, -3, 8), Vector(5, -1, -9, -3, 2, -9, -2, 0), Vector(-2, 2, 9, -6, 3, 2, 3, -3))
     assert(matObj.restMatriz(v1, v2) == res)
 }

 //multMatrizRec
  test("MultMatrizRec 1") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(0, 2), Vector(1, 2))
    val v2 = Vector(Vector(1, 1), Vector(1, 2))
    val res = matObj.multMatrizRec(v1, v2)
    assert(res == Vector(Vector(2, 4), Vector(3, 5)))
  }

test("multMatrizRec 2") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(1))
     val v2 = Vector(Vector(2))
     val res = Vector(Vector(2))
     assert(matObj.multMatrizRec(v1, v2) == res)
 }
test("multMatrizRec 3") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(6, 0), Vector(1, 9))
     val v2 = Vector(Vector(7, 5), Vector(7, 5))
     val res = Vector(Vector(42, 30), Vector(70, 50))
     assert(matObj.multMatrizRec(v1, v2) == res)
 }
test("multMatrizRec 4") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(8, 3, 0, 9), Vector(0, 2, 6, 7), Vector(3, 8, 6, 8), Vector(3, 9, 9, 1))
     val v2 = Vector(Vector(1, 4, 8, 5), Vector(9, 6, 4, 2), Vector(5, 6, 0, 9), Vector(4, 4, 5, 6))
     val res = Vector(Vector(71, 86, 121, 100), Vector(76, 76, 43, 100), Vector(137, 128, 96, 133), Vector(133, 124, 65, 120))
     assert(matObj.multMatrizRec(v1, v2) == res)
 }
test("multMatrizRec 5") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(5, 9, 0, 1, 9, 7, 3, 5), Vector(7, 5, 4, 4, 9, 4, 8, 0), Vector(6, 9, 0, 1, 2, 3, 2, 7), Vector(1, 4, 2, 1, 5, 8, 4, 8), Vector(9, 7, 0, 9, 9, 3, 4, 9), Vector(8, 3, 4, 7, 7, 6, 6, 5), Vector(8, 9, 3, 7, 7, 6, 4, 5), Vector(0, 4, 5, 2, 6, 7, 9, 5))
     val v2 = Vector(Vector(4, 9, 5, 0, 0, 3, 7, 2), Vector(7, 7, 8, 0, 2, 8, 0, 6), Vector(0, 3, 6, 6, 2, 4, 6, 1), Vector(3, 4, 8, 8, 0, 1, 8, 7), Vector(2, 8, 1, 3, 7, 5, 7, 1), Vector(0, 1, 5, 3, 0, 3, 1, 6), Vector(0, 3, 4, 3, 5, 1, 8, 1), Vector(2, 2, 0, 2, 7, 8, 7, 2))
     val res = Vector(Vector(114, 210, 161, 75, 131, 197, 172, 135), Vector(93, 226, 192, 119, 121, 146, 236, 117), Vector(148, 271, 213, 138, 160, 222, 296, 172), Vector(98, 223, 205, 147, 128, 170, 274, 146))
     assert(matObj.multMatrizRec(v1, v2) == res)
 }
  //multMatrizRecPar
test("multMatrizRecPar 1") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(7))
     val v2 = Vector(Vector(8))
     val res = Vector(Vector(56))
     assert(matObj.multMatrizRecPar(v1, v2, 1, 0) == res)
 }
test("multMatrizRecPar 2") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(5, 0), Vector(4, 5))
     val v2 = Vector(Vector(5, 2), Vector(4, 7))
     val res = Vector(Vector(25, 10), Vector(40, 43))
     assert(matObj.multMatrizRecPar(v1, v2, 1, 0) == res)
 }
test("multMatrizRecPar 3") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(9, 1, 3, 5), Vector(7, 0, 1, 9), Vector(2, 6, 4, 7), Vector(5, 7, 6, 2))
     val v2 = Vector(Vector(7, 6, 6, 5), Vector(9, 0, 2, 0), Vector(6, 7, 8, 0), Vector(5, 3, 0, 1))
     val res = Vector(Vector(115, 90, 80, 50), Vector(100, 76, 50, 44), Vector(127, 61, 56, 17), Vector(144, 78, 92, 27))
     assert(matObj.multMatrizRecPar(v1, v2, 1, 0) == res)
 }
test("multMatrizRecPar 4") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(0, 0, 1, 7), Vector(4, 4, 8, 3), Vector(0, 9, 2, 5), Vector(0, 1, 9, 8))
     val v2 = Vector(Vector(1, 5, 4, 1), Vector(5, 3, 0, 8), Vector(9, 1, 6, 5), Vector(6, 0, 3, 9))
     val res = Vector(Vector(51, 1, 27, 68), Vector(114, 40, 73, 103), Vector(93, 29, 27, 127), Vector(134, 12, 78, 125))
     assert(matObj.multMatrizRecPar(v1, v2, 1, 0) == res)
 }
test("multMatrizRecPar 5") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(8, 3, 5, 5, 8, 6, 0, 7), Vector(6, 6, 3, 3, 4, 3, 9, 0), Vector(3, 1, 7, 7, 5, 6, 7, 5), Vector(9, 5, 3, 0, 6, 4, 7, 5), Vector(0, 3, 0, 1, 4, 0, 8, 5), Vector(1, 0, 6, 7, 5, 7, 1, 0), Vector(4, 4, 8, 6, 4, 6, 5, 5), Vector(6, 8, 7, 4, 4, 9, 2, 0))
     val v2 = Vector(Vector(6, 6, 1, 6, 4, 9, 7, 7), Vector(0, 1, 2, 2, 7, 5, 2, 8), Vector(8, 9, 2, 6, 7, 5, 1, 5), Vector(8, 4, 0, 6, 9, 0, 8, 0), Vector(4, 4, 4, 1, 3, 4, 7, 0), Vector(0, 3, 2, 6, 7, 1, 7, 8), Vector(4, 2, 2, 2, 1, 8, 2, 1), Vector(2, 7, 5, 3, 0, 6, 0, 7))
     val res = Vector(Vector(174, 215, 103, 179, 199, 192, 205, 202), Vector(136, 124, 64, 124, 156, 190, 148, 138), Vector(66, 74, 63, 47, 50, 125, 58, 67), Vector(134, 131, 49, 133, 174, 74, 155, 94))
     assert(matObj.multMatrizRecPar(v1, v2, 1, 0) == res)
 }
 
  test("MultStrassen 1") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(0, 2), Vector(1, 2))
    val v2 = Vector(Vector(1, 1), Vector(1, 2))
    val res = matObj.multStrassen(v1, v2)
    assert(res == Vector(Vector(2, 4), Vector(3, 5)))
  }

test("MultStrassen 2") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(1))
     val v2 = Vector(Vector(2))
     val res = Vector(Vector(2))
     assert(matObj.multStrassen(v1, v2) == res)
 }
test("MultStrassen 3") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(6, 0), Vector(1, 9))
     val v2 = Vector(Vector(7, 5), Vector(7, 5))
     val res = Vector(Vector(42, 30), Vector(70, 50))
     assert(matObj.multStrassen(v1, v2) == res)
 }
test("MultStrassen 4") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(8, 3, 0, 9), Vector(0, 2, 6, 7), Vector(3, 8, 6, 8), Vector(3, 9, 9, 1))
     val v2 = Vector(Vector(1, 4, 8, 5), Vector(9, 6, 4, 2), Vector(5, 6, 0, 9), Vector(4, 4, 5, 6))
     val res = Vector(Vector(71, 86, 121, 100), Vector(76, 76, 43, 100), Vector(137, 128, 96, 133), Vector(133, 124, 65, 120))
     assert(matObj.multStrassen(v1, v2) == res)
 }
test("MultStrassen 5") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(5, 9, 0, 1, 9, 7, 3, 5), Vector(7, 5, 4, 4, 9, 4, 8, 0), Vector(6, 9, 0, 1, 2, 3, 2, 7), Vector(1, 4, 2, 1, 5, 8, 4, 8), Vector(9, 7, 0, 9, 9, 3, 4, 9), Vector(8, 3, 4, 7, 7, 6, 6, 5), Vector(8, 9, 3, 7, 7, 6, 4, 5), Vector(0, 4, 5, 2, 6, 7, 9, 5))
     val v2 = Vector(Vector(4, 9, 5, 0, 0, 3, 7, 2), Vector(7, 7, 8, 0, 2, 8, 0, 6), Vector(0, 3, 6, 6, 2, 4, 6, 1), Vector(3, 4, 8, 8, 0, 1, 8, 7), Vector(2, 8, 1, 3, 7, 5, 7, 1), Vector(0, 1, 5, 3, 0, 3, 1, 6), Vector(0, 3, 4, 3, 5, 1, 8, 1), Vector(2, 2, 0, 2, 7, 8, 7, 2))
     val res = Vector(Vector(114, 210, 161, 75, 131, 197, 172, 135), Vector(93, 226, 192, 119, 121, 146, 236, 117), Vector(148, 271, 213, 138, 160, 222, 296, 172), Vector(98, 223, 205, 147, 128, 170, 274, 146))
     assert(matObj.multStrassen(v1, v2) == res)
 }

test("MultStrassenPar 1") {
    val matObj = new Matrices()
    val v1 = Vector(Vector(0, 2), Vector(1, 2))
    val v2 = Vector(Vector(1, 1), Vector(1, 2))
    val res = matObj.multStrassenPar(v1, v2, 1, 0)
    assert(res == Vector(Vector(2, 4), Vector(3, 5)))
  }

test("MultStrassenPar 2") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(1))
     val v2 = Vector(Vector(2))
     val res = Vector(Vector(2))
     assert(matObj.multStrassenPar(v1, v2, 1, 0) == res)
 }
test("MultStrassenPar 3") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(6, 0), Vector(1, 9))
     val v2 = Vector(Vector(7, 5), Vector(7, 5))
     val res = Vector(Vector(42, 30), Vector(70, 50))
     assert(matObj.multStrassenPar(v1, v2, 1, 0) == res)
 }
test("MultStrassenPar 4") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(8, 3, 0, 9), Vector(0, 2, 6, 7), Vector(3, 8, 6, 8), Vector(3, 9, 9, 1))
     val v2 = Vector(Vector(1, 4, 8, 5), Vector(9, 6, 4, 2), Vector(5, 6, 0, 9), Vector(4, 4, 5, 6))
     val res = Vector(Vector(71, 86, 121, 100), Vector(76, 76, 43, 100), Vector(137, 128, 96, 133), Vector(133, 124, 65, 120))
     assert(matObj.multStrassenPar(v1, v2, 1, 0) == res)
 }
test("MultStrassenPar 5") {
     val matObj = new Matrices()
     val v1 = Vector(Vector(5, 9, 0, 1, 9, 7, 3, 5), Vector(7, 5, 4, 4, 9, 4, 8, 0), Vector(6, 9, 0, 1, 2, 3, 2, 7), Vector(1, 4, 2, 1, 5, 8, 4, 8), Vector(9, 7, 0, 9, 9, 3, 4, 9), Vector(8, 3, 4, 7, 7, 6, 6, 5), Vector(8, 9, 3, 7, 7, 6, 4, 5), Vector(0, 4, 5, 2, 6, 7, 9, 5))
     val v2 = Vector(Vector(4, 9, 5, 0, 0, 3, 7, 2), Vector(7, 7, 8, 0, 2, 8, 0, 6), Vector(0, 3, 6, 6, 2, 4, 6, 1), Vector(3, 4, 8, 8, 0, 1, 8, 7), Vector(2, 8, 1, 3, 7, 5, 7, 1), Vector(0, 1, 5, 3, 0, 3, 1, 6), Vector(0, 3, 4, 3, 5, 1, 8, 1), Vector(2, 2, 0, 2, 7, 8, 7, 2))
     val res = Vector(Vector(114, 210, 161, 75, 131, 197, 172, 135), Vector(93, 226, 192, 119, 121, 146, 236, 117), Vector(148, 271, 213, 138, 160, 222, 296, 172), Vector(98, 223, 205, 147, 128, 170, 274, 146))
     assert(matObj.multStrassenPar(v1, v2, 1, 0) == res)
 }
}
