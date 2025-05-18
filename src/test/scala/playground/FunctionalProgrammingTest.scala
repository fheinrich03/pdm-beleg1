package playground

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class FunctionalProgrammingTest extends AnyFunSuite with Matchers {

  // Instanz der zu testenden Klasse
  val fp = new FunctionalProgramming()

  // 1. Test für die squareList Funktion
  test("squareList sollte jede Zahl in der Liste quadrieren") {
    val numbers = List(1, 2, 3, 4, 5)
    val expected = List(1, 4, 9, 16, 25)

    fp.squareList(numbers) should be (expected)
    fp.squareList(List.empty[Int]) should be (List.empty[Int])
    fp.squareList(List(-2, 0, 2)) should be (List(4, 0, 4))
  }

  // 2. Test für die filterEven Funktion
  test("filterEven sollte nur gerade Zahlen zurückgeben") {
    val numbers = List(1, 2, 3, 4, 5, 6)
    val expected = List(2, 4, 6)

    fp.filterEven(numbers) should be (expected)
    fp.filterEven(List.empty[Int]) should be (List.empty[Int])
    fp.filterEven(List(1, 3, 5)) should be (List.empty[Int])
  }

  // 3. Test für die sumList Funktion
  test("sumList sollte die Summe aller Zahlen in der Liste berechnen") {
    val numbers = List(1, 2, 3, 4, 5)
    val expected = 15

    fp.sumList(numbers) should be (expected)
    fp.sumList(List.empty[Int]) should be (0)
    fp.sumList(List(-1, 1)) should be (0)
  }

  // 4. Test für die multiplyAndFilter Funktion
  test("multiplyAndFilter sollte Zahlen mit 2 multiplizieren und nur diejenigen zurückgeben, die größer als 5 sind") {
    val numbers = List(1, 2, 3, 4, 5)
    val expected = List(6, 8, 10)

    fp.multiplyAndFilter(numbers) should be (expected)
    fp.multiplyAndFilter(List.empty[Int]) should be (List.empty[Int])
    fp.multiplyAndFilter(List(1, 2)) should be (List.empty[Int])
  }

  // 5. Test für die average Funktion
  test("average sollte den Durchschnitt einer Liste von Zahlen berechnen") {
    val numbers = List(1.0, 2.0, 3.0)
    val expected = Some(2.0)

    fp.average(numbers) should be (expected)
    fp.average(List.empty[Double]) should be (None)
    fp.average(List(0.5, 1.5)) should be (Some(1.0))
  }

  // 6. Test für die listLength Funktion
  test("listLength sollte rekursiv die Länge einer Liste berechnen") {
    val list = List(1, 2, 3, 4)
    val expected = 4

    fp.listLength(list) should be (expected)
    fp.listLength(List.empty[Int]) should be (0)
    fp.listLength(List("a", "b", "c")) should be (3)
  }

  // 7. Test für die zipWith Funktion
  test("zipWith sollte zwei Listen kombinieren, indem Elemente mit gleichem Index durch eine Funktion kombiniert werden") {
    val list1 = List(1, 2, 3)
    val list2 = List(4, 5, 6)
    val expected = List(5, 7, 9)

    fp.zipWith(list1, list2)(_ + _) should be (expected)
    fp.zipWith(list1, list2)(_ * _) should be (List(4, 10, 18))
    fp.zipWith(List.empty[Int], list2)(_ + _) should be (List.empty[Int])
    fp.zipWith(list1, List(4))(_ + _) should be (List(5))
  }

  // 8. Test für die normalizeData Funktion
  test("normalizeData sollte eine Liste von Datenpunkten normalisieren") {
    val data = List(List(1.0, 2.0), List(3.0, 4.0), List(5.0, 1.0))
    val expected = List(List(0.2, 0.5), List(0.6, 1.0), List(1.0, 0.25))

    // Hilfsfunktion zum Vergleichen von Double-Listen mit Toleranz
    def approxEqual(actual: List[List[Double]], expected: List[List[Double]], tolerance: Double = 0.001): Boolean = {
      actual.size == expected.size &&
        actual.zip(expected).forall { case (actualRow, expectedRow) =>
          actualRow.size == expectedRow.size &&
            actualRow.zip(expectedRow).forall { case (a, e) =>
              Math.abs(a - e) < tolerance
            }
        }
    }

    approxEqual(fp.normalizeData(data), expected) should be (true)
    fp.normalizeData(List.empty[List[Double]]) should be (List.empty[List[Double]])

    val singleDimensionData = List(List(2.0), List(4.0), List(6.0))
    val expectedNormalized = List(List(0.33333), List(0.66667), List(1.0))
    approxEqual(fp.normalizeData(singleDimensionData), expectedNormalized) should be (true)
  }
}