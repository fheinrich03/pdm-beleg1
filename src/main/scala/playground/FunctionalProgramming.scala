package playground

class FunctionalProgramming {

  // 1. Listen transformieren mit map
  def squareList(numbers: List[Int]): List[Int] = {
    numbers.map(n => n * n)
  }
  // Beispielaufruf
  // squareList(List(1, 2, 3, 4, 5)) sollte List(1, 4, 9, 16, 25) zurückgeben

  // 2. Filtern mit filter
  def filterEven(numbers: List[Int]): List[Int] = {
    numbers.filter(n => n % 2 == 0)
  }
  // Beispielaufruf
  // filterEven(List(1, 2, 3, 4, 5, 6)) sollte List(2, 4, 6) zurückgeben

  // 3. Datenreduktion mit fold/reduce
  def sumList(numbers: List[Int]): Int = {
    numbers.foldRight(0)((a, b) => a + b) //oder numbers.sum
  }
  // Beispielaufruf
  // sumList(List(1, 2, 3, 4, 5)) sollte 15 zurückgeben

  // 4. Kombinieren von Funktionen
  def multiplyAndFilter(numbers: List[Int]): List[Int] = {
    numbers.map(n => n * 2).filter(n => n > 5)
  }
  // Beispielaufruf
  // multiplyAndFilter(List(1, 2, 3, 4, 5)) sollte List(6, 8, 10) zurückgeben

  // 5. Arbeiten mit Option
  def average(numbers: List[Double]): Option[Double] = {
    if (numbers.isEmpty) None
    else Some(numbers.sum / numbers.length)
  }
  // Beispielaufruf
  // average(List(1.0, 2.0, 3.0)) sollte Some(2.0) zurückgeben
  // average(List()) sollte None zurückgeben

  // 6. Pattern Matching
  def listLength[A](list: List[A]): Int = {
    list.foldRight(0)((_, acc) => acc + 1)
  }
  // Beispielaufruf
  // listLength(List(1, 2, 3, 4)) sollte 4 zurückgeben

  // 7. Higher-Order Functions
  def zipWith[A, B, C](list1: List[A], list2: List[B])(f: (A, B) => C): List[C] = {
    list1.zip(list2).map((a, b) => f(a, b))
    //    list1.lazyZip(list2).map(f)
  }
  // Beispielaufruf
  // zipWith(List(1, 2, 3), List(4, 5, 6))(_ + _) sollte List(5, 7, 9) zurückgeben

  // 8. Datentransformation für Data Mining
  def normalizeData(data: List[List[Double]]): List[List[Double]] = {
    if (data.isEmpty) return List()
    val initialMaxValues = data.head
    val normVektor = data.tail.foldLeft(initialMaxValues)(
      (maxima, current) => zipWith(maxima, current)(
        (a, b) => if (a > b) a else b)
      )
    data.map(list => zipWith(list, normVektor)(
      (listVal, normVal) => listVal/normVal))
  }
  // Beispielaufruf
  // normalizeData(List(List(1.0, 2.0), List(3.0, 4.0), List(5.0, 1.0)))
  // sollte List(List(0.2, 0.5), List(0.6, 1.0), List(1.0, 0.25)) zurückgeben
}