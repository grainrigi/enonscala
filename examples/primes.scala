def div(x: Int, y: Int): Int = x / y * y

def remainder(x: Int, y: Int): Int = x - x / y * y

def range(x: Int, y: Int): List[Int] =
  if (y < x+1)
    Nil
  else
    x::range(x+1, y)

def filter(x: Int, l: List[Int]): List[Int] =
  if (l.isEmpty)
    Nil
  else if (remainder(l.head, x) == 0)
    filter(x, l.tail)
  else
    l.head::filter(x, l.tail)

def primesList(l: List[Int]): List[Int] =
  if (l.isEmpty)
    Nil
  else
    l.head::primesList(filter(l.head, l.tail))

def primes(n: Int): List[Int] =
  primesList(range(2, n))

def test(n: Int): List[Int] =
  primes(n)

