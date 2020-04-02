def fact(n: Int): Int =
  if (n == 0) 1 else n*fact(n-1)

def test(n: Int): Int = fact(n)
