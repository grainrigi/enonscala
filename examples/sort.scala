def insert(x: Int, l: List[Int]): List[Int] =
  if (l.isEmpty)
    x::Nil
  else if (x<l.head)
    x::l
  else
    l.head::insert(x, l.tail)

def sort(l: List[Int]): List[Int] =
  if (l.isEmpty)
    Nil
  else
    insert(l.head, sort(l.tail))

def test(n: Int): List[Int] =
  if (n == 0)
    Nil
  else
    insert(n, test(n-1))
