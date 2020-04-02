def insert(x: Int, l: List[Int]): List[Int] =
  if (l.isEmpty)
    x::Nil
  else if (x<l.head)
    x::l
  else
    l.head::insert(x, l.tail)



