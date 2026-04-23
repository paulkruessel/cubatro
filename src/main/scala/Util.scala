def removeAt[A](items: List[A], i: Int): List[A] =
  if i < 0 || i >= items.length then items
  else items.patch(i, Nil, 1)