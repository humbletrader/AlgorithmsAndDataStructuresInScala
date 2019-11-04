package structures.trees

package object binary {

  sealed trait Tree[T]{
    def preorderTraverse() : Iterable[T]
    def inorderTraverse() : Iterable[T]
    def add(value: T) : Tree[T]
    def find(value: T) : Tree[T]
    def remove(value : T) : Tree[T]
  }

  case class Node[T: Ordering](left: Tree[T], value : T, right : Tree[T]) extends Tree[T]{
    private val comparator = implicitly[Ordering[T]]

    override def preorderTraverse(): Iterable[T] = {
      Iterable(value) ++ left.preorderTraverse() ++ right.preorderTraverse()
    }

    override def inorderTraverse(): Iterable[T]= {
      left.inorderTraverse() ++ Iterable(value) ++ right.inorderTraverse()
    }

    override def add(newValue : T) : Tree[T] = {
      val comparison = comparator.compare(newValue, value)
      if(comparison < 0) //newValue < value
        new Node(left.add(newValue), value, right)
      else if(comparison > 0) //newValue > value
        new Node(left, value, right.add(newValue))
      else this //else they're equal ...do nothing
    }

    override def find(v: T) : Tree[T] = {
      val comparison = comparator.compare(v, value)
      if(comparison < 0) left.find(v)
      else if(comparison > 0) right.find(v)
      else this
    }

    override def remove(v: T): Tree[T] = ???

  }

  case class Empty[T:Ordering]() extends Tree[T]{
    override def preorderTraverse(): Iterable[T] = Iterable.empty
    override def inorderTraverse(): Iterable[T] = Iterable.empty
    override def add(newValue : T) : Tree[T] =
      new Node(Empty[T](), newValue, Empty[T]())
    override def find(v: T) : Tree[T] = this
    override def remove(value: T): Tree[T] = this

  }


}
