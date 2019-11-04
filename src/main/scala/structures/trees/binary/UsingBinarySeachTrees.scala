package structures.trees.binary

object UsingBinarySeachTrees extends App {
  println("using binary trees ... ")

  val myTree = Empty[Int].add(10).add(8).add(15).add(7).add(6)

  println(myTree)

  println(myTree.inorderTraverse())

  println(myTree.preorderTraverse())

  println(myTree.find(7))

  println(myTree.find(9))

}
