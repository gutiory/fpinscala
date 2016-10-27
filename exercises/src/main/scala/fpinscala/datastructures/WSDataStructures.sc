import fpinscala.datastructures._


val listInt = List(1,2,3)
val listInt2 = List(4,5,6)

val listDouble = List(1.0, 2.0, 4.0)
val listEmpty = List()

val listString = List("1","2","3")
val listString2 = List("4","5","6")

List.lengthFoldLeft(listInt)
List.prodFoldLeft(listDouble)
List.sumFoldLeft(listInt)
List.reverseFold(listInt)

List.sum2(listInt)
List.foldRightViaFoldLeft(listInt, 0)(_ + _)
List.foldLeftViaFoldRight(listInt, 0)(_ + _)
List.addOne(listInt)
List.doublesToStrings(listDouble)
List.map(listInt)(x => x * 4)
List.filter(listInt)(_ % 2 == 0)
List.flatMap(listInt)(i => List(i,i))
List.filterViaFlatMap(listInt)(_ % 2 == 0)
List.add2ListInts(listInt, listInt2)
List.zipWith(listInt, listInt2)(_ + _)
List.zipWith(listString, listString2)(_ + _)
List.hasSubsequence(List(1,2,3,4), List(1,2))
List.hasSubsequence(List(1,2,3,4), List(2,3))
List.hasSubsequence(List(1,2,3,4), List(4))


val t = Branch(Branch(Leaf(2), Branch(Leaf(3), Leaf(4))), Leaf(5))
Tree.size(t)
Tree.maximum(t)
Tree.depth(t)
Tree.map(t)(x => x * 3)
Tree.sizeFold(t)
Tree.maximumFold(t)
Tree.depthFold(t)
Tree.mapFold(t)(x => x * 3)