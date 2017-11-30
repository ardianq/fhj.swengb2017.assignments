package at.fhj.swengb.assignments.tree

import java.util.Currency
import javafx.scene.paint.Color

import scala.util.Random

object Graph {

  val colorMap: Map[Int, Color] =
    Map[Int, Color](
      0 -> Color.ROSYBROWN,
      1 -> Color.BROWN,
      2 -> Color.SADDLEBROWN,
      3 -> Color.INDIANRED,
      4 -> Color.DARKGREEN,
      5 -> Color.GREEN,
      6 -> Color.YELLOWGREEN,
      7 -> Color.GREENYELLOW,
      8 -> Color.YELLOW
    )

  /**
    * creates a random tree
    *
    * @param root - Startpoint of graph
    * @return
    */
  def randomTree(root: Pt2D): Tree[L2D] =
    mkGraph(root,
      Random.nextInt(360),
      Random.nextDouble() * 150,
      Random.nextInt(7))

  /**
    * Given a Tree of L2D's and a function which can convert any L2D to a Line,
    * you have to traverse the tree (visit all nodes) and create a sequence
    * of Line's. The ordering of the lines is not important.
    *
    * @param tree  a tree which contains L2D instances
    * @param convert a converter function
    * @return
    */
  def traverse[A, B](tree: Tree[A])(convert: A => B): Seq[B] = {

    //Helper to add all elements to list
    def addElemToList(elem: Tree[A], list: Seq[A]): Seq[A] = {
      elem match {
        case Node(x) => list.seq :+ x
        case Branch(left, right) =>
          addElemToList(left, addElemToList(right, list))
      }
    }

    //Add all nodes to list and map operation
    addElemToList(tree, List()).reverse.map(convert)
  }

  /**
    * Creates / constructs a tree graph.
    *
    * @param start the startpoint (root) of the tree
    * @param initialAngle initial angle of the tree
    * @param length the initial length of the tree
    * @param treeDepth the depth of the tree
    * @param factor the factor which the length is decreasing for every iteration
    * @param angle the angle between a branch and the root
    * @param colorMap color map, by default it is the colormap given in the companion object Graph
    *
    * @return a Tree[L2D] which can be traversed by other algorithms
    */
  def mkGraph(start: Pt2D,
              initialAngle: AngleInDegrees,
              length: Double,
              treeDepth: Int,
              factor: Double = 0.75,
              angle: Double = 45.0,
              colorMap: Map[Int, Color] = Graph.colorMap): Tree[L2D] = {

    //Allow just positive depth until max depth of given colors -1 to avoid IndexOutOfBound exceptions...
    require(treeDepth >= 0 && treeDepth <= (colorMap.size - 1))

    /** Helper function to create Branch with given node
      * @param leaf   currently a leaf but becomes root of created subtree
      * @param factor factor which the lengh is decreasing to child
      * @param angle  Angle between child and root
      * @param color  Color of childs
      *
      * @return A Subtree wich given node as root and a left & right child
      */
    def createSubTree(leaf: Node[L2D],
                      factor: Double,
                      angle: Double,
                      color: Color): Branch[L2D] = {
      //Create new childs for given leaf
      val nodeLeft = Node(leaf.value.left(factor, angle, color))
      val nodeRight = Node(leaf.value.right(factor, angle, color))

      //Return created subtree
      Branch(leaf, Branch(nodeLeft, nodeRight))
    }

    /**
      * Helper function to create a tree with a certain depth.
      * @param currTree Current tree. This is the start and will increased to the given level
      * @param depth    Start-Value of dept to start
      * @param maxDepth Maximum depth of tree. Usually limited by amount of colors
      * @return
      */
    def createTree(currTree: Tree[L2D],
                   depth: Int,
                   maxDepth: Int): Tree[L2D] = {
      if (depth == maxDepth)
      /*We've reached requested depth of tree, we're done*/
        currTree
      else {
        /*Still levels to add...*/

        /** Subhelper to add a new level to given tree if iteration requires.
          * According to the given tree, heler has to deal with it:
          * A tree can look like:
          *   Node(x) => Its a level 0 tree(just root). Create a subtree out of it. (create Level 1)
          *   Branch(Node,Branch(Node,Node) => It is a leaf. In this case a new level needs to added
          *   Branch(Node(Branch(Branch,Branch) => Somewhere in the middle of the tree. Just iterate further
          * @param tree - Tree to verify and add new level is possible in this iteration
          * @param currLevel - current level of iteration. Defines color for new possible created level
          * @return Tree with new level if neccessary in given iteration.
          */
        def addNewLevel(tree: Tree[L2D], currLevel: Int): Branch[L2D] = {
          tree match {
            case Node(root) =>
              /*Given tree was only a node(root), create a Branch out of it (Level 1) */
              createSubTree(Node(root), factor, angle, colorMap(currLevel))
            case Branch(Node(root), Branch(Node(left), Node(right))) =>
              /*Given tree is a leaf. Append new level */
              val newSubtreeLeft =
                createSubTree(Node(left), factor, angle, colorMap(currLevel))
              val newSubtreeRight =
                createSubTree(Node(right), factor, angle, colorMap(currLevel))
              Branch(Node(root), Branch(newSubtreeLeft, newSubtreeRight))
            case Branch(Node(root), Branch(left, right)) =>
              /*Given tree is a subtree in the middle of the tree.
                Go further(deeper) in the tree on the left and right side and increase level*/
              Branch(Node(root),
                Branch(addNewLevel(left, depth + 1),
                  addNewLevel(right, depth + 1)))
            case Branch(_, _) => ??? /*Fallback - If this happens, some crazy shit is going on...*/
          }
        }

        //Recursive call but with newly created tree from helper function.
        createTree(addNewLevel(currTree, depth), depth + 1, maxDepth)
      }
    }

    //Create Tree according depth
    //Create initial root node
    val rootNode: Tree[L2D] = Node(
      L2D(start, initialAngle, length, colorMap(0)))

    //Append tree according given depth
    treeDepth match {
      case 0 => rootNode
      case _ => createTree(rootNode, 0, treeDepth)
    }
  }
}

object L2D {

  import MathUtil._

  /**
    * Given a startpoint, an angle and a length the endpoint of the line
    * is calculated and finally a L2D class is returned.
    *
    * @param start the startpoint
    * @param angle the angle
    * @param length the length of the line
    * @param color the color
    * @return
    */
  def apply(start: Pt2D,
            angle: AngleInDegrees,
            length: Double,
            color: Color): L2D = {
    val angleInRadiants = toRadiants(angle)
    val end = Pt2D(start.x + length * Math.cos(angleInRadiants),
      start.y + length * Math.sin(angleInRadiants)).normed
    new L2D(start, end, color)
  }

}

case class L2D(start: Pt2D, end: Pt2D, color: Color) {

  lazy val xDist = end.x - start.x

  lazy val yDist = end.y - start.y

  lazy val angle = {
    assert(!((xDist == 0) && (yDist == 0)))
    (xDist, yDist) match {
      case (x, 0) if x > 0          => 0
      case (0, y) if y > 0          => 90
      case (0, y) if y < 0          => 270
      case (x, 0) if x < 0          => 180
      case (x, y) if x < 0 && y < 0 => Math.atan(y / x) * 180 / Math.PI + 180
      case (x, y) if x < 0 && y > 0 => Math.atan(y / x) * 180 / Math.PI + 180
      case (x, y) if x > 0 && y < 0 => Math.atan(y / x) * 180 / Math.PI + 360
      case (x, y)                   => Math.atan(y / x) * 180 / Math.PI
    }
  }

  /** the line length **/
  lazy val length: Double = {
    Math.sqrt(xDist * xDist + yDist * yDist)
  }

  def left(factor: Double, deltaAngle: AngleInDegrees, c: Color): L2D = {
    L2D(end, angle - deltaAngle, length * factor, c)
  }

  def right(factor: Double, deltaAngle: AngleInDegrees, c: Color): L2D = {
    L2D(end, angle + deltaAngle, length * factor, c)
  }

}