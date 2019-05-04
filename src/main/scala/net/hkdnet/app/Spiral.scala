package net.hkdnet.app

import net.hkdnet.layout._

object Spiral {
  val space = Element.elem(" ")
  val corner = Element.elem("+")
  def spiral(nEdges: Int, direction: Int): Element = {
    if (nEdges == 1)
      corner
    else {
      val sp = spiral(nEdges - 1, (direction + 3) % 4)
      def verticalBar = Element.elem('|', 1, sp.height)
      def horizontalBar = Element.elem('-', sp.width, 1)

      direction match {
        case 0 => (corner beside horizontalBar) above (sp beside space)
        case 1 => (sp above space) beside (corner above verticalBar)
        case 2 => (space beside sp) above (horizontalBar beside corner)
        case _ => (verticalBar above corner) beside(space above sp)
      }
    }
  }
}
