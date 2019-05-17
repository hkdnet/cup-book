package net.hkdnet.app

import net.hkdnet.gui.SpreadSheet

import scala.swing._

object Scell extends SimpleSwingApplication {
  override def top: Frame = new Frame {
    title = "Scell"
    contents = new SpreadSheet(100, 26)
  }
}
