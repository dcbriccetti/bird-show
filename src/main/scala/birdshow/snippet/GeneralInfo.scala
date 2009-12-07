package birdshow.snippet

import xml.NodeSeq
import net.liftweb.util.Helpers._
import xml.{Text, Node, NodeSeq}
import net.liftweb.util.{Full}
import net.liftweb.http.LiftRules

class GeneralInfo {
  def info(content: NodeSeq): NodeSeq = {
      bind("info", content,
        "title" -> LiftRules.context.getInitParameter("appTitle"))
  }
  
}