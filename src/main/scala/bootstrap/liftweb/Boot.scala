package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.http._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.birdshow.model._
import _root_.javax.servlet.http.{HttpServletRequest}

/**
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("birdshow")

    // Build SiteMap
    val entries = List(
        Menu(Loc("Home", List("index"), "Home")), 
        Menu(Loc("Galleries", List("galleries"), "Galleries")))
    LiftRules.setSiteMap(SiteMap(entries:_*))

    LiftRules.early.append(makeUtf8)
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HttpServletRequest) {
    req.setCharacterEncoding("UTF-8")
  }

}


