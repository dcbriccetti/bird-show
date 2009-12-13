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
    LiftRules.setSiteMap(SiteMap(
      Menu(Loc("Home",      List("index"    ), "Home"     )),
      Menu(Loc("Biography", List("bio"      ), "Biography")),
      Menu(Loc("Galleries", List("galleries"), "Galleries")),
      Menu(Loc("Shows",     List("shows"    ), "Shows"    )),
      Menu(Loc("Prints",    List("prints"   ), "Prints"   ))
      ))
    LiftRules.early.append(makeUtf8)
    LiftRules.enableLiftGC = false
    
    Flickr.addUser(LiftRules.context.getInitParameter("flickrUserName"),
      LiftRules.context.getInitParameter("flickrCollection"),
      LiftRules.context.getInitParameter("flickrHomeSet"),
      LiftRules.context.getInitParameter("flickrShowSet"))
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HttpServletRequest) {
    req.setCharacterEncoding("UTF-8")
  }

}
