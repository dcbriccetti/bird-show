package bootstrap.liftweb

import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import _root_.birdshow.model._

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    // where to search snippet
    LiftRules.addToPackages("birdshow")

    // Build SiteMap
    val entries =
      Menu(Loc("Home",      List("index"    ), "Home"     )) ::
      Menu(Loc("Biography", List("bio"      ), "Biography")) ::
      Menu(Loc("Galleries", List("galleries"), "Galleries")) ::
      Menu(Loc("Shows",     List("shows"    ), "Shows"    )) ::
      Menu(Loc("Prints",    List("prints"   ), "Prints"   )) :: Nil
    LiftRules.setSiteMap(SiteMap(entries: _*))

    LiftRules.early.append(makeUtf8)
    LiftRules.enableLiftGC = false
    
    ResourceServer.allow {
      case "css" :: _ => true
      case "js" :: _ => true
      case "media" :: _ => true
    }

    Flickr.addUser(
      LiftRules.context.initParam("flickrUserName") openOr "",
      LiftRules.context.initParam("flickrCollection") openOr "",
      LiftRules.context.initParam("flickrHomeSet") openOr "",
      LiftRules.context.initParam("flickrShowSet") openOr "")
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }

}
