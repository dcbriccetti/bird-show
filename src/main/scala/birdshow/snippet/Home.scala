package birdshow.snippet

import xml.NodeSeq
import net.liftweb.util.Helpers._
import birdshow.model.Flickr

class Home {
  def home(content: NodeSeq) = bind("home", content,
    "randomUrl" -> <img src={Flickr.getRandomHomePhotoUrl}/>)
}