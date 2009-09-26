package birdshow.snippet

import model.Flickr
import net.liftweb.util.Helpers._
import xml.{Node, Text, NodeSeq}

class Pictures {
  val tag = Flickr.getTag
  val photos = Flickr.getPhotos(tag)
  
  def render(content: NodeSeq): NodeSeq = 
    bind("tagcol", content,
      "tag" -> Text(tag),
      "photos" -> photos.flatMap(photo => 
        bind("item", chooseTemplate("photo", "list", content),
          "img" -> <img src={url(photo)}/>, 
          "title" -> (photo \ "@title")
          )))
                               
  private def url(p: Node) = "http://farm" + (p \ "@farm") + ".static.flickr.com/" + 
      (p \ "@server") + "/" + (p \ "@id") + "_" + (p \ "@secret") + "_m.jpg"

}
