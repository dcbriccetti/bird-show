package birdshow.snippet

import birdshow.model.Flickr
import net.liftweb.util.Helpers._
import xml.{Text, NodeSeq}
import net.liftweb.util.{Full}
import net.liftweb.http.{S}

class Pictures {
  val tag = Flickr.getTag
  
  def showRandomTag(content: NodeSeq): NodeSeq = 
    bind("tagcol", content,
      "title" -> Text(tag),
      "photos" -> getPhotos.flatMap(photo => 
        bind("item", chooseTemplate("photo", "list", content),
          "img" -> <img src={Flickr.url(photo, "id")}/>, 
          "title" -> (photo \ "@title")
          )))
  
  def showGalleries(content: NodeSeq): NodeSeq = {
    
    def bindGallery(content: NodeSeq, id: String): NodeSeq = {
      bind("gal", content,
        "heading" -> <h3>{Flickr.getSets.find(s => (s \ "@id").text == id) match {
          case Some(photoSet) => (photoSet \ "title").text
          case _ => ""
        }}</h3>,
        "showAll" -> <a href="?">Show gallery index</a>,
        "galleries" -> Flickr.getSetPhotos(id).flatMap(photo => 
          bind("item", chooseTemplate("photo", "list", content),
            "img" -> <img src={Flickr.url(photo, "id")}/>, 
            "title" -> (photo \ "@title").text
            )))
    }
    
    def bindGalleries(content: NodeSeq): NodeSeq = {
      bind("gal", content,
        "heading" -> <h3>All Galleries</h3>,
        "showAll" -> <span/>,
        "galleries" -> Flickr.getSets.flatMap(photoSet => 
          bind("item", chooseTemplate("photo", "list", content),
            "img" -> <a href={"?id=" + ((photoSet \ "@id").text)}><img src={Flickr.url(photoSet, "primary")}/></a>, 
            "title" -> (photoSet \ "title").text
            )))
    }
    
    S.param("id") match {
      case Full(id) => bindGallery(content, id)
      case _ => bindGalleries(content)
    }
  }
  
  def tags(content: NodeSeq): NodeSeq = <p>{Flickr.getTags.mkString(", ")}</p>
  
  def titles(content: NodeSeq): NodeSeq = <p>{Flickr.getAllPhotos.map(_ \ "@title").mkString(", ")}</p>
  
  def sets(content: NodeSeq): NodeSeq = <p>{getSetTitles.mkString(", ")}</p>
  
  private def getSetTitles: List[Tuple2[String,String]] = {
    Flickr.getSets.map(s => ((s \ "@id").text, (s \ "title").text)).toList.sort(_._2 < _._2)
  }
  
  private def getPhotos: NodeSeq = Flickr.getPhotos(tag)
}
