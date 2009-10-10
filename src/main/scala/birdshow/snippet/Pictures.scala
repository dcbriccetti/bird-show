package birdshow.snippet

import birdshow.model.Flickr
import net.liftweb.util.Helpers._
import xml.{Node, Text, NodeSeq}
import net.liftweb.http.{RequestVar, SHtml}
import net.liftweb.util.{Full}

class Pictures {
  println("new Pictures")
  val tag = Flickr.getTag
  object selectedSet extends RequestVar("")
  
  def render(content: NodeSeq): NodeSeq = 
    bind("tagcol", content,
      "title" -> Text(tag),
      "photos" -> getPhotos.flatMap(photo => 
        bind("item", chooseTemplate("photo", "list", content),
          "img" -> <img src={Flickr.url(photo, "id")}/>, 
          "title" -> (photo \ "@title")
          )))
  
  def showGalleries(content: NodeSeq): NodeSeq = 
    bind("tagcol", content,
      "galleries" -> Flickr.getSets.flatMap(photoSet => 
        bind("item", chooseTemplate("photo", "list", content),
          "img" -> <img src={Flickr.url(photoSet, "primary")}/>, 
          "title" -> (photoSet \ "title").text
          )))
  
  def tags(content: NodeSeq): NodeSeq = <p>{Flickr.getTags.mkString(", ")}</p>
  
  def titles(content: NodeSeq): NodeSeq = <p>{Flickr.getAllPhotos.map(_ \ "@title").mkString(", ")}</p>
  
  def sets(content: NodeSeq): NodeSeq = <p>{getSetTitles.mkString(", ")}</p>
  
  def setSelect(xhtml: NodeSeq): NodeSeq = {
    def doNothing() {println("Selected gallery now " + selectedSet)}
    bind("entry", xhtml, 
      "galleries" -> SHtml.select(getSetTitles, Full(selectedSet.is), 
         selectedSet(_), "class" -> "myselect"), 
      "submit" -> SHtml.submit("Select", doNothing)) 
  }
  
  private def getSetTitles: List[Tuple2[String,String]] = {
    Flickr.getSets.map(s => ((s \ "@id").text, (s \ "title").text)).toList.sort(_._2 < _._2)
  }
  
  private def getPhotos: NodeSeq = {
    println("getPhotos, selectedSet: " + selectedSet.is)
    if (selectedSet.is == "") 
      Flickr.getPhotos(tag) 
    else 
      Flickr.getSetPhotos(selectedSet.is) 
  }
}
