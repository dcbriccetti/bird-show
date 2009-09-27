package birdshow.snippet

import model.Flickr
import net.liftweb.http.SHtml
import net.liftweb.util.Empty
import net.liftweb.util.Helpers._
import xml.{Node, Text, NodeSeq}

class Pictures {
  println("Pictures new instance")
  val tag = Flickr.getTag
  var selectedSet = ""
  
  def render(content: NodeSeq): NodeSeq = 
    bind("tagcol", content,
      "tag" -> Text(tag),
      "photos" -> getPhotos.flatMap(photo => 
        bind("item", chooseTemplate("photo", "list", content),
          "img" -> <img src={url(photo)}/>, 
          "title" -> (photo \ "@title")
          )))
  
  def tags(content: NodeSeq): NodeSeq = <p>{Flickr.getTags.mkString(", ")}</p>
  def titles(content: NodeSeq): NodeSeq = <p>{Flickr.getAllPhotos.map(_ \ "@title").mkString(", ")}</p>
  def sets(content: NodeSeq): NodeSeq = <p>{getSetTitles.mkString(", ")}</p>
  def setSelect(xhtml: NodeSeq): NodeSeq = {
    def doNothing() {println("Selected set now " + selectedSet)}
    bind("entry", xhtml, 
      "sets" -> SHtml.select(getSetTitles, Empty, 
         selectedSet = _, "class" -> "myselect"), 
      "submit" -> SHtml.submit("Select", doNothing)) 
  }
  
  private def url(p: Node) = "http://farm" + (p \ "@farm") + ".static.flickr.com/" + 
      (p \ "@server") + "/" + (p \ "@id") + "_" + (p \ "@secret") + "_m.jpg"

  private def getSetTitles: List[Tuple2[String,String]] = {
    // I’m making a new list because I don’t know how to sort a Seq[String]
    var titles = List[Tuple2[String,String]]()
    Flickr.getSets.foreach(s => titles ::= ((s \ "@id").text, (s \ "title").text))
    titles.sort(_._2 < _._2)
  }
  
  private def getPhotos: NodeSeq = {
    println("getPhotos, selectedSet: " + selectedSet)
    if (selectedSet == "") 
      Flickr.getPhotos(tag) 
    else 
      Flickr.getSetPhotos(selectedSet) 
  }
}
