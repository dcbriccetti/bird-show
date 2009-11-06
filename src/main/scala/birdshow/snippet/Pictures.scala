package birdshow.snippet

import birdshow.model.Flickr
import net.liftweb.util.Helpers._
import xml.{Text, Node, NodeSeq}
import net.liftweb.util.{Full}
import net.liftweb.http.{S}

class Pictures {
  
  def showGalleries(content: NodeSeq): NodeSeq = {
    
    def bindGallery(content: NodeSeq, id: String): NodeSeq = {
      bind("gal", content,
        "heading" -> Text(Flickr.getSets.find(s => (s \ "@id").text == id) match {
          case Some(photoSet) => (photoSet \ "title").text
          case _ => ""
        }),
        "showAll" -> <a href="?">Show gallery index</a>,
        "galleries" -> group(Flickr.getSetPhotos(id)).flatMap(pGroup => 
          bind("item", chooseTemplate("photo", "list", content),
            "img1"   -> pImg(pGroup._1), 
            "title1" -> pTitle(pGroup._1),
            "img2"   -> pImg(pGroup._2), 
            "title2" -> pTitle(pGroup._2),
            "img3"   -> pImg(pGroup._3), 
            "title3" -> pTitle(pGroup._3)
            )))
    }
    
    def bindGalleries(content: NodeSeq): NodeSeq = {
      bind("gal", content,
        "heading" -> "",
        "showAll" -> <span/>,
        "galleries" -> group(Flickr.getSets).flatMap(psGroup => 
          bind("item", chooseTemplate("photo", "list", content),
            "img1"   -> psAnchor(psGroup._1), 
            "title1" -> psTitle(psGroup._1),
            "img2"   -> psAnchor(psGroup._2), 
            "title2" -> psTitle(psGroup._2),
            "img3"   -> psAnchor(psGroup._3), 
            "title3" -> psTitle(psGroup._3)
            )))
    }
    
    S.param("id") match {
      case Full(id) => bindGallery(content, id)
      case _ => bindGalleries(content)
    }
  }
  
  def titles(content: NodeSeq): NodeSeq = <p>{Flickr.getAllPhotos.map(_ \ "@title").mkString(", ")}</p>
  
  def sets(content: NodeSeq): NodeSeq = <p>{getSetTitles.mkString(", ")}</p>
  
  type Row[T] = Tuple3[Option[T], Option[T], Option[T]]
  
  def group[T](items: Seq[T]): Seq[Row[T]] = {
    def hss(it: Iterator[T]) = if (it.hasNext) Some(it.next) else None
    val it = items.elements
    var result = List[Row[T]]()
    while(it.hasNext) {
      result = result ::: List((hss(it), hss(it), hss(it)))
    }
    result
  }
  
  private def pImg(photoSet: Option[Node]): NodeSeq = photoSet match {
    case Some(p) => <img src={Flickr.url(p, "id")}/>
    case None => <p/>
  }
  
  private def pTitle(photoSet: Option[Node]): String = photoSet match {
    case Some(ps) => (ps \ "@title").text
    case None => ""
  }
  
  private def psAnchor(photoSet: Option[Node]): NodeSeq = photoSet match {
    case Some(ps) => 
      <a href={"?id=" + ((ps \ "@id").text)}><img src={Flickr.url(ps, "primary")}/></a>
    case None => <p/>
  }
  
  private def psTitle(photoSet: Option[Node]): String = photoSet match {
    case Some(ps) => (ps \ "title").text
    case None => ""
  }
  
  private def getSetTitles: List[Tuple2[String,String]] = {
    Flickr.getSets.map(s => ((s \ "@id").text, (s \ "title").text)).toList.sort(_._2 < _._2)
  }
  
}
