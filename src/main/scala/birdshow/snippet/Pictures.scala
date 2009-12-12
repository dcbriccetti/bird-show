package birdshow.snippet

import net.liftweb.util.Helpers._
import xml.{Text, Node, NodeSeq}
import net.liftweb.util.{Full}
import net.liftweb.http.{RequestVar, SHtml, S}
import birdshow.model.Flickr
import birdshow.model.flickr.PictureIdAndSizes
import birdshow.util.{Group, Loggable}

class Pictures extends Loggable {
  private object searchText extends RequestVar("")
  
  def home(content: NodeSeq): NodeSeq = 
    bind("home", content,
      "randomUrl" -> <img src={Flickr.getRandomHomePhotoUrl}/>)
  
  def search(content: NodeSeq): NodeSeq = {
    def processSearch() {
      debug("Searching for " + searchText.is)
    }

    bind("search", content,
      "text" -> SHtml.text(searchText.is, searchText(_)),
      "submit" -> SHtml.submit("Search", processSearch)
      )
  }
  
  def showShowPictures(content: NodeSeq): NodeSeq = {
    def pImg(photoAndSize: Option[Tuple2[Node, PictureIdAndSizes]]): NodeSeq = photoAndSize match {
      case Some((photo, pictureIdAndSizes)) => <img src={pictureIdAndSizes.getSmallSizeUrl}/>
      case None => <span/>
    }

    def bindGroup(content: NodeSeq, photosAndSizes: Flickr.PhotosSeq) = { 
      Group.group(photosAndSizes).flatMap(pGroup => 
      bind("item", chooseTemplate("photo", "list", content),
        "img1"   -> pImg(pGroup._1), 
        "title1" -> pTitle(pGroup._1),
        "img2"   -> pImg(pGroup._2), 
        "title2" -> pTitle(pGroup._2),
        "img3"   -> pImg(pGroup._3), 
        "title3" -> pTitle(pGroup._3)
        ))
    }
    bind("gal", content,
      "galleries" -> bindGroup(content, Flickr.getSetPhotosAndSizes(Flickr.getUser.showSetId)))
  }
  
  def showGalleries(content: NodeSeq): NodeSeq = {
    
    def bindAllGalleries(content: NodeSeq): NodeSeq = {
      bind("gal", content,
        "heading" -> "",
        "showAll" -> <span/>,
        "galleries" -> Group.group(Flickr.getSets).flatMap(psGroup => 
          bind("item", chooseTemplate("photo", "list", content),
            "img1"   -> psAnchor(psGroup._1), 
            "title1" -> psTitle(psGroup._1),
            "img2"   -> psAnchor(psGroup._2), 
            "title2" -> psTitle(psGroup._2),
            "img3"   -> psAnchor(psGroup._3), 
            "title3" -> psTitle(psGroup._3)
            )))
    }
    
    def bindGalleryWithId(content: NodeSeq, id: String): NodeSeq = {
      bind("gal", content,
        "heading" -> Text(Flickr.getSets.find(s => (s \ "@id").text == id) match {
          case Some(photoSet) => (photoSet \ "title").text
          case _ => ""
        }),
        "showAll" -> <a href="?">Show gallery index</a>,
        "galleries" -> bindGroup(content, Flickr.getSetPhotosAndSizes(id)))
    }
    
    def bindSearchResults(content: NodeSeq): NodeSeq = {
      debug("bindGallery. searchText: " + searchText.is)
      bind("gal", content,
        "heading" -> Text("Results for " + searchText.is),
        "showAll" -> <a href="?">Show gallery index</a>,
        "galleries" -> bindGroup(content, Flickr.searchPhotos(searchText.is)))
    }
    
    if (searchText.is != "") 
      bindSearchResults(content)
    else
      S.param("id") match {
        case Full(id) => bindGalleryWithId(content, id)
        case _ => bindAllGalleries(content)
      }
  }
  
  def sets(content: NodeSeq): NodeSeq = <p>{getSetTitles.mkString(", ")}</p>
  
  private def bindGroup(content: NodeSeq, photosAndSizes: Flickr.PhotosSeq) = 
    Group.group(photosAndSizes).flatMap(pGroup => 
    bind("item", chooseTemplate("photo", "list", content),
      "img1"   -> pImg(pGroup._1), 
      "title1" -> pTitle(pGroup._1),
      "img2"   -> pImg(pGroup._2), 
      "title2" -> pTitle(pGroup._2),
      "img3"   -> pImg(pGroup._3), 
      "title3" -> pTitle(pGroup._3)
      ))

  private case class PrioritizedSource(val priority: Int, val source: String)
  
  private def pImg(photoAndSize: Option[Tuple2[Node, PictureIdAndSizes]]): NodeSeq = photoAndSize match {
    case Some((photo, pictureIdAndSizes)) =>
      <a href="#">
        <img onclick={"BIRDSHOW.showBig('" + pictureIdAndSizes.getPreferredSizeUrl + "'); return false;"} 
            src={pictureIdAndSizes.getSmallSizeUrl}/>
      </a>
    case None => <p/>
  }
  
  private def pTitle(photoSet: Option[Tuple2[Node, PictureIdAndSizes]]): String = photoSet match {
    case Some(ps) => (ps._1 \ "@title").text
    case None => ""
  }
  
  private def psAnchor(photoSet: Option[Node]): NodeSeq = photoSet match {
    case Some(ps) => 
      <a href={"?id=" + ((ps \ "@id").text)}><img src={Flickr.url(ps, "primary", "_m")}/></a>
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
