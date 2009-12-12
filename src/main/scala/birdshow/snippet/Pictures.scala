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
  private type PicAndSizes = Tuple2[Node, PictureIdAndSizes]
  
  def home(content: NodeSeq) = bind("home", content,
    "randomUrl" -> <img src={Flickr.getRandomHomePhotoUrl}/>)
  
  def search(content: NodeSeq) = bind("search", content,
    "text" -> SHtml.text(searchText.is, searchText(_)),
    "submit" -> SHtml.submit("Search", () => {}))

  def showShowPictures(content: NodeSeq): NodeSeq = {
    def pImg(photoAndSize: Option[PicAndSizes]): NodeSeq = photoAndSize match {
      case Some((photo, pictureIdAndSizes)) => <img src={pictureIdAndSizes.getSmallSizeUrl}/>
      case None => <span/>
    }

    def bindGroup(content: NodeSeq, photosAndSizes: Flickr.PhotosSeq) =  
      Group.group(photosAndSizes).flatMap(picGroup => 
        bind("item", chooseTemplate("photo", "list", content),
          "img1"   -> pImg  (picGroup._1), 
          "title1" -> pTitle(picGroup._1),
          "img2"   -> pImg  (picGroup._2), 
          "title2" -> pTitle(picGroup._2),
          "img3"   -> pImg  (picGroup._3), 
          "title3" -> pTitle(picGroup._3)))
    
    bind("gal", content,
      "galleries" -> bindGroup(content, Flickr.getSetPhotosAndSizes(Flickr.getUser.showSetId)))
  }
  
  def showGalleries(content: NodeSeq): NodeSeq = {
    
    def bindAllGalleries(content: NodeSeq) = bind("gal", content,
      "heading" -> "",
      "showAll" -> "",
      "galleries" -> Group.group(Flickr.getSets).flatMap(setGroup => 
        bind("item", chooseTemplate("photo", "list", content),
          "img1"   -> psAnchor(setGroup._1), 
          "title1" -> psTitle (setGroup._1),
          "img2"   -> psAnchor(setGroup._2), 
          "title2" -> psTitle (setGroup._2),
          "img3"   -> psAnchor(setGroup._3), 
          "title3" -> psTitle (setGroup._3))))

    def bindGalleryWithId(content: NodeSeq, id: String) = bind("gal", content,
        "heading" -> Text(Flickr.getSets.find(s => (s \ "@id").text == id) match {
          case Some(photoSet) => (photoSet \ "title").text
          case _ => ""
        }),
        "showAll" -> <a href="?">Show gallery index</a>,
        "galleries" -> bindGroup(content, Flickr.getSetPhotosAndSizes(id)))
    
    def bindSearchResults(content: NodeSeq) = bind("gal", content,
        "heading" -> Text("Results for " + searchText.is),
        "showAll" -> <a href="?">Show gallery index</a>,
        "galleries" -> bindGroup(content, Flickr.searchPhotos(searchText.is)))
    
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
        "img1"   -> pImg  (pGroup._1), 
        "title1" -> pTitle(pGroup._1),
        "img2"   -> pImg  (pGroup._2), 
        "title2" -> pTitle(pGroup._2),
        "img3"   -> pImg  (pGroup._3), 
        "title3" -> pTitle(pGroup._3)))

  private case class PrioritizedSource(val priority: Int, val source: String)
  
  private def pImg(photoAndSize: Option[PicAndSizes]): NodeSeq = photoAndSize match {
    case Some((photo, pictureIdAndSizes)) =>
      <a href="#">
        <img onclick={"BIRDSHOW.showBig('" + pictureIdAndSizes.getPreferredSizeUrl + "'); return false;"} 
            src={pictureIdAndSizes.getSmallSizeUrl}/>
      </a>
    case None => <p/>
  }
  
  private def pTitle(photoSet: Option[PicAndSizes]): String = photoSet match {
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
