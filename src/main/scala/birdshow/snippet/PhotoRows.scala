package birdshow.snippet

import xml.{Node, NodeSeq}
import net.liftweb.util.Helpers._
import birdshow.util.Group
import birdshow.model.Flickr
import birdshow.model.flickr.PictureIdAndSizes

trait PhotoRows {
  type PicAndSizes = Tuple2[Node, PictureIdAndSizes]

  def bindPhotoRows[T](content: NodeSeq, photos: Seq[T], 
      img: (Option[T]) => NodeSeq, title: (Option[T]) => String) =  
    Group.group(photos).flatMap(g => 
      bind("item", chooseTemplate("gal", "photoRows", content),
        "img1"   -> img  (g._1), 
        "title1" -> title(g._1),
        "img2"   -> img  (g._2), 
        "title2" -> title(g._2),
        "img3"   -> img  (g._3), 
        "title3" -> title(g._3)))

  protected def pImg(photoAndSize: Option[PicAndSizes]): NodeSeq = photoAndSize match {
    case Some((photo, pictureIdAndSizes)) =>
      <a href="#">
        <img onclick={"BIRDSHOW.showBig('" + pictureIdAndSizes.getPreferredSizeUrl + "'); return false;"} 
            src={pictureIdAndSizes.getSmallSizeUrl}/>
      </a>
    case None => <p/>
  }
  
  protected def pTitle(photoSet: Option[PicAndSizes]): String = photoSet match {
    case Some(ps) => (ps._1 \ "@title").text
    case None => ""
  }
  
  protected def psAnchor(photoSet: Option[Node]): NodeSeq = photoSet match {
    case Some(ps) => 
      <a href={"?id=" + ((ps \ "@id").text)}><img src={Flickr.url(ps, "primary", "_m")}/></a>
    case None => <p/>
  }
  
  protected def psTitle(photoSet: Option[Node]): String = photoSet match {
    case Some(ps) => (ps \ "title").text
    case None => ""
  }
  
}