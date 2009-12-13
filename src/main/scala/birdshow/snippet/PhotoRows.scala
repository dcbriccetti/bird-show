package birdshow.snippet

import xml.{NodeSeq}
import net.liftweb.util.Helpers._
import birdshow.util.Group
import birdshow.model.flickr.{PhotoSet}
import birdshow.model.PhotoAndSizes

trait PhotoRows {

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

  protected def pImg(photoAndSize: Option[PhotoAndSizes]): NodeSeq = photoAndSize match {
    case Some(PhotoAndSizes(photo, pictureIdAndSizes)) =>
      <a href="#">
        <img onclick={"BIRDSHOW.showBig('" + pictureIdAndSizes.getPreferredSizeUrl + "'); return false;"} 
            src={pictureIdAndSizes.getSmallSizeUrl}/>
      </a>
    case None => <p/>
  }
  
  protected def psAnchor(photoSet: Option[PhotoSet]): NodeSeq = photoSet match {
    case Some(ps) => 
      <a href={"?id=" + ps.id}><img src={ps.url("_m")}/></a>
    case None => <p/>
  }
  
  protected def pTitle(photoAndSizes: Option[PhotoAndSizes]): String = photoAndSizes match {
    case Some(ps) => ps.photo.title
    case None => ""
  }
  
  protected def psTitle(photoSet: Option[PhotoSet]): String = photoSet match {
    case Some(ps) => ps.title
    case None => ""
  }
  
}