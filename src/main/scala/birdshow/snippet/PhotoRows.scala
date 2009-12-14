package birdshow.snippet

import xml.{NodeSeq}
import net.liftweb.util.Helpers._
import birdshow.util.Group
import birdshow.model.flickr.{PhotoSet}
import birdshow.model.{Titled, PhotoAndSizes}

trait PhotoRows {

  def bindPhotoRows[T <: Titled](content: NodeSeq, photos: Seq[T], img: (Option[T]) => NodeSeq) =  
    Group.group(photos).flatMap(g => 
      bind("item", chooseTemplate("gal", "photoRows", content),
        "img1"   -> img   (g._1), 
        "title1" -> pTitle(g._1),
        "img2"   -> img   (g._2), 
        "title2" -> pTitle(g._2),
        "img3"   -> img   (g._3), 
        "title3" -> pTitle(g._3)))

  protected def pImg(photoAndSize: Option[PhotoAndSizes]): NodeSeq = photoAndSize match {
    case Some(PhotoAndSizes(photo, sizes)) =>
      <a href="#">
        <img onclick={"BIRDSHOW.showBig('" + sizes.getPreferredSizeUrl + "'); return false;"} 
            src={sizes.getSmallSizeUrl}/>
      </a>
    case None => <p/>
  }
  
  protected def psAnchor(photoSet: Option[PhotoSet]): NodeSeq = photoSet match {
    case Some(ps) => 
      <a href={"?id=" + ps.id}><img src={ps.url("_m")}/></a>
    case None => <p/>
  }
  
  private def pTitle(photoAndSizes: Option[Titled]): String = photoAndSizes match {
    case Some(ps) => ps.title
    case None => ""
  }
  
}