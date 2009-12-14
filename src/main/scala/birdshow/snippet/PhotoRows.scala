package birdshow.snippet

import net.liftweb.util.Helpers._
import birdshow.util.Group
import birdshow.model.flickr.{PhotoSet}
import birdshow.model.{Titled, PhotoAndSizes}
import xml.{Text, NodeSeq}

trait PhotoRows {

  def bindPhotoRows[T <: Titled](content: NodeSeq, photos: Seq[T], img: (T) => NodeSeq) =  
    Group.group(photos).flatMap(g => 
      bind("item", chooseTemplate("gal", "photoRows", content),
        "img1"   -> render(g._1, img), 
        "title1" -> render(g._1, title),
        "img2"   -> render(g._2, img), 
        "title2" -> render(g._2, title),
        "img3"   -> render(g._3, img), 
        "title3" -> render(g._3, title)))

  protected def photoExpandableImage(ps: PhotoAndSizes): NodeSeq = {
    val sizes = ps.pictureIdAndSizes
    <a href="#">
      <img onclick={"BIRDSHOW.showBig('" + sizes.getPreferredSizeUrl + "'); return false;"}
           src={sizes.getSmallSizeUrl}/>
    </a>
  }
  
  protected def photoSetLinkableImage(ps: PhotoSet): NodeSeq =
    <a href={"?id=" + ps.id}><img src={ps.url("_m")}/></a>

  private def render[T](item: Option[T], f: (T => NodeSeq)) =
    if (item.isEmpty) Text("") else f(item.get)
  
  private def title(photoAndSizes: Titled) = Text(photoAndSizes.title)
}