package birdshow.snippet

import net.liftweb.util.Helpers._
import xml.NodeSeq
import birdshow.model.{PhotoAndSizes, Flickr}

class Shows extends PhotoRows {
  
  def showShowPictures(content: NodeSeq): NodeSeq = {
    def pImg(photoAndSize: Option[PhotoAndSizes]): NodeSeq = photoAndSize match {
      case Some(PhotoAndSizes(photo, pictureIdAndSizes)) => 
          <img src={pictureIdAndSizes.getSmallSizeUrl}/>
      case None => <span/>
    }

    bind("gal", content,
      "photoRows" -> bindPhotoRows(content, Flickr.getSetPhotosAndSizes(Flickr.getUser.showSetId), 
        pImg, pTitle))
  }
}
