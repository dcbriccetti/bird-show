package birdshow.snippet

import net.liftweb.util.Helpers._
import xml.NodeSeq
import birdshow.model.Flickr
import birdshow.util.Loggable

class Shows extends Loggable with PhotoRows {
  
  def showShowPictures(content: NodeSeq): NodeSeq = {
    def pImg(photoAndSize: Option[PicAndSizes]): NodeSeq = photoAndSize match {
      case Some((photo, pictureIdAndSizes)) => <img src={pictureIdAndSizes.getSmallSizeUrl}/>
      case None => <span/>
    }

    bind("gal", content,
      "photoRows" -> bindPhotoRows(content, Flickr.getSetPhotosAndSizes(Flickr.getUser.showSetId), 
        pImg, pTitle))
  }
}
