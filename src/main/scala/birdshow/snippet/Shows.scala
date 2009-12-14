package birdshow.snippet

import net.liftweb.util.Helpers._
import xml.NodeSeq
import birdshow.model.{PhotoAndSizes, Flickr}

class Shows extends PhotoRows {
  
  def showShowPictures(content: NodeSeq): NodeSeq = {
    
    def makeImgTag(photoAndSize: PhotoAndSizes): NodeSeq =
      <img src={photoAndSize.pictureIdAndSizes.getSmallSizeUrl}/>

    bind("gal", content, "photoRows" -> 
      bindPhotoRows(content, Flickr.getSetPhotosAndSizes(
                    Flickr.getUser.showSetId), makeImgTag))
  }
}
