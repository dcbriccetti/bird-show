package birdshow.model

import flickr.{Photo, PictureIdAndSizes}

case class PhotoAndSizes(val photo: Photo, val pictureIdAndSizes: PictureIdAndSizes) 
    extends Titled {
  
  def title = photo.title
}

