package birdshow.model

import flickr.{FlickrUser, PictureIdAndSizes}
import xml.{NodeSeq, Node}
import java.net.{URLEncoder}
import net.liftweb.http.LiftRules
import java.util.Date
import birdshow.util.{XmlFetcher, Parallelizer, Loggable}

object Flickr extends Loggable {
  val urlPart1 = "http://api.flickr.com/services/rest/?method=flickr."
  val apiKey = "&api_key=979e4a1aa2eb498c845415e254e70f53"

  val user = FlickrUser(LiftRules.context.getInitParameter("flickrUserName"),
    LiftRules.context.getInitParameter("flickrCollection"))

  def searchPhotos(searchText: String): Seq[Tuple2[Node, PictureIdAndSizes]] =
    getPhotosFromFlickr("photos.search&user_id=" + user.userId + "&text=" + enc(searchText), "photos")

  def getSetPhotos(setId: String): Seq[Tuple2[Node, PictureIdAndSizes]] =
    getPhotosFromFlickr("photosets.getPhotos&user_id=" + user.userId + "&photoset_id=" + setId, "photoset")

  def getSets: NodeSeq = user.sets

  def url(p: Node, idAttr: String, sizeSuffix: String) =
    "http://farm" + (p \ "@farm") + ".static.flickr.com/" +
            (p \ "@server") + "/" + (p \ ("@" + idAttr)) + "_" + (p \ "@secret") + sizeSuffix + ".jpg"

  def getFromFlickr(urlBody: String) = XmlFetcher.get(urlPart1 + urlBody + apiKey)

  private def getPhotosFromFlickr(urlBody: String, tag: String): Seq[Tuple2[Node, PictureIdAndSizes]] = {
    val timeMs = (new Date).getTime
    val photos = getFromFlickr(urlBody + "&per_page=500") \ tag \ "photo"
    info("Got " + photos.length + " photos from " + urlBody + " in " + ((new Date).getTime - timeMs) + " ms.")

    val pictureIdAndSizes: List[PictureIdAndSizes] = Parallelizer.run(20, photos,
      (photo: Node) => {
        val id = (photo \ "@id").text
        PictureIdAndSizes.fromNode(id, getFromFlickr("photos.getSizes&photo_id=" + id))
      })

    photos.map(p => {
      val id = (p \ "@id").text
      (p, pictureIdAndSizes.find(_.id == id).get)
    })
  }

  def enc(text: String) = URLEncoder.encode(text, "UTF-8")
}

