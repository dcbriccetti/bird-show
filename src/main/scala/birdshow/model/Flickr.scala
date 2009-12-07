package birdshow.model

import xml.{NodeSeq, Node, XML}
import java.net.{URLEncoder, HttpURLConnection, URL}
import net.liftweb.http.LiftRules
import birdshow.util.{Parallelizer, Loggable}
import java.util.Date

case class FlickrUser(val userName: String, val topCollection: String) {
  val userId: NodeSeq = Flickr.getFromFlickr("people.findByUsername&username=" + 
      Flickr.enc(userName)) \ "user" \ "@nsid"
  
  val collections: NodeSeq = Flickr.getFromFlickr("collections.getTree&user_id=" + userId) \\ "collection"
  
  val setIds = ((collections filter(c => (c \ "@title") == topCollection)) \\ "set") map(_ \ "@id")

  val sets: NodeSeq = (Flickr.getFromFlickr("photosets.getList&user_id=" + userId) \ "photosets" \ "photoset") filter(
          s => setIds.contains(s \\ "@id"))
}

object Flickr extends Loggable {
  val urlPart1 = "http://api.flickr.com/services/rest/?method=flickr."
  val apiKey = "&api_key=979e4a1aa2eb498c845415e254e70f53"

  val user = FlickrUser(LiftRules.context.getInitParameter("flickrUserName"),
    LiftRules.context.getInitParameter("flickrCollection"))

  def searchPhotos(searchText: String): Seq[Tuple2[Node, Node]] = 
    getPhotosFromFlickr("photos.search&user_id=" + user.userId + "&text=" + enc(searchText), "photos")

  def getSetPhotos(setId: String): Seq[Tuple2[Node, Node]] = 
    getPhotosFromFlickr("photosets.getPhotos&user_id=" + user.userId + "&photoset_id=" + setId, "photoset")

  def getSets: NodeSeq = user.sets

  def url(p: Node, idAttr: String, sizeSuffix: String) = 
      "http://farm" + (p \ "@farm") + ".static.flickr.com/" + 
      (p \ "@server") + "/" + (p \ ("@" + idAttr)) + "_" + (p \ "@secret") + sizeSuffix + ".jpg"

  def getFromFlickr(urlBody: String) = XmlFetcher.get(urlPart1 + urlBody + apiKey)
  
  private def getPhotosFromFlickr(urlBody: String, tag: String): Seq[Tuple2[Node, Node]] = {
    val timeMs = (new Date).getTime
    val photos = getFromFlickr(urlBody + "&per_page=500") \ tag \ "photo"
    info("Got " + photos.length + " photos from " + urlBody + " in " + ((new Date).getTime - timeMs) + " ms.")

    photos.toList.zip(Parallelizer.run(20, photos, 
      (photo: Node) => getFromFlickr("photos.getSizes&photo_id=" + (photo \ "@id").text)))
  }
  
  def enc(text: String) = URLEncoder.encode(text, "UTF-8") 
}

object XmlFetcher extends Loggable {
  def get(urlString: String): Node = {
    val conn = new URL(urlString).openConnection.asInstanceOf[HttpURLConnection]
    val statusCode = conn.getResponseCode
    if (statusCode != 200)
      error(statusCode.toString)
    XML.load(conn.getInputStream())
  }
}

