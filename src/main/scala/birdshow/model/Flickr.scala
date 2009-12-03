package birdshow.model

import java.net.{HttpURLConnection, URL}
import xml.{NodeSeq, Node, XML}
import birdshow.util.Loggable

object Flickr extends Loggable {
  val userName = "Eleanor%20Briccetti"
  val masterCollectionName = "Selected"

  val urlPart1 = "http://api.flickr.com/services/rest/?method=flickr."
  val apiKey = "&api_key=979e4a1aa2eb498c845415e254e70f53"
    
  val userId: NodeSeq = getFromFlickr("people.findByUsername&username=" + userName) \ "user" \ "@nsid"
  val collections: NodeSeq = getFromFlickr("collections.getTree&user_id=" + userId) \\ "collection"
  val setIds = ((collections filter(c => (c \ "@title") == "Selected")) \\ "set") map(_ \ "@id")

  val sets: NodeSeq = (getFromFlickr("photosets.getList&user_id=" + userId) \ "photosets" \ "photoset") filter(
          s => setIds.contains(s \\ "@id"))

  def searchPhotos(searchText: String) = getFromFlickr("photos.search&user_id=" + userId + "&text=" + searchText) \ "photos" \ "photo"
  def getAllPhotos = getFromFlickr("photos.search&user_id=" + userId + "&per_page=500") \ "photos" \ "photo"
  def getSetPhotos(setId: String) = {
    val photos = getFromFlickr("photosets.getPhotos&user_id=" + userId +
            "&photoset_id=" + setId + "&per_page=500") \ "photoset" \ "photo"
    println("Got " + photos.length + " photos")
    photos
  }
  def getSets = sets

  def url(p: Node, idAttr: String) = "http://farm" + (p \ "@farm") + ".static.flickr.com/" + 
      (p \ "@server") + "/" + (p \ ("@" + idAttr)) + "_" + (p \ "@secret") + "_m.jpg"

  private def getFromFlickr(urlBody: String) = XmlFetcher.get(urlPart1 + urlBody + apiKey)
}

object XmlFetcher extends Loggable {
  def get(urlString: String): Node = {
    info(urlString)
    val conn = new URL(urlString).openConnection.asInstanceOf[HttpURLConnection]
    val statusCode = conn.getResponseCode
    if (statusCode != 200)
      println(statusCode)
    XML.load(conn.getInputStream())
  }
}

