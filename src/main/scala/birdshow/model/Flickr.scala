package birdshow.model

import java.net.{HttpURLConnection, URL}
import scala.util.Random
import xml.{NodeSeq, Node, XML}

object Flickr {
  val userName = "Eleanor%20Briccetti"

  val urlPart1 = "http://api.flickr.com/services/rest/?method=flickr."
  val apiKey = "&api_key=979e4a1aa2eb498c845415e254e70f53"
    
  val id: NodeSeq = getFromFlickr("people.findByUsername&username=" + userName) \ "user" \ "@nsid"
  val tags: Seq[String] = (getFromFlickr("tags.getListUser&user_id=" + id) \\ "tag") map(_.text)
  val sets: NodeSeq = getFromFlickr("photosets.getList&user_id=" + id) \ "photosets" \ "photoset"

  def getTag = tags(new Random().nextInt(tags.length))
  def getTags = tags

  def getPhotos(tag: String) = getFromFlickr("photos.search&user_id=" + id + "&tags=" + tag) \ "photos" \ "photo"
  def getAllPhotos = getFromFlickr("photos.search&user_id=" + id + "&per_page=500") \ "photos" \ "photo"
  def getSetPhotos(setId: String) = {
    val photos = getFromFlickr("photosets.getPhotos&user_id=" + id +
            "&photoset_id=" + setId + "&per_page=500") \ "photoset" \ "photo"
    println("Got " + photos.length + " photos")
    photos
  }
  def getSets = sets

  def url(p: Node, idAttr: String) = "http://farm" + (p \ "@farm") + ".static.flickr.com/" + 
      (p \ "@server") + "/" + (p \ ("@" + idAttr)) + "_" + (p \ "@secret") + "_m.jpg"

  private def getFromFlickr(urlBody: String) = XmlFetcher.get(urlPart1 + urlBody + apiKey)
}

object XmlFetcher {
  def get(urlString: String): Node = {
    println(urlString)
    val conn = new URL(urlString).openConnection.asInstanceOf[HttpURLConnection]
    val statusCode = conn.getResponseCode
    if (statusCode != 200)
      println(statusCode)
    XML.load(conn.getInputStream())
  }
}

