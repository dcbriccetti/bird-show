package birdshow.model

import java.net.URL
import scala.util.Random
import xml.{Node, XML}

object Flickr {
  val userName = "Eleanor%20Briccetti"

  val urlPart1 = "http://api.flickr.com/services/rest/?method=flickr."
  val apiKey = "&api_key=979e4a1aa2eb498c845415e254e70f53"
    
  val id = getFromFlickr("people.findByUsername&username=" + userName) \ "user" \ "@nsid"
  val tags = (getFromFlickr("tags.getListUser&user_id=" + id) \\ "tag") map(_.text)
  val sets = getFromFlickr("photosets.getList&user_id=" + id) \ "photosets" \ "photoset"

  def getTag = tags(new Random().nextInt(tags.length))
  def getTags = tags

  def getPhotos(tag: String) = getFromFlickr("photos.search&user_id=" + id + "&tags=" + tag) \ "photos" \ "photo"
  def getAllPhotos = getFromFlickr("photos.search&user_id=" + id + "&per_page=500") \ "photos" \ "photo"
  def getSetPhotos(setId: String) = getFromFlickr("photosets.getPhotos&user_id=" + id + 
          "&photoset_id=" + setId + "&per_page=500") \ "photos" \ "photo"
  def getSets = sets

  private def getFromFlickr(urlBody: String) = XmlFetcher.get(urlPart1 + urlBody + apiKey)
}

object XmlFetcher {
  def get(urlString: String): Node = {
    println(urlString)
    val conn = new URL(urlString).openConnection
    val status = conn.getHeaderFields.get("Status") // Looking for "200 OK"
    val statusCode = if (status == null || status.size == 0) 200 else
      Integer.parseInt(status.get(0).split(" ")(0))
    if (statusCode != 200) println(statusCode)
    XML.load(conn.getInputStream())
  }
}

