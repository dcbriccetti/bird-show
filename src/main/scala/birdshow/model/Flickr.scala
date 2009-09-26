package birdshow.model

import java.net.URL
import scala.util.Random
import xml.{Node, XML}

object Flickr {
  val userName = "Eleanor%20Briccetti"

  val urlPart1 = "http://api.flickr.com/services/rest/?method=flickr."
  val apiKey = "&api_key=979e4a1aa2eb498c845415e254e70f53"
    
  val id = getFromFlickr("people.findByUsername&username=" + userName) \ "user" \ "@nsid"
  val tags = getFromFlickr("tags.getListUser&user_id=" + id) \\ "tag"
  val tagStrings = tags map(_.text)

  def getTag = tagStrings(new Random().nextInt(tagStrings.length))

  def getPhotos(tag: String) = getFromFlickr("photos.search&user_id=" + id + "&tags=" + tag) \ "photos" \ "photo"

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

