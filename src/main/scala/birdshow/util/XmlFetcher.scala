package birdshow.util

import xml.{Node, XML}
import java.net.{URL, HttpURLConnection}

object XmlFetcher extends Loggable {
  def get(urlString: String): Node = {
    val conn = new URL(urlString).openConnection.asInstanceOf[HttpURLConnection]
    val statusCode = conn.getResponseCode
    if (statusCode != 200)
      error(statusCode.toString)
    XML.load(conn.getInputStream())
  }
}

