package birdshow.util

import xml.{Node, XML}
import java.net.{URL, HttpURLConnection}
import net.liftweb.util.Log

object XmlFetcher {
  def get(urlString: String): Node = {
    val conn = new URL(urlString).openConnection.asInstanceOf[HttpURLConnection]
    val statusCode = conn.getResponseCode
    if (statusCode != 200)
      Log.error(statusCode.toString)
    XML.load(conn.getInputStream())
  }
}

