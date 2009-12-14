package birdshow.model.flickr

import xml.Node
import birdshow.model.Titled

/**
 * Something (such as a Photo or PhotoSet) that has a URL for accessing it, that URL 
 * having a “base part.”
 */
trait BaseUrl {
  /**
   * Combines several data values into a “base URL.”
   */
  def createBaseUrl(xml: Node): String =
    "http://farm" + (xml \ "@farm") + ".static.flickr.com/" + (xml \ "@server") + "/"
}

/**
 * Wraps the <code>photo</code> tag returned by Flickr.
 */
class Photo(val id: String, val title: String, val urlBase: String, val secret: String) {
  def url(sizeSuffix: String) = urlBase + id + "_" + secret + sizeSuffix + ".jpg"
}

object Photo extends BaseUrl {
  def apply(xml: Node) = new Photo((xml \ "@id").text, (xml \ "@title").text, 
                                   createBaseUrl(xml), (xml \ "@secret").text)
}

/**
 * Wraps the <code>photoset</code> tag returned by Flickr.
 */
class PhotoSet(val id: String, val primary: String, val title: String, 
               val urlBase: String, val secret: String) extends Titled {
  def url(sizeSuffix: String) = urlBase + primary + "_" + secret + sizeSuffix + ".jpg"
}

object PhotoSet extends BaseUrl {
  def apply(xml: Node) = new PhotoSet((xml \ "@id").text, (xml \ "@primary").text, 
                                      (xml \ "title").text, createBaseUrl(xml), 
                                      (xml \ "@secret").text)
}
