package birdshow.model.flickr

import xml.Node

trait BaseUrl {
  def createBaseUrl(xml: Node): String =
    "http://farm" + (xml \ "@farm") + ".static.flickr.com/" + (xml \ "@server") + "/"
}

class Photo(val id: String, val title: String, val urlBase: String, val secret: String) {
  def url(sizeSuffix: String) = urlBase + id + "_" + secret + sizeSuffix + ".jpg"
}

object Photo extends BaseUrl {
  def apply(xml: Node): Photo = {
    new Photo((xml \ "@id").text, (xml \ "title").text, createBaseUrl(xml),
        (xml \ "@secret").text)
  }
}

class PhotoSet(val id: String, val primary: String, val title: String, 
               val urlBase: String, val secret: String) {
  def url(sizeSuffix: String) = urlBase + primary + "_" + secret + sizeSuffix + ".jpg"
}

object PhotoSet extends BaseUrl {
  def apply(xml: Node): PhotoSet = {
    new PhotoSet((xml \ "@id").text, (xml \ "@primary").text, (xml \ "title").text, 
      createBaseUrl(xml), (xml \ "@secret").text)
  }
}
