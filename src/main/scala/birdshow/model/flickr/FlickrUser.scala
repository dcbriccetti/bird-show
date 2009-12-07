package birdshow.model.flickr

import xml.NodeSeq
import birdshow.model.Flickr

case class FlickrUser(val userName: String, val topCollection: String) {
  val userId: String = (Flickr.getFromFlickr("people.findByUsername&username=" +
      Flickr.enc(userName)) \ "user" \ "@nsid").text

  val collections: NodeSeq = 
      Flickr.getFromFlickr("collections.getTree&user_id=" + userId) \\ "collection"

  val setIds = ((collections filter (c => 
      (c \ "@title") == topCollection)) \\ "set") map (_ \ "@id")

  val sets: NodeSeq = (Flickr.getFromFlickr("photosets.getList&user_id=" + userId) \ 
      "photosets" \ "photoset") filter (s => setIds.contains(s \\ "@id"))
}

