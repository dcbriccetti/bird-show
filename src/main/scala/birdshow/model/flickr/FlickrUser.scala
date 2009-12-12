package birdshow.model.flickr

import xml.NodeSeq
import birdshow.model.Flickr
import birdshow.util.Loggable

case class FlickrUser(val userName: String, val topCollectionTitle: String, 
  val homeSetTitle: String, val showSetTitle: String) extends Loggable {
  val userId: String = (Flickr.getFromFlickr("people.findByUsername&username=" +
      Flickr.enc(userName)) \ "user" \ "@nsid").text

  val collections: NodeSeq = 
      Flickr.getFromFlickr("collections.getTree&user_id=" + userId) \\ "collection"

  val setIds = ((collections filter (c => (c \ "@title") == topCollectionTitle)) \\ "set") map (_ \ "@id")

  val allSets: NodeSeq = 
    (Flickr.getFromFlickr("photosets.getList&user_id=" + userId) \ "photosets" \ "photoset")
  
  val sets: NodeSeq = allSets filter (s => setIds.contains(s \\ "@id"))
  
  def getSetIdFromTitle(title: String): String = {
    val set = allSets.find(s => (s \ "title").text == title).get
    (set \ "@id").text
  } 

  val homeSetId: String = getSetIdFromTitle(homeSetTitle)
  val showSetId: String = getSetIdFromTitle(showSetTitle)
}

