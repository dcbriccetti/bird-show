package birdshow.model.flickr

import xml.NodeSeq
import birdshow.model.Flickr
import birdshow.util.Loggable

case class FlickrUser(val userName: String, val topCollectionTitle: String, 
  val homeSetTitle: String, val showSetTitle: String) extends Loggable {
  
  val userId: String = (Flickr.getFromFlickr("people.findByUsername&username=" +
      Flickr.enc(userName)) \ "user" \ "@nsid").text

  private val collections: NodeSeq = 
      Flickr.getFromFlickr("collections.getTree&user_id=" + userId) \\ "collection"

  private val setIds: Seq[String] = ((collections filter (
    c => (c \ "@title") == topCollectionTitle)) \\ "set").map(set => (set \ "@id").text)

  private val allSets: Seq[PhotoSet] = 
    (Flickr.getFromFlickr("photosets.getList&user_id=" + userId) \ 
      "photosets" \ "photoset").map(PhotoSet.apply)
  
  val sets: Seq[PhotoSet] = allSets filter (s => setIds.contains(s.id))
  
  private def getSetIdFromTitle(title: String): String = {
    val set = allSets.find(_.title == title).get
    set.id
  } 

  val homeSetId: String = getSetIdFromTitle(homeSetTitle)
  val showSetId: String = getSetIdFromTitle(showSetTitle)
}

