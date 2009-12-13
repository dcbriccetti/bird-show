package birdshow.model.flickr

import xml.NodeSeq
import net.liftweb.util.Log
import birdshow.model.Flickr

case class FlickrUser(val userName: String, val topCollectionTitle: String, 
  val homeSetTitle: String, val showSetTitle: String) {
  
  val userId: String = (Flickr.getFromFlickr("people.findByUsername&username=" +
      Flickr.enc(userName)) \ "user" \ "@nsid").text

  private var allPhotoSets: Seq[PhotoSet] = _
  var photoSets: Seq[PhotoSet] = _

  buildData()
  
  private def getSetIdFromTitle(title: String) = allPhotoSets.find(_.title == title).get.id

  val homeSetId: String = getSetIdFromTitle(homeSetTitle)
  val showSetId: String = getSetIdFromTitle(showSetTitle)

  def reload() = {
    Log.info("reloading")
    buildData()
  }
  
  private def buildData() {
    val collections: NodeSeq = 
      Flickr.getFromFlickr("collections.getTree&user_id=" + userId) \\ "collection"

    val topCollectionSetIds: Seq[String] = ((collections filter (
      c => (c \ "@title") == topCollectionTitle)) \\ "set").map(set => (set \ "@id").text)

    allPhotoSets = (Flickr.getFromFlickr("photosets.getList&user_id=" + userId) \ 
      "photosets" \ "photoset").map(PhotoSet.apply)
  
    photoSets = allPhotoSets filter (s => topCollectionSetIds.contains(s.id))
  }
}

