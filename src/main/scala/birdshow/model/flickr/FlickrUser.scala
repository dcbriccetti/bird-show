package birdshow.model.flickr

import xml.NodeSeq
import birdshow.model.Flickr
import java.util.Date

/**
 * A Flickr user, upon which an instance of this application is based. In a future
 * version, multiple instances will be supported. Contains all the relevant 
 * photo information for the user, from Flickr. 
 */
case class FlickrUser(val userName: String, val topCollectionTitle: String, 
  val homeSetTitle: String, val showSetTitle: String) {

  val userId: String = (Flickr.getFromFlickr("people.findByUsername&username=" +
      Flickr.enc(userName)) \ "user" \ "@nsid").text

  private var allPhotoSets: Seq[PhotoSet] = _
  private var _photoSets: Seq[PhotoSet] = _
  private var lastLoadTime: Long = _

  def photoSets: Seq[PhotoSet] = {
    if ((new Date).getTime - lastLoadTime > 1000 * 60 * 30) {
      loadData()
    }
    _photoSets
  }

  loadData()
  
  private def getSetIdFromTitle(title: String) = allPhotoSets.find(_.title == title).get.id

  val homeSetId: String = getSetIdFromTitle(homeSetTitle)
  val showSetId: String = getSetIdFromTitle(showSetTitle)

  private def loadData() {
    val collections: NodeSeq = 
      Flickr.getFromFlickr("collections.getTree&user_id=" + userId) \\ "collection"

    val topCollectionSetIds: Seq[String] = ((collections filter (
      c => (c \ "@title") == topCollectionTitle)) \\ "set").
      map(set => (set \ "@id").text)

    allPhotoSets = (Flickr.getFromFlickr("photosets.getList&user_id=" + userId) \ 
      "photosets" \ "photoset").map(PhotoSet.apply)
  
    _photoSets = allPhotoSets filter (s => topCollectionSetIds.contains(s.id))
    
    lastLoadTime = (new Date).getTime
  }
}

