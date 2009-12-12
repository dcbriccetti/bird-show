package birdshow.model

import java.net.URLEncoder
import java.util.{Date, Random}
import xml.{NodeSeq, Node}
import flickr.{FlickrUser, PictureIdAndSizes}
import birdshow.util.{XmlFetcher, Parallelizer, Loggable}

object Flickr extends Loggable {
  type PhotosSeq = Seq[Tuple2[Node, PictureIdAndSizes]]

  private val urlPart1 = "http://api.flickr.com/services/rest/?method=flickr."
  private val apiKey = "&api_key=979e4a1aa2eb498c845415e254e70f53"

  private var users = List[FlickrUser]()

  def addUser(userName: String, topCollectionName: String, homeSetName: String, showSetName: String) = 
    users ::= FlickrUser(userName, topCollectionName, homeSetName, showSetName)
  
  def searchPhotos(searchText: String): PhotosSeq =
    getPhotosAndSizesFromFlickr("photos.search&user_id=" + getUser.userId + "&text=" + enc(searchText), "photos")

  def getSetPhotosAndSizes(setId: String): PhotosSeq =
    getPhotosAndSizesFromFlickr(getSetPhotosUrlBody(setId), "photoset")
  
  private def getSetPhotos(setId: String): NodeSeq = getPhotosFromFlickr(getSetPhotosUrlBody(setId), "photoset")
  
  def getHomePhotos: NodeSeq = getSetPhotos(getUser.homeSetId)
  
  def getRandomHomePhotoUrl: String = {
    val photos = getHomePhotos
    info("home photos: " + photos)
    val numPhotos = photos.size
    url(photos(new Random().nextInt(numPhotos)), "id", "")
  }

  def getSets: NodeSeq = getUser.sets

  def url(ps: Node, idAttr: String, sizeSuffix: String) =
    "http://farm" + (ps \ "@farm") + ".static.flickr.com/" +
            (ps \ "@server") + "/" + (ps \ ("@" + idAttr)) + "_" + (ps \ "@secret") + sizeSuffix + ".jpg"

  def getFromFlickr(urlBody: String): NodeSeq = XmlFetcher.get(urlPart1 + urlBody + apiKey)
  
  def enc(text: String) = URLEncoder.encode(text, "UTF-8")

  private def getSetPhotosUrlBody(setId: String) = 
    "photosets.getPhotos&user_id=" + getUser.userId + "&photoset_id=" + setId

  def getUser = users(0)  // TODO devise a way to identify one among multiple users

  private def getPhotosFromFlickr(urlBody: String, tag: String): NodeSeq = {
    val timeMs = (new Date).getTime
    val photos = getFromFlickr(urlBody + "&per_page=500") \ tag \ "photo"
    info("Got " + photos.length + " photos from " + urlBody + " in " + ((new Date).getTime - timeMs) + " ms.")
    photos
  }
  
  private def getPhotosAndSizesFromFlickr(urlBody: String, tag: String): PhotosSeq = {
    val photos = getPhotosFromFlickr(urlBody, tag)

    val pictureIdAndSizes: List[PictureIdAndSizes] = Parallelizer.run(20, photos,
      (photo: Node) => {
        val id = (photo \ "@id").text
        PictureIdAndSizes.fromNode(id, getFromFlickr("photos.getSizes&photo_id=" + id))
      })

    photos.map(p => {
      val id = (p \ "@id").text
      (p, pictureIdAndSizes.find(_.id == id).get)
    })
  }
}

