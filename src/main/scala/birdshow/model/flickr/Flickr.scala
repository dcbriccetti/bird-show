package birdshow.model

import java.net.URLEncoder
import java.util.Random
import xml.NodeSeq
import flickr.{Photo, PhotoSet, FlickrUser, PictureIdAndSizes}
import birdshow.util.{XmlFetcher, Parallelizer}

/**
 * Fetches collection, set and photo information from Flickr.
 */
object Flickr {

  private val urlPart1 = "https://api.flickr.com/services/rest/?method=flickr."
  private val apiKey = "&api_key=979e4a1aa2eb498c845415e254e70f53"
  private val parallelizer = new Parallelizer(20) 

  private var users = List[FlickrUser]()

  def addUser(userName: String, topCollection: String, homeSet: String, showSet: String) = 
    users ::= FlickrUser(userName, topCollection, homeSet, showSet)
  
  def searchPhotos(searchText: String): Seq[PhotoAndSizes] = getPhotosAndSizesFromFlickr(
    "photos.search&user_id=" + getUser.userId + "&text=" + enc(searchText), "photos")

  def getSetPhotosAndSizes(setId: String): Seq[PhotoAndSizes] =
    getPhotosAndSizesFromFlickr(getSetPhotosUrlBody(setId), "photoset")
  
  def getRandomHomePhotoUrl: String = {
    val photos = getHomePhotos
    photos(new Random().nextInt(photos.size)).url("")
  }

  def getSets: Seq[PhotoSet] = getUser.photoSets

  def getFromFlickr(urlBody: String): NodeSeq = XmlFetcher.get(urlPart1 + urlBody + apiKey)
  
  def enc(text: String) = URLEncoder.encode(text, "UTF-8")

  def getUser = users(0)  // TODO devise a way to identify one among multiple users
  
  def shutDown() = parallelizer.shutDown()

  private def getHomePhotos: Seq[Photo] = getSetPhotos(getUser.homeSetId)
  
  private def getSetPhotosUrlBody(setId: String) = 
    "photosets.getPhotos&user_id=" + getUser.userId + "&photoset_id=" + setId

  private def getSetPhotos(setId: String): Seq[Photo] = 
    getPhotosFromFlickr(getSetPhotosUrlBody(setId), "photoset")
  
  private def getPhotosAndSizesFromFlickr(urlBody: String, tag: String): Seq[PhotoAndSizes] = {
    val photos = getPhotosFromFlickr(urlBody, tag)

    val idAndSizes = parallelizer.run(photos.map(_.id),
      (id: String) => PictureIdAndSizes.fromNode(id, getFromFlickr("photos.getSizes&photo_id=" + id)))

    photos.map(p => PhotoAndSizes(p, idAndSizes.find(_.id == p.id).get))
  }

  private def getPhotosFromFlickr(urlBody: String, tag: String): Seq[Photo] = 
    (getFromFlickr(urlBody + "&per_page=500") \ tag \ "photo").map(Photo.apply)
  
}
