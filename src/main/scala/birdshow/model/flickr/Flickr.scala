package birdshow.model

import java.net.URLEncoder
import java.util.Random
import xml.NodeSeq
import flickr.{Photo, PhotoSet, FlickrUser, PictureIdAndSizes}
import birdshow.util.{XmlFetcher, Parallelizer, Loggable}

case class PhotoAndSizes(val photo: Photo, val pictureIdAndSizes: PictureIdAndSizes)

object Flickr extends Loggable {

  private val urlPart1 = "http://api.flickr.com/services/rest/?method=flickr."
  private val apiKey = "&api_key=979e4a1aa2eb498c845415e254e70f53"

  private var users = List[FlickrUser]()

  def addUser(userName: String, topCollection: String, homeSet: String, showSet: String) = 
    users ::= FlickrUser(userName, topCollection, homeSet, showSet)
  
  def searchPhotos(searchText: String): Seq[PhotoAndSizes] = getPhotosAndSizesFromFlickr(
    "photos.search&user_id=" + getUser.userId + "&text=" + enc(searchText), "photos")

  def getSetPhotosAndSizes(setId: String): Seq[PhotoAndSizes] =
    getPhotosAndSizesFromFlickr(getSetPhotosUrlBody(setId), "photoset")
  
  private def getSetPhotos(setId: String): Seq[Photo] = 
    getPhotosFromFlickr(getSetPhotosUrlBody(setId), "photoset")
  
  def getHomePhotos: Seq[Photo] = getSetPhotos(getUser.homeSetId)
  
  def getRandomHomePhotoUrl: String = {
    val photos = getHomePhotos
    val numPhotos = photos.size
    photos(new Random().nextInt(numPhotos)).url("")
  }

  def getSets: Seq[PhotoSet] = getUser.sets

  def getFromFlickr(urlBody: String): NodeSeq = XmlFetcher.get(urlPart1 + urlBody + apiKey)
  
  def enc(text: String) = URLEncoder.encode(text, "UTF-8")

  private def getSetPhotosUrlBody(setId: String) = 
    "photosets.getPhotos&user_id=" + getUser.userId + "&photoset_id=" + setId

  def getUser = users(0)  // TODO devise a way to identify one among multiple users

  private def getPhotosFromFlickr(urlBody: String, tag: String): Seq[Photo] = 
    (getFromFlickr(urlBody + "&per_page=500") \ tag \ "photo").map(Photo.apply)
  
  private def getPhotosAndSizesFromFlickr(urlBody: String, tag: String): Seq[PhotoAndSizes] = {
    val photos = getPhotosFromFlickr(urlBody, tag)

    val idAndSizes = Parallelizer.run(20, photos.map(_.id),
      (id: String) => PictureIdAndSizes.fromNode(id, getFromFlickr("photos.getSizes&photo_id=" + id)))

    photos.map(p => PhotoAndSizes(p, idAndSizes.find(_.id == p.id).get))
  }
}

