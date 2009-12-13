package birdshow.snippet

import net.liftweb.util.Helpers._
import xml.{Text, NodeSeq}
import net.liftweb.common.{Full}
import net.liftweb.http.{RequestVar, SHtml, S}
import birdshow.model.Flickr
import birdshow.util.{Loggable}

class Pictures extends Loggable with PhotoRows {
  private object searchText extends RequestVar("")

  def search(content: NodeSeq) = bind("search", content,
    "text" -> SHtml.text(searchText.is, searchText(_)),
    "submit" -> SHtml.submit("Search", () => {}))

  def showGalleries(content: NodeSeq): NodeSeq = {

    def bindAllGalleries(content: NodeSeq) = bind("gal", content,
      "heading" -> "",
      "showAll" -> "",
      "photoRows" -> bindPhotoRows(content, Flickr.getSets, psAnchor, psTitle))

    def bindGalleryWithId(content: NodeSeq, id: String) = bind("gal", content,
        "heading" -> Text(Flickr.getSets.find(_.id == id) match {
          case Some(photoSet) => photoSet.title
          case _ => ""
        }),
        "showAll" -> <a href="?">Show gallery index</a>,
        "photoRows" -> bindPhotoRows(content, Flickr.getSetPhotosAndSizes(id), pImg, pTitle))

    def bindSearchResults(content: NodeSeq) = bind("gal", content,
        "heading" -> Text("Results for " + searchText.is),
        "showAll" -> <a href="?">Show gallery index</a>,
        "photoRows" -> bindPhotoRows(content, Flickr.searchPhotos(searchText.is), pImg, pTitle))

    if (searchText.is != "")
      bindSearchResults(content)
    else
      S.param("id") match {
        case Full(id) => bindGalleryWithId(content, id)
        case _ => bindAllGalleries(content)
      }
  }
}
