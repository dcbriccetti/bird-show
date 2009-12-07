package birdshow.model.flickr

import xml.NodeSeq

case class PictureSize(val sizeName: String, val url: String)

case class PictureIdAndSizes(val id: String, val sizes: Seq[PictureSize]) {
  private val SmallSizeName = "Small" // Avoid duplicating literal
  
  private val sizeSelectionOrder = Map(
    "Large" -> 1,
    "Medium" -> 2,
    SmallSizeName -> 3,
    "Original" -> 4,
    "Thumbnail" -> 5,
    "Square" -> 6
    )

  def getPreferredSizeUrl: String = {
    sizes.toList.sort((a,b) => sizeSelectionOrder(a.sizeName) < sizeSelectionOrder(b.sizeName))(0).url
  }

  def getSmallSizeUrl: String = {
    sizes.find(_.sizeName == SmallSizeName).get.url
  }
}

object PictureIdAndSizes {
  def fromNode(id: String, sizes: NodeSeq): PictureIdAndSizes = {
    val pictureSizes = (sizes \ "sizes" \ "size").map(s => {
      PictureSize((s \\ "@label").text, (s \\ "@source").text)
    })
    PictureIdAndSizes(id, pictureSizes)
  }
}


