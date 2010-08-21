package birdshow.model.flickr

import xml.NodeSeq

case class PictureSize(val sizeName: String, val url: String)

case class PictureIdAndSizes(val id: String, val sizes: Seq[PictureSize]) {
  private val SmallSizeName = "Small" // Avoid duplicating literal

  private val sizeSelectionOrder = Map(
    "Large" -> 1,
    "Medium 640" -> 2,
    "Medium 500" -> 3,
    "Medium" -> 4, // Still exists?
    SmallSizeName -> 5,
    "Original" -> 6,
    "Thumbnail" -> 7,
    "Square" -> 8
    )

  def getPreferredSizeUrl: String = {
    def getOrderCode(key: String) = sizeSelectionOrder.getOrElse(key, 100)
    sizes.toList.sort((a,b) => getOrderCode(a.sizeName) < getOrderCode(b.sizeName))(0).url
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


