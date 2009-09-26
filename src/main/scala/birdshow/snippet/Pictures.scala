package birdshow.snippet

import model.Flickr

class Pictures {
  val tag = Flickr.getTag
  val photos = Flickr.getPhotos(tag)
  
  def list = <span><p>Pictures with tag “{tag}”</p> { photos.map(p => {
    val picloc = "http://farm" + (p \ "@farm") + ".static.flickr.com/" + (p \ "@server") + 
            "/" + (p \ "@id") + "_" + (p \ "@secret") + "_m.jpg"
    <img style="margin-right: 1em" src={picloc}/>
  })} </span>
  
}
