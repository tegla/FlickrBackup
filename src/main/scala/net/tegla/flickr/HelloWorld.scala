package net.tegla.flickr

object HelloWorld { 
	def main(args: Array[String]) {
		val flickr = Flickr.ProbaApp

		var all = new scala.collection.mutable.HashSet[Photo]
		for(photoset <- flickr.photosets.getList())
		for(photo <- flickr.photosets.getPhotos(photoset.id)) {
			println(photo)
			all+=photo
		}
		for ( photo <- flickr.photos.getNotInSet() ) {
			println(photo)
			all+=photo
		}
		println("total: " + all.size)
	}
}
