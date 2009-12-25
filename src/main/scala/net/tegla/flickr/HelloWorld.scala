package net.tegla.flickr

object HelloWorld { 
	def main(args: Array[String]) {
		val flickr = Flickr.ProbaApp
		val photosets = flickr.photosets.getPhotos(72157610703221265L, 10, 1)
		println(photosets)
		//for(photo <- photosets) println(photo)
	}
}
