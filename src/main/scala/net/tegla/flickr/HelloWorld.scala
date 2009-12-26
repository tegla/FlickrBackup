package net.tegla.flickr

object HelloWorld { 
	def main(args: Array[String]) {
		val flickr = Flickr.ProbaApp
		val mikulas = 72157610703221265L
		val getPage = flickr.photosets.getPhotos(mikulas, 10, _:Int)
		val all = flickr.photosets.getPhotos(getPage)
		for(photo <- all) {
			println(photo.title)
		}
		println("total: " + all.length)
	}
}
