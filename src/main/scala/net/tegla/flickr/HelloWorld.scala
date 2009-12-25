package net.tegla.flickr

object HelloWorld { 
	def main(args: Array[String]) {
		val flickr = Flickr.ProbaApp
		val photosets = flickr.photosets.getList()
		println(photosets.length)
		println(photosets.label)
	}
}
