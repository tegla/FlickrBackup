package net.tegla.flickr

import java.io.File
import java.net.URL

object SavePhotoset {
	def download(source: URL, target:java.io.File) {
		print("Downloading " + source +" ")
		val inputStream = source.openStream
		val tmp = new File(target.toString + ".download")
		try {
			val outputStream = new java.io.FileOutputStream(tmp)
			val bytes = new Array[Byte](4096)
			var i = inputStream.read(bytes)
			while(i > 0) {
				print(".")
				outputStream.write(bytes, 0, i)
				i = inputStream.read(bytes)
			}
			outputStream.close()
			tmp.renameTo(target)
			println
			println("Written as  " + target)
		} finally {
			inputStream.close()
			tmp.delete()
		}
	}

	lazy val ImageFile = new scala.util.matching.Regex("""(\d{5})_(\d+)\.jpg""")
	def toImageFile(nr:Int,id:Long) = "%05d".format(nr) + "_" + id + ".jpg"

	// figure out what files need deleting, renaming or downloading.
	// return this info as a triplet, but don't actually DO anything
	def resolve(dirlist:Collection[String], ids:Seq[Long]) = {
		// for the current file listing, create a photo id->nr map
		val current:Map[Long,Int] = Map() ++ (for {
				file <- dirlist
				pair <- file match {
				  case ImageFile(nr,id) => Some((id.toLong,nr.toInt))
				  case _=> None }
			} yield pair)

		// for the new file listing, create a photo id->nr map
		val target:Map[Long,Int] = Map() ++
			ids.toList.zipWithIndex.map { p => (p._1, p._2+1) } // we count from 1

		// what needs deleting? Create a list of files
		val deleteFiles = for {
			id <- current.keySet
			if !(target isDefinedAt id) } yield toImageFile(current(id), id)

		// what needs moving? Create a map of (old name, new name) pairs
		// (I do remember a "lift" somewhere, but I cannot find it anymore)
		def liftedTarget(id:Long) = if (target.isDefinedAt(id)) Some(target(id)) else None
		val moveFiles = Map() ++ (for {
			(id, old_nr) <- current
			new_nr <- liftedTarget(id)
			if old_nr != new_nr }
			yield (toImageFile(current(id),id), toImageFile(target(id),id)))

		// what needs downloading? Create a map of (nr, id) pairs
		val downloadIds = Map() ++ (for {
			(id, nr) <- target
			if !(current isDefinedAt id) }
			yield (nr, id))

		(deleteFiles, moveFiles, downloadIds)
	}

	def main(args : Array[String]) : Unit = {
		val photoset_id = args(0).toLong
		val flickr = Flickr.ProbaApp
		val pwd = new File(System.getProperty("user.dir"))

		// first, figure out the directory to write the files
		val photoset = flickr.photosets.getInfo(photoset_id)
		val target_dir = new File(pwd, photoset.title.replace('/',' '))
		if (!target_dir.exists) {
			target_dir.mkdir()
			println("Creating " + target_dir)
			assert(target_dir.exists)
		}

		val ids = flickr.photosets.getPhotos(args(0).toLong).map(_.id)
		val dirlist = target_dir.list()
		def source(id:Long) = {
			println("Getting info for image " + id)
			flickr.photos.getSizes(id)("Large").source
		}
		val (deleteFiles, moveFiles, downloadIds) = resolve(dirlist, ids)

		if (deleteFiles.isEmpty && moveFiles.isEmpty && downloadIds.isEmpty) {
			println("Nothing to do.")
			return
		}

		for(fileName <- deleteFiles) {
			val file = new File(target_dir, fileName)
			println("Deleting    " + file)
			file.delete
		}

		for((from,to) <- moveFiles) {
			val fromFile = new File(target_dir, from)
			val toFile = new File(target_dir, to)
			println("Renaming    " + fromFile + " to " + toFile)
			fromFile.renameTo(toFile)
		}

		for((nr,id) <- downloadIds) {
			val file = new File(target_dir, toImageFile(nr,id))
			val source = new java.net.URL(flickr.photos.getSizes(id)("Large").source)
			println("Downloading " + file + " to " + source)
			download(source, file)
		}
	}
}
