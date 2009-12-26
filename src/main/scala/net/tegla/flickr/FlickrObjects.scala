package net.tegla.flickr

import scala.xml.Elem

trait XMLResponseWrapper {
	// every wrapper class name corresponds to one xml reply
	// net.tegla.flickr.Photosets - <photosets/>
	protected def elemlabel = this.getClass.getName.replaceFirst("net.tegla.flickr.","").toLowerCase
	assert(elem.label == elemlabel)

	val elem:Elem
	protected def attrib(name:String) = (elem \ ("@" + name)) text
	protected def child(name:String) = (elem \ name) text
	override def toString = elem.toString
}

trait WithChildren[CHILD] extends SeqProxy[CHILD] with XMLResponseWrapper {
	protected def createChild(elem:Elem):CHILD
	protected def childElemName:String

	private lazy val seq = (elem \ childElemName).map( n => createChild(n.asInstanceOf[Elem]) )
	def self = seq
}

trait HasId {
	def id:Long
}

trait CompareId[T <: HasId] extends HasId {
	override def hashCode = id.hashCode
	override def equals(other:Any) = {
		if (other.isInstanceOf[HasId]) {
			val that = other.asInstanceOf[HasId]
			(this.getClass == that.getClass) && (this.id == that.id)
		} else {
			false
		}
	}
}

trait Paged {
	def pages:Int
	def page:Int
	def total:Int 
}

final class Photoset(val elem:Elem) extends XMLResponseWrapper with CompareId[Photoset] {
	def videos = attrib("videos").toInt
	def photos = attrib("photos").toInt
	def farm = attrib("farm").toInt
	def server = attrib("server").toInt
	def secret = attrib("secret")
	def primary = attrib("primary")
	def id = attrib("id").toLong
	def title = child("title")
	def description = child("description")
}

final class Photosets(val elem:Elem) extends WithChildren[Photoset] {
	def cancreate = attrib("cancreate") == "1"
	override def createChild(elem:Elem) = new Photoset(elem)
	override def childElemName = "photoset"
}

final class User(val elem:Elem) extends XMLResponseWrapper {
	def nsid = attrib("nsid")
	def username = attrib("username")
	def fullname = attrib("fullname")
}

final class Auth(val elem:Elem) extends XMLResponseWrapper {
	def token = child("token")
	def perms = child("perms")
	def user = new User((elem \ "user").first.asInstanceOf[Elem])
}

final class Photo(val elem:Elem) extends XMLResponseWrapper with CompareId[Photo] {
	def isprimary = attrib("isprimary") == "1"
	def title = attrib("title")
	def farm = attrib("farm").toInt
	def server = attrib("server").toInt
	def secret = attrib("secret")
	def id = attrib("id").toLong
}

final class PhotosetList(val elem:Elem) extends Paged with WithChildren[Photo] {
	override def elemlabel = "photoset"
	def total = attrib("total").toInt
	def pages = attrib("pages").toInt
	def per_page = attrib("per_page").toInt
	def page = attrib("page").toInt
	def ownername = attrib("ownername")
	def owner = attrib("owner")
	def primary = attrib("primary")
	def id = attrib("id").toLong

	override def createChild(elem:Elem) = new Photo(elem)
	override def childElemName = "photo"
}

final class Photos(val elem:Elem) extends Paged with WithChildren[Photo] {
	def page = attrib("page").toInt
	def pages = attrib("pages").toInt
	def total = attrib("total").toInt
	override def createChild(elem:Elem) = new Photo(elem)
	override def childElemName = "photo"
}

final class Size(val elem:Elem) extends XMLResponseWrapper {
	def media = attrib("media")
	def url = attrib("url")
	def source = attrib("source")
	def height = attrib("height").toInt
	def width = attrib("width").toInt
	def label = attrib("label")
}

final class Sizes(val elem:Elem) extends scala.collection.MapProxy[String, Size]
                                 with XMLResponseWrapper {
	def candownload = attrib("candownload") == "1"
	def canprint = attrib("canprint") == "1"
	def canblog = attrib("canblog") == "1"

	private lazy val nodeMap = Map() ++ (
		(elem \ "size").
			map(n => new Size(n.asInstanceOf[Elem])).
			map(s => (s.label,s))
	)
	def self = nodeMap
}
