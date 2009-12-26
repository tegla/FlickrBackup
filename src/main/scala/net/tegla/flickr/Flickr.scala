package net.tegla.flickr

import scala.util.Sorting
import scala.xml.Elem
import scala.xml.XML

import java.security.MessageDigest

trait Transport {
	def get(url:String):java.io.InputStream
}

final class Flickr(
		val api_key:String,
		val secret:String,
		val auth_token:Option[String],
		val transport:Transport) 
{
	def sign(params:Map[String,String]):String = {
		val arr = Array[String]() ++ params.keySet
		Sorting.quickSort(arr)
		val md5 = MessageDigest.getInstance("MD5")
		def add(s:String) { md5.update(s.getBytes("ASCII")) }
		md5.reset
		add(secret)
		for(key <- arr) {
			add(key)
			add(params(key))
		}
		md5.digest().map(0xff & _).map {"%02x".format(_)}.mkString("")
	}

	def createURL0(	base:String,
			params:Map[String, Option[String]],
			needSignature:Boolean):String = {
		val p = Map("api_key" -> api_key) ++
			(for ( (key,option) <- params; value <- option ) yield key -> value) ++
			auth_token.map( "auth_token" -> _ )
		val all = if (needSignature) {
			p.update("api_sig", sign(p))
		} else {
			p
		}
		val pairs = all.toList.sort((a,b) => a._1 < b._1) // make it deterministic, to ease testing
		val query = (for ( (key,value) <- pairs) yield key + "=" + value).mkString("&")
		"http://api.flickr.com/services/" + base + "/?" + query
	}

	def createURL(	method:String,
			params:Map[String, Option[String]],
			needSignature:Boolean):String = {
		createURL0("rest", params.update("method", Some(method)), needSignature)
	}

	def parseResponse(doc:Elem):Elem = {
		if (doc.label != "rsp") throw new RuntimeException("unknown: " + doc.label)
		val sub = (doc \ "@stat").text match {
			case "ok" => {
				val subs = for (child <- doc.child
						if child.isInstanceOf[Elem]) yield child.asInstanceOf[Elem]
				if (subs.length != 1) throw new RuntimeException("too many" + subs.length)
				subs.first 
			}
			case "fail" => {
				val x = (doc \ "err").firstOption.map(d =>
					 (d \ "@code" text, d \ "@msg" text))
				// TODO: define exception
				throw new RuntimeException(x.map(_.toString).getOrElse("?"))
			}
			case _ => throw new RuntimeException("unknown")
		}
		sub

	}

	def getLoginLink(frob:String, perms:String):String = {
		createURL0("auth", Map("frob" -> Some(frob), "perms" -> Some(perms)), true)
	}

	private def loadAllPages[CHILD, P <: Paged with Seq[CHILD]](loader:Int => P):Seq[CHILD] = {
		val first = loader(1)	
		val restRange = (2 until first.pages inclusive) // flickr API counts pages as [1...max]
		val result = restRange.foldLeft(List() ++ first)(_++loader(_))
		assert(result.length == first.total)
		result	
	}
	// This subclass system ensures that we look very like the official Flickr API
	abstract class Method[T] {
		val method = {
			// This is a bit of black magick, totally unportable.
			// Might remove it totally, when I'm shamed enough
			val a = this.getClass.getName.split('$')
			a.update(0, "flickr")
			a.mkString(".")
		}
		val needApiSig = true
		protected def result(elem:Elem):T
		protected def call(params:Map[String,Option[String]]):T = {
			val stream = transport.get(createURL(method, params, needApiSig))
			val doc = XML.load(stream)
			val subNode = parseResponse(doc)
			result(subNode)
		}
	}

	object auth {
		object getFrob extends Method[String] {
			def apply() = call(Map())
			def result(e:Elem) = e text
		}
		object getToken extends Method[Auth] {
			def apply(frob:String) = call(Map("frob" -> Some(frob)))
			def result(e:Elem) = new Auth(e)
		}
		object checkToken extends Method[Auth] {
			def apply(auth_token:String) = call(Map("auth_token" -> Some(auth_token)))
			def result(e:Elem) = new Auth(e)
		}
	}

	object photos {
		object getNotInSet extends Method[Photos] {
			def apply(per_page:Int, page:Int) = call(Map(
				"per_page" -> Some(per_page.toString),
				"page" -> Some(page.toString)))
			def apply(method:Int => Photos) = loadAllPages[Photo,Photos](method)
			def apply():Seq[Photo] = apply({i:Int => apply(100, i)})
			def result(e:Elem) = new Photos(e)
		}
	}

	object photosets {
		object getList extends Method[Photosets] {
			def apply() = call(Map())
			def apply(user_id:String) = call(Map("user_id" -> Some(user_id)))
			def apply(user:User) = call(Map("user_id" -> Some(user.nsid)))
			def result(e:Elem) = new Photosets(e)
		}
		object getPhotos extends Method[PhotosetList] {
			def apply(photoset_id:Long, per_page:Int, page:Int) = call(Map(
				"photoset_id" -> Some(photoset_id.toString),
				"per_page" -> Some(per_page.toString),
				"page" -> Some(page.toString)))
			def result(e:Elem) = new PhotosetList(e)
			def apply(method:Int => PhotosetList) = loadAllPages[Photo,PhotosetList](method)
			def apply(photoset_id:Long):Seq[Photo] = apply({i:Int => apply(photoset_id, 100, i)})
		}
	}
}

object Flickr {
	object FlickrTransport extends Transport {
		override def get(url:String) = new java.net.URL(url).openStream
	}

	def ProbaApp = {
		val auth_token = System.getenv("FLICKR_AUTH_TOKEN")
		new Flickr(
			"3f85b72e715c123e97800aaa95d8b56e",
			"2fd0efe09d4d3a6e",
			if (auth_token == null) { None } else { Some(auth_token) },
			FlickrTransport)
	}

	def apply(api_key:String, secret:String) = new Flickr(api_key, secret, None, FlickrTransport)
	def apply(api_key:String, secret:String, token:String) = new Flickr(api_key, secret, Some(token), FlickrTransport)
}

