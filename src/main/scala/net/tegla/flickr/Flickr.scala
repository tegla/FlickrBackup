package net.tegla.flickr

import scala.util.Sorting
import scala.xml.Elem
import scala.xml.Node
import scala.xml.XML
import java.security.MessageDigest
import java.net.URL

trait Transport {
	def get(url:String):java.io.InputStream
}

abstract class XMLResponseWrapper(val node:Node) {
	protected def attrib(name:String) = (node \ ("@" + name)) text
	protected def child(name:String) = (node \ name) text
}

final class Photoset(node:Node) extends XMLResponseWrapper(node) {
	def videos = attrib("videos").toInt
	def photos = attrib("photos").toInt
	def farm = attrib("farm").toInt
	def server = attrib("server").toInt
	def secret = attrib("secret")
	def primary = attrib("primary")
	def id = attrib("id").toLong
	def title = child("title")
	def description = child("description")

	// AnyRef overrides
	override def hashCode = id.hashCode
	override def equals(that:Any) = that match {
		case photoset:Photoset => photoset.id == id
		case _ => false
	}
	override def toString = title
}

final class Photosets(node:Node) extends XMLResponseWrapper(node) with Seq[Photoset] {
	def cancreate = attrib("cancreate") == "1"
	
	// is there a SeqProxy trait?
	lazy val seq = (node \ "photoset").map( new Photoset(_) )
	def length = seq.length
	def elements = seq.elements
	def apply(i:Int) = seq.apply(i)
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

	def parseResponse(doc:Elem, expected:String) = {
		if (doc.label != "rsp") throw new RuntimeException("unknown: " + doc.label)
		val sub = (doc \ "@stat").text match {
			case "ok" => {
				val sub = (doc \ expected).firstOption
				sub.getOrElse(throw new RuntimeException("not found: " + expected))
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

	def getFrob():String = {
		val stream = transport.get(createURL("flickr.auth.getFrob", Map(), true))
		val doc = XML.load(stream)
		val frob = parseResponse(doc, "frob")
		frob.text
	}

	def getLoginLink(frob:String, perms:String):String = {
		createURL0("auth", Map("frob" -> Some(frob), "perms" -> Some(perms)), true)
	}

	def getToken(frob:String) = {
		val stream = transport.get(createURL("flickr.auth.getToken", 
			Map("frob" -> Some(frob)), true))
		val doc = XML.load(stream)
		val auth = parseResponse(doc, "auth")
		(auth \ "token") text

	}

	def getPhotoSets(user_id:Option[String]) = {
		val stream = transport.get(createURL("flickr.photosets.getList",
			Map("user_id" -> user_id), true))
		val doc = XML.load(stream)
		val photosets = parseResponse(doc, "photosets")
		new Photosets(photosets)
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

