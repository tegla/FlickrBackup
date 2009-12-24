package net.tegla.flickr

import scala.util.Sorting
import scala.xml.Elem
import java.security.MessageDigest
import java.net.URL

trait Transport {
	def get(url:String):java.io.InputStream
}

final class Flickr(
		val api_key:String,
		val secret:String,
		val token:Option[String],
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

	def createURL(	method:String,
			params:Map[String, Option[String]],
			needSignature:Boolean):String = {
		val p = Map("method" -> method, "api_key" -> api_key) ++
			(for ( (key,option) <- params; value <- option ) yield key -> value)
		val all = if (needSignature) {
			p.update("api_sig", sign(p))
		} else {
			p
		}
		val query = (for ( (key,value) <- all) yield key + "=" + value).mkString("&")
		"http://api.flickr.com/services/rest/?" + query
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
		val url = new java.net.URL( createURL("flickr.auth.getFrob", Map(), true))
		println(url)
		"foo"
	}
}

object Flickr {
	object FlickrTransport extends Transport {
		override def get(url:String) = new java.net.URL(url).openStream
	}

	def ProbaApp(token:Option[String]) = new Flickr(
		"3f85b72e715c123e97800aaa95d8b56e",
		"2fd0efe09d4d3a6e",
		token,
		FlickrTransport)

	def apply(api_key:String, secret:String) = new Flickr(api_key, secret, None, FlickrTransport)
	def apply(api_key:String, secret:String, token:String) = new Flickr(api_key, secret, Some(token), FlickrTransport)
}

