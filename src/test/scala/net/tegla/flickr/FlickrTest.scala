package net.tegla.flickr

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers._

import scala.xml.XML

class FlickrTest extends FunSuite {

	class MockTransport(worker:PartialFunction[String, String]) extends Transport {
		override def get(url:String):java.io.InputStream = {
			if (!worker.isDefinedAt(url)) { fail(url) }
			new java.io.ByteArrayInputStream(worker(url).getBytes("UTF-8"))
		}
	}

	test("api_sig works, as in flickr docs") {
		val flickr = new Flickr(null, "000005fab4534d05", null, null)
		val signature = flickr.sign(Map(
			"method" -> "flickr.auth.getFrob",
			"api_key" -> "9a0554259914a86fb9e7eb014e4e5d52"))
		signature should be ("8ad70cd3888ce493c8dde4931f7d6bd0")		
	}

	test("parseResponse can handle failure") {
		val flickr = new Flickr(null, null, null, null)
		val doc = XML.loadString(
			"""<?xml version="1.0" encoding="UTF-8"?>
			<rsp stat="fail">
        			<err code="96" msg="Invalid signature"/>
			</rsp>""")
		val exception = intercept[RuntimeException] {
			flickr.parseResponse(doc, "frob")
		}
		exception should have ('message ("(96,Invalid signature)"))
	}


	test("parseresponse gets frob right") {
		val flickr = new Flickr(null, null, null, null)
		val doc = XML.loadString(
			"""<?xml version="1.0" encoding="UTF-8"?>
			<rsp stat="ok">
				<frob>72157622937385745-e9e798d6cd643682-520711</frob>
			</rsp>""")
		val frob = flickr.parseResponse(doc, "frob")
		frob should be (<frob>72157622937385745-e9e798d6cd643682-520711</frob>)
	}

	test("login loop") {
		// this is copied from real life
		val flickr = new Flickr(
			"3f85b72e715c123e97800aaa95d8b56e",
			"2fd0efe09d4d3a6e",
			None,
			new MockTransport(Map(
				"http://api.flickr.com/services/rest/?api_key=3f85b72e715c123e97800aaa95d8b56e&api_sig=12d4c3ea32aafd1f8675bdf2c7f40fb8&method=flickr.auth.getFrob"->
				"""<?xml version="1.0" encoding="UTF-8"?>
				<rsp stat="ok">
					<frob>72157622939479697-6776e136f19598ce-971749</frob>
				</rsp>""",
				"http://api.flickr.com/services/rest/?api_key=3f85b72e715c123e97800aaa95d8b56e&api_sig=8be32660cd494178c79387b2d00cbc14&frob=72157622939479697-6776e136f19598ce-971749&method=flickr.auth.getToken" ->
				"""<?xml version="1.0" encoding="UTF-8"?>
				<rsp stat="ok">
				<auth>
				<token>72157622931806485-5285ff7f60695ef3</token>
					<perms>read</perms>
					<user fullname="" username="tegla" nsid="10686481@N00"></user>
				</auth>
				</rsp>""")))
		
		val frob = flickr.auth.getFrob()
		frob should be ("72157622939479697-6776e136f19598ce-971749")

		val loginLink = flickr.getLoginLink(frob, "read")
		loginLink should be ("http://api.flickr.com/services/auth/?api_key=3f85b72e715c123e97800aaa95d8b56e&api_sig=59f46161a3eba13bf8344feef5d3b51c&frob=72157622939479697-6776e136f19598ce-971749&perms=read")

		val auth = flickr.auth.getToken(frob)
		auth should have ('token ("72157622931806485-5285ff7f60695ef3"))
	}
}

