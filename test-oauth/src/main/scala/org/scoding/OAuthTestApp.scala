package org.scoding

import isamal._

object OAuthTestApp extends App {
  val scalaNode1 = OAuth1Account(
    name              = "TestAccount", 
    consumerKey       = "dpf43f3p2l4k3l03",
    consumerSecret    = "kd94hf93k423kf44",
    requestTokenURL   = "http://localhost:9000/oauth-provider/request_token",
    authURL           = "http://localhost:9000/oauth-provider/authorize",
    accessURL         = "http://localhost:9000/oauth-provider/access_token"
    //accessToken       = ""
    //accessTokenSecret = ""
  )
  
  val scalaNode2 = OAuth1Account(
    name              = "TestAccount", 
    consumerKey       = "dpf43f3p2l4k3l03",
    consumerSecret    = "kd94hf93k423kf44",
    requestTokenURL   = "http://localhost:9000/oauth-provider/request_token",
    authURL           = "http://localhost:9000/oauth-provider/authorize",
    accessURL         = "http://localhost:9000/oauth-provider/access_token",
    accessToken       = "f49f6d71327eebc36699e36d2edaddfb",
    accessTokenSecret = "fef0249c237470fb7d2ad2476c1e625c"
  )
    
  def doTest1 = {
    val oauth = new OAuth1(scalaNode1, true)
    oauth.doPost("http://localhost:9000/oauth-provider/test", Map[String,String]())
  }
  
  def doTest2 = {
    val oauth = new OAuth1(scalaNode2, true)
    oauth.doPostFail("http://localhost:9000/oauth-provider/test", Map[String,String]())
  }
  
  def doTest3 = {
    val oauth = new OAuth1(scalaNode1, true)
    oauth.doPostFail2("http://localhost:9000/oauth-provider/test", Map[String,String]())
  }
  
  def doTest4 = {
    val oauth = new OAuth1(scalaNode1, true)
    oauth.doGet("http://localhost:9000/oauth-provider/test", Map[String,String]())
  }
  
  def doTest5 = {
    val oauth = new OAuth1(scalaNode1, true)
    oauth.doGetFail("http://localhost:9000/oauth-provider/test", Map[String,String]())
  }
  
  def doTest6 = {
    val oauth = new OAuth1(scalaNode1, true)
    oauth._get("http://localhost:9000/rest/2.0",Map("method" -> "get.news"), Map[String,String]())
  }
  
  def doTest7 = {
    val oauth = new OAuth1(scalaNode1, true)
    oauth._post("http://localhost:9000/rest/2.0",Map("method" -> "get.news"), Map[String,String]())
  }
  
  println("### Test access via post")
  println(doTest1)
  println("### Test no access via post with wrong token")
  println(doTest2)
  println("### Test no access via post with wrong signature")
  println(doTest3)
  println("### Test access via get")
  println(doTest4)
  println("### Test access via get with wrong signature")
  println(doTest5)
  println("### Test get.news via get")
  println(doTest6)
  println("### Test get.news via post")
  println(doTest7)
}
