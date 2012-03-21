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
  println("### Test access")
  println(doTest1)
  println("### Test no access")
  println(doTest2)
}
