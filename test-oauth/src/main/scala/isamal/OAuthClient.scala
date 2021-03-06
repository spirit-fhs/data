package isamal

/**
 *  This is a simple implementation of OAuth for Scala.
 *
 *  Author: Bob Jamison
 *  Copyright, 2010 
 *  
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program.  If not, see .
 *
 */   


/**
 * This trait has methods common to OAuth 1 and 2.
 */ 
trait OAuth
{
    val debug = false

    //Override to redirect messages
    def trace(msg: String) =
        println("OAuth: " + msg)
    
    //Override to redirect messages
    def error(msg: String) =
        println("OAuth err: " +msg)
        
    protected def paramStr(mp: Map[String, String]) : String =
        mp.toList.sortWith((a,b) => a._1 < b._1).
        map(e => encode(e._1)+"="+encode(e._2)).mkString("&")
    
    
    /*protected*/ def _post(urls: String, 
             params: Map[String, String] = Map(),
             props: Map[String, String] = Map()) : Option[String] =
        {
        val outs = paramStr(params).getBytes
        val url = new java.net.URL(urls)
        val conn = url.openConnection.asInstanceOf[java.net.HttpURLConnection]
        try
            {
            conn.setRequestMethod("POST")
            conn.setDoOutput(true)
            conn.setRequestProperty("Content-Length", outs.length.toString)
            props.foreach(e => conn.setRequestProperty(e._1, e._2))
            conn.getOutputStream.write(outs)
            conn.getOutputStream.close 
            Some(read(conn.getInputStream))
            }
        catch
            {
            case e:Exception => error("post: " + e)
                error("post:" + read(conn.getErrorStream))
            None
            }
        }

    protected def _postv(urls: String, 
             params: Map[String, String] = Map(),
             props: Map[String, String] = Map()) : Option[Map[String,String]] =
        {
        
        val res = _post(urls, params, props)
        res.flatMap(s=>Some(parseValues(s)))
        }

    /*protected*/ def _get(baseUrl: String, 
             params: Map[String, String] = Map(),
             props: Map[String, String] = Map()) : Option[String] =
        {
        val urls = if (params.size == 0) baseUrl else baseUrl + "?" + paramStr(params)
        val url = new java.net.URL(urls)
        val conn = url.openConnection.asInstanceOf[java.net.HttpURLConnection]
        try
            {
            props.foreach(e => conn.setRequestProperty(e._1, e._2))
            Some(read(conn.getInputStream))
            }
        catch
            {
            case e:Exception => error("get: " + e)
                error("get:" + read(conn.getErrorStream))
            None
            }
        }


    //Parse ?name1=value1&name2=value2&  ...etc
    def parseValues(str: String) : Map[String, String] =
        {
        trace("parseValues:" + str)
        val vals = scala.collection.mutable.Map[String, String]()
        str.split("&").foreach(pair=>
            {
            val p = pair.split("=")
            if (p.size>=2)        // name=value
                vals += p(0) -> p(1)
            else if (p.size>=1)   // name=  or just name
                vals += p(0) -> ""
            })
        vals.toMap
        }

    private val hex = "0123456789ABCDEF".toCharArray //must be uppers

    /**
     * This is not really url-encoding, but RFC 3986
     */
    protected def encode(str: String) : String =
        {
        val buf = new StringBuilder
        str.foreach(ch =>
            {
            if (ch.isLetterOrDigit || ch == '-' || ch == '.' || ch == '_' || ch == '~')
                buf.append(ch)
            else
                buf.append("%").append(hex((ch>>4)&0xf)).append(hex(ch&0xf))
            })
        buf.toString
        }

    //Use the runtime's builtin base64, cleverly hidden away
    protected def base64(plain: Array[Byte]) : String =
        javax.xml.bind.DatatypeConverter.printBase64Binary(plain)

    //read an input stream into a string
    protected def read(ins: java.io.InputStream) : String = {
      scala.io.Source.fromInputStream(ins).mkString
    }

    /**
     * Perform a POST with the given base URL string and parameters.
     * A raw string is returned.  parseValues can read it if desired to
     * pull out name/value pairs.
     */     
    def doPost(urls: String, params: Map[String, String] = Map()) : Option[String] =
        {
        error("doPost not implemented")
        None
        }
 
    /**
     * Perform a GET with the given base URL string and parameters.
     * A raw string is returned.  parseValues can read it if desired to
     * pull out name/value pairs.
     */     
    def doGet(urls: String, params: Map[String, String] = Map()) : Option[String] =
        {
        error("doGet not implemented")
        None
        }

}


//########################################################################
//### O A U T H  version 1
//########################################################################

/**
 * Note that the following fields are basically used in order
 * If fields 7 and 8 are provided, then 2-6 are skipped.  If
 * not, then 2-6 are required.  
 */ 
case class OAuth1Account
    (
    //1. Name of account, to be used later for load/save
    val name               : String = "",
    //2. ID number of this client.  Use your app's
    val consumerKey        : String = "",
    //3. Use to sign request token call
    val consumerSecret     : String = "",
    //4. The provider's URL for fetching an OAuth request token
    val requestTokenURL    : String = "",
    //5. The provider's URL for asking the user's permission to access the API
    //returns a 'validation' key
    val authURL            : String = "",
    //6. The provider's URL for turning a validation key into an access token
    val accessURL          : String = "",
    //7. This is the actual token for permission to a provider's API
    val accessToken        : String = "",
    //8. This secret is used to sign the API request before submitting it
    val accessTokenSecret  : String = ""
    )

class OAuth1(
    account  : OAuth1Account = new OAuth1Account,
    override val debug : Boolean = false
) extends OAuth
{
    private val hmac = "HMAC-SHA1"

    private var acct = account
    
    /*private*/ var requestToken       : Option[String] = None
    private var requestTokenSecret : Option[String] = None
    //private var oauthVerifier      : Option[String] = None
    private var oauthVerifier      : Option[String] = Some("asdfjkleadsf") 

    /**
     * This is the most important part of OAuth1.  Your desired method's signature
     * must match that calculated by the server.  No query string, please.
     */     
    private def signature(httpMethod: String, baseURI: String,
                   parms: Map[String,String], secret: String)  =
        {
        val params = paramStr(parms)
        val baseStr = httpMethod + "&" + encode(baseURI) + "&" + encode(params)
        trace("baseStr: " + baseStr)
        val method = hmac match //Convert to Java's method names
            {
            case "HMAC-SHA1" => "HmacSHA1"
            case "PLAINTEXT" => "PLAIN"
            case _           => "PLAIN"
            }
        val mac = javax.crypto.Mac.getInstance(method)
        val key = acct.consumerSecret + "&" + secret
        val signingKey = new javax.crypto.spec.SecretKeySpec(key.getBytes, method)
        mac.init(signingKey)
        val str = base64(mac.doFinal(baseStr.getBytes))
        str
        }
    
    private def authorizationString(parms: Map[String, String]) : String =
        "OAuth " + parms.toList.sortWith((a,b) => a._1 < b._1).
            map(a=> a._1 + "=\"" + encode(a._2) + "\"").mkString(", ")

    /*private*/ def getRequestToken : Boolean =
        {
        val parms = Map(
            "oauth_callback"         -> "oob",
            "oauth_consumer_key"     -> account.consumerKey,
            "oauth_nonce"            -> System.nanoTime.toString,
            "oauth_signature_method" -> hmac,
            "oauth_timestamp"        -> (System.currentTimeMillis / 1000).toString,
            "oauth_version"          -> "1.0"
            )
        val sig = signature("POST", acct.requestTokenURL, parms, "")
        println("signature: " + sig)
        val authStr = authorizationString(parms + ("oauth_signature" -> sig) )
        println("authStr: " + authStr)
        val res = _postv(acct.requestTokenURL, props = Map("Authorization"->authStr))
        requestToken = res.flatMap(_.get("oauth_token"))
        requestTokenSecret = res.flatMap(_.get("oauth_token_secret"))
        if (requestToken.isEmpty || requestTokenSecret.isEmpty)
            {
            error("getRequestToken: server did not return token")
            false
            }
        else
            true
        }

    /**
     * Open a browser to the service's authorization page. Show a dialog to
     * get the key from the user
     */              
    private def getAuthorization : Boolean =
        {
        val urls = acct.authURL + "?oauth_token=" + requestToken.get
        java.awt.Desktop.getDesktop.browse(new java.net.URI(urls))
        oauthVerifier = Option(javax.swing.JOptionPane.showInputDialog("Input key")).flatMap(s=>Some(s.trim))
        trace("oauthVerifier:'" + oauthVerifier + "'")
        oauthVerifier.isDefined
        }
    
    def getAccessToken : Boolean =
        {
        val parms = Map(
                "oauth_consumer_key"     -> acct.consumerKey,
                "oauth_nonce"            -> System.nanoTime.toString,
                "oauth_signature_method" -> hmac,
                "oauth_timestamp"        -> (System.currentTimeMillis / 1000).toString,
                "oauth_token"            -> requestToken.get,
                //"oauth_verifier"         -> oauthVerifier.get,
                "oauth_version"          -> "1.0"
                )
        
        val sig = signature("POST", acct.accessURL, parms, requestTokenSecret.get)
        val authStr = authorizationString(parms + ("oauth_signature" -> sig) )
        val res = _postv(acct.accessURL, props=Map("Authorization"->authStr))
        val accessToken = res.flatMap(_.get("oauth_token"))
        val accessTokenSecret = res.flatMap(_.get("oauth_token_secret"))
        if (accessToken.isEmpty || accessTokenSecret.isEmpty)
            {
            error("getAccessToken: server did not provide access token")
            false
            }
        else
            {
            trace("accessToken: " + accessToken.get +
                 " accessTokenSecret: " + accessTokenSecret.get)
            acct = acct.copy(accessToken=accessToken.get,
                 accessTokenSecret=accessTokenSecret.get)
            true
            } 
        }

    private def checkAccess =
        {
        if (acct.accessToken.length>0 && acct.accessTokenSecret.length>0)
            true
        else if (getRequestToken /*&& getAuthorization*/ && getAccessToken)
            true
        else
            false
        }
    
    private def accessParams = Map(
          "oauth_consumer_key"     -> acct.consumerKey,
          "oauth_nonce"            -> System.nanoTime.toString,
          "oauth_signature_method" -> hmac,
          "oauth_timestamp"        -> (System.currentTimeMillis / 1000).toString,
          "oauth_token"            -> acct.accessToken,
          "oauth_version"          -> "1.0"
          )

    override def doPost(urls: String, params: Map[String, String] = Map()) : Option[String] =
        {
        if (!checkAccess)
            None
        else
            {
            val oparams = accessParams
            val sig = signature("POST", urls, oparams++params, acct.accessTokenSecret)
            val authStr = authorizationString(oparams + ("oauth_signature" -> sig) )
            trace("authStr: " + authStr)
            _post(urls, params, props=Map("Authorization"->authStr))
            }
        }
    
    // Only for test
    def doPostFail(urls: String, params: Map[String, String] = Map()) : Option[String] =
        {       
            val oparams = accessParams
            val sig = signature("POST", urls, oparams++params, acct.accessTokenSecret)
            val authStr = authorizationString(oparams + ("oauth_signature" -> sig) )
            trace("authStr: " + authStr)
            _post(urls, params, props=Map("Authorization"->authStr))   
        }
    // Only for test
    def doPostFail2(urls: String, params: Map[String, String] = Map()) : Option[String] =
        {
        if (!checkAccess)
            None
        else
            {
            val oparams = accessParams
            val sig = signature("POST", urls, oparams++params, "fef0249c237470fb7d2ad2476c1e625c")
            val authStr = authorizationString(oparams + ("oauth_signature" -> sig) )
            trace("authStr: " + authStr)
            _post(urls, params, props=Map("Authorization"->authStr))
            }
        }

    override def doGet(urls: String, params: Map[String, String] = Map()) : Option[String] =
        {
        if (!checkAccess)
            None
        else
            {
            val oparams = accessParams
            val sig = signature("GET", urls, oparams++params, acct.accessTokenSecret)
            val authStr = authorizationString(oparams + ("oauth_signature" -> sig) )
            trace("authStr: " + authStr)
            _get(urls, params, props=Map("Authorization"->authStr))
            }
        }
    // Onlay as test
    def doGetFail(urls: String, params: Map[String, String] = Map()) : Option[String] =
        {
        if (!checkAccess)
            None
        else
            {
            val oparams = accessParams
            val sig = signature("GET", urls, oparams++params, "fef0249c237470fb7d2ad2476c1e625c")
            val authStr = authorizationString(oparams + ("oauth_signature" -> sig) )
            trace("authStr: " + authStr)
            _get(urls, params, props=Map("Authorization"->authStr))
            }
        }

}
