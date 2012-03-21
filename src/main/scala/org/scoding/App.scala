/**
 * Copyright (c) 2011 Christoph Schmidt
 *
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 * notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the author nor the names of his contributors
 * may be used to endorse or promote products derived from this software
 * without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
 * ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 */

package org.scoding

import com.typesafe.play.mini._
import play.api.mvc._
import play.api.mvc.Results._
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.Actor
import akka.pattern.ask
import akka.util.Timeout
import akka.util.duration._
import akka.dispatch.Await
import org.scoding.oauth.{OAuthProvider, 
  GetRequesetToken, 
  OAuthResult, 
  GetAccessToken, 
  ValidateSignature,
  OAuthValidationResult}
import org.scoding.oauth.ValidateSignature

object DataApp extends Application {
  private val endpoint = "/rest/2.0"
  
  val systemSpirit = ActorSystem("SpiritData")
  val spiritService = systemSpirit.actorOf(Props[SpiritService], name = "spirit-service")  
  
  val systemOAuth = ActorSystem("SpiritOAuth")
  val spiritOAuth = systemOAuth.actorOf(Props[OAuthProvider], name = "spirit-oauth")
  
  def route  =  {
    case POST(Path("/oauth-provider/request_token")) =>  Action{ request =>     
      implicit val timeout = Timeout(5 seconds)
      val future = spiritOAuth ? GetRequesetToken(request) 
      val token = Await.result(future, timeout.duration).asInstanceOf[OAuthResult]
      token.response
    }
    case GET(Path("/oauth-provider/authorize")) =>  Action{ request =>
      Ok(<h1>TODO!</h1>).as("text/html")
    }
    case POST(Path("/oauth-provider/access_token")) =>  Action{ request =>           
      implicit val timeout = Timeout(5 seconds)
      val future = spiritOAuth ? GetAccessToken(request) 
      val token = Await.result(future, timeout.duration).asInstanceOf[OAuthResult]
      token.response
    }
    // Only as test
    case POST(Path("/oauth-provider/test")) =>  Action{ request =>           
      implicit val timeout = Timeout(5 seconds)
      val future = spiritOAuth ? ValidateSignature(request) 
      val validation = Await.result(future, timeout.duration).asInstanceOf[OAuthValidationResult]
      if(validation.hasAccess) Ok(<h1>Access!</h1>).as("text/html")
      else validation.statusMsg
    }
    case GET(Path(endpoint)) => Action{ request =>
           
      val responseHandler = systemSpirit.actorOf(Props[ResponseHandler]) 
      
      spiritService.tell(SpiritRequest(request.queryString), responseHandler)
      
      implicit val timeout = Timeout(5 seconds)
      val future = responseHandler ? SpiritResult 
      Await.result(future, timeout.duration).asInstanceOf[Result]
    }
  }
}

class ResponseHandler extends Actor {
  import play.api.libs.json._
  import Json._ 
  private var response = Ok(toJson(Map[String,String]()))
  private var status = false
  private var isError = false
  private var error = Ok("")
  def receive = {
    case SpiritResponse(result) =>
      response = Ok(result)
      status = true
    case SpiritResult =>
      if(status) {
        if(isError){
          sender ! error
          context.stop(self)
        } else {
          sender ! response
          context.stop(self)
        }
      } else {
        Thread.sleep(100)
        context.self.tell(SpiritResult,sender)
      }
    case SpiritError(err) => 
      isError = true
      error = err
      status = true 
  }
}
