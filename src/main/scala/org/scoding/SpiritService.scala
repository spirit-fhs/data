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

import akka.actor.Actor
import akka.actor.Props
import akka.actor._
import play.api.mvc.Results.NotFound
import play.api.mvc.Results.Status

class SpiritService extends Actor {
  
  val newsActor = context.actorOf(Props[NewsActor], name = "news-actor")
  
  def receive = {
    case SpiritRequest(request) =>
     val query = toStringMap(request)
     query.get("method") match {
        case Some("get.news") => 
          val replyTo = sender
          val aggregator = context.actorOf(Props(new Aggregator(replyTo)))
          newsActor.tell(query, aggregator)  
        case _ => 
          val error = Status(404)("Not Found")
          sender ! SpiritError(error) 
     }
  }
  
  def toStringMap(in: Map[String,Seq[String]]) = in.map(e => e._1 -> e._2.head)
}
