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

import play.api.libs.json._
import Json._

object TestData {
  val news = toJson(
    Map(  
        "semester"-> "semester alte_semester alte_i alte_wi ",
        "name"-> "braun3",
        "subject" -> "LaTeX-Template für Bachelor- / Masterarbeiten",
        "writer"-> "Prof. Dr. Braun",
        "lifecycle" -> "31.03.2012",
        "nr"-> "155",
        "_id" -> "4dd11f071165752e513e01c0",
        "date" -> "Mon, 16 May 2011 15:16:42 +0200",
        "news" -> "um Ihnen den Umstieg auf LaTeX etwas leichter zu machen :-), gibt es ab sofort ein Template auf \"GitHub\":https://github.com/pads-fhs/LaTeX-Template-Thesis .\r\n\r\nWie das Ganze dann aussieht, können Sie \"hier\":https://pads.fh-schmalkalden.de/wiki/LaTeX-Template-Thesis.pdf bewundern (Login mit fhsID und Passwort).\r\n\r\nUpdate und Fehlerberichtigung: Vielen Dank für die Unterstützung an Florian Schuhmann, Marcus Denison und Christian Linde."
    ) 
  )
}
