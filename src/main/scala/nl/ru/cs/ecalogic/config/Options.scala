/*
 * ecalogic: a tool for performing energy consumption analysis.
 *
 * Copyright (c) 2013, J. Neutelings, D. Peelen, M. Schoolderman
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *   Redistributions of source code must retain the above copyright notice, this
 *   list of conditions and the following disclaimer.
 *
 *   Redistributions in binary form must reproduce the above copyright notice, this
 *   list of conditions and the following disclaimer in the documentation and/or
 *   other materials provided with the distribution.
 *
 *   Neither the name of the Radboud University Nijmegen nor the names of its
 *   contributors may be used to endorse or promote products derived from
 *   this software without specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package nl.ru.cs.ecalogic
package config

import util.{Positional, Position}

/*
  Stub.
TODO: adds some code that reads cmdline flags here.
 */

object Options {
  object Model {
    /* Should a delta-function always forward the energy-aware
       state-information even if the component state did not change?
       Note: if beforeSync and afterSync are set, this setting is useless.

       Technical report: false */
    var alwaysForwardTime = false
  }

  object Analysis {
    /* Should all component states be forwarded just before a decision
       in the control flow? (if/while statement). Setting this to false
       results in over-estimations.

       Technical report: false */
    var beforeSync = true

    /* In the original document, at the exit of a while-loop, all timestamps
       of components get reset to the time before entering the while loop,
       while the global timestamp gets set to a time *after*. This is consistent
       with the if-statement (similar problem), but causes a factor two over-
       estimation in even the most simple cases.

       Technical report: false */
    var afterSync = true

    /* How long should we attempt to find fixpoint? Note that 10000 is a high setting */
    var fixPatience = 10000

    /* If the ranking function is a concrete value, take that instead of the above global value?
       This will produce better estimates in exotic cases.

       Technical report: false? -- but not clear on this point */
    var fixLimit = false
  }
}
