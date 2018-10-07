package test

import com.todesking.{ scalanb => nb }

class RecordEventListener extends nb.EventListener {
  var events = Seq.empty[(nb.NBState, nb.Event)]
  override def event(st: nb.NBState, e: nb.Event) =
    events = events :+ (st -> e)
}
