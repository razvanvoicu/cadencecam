package sgrv.fe.acquire

/** The words the acquirer shows for what the detector is doing.
  *
  * In one place because two screens show it. The acquirer renders this and also sends it to whatever is watching, so
  * the dashboard repeats the text rather than re-deriving it — which is the only way the two can be guaranteed to
  * read identically, rather than merely intended to.
  */
private[fe] object StatusLine:
  def of(lock: LockState): String = lock match
    case LockState.Acquiring(samples, needed) =>
      val seconds = math.max(0, needed - samples) / 10
      if needed == 0 then "Waiting for the camera…" else s"Finding a cadence… about ${seconds}s"
    case LockState.Searching => "No steady cadence — paused"
    // Kept short enough to sit on one line between the reset control and its counterweight.
    case LockState.Locked(channel, partner, periodSeconds) =>
      f"Counting $channel+$partner · $periodSeconds%.1fs/rep"

  /** What a watching device shows before its first reading arrives, or once the acquirer has gone away. */
  val Waiting = "Waiting for the acquirer…"
