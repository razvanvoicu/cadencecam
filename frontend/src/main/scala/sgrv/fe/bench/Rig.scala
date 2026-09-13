package sgrv.fe.bench

/** Which handset is counting, and which of its cameras is pointed at the screen.
  *
  * Both are chosen at the bench rather than detected. The counting device does report a user-agent string, but that
  * string names a browser engine and an operating system: it cannot tell two handsets of the same family apart, Chrome
  * freezes every Android model to "K", and it says nothing whatever about which camera is in use. The person running
  * the suite knows both, so they are asked once and carried with the recording.
  */
private[fe] object Rig:
  /** The handsets and machines the suites are run on, under the names they are known by here.
    *
    * Deliberately the operator's own names rather than model numbers. The point of the field is to tell one recording
    * from another months later, and "Yoga battery problem" identifies a machine in a way "82JD" does not.
    */
  val Devices: Seq[String] = Seq(
    "Pixel",
    "Fold",
    "S23",
    "Oppo",
    "Xiaomi",
    "A53",
    "A03",
    "Iphone 17",
    "Iphone 12",
    "Iphone 9",
    "Iphone 8",
    "Ipad",
    "Lenovo Legion",
    "Lenovo Pen",
    "Lenovo No Pen",
    "MacBook",
    "Mac mini",
    "HP",
    "LG",
    "Yoga battery problem",
    "Yoga small",
    "Yoga 16g",
    "Yoga 32g"
  )

  /** Which camera is doing the counting. Front and back differ in sensor, optics and processing on the same handset. */
  val Cameras: Seq[String] = Seq("Back camera", "Front camera")

  val DefaultDevice: String = Devices.head
  val DefaultCamera: String = Cameras.head

  /** What the choice adds to a capture's note.
    *
    * Appended to the note the bench already sends rather than carried in fields of its own. The note travels to the
    * counting device inside the capture command and is written with the recording there, so this reaches the stored
    * trace without a change to the wire format, the backend schema or the documents already filed.
    */
  def describe(device: String, camera: String): String = s"$device, $camera"
