package sgrv.api

/** The static documents the app publishes about itself.
  *
  * Named in one place because two ends have to agree on them and neither would notice if they stopped: the backend
  * serves these files, the app links to them, and a link to a privacy policy that four-oh-fours is worse than no link
  * at all -- it is the one page a person goes looking for when they have already decided to be suspicious.
  */
object Documents:
  val Privacy = "privacy.html"
  val Terms = "tos.html"
  val AccountDeletion = "delete-account.html"

  /** Every legal document the application menu should offer, in display order. */
  val All: Seq[String] = Seq(Privacy, Terms)

  /** Where a document is served, as a link should ask for it. */
  def path(document: String): String = s"/$document"
