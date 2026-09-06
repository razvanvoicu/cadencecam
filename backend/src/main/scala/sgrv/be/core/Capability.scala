package sgrv.be.core

import zio.{Chunk, Tag, ZEnvironment}

/** A stable, human-readable name for a service that a dynamically loaded plugin can require. */
final class Capability[A] private (val id: String, private[core] val tag: Tag[A])

object Capability:
  def apply[A: Tag](id: String): Capability[A] =
    require(id.nonEmpty, "A capability id must not be empty")
    new Capability(id, summon[Tag[A]])

final case class MissingCapability(id: String)

/** The services that are available to plugins in this backend process. */
final class CapabilityRegistry private (private val environment: ZEnvironment[Any]):
  private[core] def get[A](capability: Capability[A]): Option[A] =
    environment.getDynamic[A](using capability.tag)

object CapabilityRegistry:
  def fromEnvironment[R](environment: ZEnvironment[R]): CapabilityRegistry =
    new CapabilityRegistry(environment)

  val empty: CapabilityRegistry = fromEnvironment(ZEnvironment.empty)

/** A runtime-resolvable description of the environment required by a plugin.
  *
  * The type parameter is preserved through composition: combining requirements for `A` and `B` yields a requirement for
  * `A & B` and resolving it yields a correspondingly typed [[ZEnvironment]].
  */
sealed trait CapabilitySet[R]:
  self =>
  private[core] def resolve(registry: CapabilityRegistry): Either[Chunk[MissingCapability], ZEnvironment[R]]
  final def ++[R2](that: CapabilitySet[R2]): CapabilitySet[R & R2] = CapabilitySet.Both(self, that)

object CapabilitySet:
  val empty: CapabilitySet[Any] = Empty
  def one[A](capability: Capability[A]): CapabilitySet[A] = One(capability)

  private case object Empty extends CapabilitySet[Any]:
    override def resolve(registry: CapabilityRegistry): Either[Chunk[MissingCapability], ZEnvironment[Any]] =
      Right(ZEnvironment.empty)

  private final case class One[A](capability: Capability[A]) extends CapabilitySet[A]:
    override def resolve(registry: CapabilityRegistry): Either[Chunk[MissingCapability], ZEnvironment[A]] =
      registry
        .get(capability)
        .map(value => ZEnvironment(value)(using capability.tag))
        .toRight(Chunk.single(MissingCapability(capability.id)))

  private final case class Both[A, B](left: CapabilitySet[A], right: CapabilitySet[B]) extends CapabilitySet[A & B]:
    override def resolve(registry: CapabilityRegistry): Either[Chunk[MissingCapability], ZEnvironment[A & B]] =
      (left.resolve(registry), right.resolve(registry)) match
        case (Right(leftEnvironment), Right(rightEnvironment)) => Right(leftEnvironment.unionAll(rightEnvironment))
        case (Left(leftMissing), Left(rightMissing))           => Left(leftMissing ++ rightMissing)
        case (Left(missing), _)                                => Left(missing)
        case (_, Left(missing))                                => Left(missing)
