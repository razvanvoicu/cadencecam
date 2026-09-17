package sgrv.api

import zio.json.{DeriveJsonCodec, JsonCodec, jsonNoExtraFields}

/** Which of the two ways of turning movement into energy an exercise is measured by.
  *
  * They are not two approximations of one thing. Counting reps says every repetition costs the same, which is true of a
  * weight lifted the same way each time and false of a bicycle, where the whole of the effort is in how fast the legs
  * are going round. Counting frequency says the opposite, and the exercise decides which is the honest one.
  */
enum CountsBy:
  /** `Reps x Weight x Factor`. */
  case RepCount

  /** Every rep from the second adds `1 / (its time - the previous rep's time)` to a running sum, and that sum takes the
    * place of the rep count: `Sum x Weight x Factor`. A rep performed twice as fast is worth twice as much.
    */
  case Frequency

object CountsBy:
  given JsonCodec[CountsBy] = DeriveJsonCodec.gen[CountsBy]

/** The unit a weight is entered and shown in. What is stored is always kilograms, so changing this changes the display
  * and nothing else -- a calorie figure that moved by a factor of 2.2 because someone looked at their weight in pounds
  * would be reporting the unit rather than the effort.
  */
enum WeightUnit:
  case Kilograms
  case Pounds

object WeightUnit:
  val PoundsPerKilogram = 2.2046226218

  given JsonCodec[WeightUnit] = DeriveJsonCodec.gen[WeightUnit]

  def show(kilograms: Double, unit: WeightUnit): Double = unit match
    case Kilograms => kilograms
    case Pounds    => kilograms * PoundsPerKilogram

  def toKilograms(entered: Double, unit: WeightUnit): Double = unit match
    case Kilograms => entered
    case Pounds    => entered / PoundsPerKilogram

  def label(unit: WeightUnit): String = unit match
    case Kilograms => "kg"
    case Pounds    => "lb"

/** One thing the account counts: what it is called, what it is done on, and how its effort is measured.
  *
  * The equipment is separate from the name because it is what changes between two otherwise identical exercises, and
  * what the factor is really attached to: "stationary bicycle" costs a different amount of effort on two machines set
  * to two resistances, and the factor is where that lives.
  */
@jsonNoExtraFields
final case class ExerciseType(
    id: String,
    name: String,
    equipment: Option[String] = None,
    countsBy: CountsBy = CountsBy.RepCount,
    /** What a rep, or a unit of cadence, is worth for this exercise. */
    factor: Double = AccountSettings.InitialFactor
)

object ExerciseType:
  given JsonCodec[ExerciseType] = DeriveJsonCodec.gen[ExerciseType]

/** Everything an account has set, held on the account's own record and read back on every login.
  *
  * On the account rather than on the device: the weight and the exercises belong to the person, and a dashboard opened
  * on a tablet must show the same figures as the same account opened anywhere else.
  */
@jsonNoExtraFields
final case class AccountSettings(
    /** Always kilograms. See [[WeightUnit]]. */
    weightKilograms: Double = AccountSettings.InitialWeightKilograms,
    weightUnit: WeightUnit = WeightUnit.Kilograms,
    /** What a new exercise starts at until it is given its own. */
    defaultFactor: Double = AccountSettings.InitialFactor,
    exercises: Seq[ExerciseType] = Seq.empty,
    /** Which exercise the dashboard is reporting. `None` while the account has none, or while the one it named has
      * been deleted -- in which case the dashboard falls back to counting reps at the default factor rather than
      * showing nothing.
      */
    selected: Option[String] = None
):
  def selectedExercise: Option[ExerciseType] = selected.flatMap(id => exercises.find(_.id == id))

  /** How the chosen exercise is measured, or how an account that has chosen none is. */
  def countsBy: CountsBy = selectedExercise.fold(CountsBy.RepCount)(_.countsBy)

  def factor: Double = selectedExercise.fold(defaultFactor)(_.factor)

object AccountSettings:
  /** One, as asked: a factor of one means the formula reports its own raw product, and every exercise is calibrated
    * against that rather than against a number someone guessed at.
    */
  val InitialFactor = 1.0

  /** A starting weight rather than a claim about anyone. Nothing can be calculated from zero, and an account that has
    * not been through settings yet should still show a figure that moves.
    */
  val InitialWeightKilograms = 70.0

  val Path = "/account/settings"

  val Initial: AccountSettings = AccountSettings()

  given JsonCodec[AccountSettings] = DeriveJsonCodec.gen[AccountSettings]
