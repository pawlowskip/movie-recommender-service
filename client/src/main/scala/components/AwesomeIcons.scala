package components

import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._

/**
 * Created by pp on 4/4/16.
 */
object AwesomeIcons {

  case class Icon(name: String,
                  size: Icon.Size = Icon.Size.NORMAL,
                  animation: Icon.Animation = Icon.Animation.NONE,
                  rotation: Icon.Rotation = Icon.Rotation.NONE) {

    def htmlClass = s"fa fa-$name ${size.name} ${animation.name} ${rotation.name}"
  }

  object Icon {
    sealed abstract class Size(val name: String)

    object Size {
      case object NORMAL extends Size("")
      case object LARGE extends Size("fa-lg")
      case object X2 extends Size("fa-2x")
      case object X3 extends Size("fa-3x")
      case object X4 extends Size("fa-4x")
      case object X5 extends Size("fa-5x")
    }

    sealed abstract class Animation(val name: String)
    object Animation {
      case object NONE extends Animation("")
      case object SPIN extends Animation("fa-spin")
      case object PULSE extends Animation("fa-pulse")
    }

    sealed abstract class Rotation(val deg: Int, val name: String)
    object Rotation {
      case object NONE extends Rotation(0, "")
      case object DEG_90 extends Rotation(90, "fa-rotate-90")
      case object DEG_180 extends Rotation(180, "fa-rotate-180")
      case object DEG_270 extends Rotation(270, "fa-rotate-270")
    }

  }

  case class Props(icon: Icon)

  val component =
    ReactComponentB[Props]("awesome-icon")
      .renderP((_, props) =>
        <.i(^.`class` := props.icon.htmlClass)
      ).build

  def apply(props: Props) = component(props)

  object Type {
    val film = "film"
    val thumbsUp = "thumbs-up"
    val star = "star"
    val user = "user"
  }

}
