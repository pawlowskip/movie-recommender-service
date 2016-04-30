package components

import japgolly.scalajs.react.ReactComponentB
import japgolly.scalajs.react.vdom.prefix_<^._

/**
 * Created by pp on 4/4/16.
 */
object AwesomeIcons {

  case class Icon(typ: Type,
                  size: Icon.Size = Icon.Size.NORMAL,
                  animation: Icon.Animation = Icon.Animation.NONE,
                  rotation: Icon.Rotation = Icon.Rotation.NONE) {

    def htmlClass = s"fa fa-${typ.name} ${size.name} ${animation.name} ${rotation.name}"
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

    val film = Icon(Type.film)
    val thumbsUp = Icon(Type.thumbsUp)
    val star = Icon(Type.star)
    val user = Icon(Type.user)
    val search = Icon(Type.search)
    val wrench = Icon(Type.wrench)
    val star_o = Icon(Type.star_o)
  }

  case class Props(icon: Icon)

  val component =
    ReactComponentB[Props]("awesome-icon")
      .renderP((_, props) =>
        <.i(^.`class` := props.icon.htmlClass)
      ).build

  def apply(icon: Icon) = component(Props(icon))

  class Type(val name: String) extends AnyVal

  object Type {
    def apply(name: String) = new Type(name)

    val film = apply("film")
    val thumbsUp = apply("thumbs-up")
    val star = apply("star")
    val user = apply("user")
    val search = apply("search")
    val wrench = apply("wrench")
    val star_o = apply("star-o")
  }

}
