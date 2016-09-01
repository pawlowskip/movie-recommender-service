package model

/**
  * Created by pp on 8/27/16.
  */

object User {
  object Role {
    val normal = "Normal"
    val admin = "Admin"
  }
}

case class User(id: Long, login: String, password: String, role: String)
