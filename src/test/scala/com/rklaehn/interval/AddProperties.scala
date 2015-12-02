package com.rklaehn.interval

import org.scalacheck.Properties
import org.typelevel.discipline.Laws

trait AddProperties { self: Properties =>

  def addProperties(name: String, ruleSet: Laws#RuleSet) {
    for ((id, prop) ← ruleSet.all.properties)
      property(name + "." + id) = prop
  }
}
