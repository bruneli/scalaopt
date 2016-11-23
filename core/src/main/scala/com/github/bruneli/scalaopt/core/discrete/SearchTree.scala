/*
 * Copyright 2016 Renaud Bruneliere
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.github.bruneli.scalaopt.core.discrete

/**
 * Binary search tree used by the branch and cut algorithm
 *
 * @author bruneli
 */
sealed trait SearchTree[+A] {

  def value: Option[A] = this match {
    case Node(l, v, r) => Some(v)
    case Leaf(v) => Some(v)
    case EmptyTree => None
  }

  def left: Option[SearchTree[A]] = this match {
    case Node(l, v, r) => Some(l)
    case _ => None
  }

  def right: Option[SearchTree[A]] = this match {
    case Node(l, v, r) => Some(r)
    case _ => None
  }

}

object EmptyTree extends SearchTree[Nothing]

case class Leaf[+A](v: A) extends SearchTree[A]

case class Node[+A](l: SearchTree[A], v: A, r: SearchTree[A]) extends SearchTree[A]
