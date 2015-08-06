/*
 * Copyright 2014 Renaud Bruneliere
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

package com.github.bruneli.scalaopt.stdapps.learning.data

/**
 * @author bruneli
 */
object Mtcars {

  // Table structure:
  // "name","mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb"
  lazy val mtcars = List(("Mazda RX4", Vector(21, 6, 160, 110, 3.9, 2.62, 16.46, 0, 1, 4, 4)),
    ("Mazda RX4 Wag", Vector(21, 6, 160, 110, 3.9, 2.875, 17.02, 0, 1, 4, 4)),
    ("Datsun 710", Vector(22.8, 4, 108, 93, 3.85, 2.32, 18.61, 1, 1, 4, 1)),
    ("Hornet 4 Drive", Vector(21.4, 6, 258, 110, 3.08, 3.215, 19.44, 1, 0, 3, 1)),
    ("Hornet Sportabout", Vector(18.7, 8, 360, 175, 3.15, 3.44, 17.02, 0, 0, 3, 2)),
    ("Valiant", Vector(18.1, 6, 225, 105, 2.76, 3.46, 20.22, 1, 0, 3, 1)),
    ("Duster 360", Vector(14.3, 8, 360, 245, 3.21, 3.57, 15.84, 0, 0, 3, 4)),
    ("Merc 240D", Vector(24.4, 4, 146.7, 62, 3.69, 3.19, 20, 1, 0, 4, 2)),
    ("Merc 230", Vector(22.8, 4, 140.8, 95, 3.92, 3.15, 22.9, 1, 0, 4, 2)),
    ("Merc 280", Vector(19.2, 6, 167.6, 123, 3.92, 3.44, 18.3, 1, 0, 4, 4)),
    ("Merc 280C", Vector(17.8, 6, 167.6, 123, 3.92, 3.44, 18.9, 1, 0, 4, 4)),
    ("Merc 450SE", Vector(16.4, 8, 275.8, 180, 3.07, 4.07, 17.4, 0, 0, 3, 3)),
    ("Merc 450SL", Vector(17.3, 8, 275.8, 180, 3.07, 3.73, 17.6, 0, 0, 3, 3)),
    ("Merc 450SLC", Vector(15.2, 8, 275.8, 180, 3.07, 3.78, 18, 0, 0, 3, 3)),
    ("Cadillac Fleetwood", Vector(10.4, 8, 472, 205, 2.93, 5.25, 17.98, 0, 0, 3, 4)),
    ("Lincoln Continental", Vector(10.4, 8, 460, 215, 3, 5.424, 17.82, 0, 0, 3, 4)),
    ("Chrysler Imperial", Vector(14.7, 8, 440, 230, 3.23, 5.345, 17.42, 0, 0, 3, 4)),
    ("Fiat 128", Vector(32.4, 4, 78.7, 66, 4.08, 2.2, 19.47, 1, 1, 4, 1)),
    ("Honda Civic", Vector(30.4, 4, 75.7, 52, 4.93, 1.615, 18.52, 1, 1, 4, 2)),
    ("Toyota Corolla", Vector(33.9, 4, 71.1, 65, 4.22, 1.835, 19.9, 1, 1, 4, 1)),
    ("Toyota Corona", Vector(21.5, 4, 120.1, 97, 3.7, 2.465, 20.01, 1, 0, 3, 1)),
    ("Dodge Challenger", Vector(15.5, 8, 318, 150, 2.76, 3.52, 16.87, 0, 0, 3, 2)),
    ("AMC Javelin", Vector(15.2, 8, 304, 150, 3.15, 3.435, 17.3, 0, 0, 3, 2)),
    ("Camaro Z28", Vector(13.3, 8, 350, 245, 3.73, 3.84, 15.41, 0, 0, 3, 4)),
    ("Pontiac Firebird", Vector(19.2, 8, 400, 175, 3.08, 3.845, 17.05, 0, 0, 3, 2)),
    ("Fiat X1-9", Vector(27.3, 4, 79, 66, 4.08, 1.935, 18.9, 1, 1, 4, 1)),
    ("Porsche 914-2", Vector(26, 4, 120.3, 91, 4.43, 2.14, 16.7, 0, 1, 5, 2)),
    ("Lotus Europa", Vector(30.4, 4, 95.1, 113, 3.77, 1.513, 16.9, 1, 1, 5, 2)),
    ("Ford Pantera L", Vector(15.8, 8, 351, 264, 4.22, 3.17, 14.5, 0, 1, 5, 4)),
    ("Ferrari Dino", Vector(19.7, 6, 145, 175, 3.62, 2.77, 15.5, 0, 1, 5, 6)),
    ("Maserati Bora", Vector(15, 8, 301, 335, 3.54, 3.57, 14.6, 0, 1, 5, 8)),
    ("Volvo 142E", Vector(21.4, 4, 121, 109, 4.11, 2.78, 18.6, 1, 1, 4, 2)))

  val fieldIndices = Map("mpg" -> 0, "cyl" -> 1, "disp" -> 2, "hp" -> 3, "drat" -> 4, "wt" -> 5, "qsec" -> 6, "vs" -> 7, "am" -> 8, "gear" -> 9, "carb" -> 10)

}
