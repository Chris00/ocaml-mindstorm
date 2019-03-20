(* File: mindstorm.mli

   Copyright (C) 2007-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: http://math.umons.ac.be/anum/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)


(** Drive Lego Minsdstorm bricks with OCaml. *)

(** Mindstorm is a library that enables you to drive Lego
    mindstorm NXT or EV3 bricks from OCaml (the computer is the master
    and the brick is the slave). Communication with the brick is done
    through Bluetooth and USB.

    @author Christophe Troestler <Christophe.Troestler@umons.ac.be>
*)

(** [Mindstorm.NXT] — Interface to NXT bricks. *)
#if OCAML_MAJOR >= 4 && OCAML_MINOR >= 2
module NXT = Mindstorm__NXT
#else
module NXT : module type of Mindstorm__NXT
#endif

(** (ALPHA VERSION)
    [Mindstorm.EV3] — Interface to EV3 bricks. *)
#if OCAML_MAJOR >= 4 && OCAML_MINOR >= 2
module EV3 = Mindstorm__EV3
#else
module EV3 : module type of Mindstorm__EV3
#endif

;;
