(** Connection to bluetooth and USB devices, Lwt version. *)

#ifdef HAS_LWT
#define LWT_t Lwt.t
#include "mindstorm_connect.mli"
#endif
