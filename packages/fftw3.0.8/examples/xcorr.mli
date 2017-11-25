(* File: xcorr.mli

   Copyright (C) 2008-

     Christophe Troestler <Christophe.Troestler@umons.ac.be>
     WWW: https://math.umons.ac.be/anum/software/

   This library is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License version 2.1 or
   later as published by the Free Software Foundation, with the special
   exception on linking described in the file LICENSE.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details. *)

open Lacaml.D

(** Cross-correlation function estimates.  This example is based on
    the Matlab® xcorr.m function and tries to respect its behavior.
*)

type scale = Biased | Unbiased | Coeff
    (** Types of scaling. *)

val xcorr : ?maxlag:int -> ?scale:scale -> vec -> vec -> vec
(** [xcorr a b], where [a] and [b] are two vectors, returns
    cross-correlation sequence of length [2*m - 1] where [m = max
    (dim a) (dim b)].  If [a == b] (e.g. if you use [xcorr(a,a)]),
    then the result is the auto-correlation sequence.  The zeroth lag
    of the output correlation is in the middle of the sequence, at
    element [maxlag+1] ([= m] by default).

    @raise Invalid_argument if [a] and [b] are of different length.

    [xcorr] produces an estimate of the correlation between two random
    (jointly stationary) sequences:
    {v
    C(m) = E[A(n+m)*conj(B(n))] = E[A(n)*conj(B(n-m))]
    v}
    It is also the deterministic correlation between two deterministic
    signals.

    @param maxlag computes the (auto/cross) correlation over the range
    of lags: [-maxlag] to [maxlag], i.e., the vector returned by
    [xcorr a b] will be of length [2 * maxlag + 1] lags.  If missing,
    default is [maxlag = m-1].

    @param scale normalizes the correlation according to the value
    - [Biased]: scales the raw cross-correlation by 1/M.
    - [Unbiased]: scales the raw correlation by 1/(M-abs(lags)).
    - [Coeff]: normalizes the sequence so that the auto-correlations
    at zero lag are identically 1.0.

    The default is [?scale=None], i.e. no scaling.
*)
