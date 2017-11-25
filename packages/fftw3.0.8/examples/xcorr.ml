open Bigarray
module FFT = Fftw3.D
module D = Lacaml.D
module Z = Lacaml.Z

let create kind n = FFT.Array1.create kind fortran_layout n

let log2 = log 2.

(** [minpow2 l] returns the smaller 2^n which is [>= l].  Works when
    [l <= 2^53]. *)
let minpow2 l = truncate(2.**(ceil(log(float l) /. log2)))

(** [copy0 x y] like the lacaml [copy] but in addtion put [0.] at the
    other places in [y]. *)
let copy0 ?n ?ofsx (x:D.vec) ?(ofsy=1) (y:D.vec) =
  let n = match n with None -> Array1.dim x | Some n -> n in
  for i = 1 to ofsy - 1 do y.{i} <- 0. done;
  ignore(D.copy ~n ?ofsx x ~ofsy ~y);
  for i = ofsy + n to Array1.dim y do y.{i} <- 0. done

type scale = Biased | Unbiased | Coeff

let xcorr ?maxlag ?scale (a:D.vec) (b:D.vec) =
  let dima = Array1.dim a
  and dimb = Array1.dim b in
  if dima < 1 || dimb < 1 then invalid_arg "xcorr: array size < 1";
  let m = max dima dimb in
  let maxlag = match maxlag with Some i -> i | None -> m - 1 in
  let ilag0 = maxlag + 1 in             (* index of 0-lag in the final vec. *)
  let dim_xcorr = 2 * maxlag + 1 in     (* dim of final vector *)
  (* We will allocate a vector of size [2*m-1] (to contain all lags)
     or [dim_xcorr] (the requested lags), whichever is greater. *)
  let idx0 = max m (maxlag + 1) in      (* 0-lag for dim_correlation vec *)
  let dim_correlation = 2 * idx0 - 1 in
  (* Fill the final extra lags with 0. (only needed if m < maxlag+1): *)
  let fill0 xcorr =
    for i = 1 to ilag0 - m do xcorr.{i} <- 0. done;
    for i = ilag0 + m to dim_xcorr do xcorr.{i} <- 0. done;
    xcorr in
  let xcorr =
    if a == b then begin
      (* Auto-correlation *)
      let n = minpow2 (2 * m - 1) in    (* just enough to store all lags *)
      let x = create FFT.float n
      and y = create FFT.complex (n/2+1) in
      let fft = FFT.Array1.r2c x y
      and ifft = FFT.Array1.c2r y x in
      copy0 a x;
      FFT.exec fft;
      for i = 1 to Array1.dim y do
        y.{i} <- { Complex.re=Complex.norm2 y.{i}; im=0.}
      done;
      FFT.exec ifft;
      D.scal (1. /. float n) x;                  (* normalize ifft *)
      (* Keep only the lags we want and move negative lags before
         positive lags. *)
      let xcorr = create FFT.float dim_xcorr in
      let l = min m (maxlag + 1) in
      ignore(D.copy ~n:l x  ~ofsy:ilag0 ~y:xcorr);
      ignore(D.copy ~n:(l-1) ~ofsx:(n - l + 2) x  ~ofsy:(ilag0-l+1) ~y:xcorr);
      fill0 xcorr
    end
    else begin
      (* Cross correlation *)
      if dima < 10 * dimb && dimb < 10 * dima then (
        (* Direct method: ifft(fft(a) .* conj(fft(b))) *)
        let n = minpow2 dim_correlation in
        let n_complex = n / 2 + 1 in
        let x = create FFT.float n
        and a' = create FFT.complex n_complex
        and b' = create FFT.complex n_complex in
        let ffta = FFT.Array1.r2c x a'
        and fftb = FFT.Array1.r2c x b' in
        let ifft = FFT.Array1.c2r a' x in
        (* Use the fact that xcorr(T(m)a, b) = T(m) xcorr(a,b) (where
           T(m)x = x(_ - m) is the translation of x by m units.  This
           allows to put the zero lag at position [m] without
           postprocessing. *)
        copy0 a ~ofsy:idx0 x;  FFT.exec ffta;
        copy0 b x;  FFT.exec fftb;
        for i = 1 to n_complex do
          (* This might be slow; lacaml should support a * conj(b) *)
          a'.{i} <- Complex.mul a'.{i} (Complex.conj b'.{i});
        done;
        FFT.exec ifft;
        D.scal (1. /. float n) x;
        (* Keep only the lags we want (share the data with [x]) and
           fill the (possible) extra ones with [0.]. *)
        let xcorr = Array1.sub x (max 1 (idx0 - maxlag)) dim_xcorr in
        fill0 xcorr
      )
      else (
        (* Overlap-add method (adapted from convolution to correlation) *)
        let a, b, permuted =
          if Array1.dim a >= Array1.dim b then a,b,false else b,a,true in
        let dima = Array1.dim a and dimb = Array1.dim b in
        let l = dimb in (* FIXME: good choice for the [a] window length? *)
        let lcorr = l + dimb - 1 in (* length of segment correlation vec *)
        let n = minpow2 lcorr in
        let n_complex = n / 2 + 1 in
        let x = create FFT.float n
        and b' = create FFT.complex n_complex in
        let fftb = FFT.Array1.r2c x b' in
        copy0 b x;  FFT.exec fftb;
        for i = 1 to n_complex do
          b'.{i} <- Complex.conj b'.{i}; (* => b' = conj(fft(b,n)) *)
        done;
        let c = create FFT.float dim_correlation in (* final result *)
        Array1.fill c 0.;
        (* Since xcorr(b,a)(i) = conj(xcorr(a,b)(-i)) and [a], [b] are
           real, one just need to add each segment correlation in
           reverse in case [a] and [b] have been permuted. *)
        let inc_seg = if permuted then -1 else 1 in
        (* Having to flip the result left-right also influences the
           index of the 1st correlation segment [idx_seg] in [c] as
           well as the direction of the steps in [c]. *)
        let idx_seg = if permuted then idx0 -l + 2 else idx0 - dimb in
        let shift_seg =
          if permuted then (fun i -> idx_seg - i) else (fun i -> idx_seg + i) in
        let y = create FFT.complex n_complex in (* Fourier variables *)
        let fft_xy = FFT.Array1.r2c x y
        and fft_yx = FFT.Array1.c2r y x in
        let i = ref 1 in
        while !i <= dima do
          let l' = min l (dima - !i + 1) in
          copy0 ~n:l' ~ofsx:!i a ~ofsy:dimb x;
          FFT.exec fft_xy;
          ignore(Z.Vec.mul y b' ~z:y);
          FFT.exec fft_yx;
          D.axpy x c ~ofsy:(shift_seg !i) ~incy:inc_seg ~n:(l' + dimb - 1);
          i := !i + l;
        done;
        D.scal (1. /. float n) c;       (* ifft normalization *)
        let xcorr = Array1.sub c (max 1 (idx0 - maxlag)) dim_xcorr in
        fill0 xcorr
      )
    end in
  (* Scale correlation as specified *)
  (match scale with
   | None -> ()
   | Some Biased ->
       D.scal (1. /. float m) xcorr
   | Some Unbiased ->
       let l = min (m-1) maxlag in
       for i = ilag0 - l to ilag0 + l do
         xcorr.{i} <- xcorr.{i} /. float(m - abs(ilag0 - i))
       done
   | Some Coeff ->
       if a == b then
         (* Auto-correlation, normalize by c(0) = nrm2(a)^2 *)
         D.scal (1. /. xcorr.{ilag0}) xcorr
       else
         (* Cross correlation *)
         D.scal (1. /. (D.nrm2 a *. D.nrm2 b)) xcorr
  );
  xcorr

