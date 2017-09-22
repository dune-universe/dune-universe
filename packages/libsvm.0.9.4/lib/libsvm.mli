(*
   LIBSVM-OCaml - OCaml-bindings to the LIBSVM library

   Copyright (C) 2013-  Oliver Gu
   email: gu.oliver@yahoo.com

   Copyright (C) 2005  Dominik Brugger
   email: dominikbrugger@fastmail.fm
   WWW:   http://ocaml-libsvm.berlios.de

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(** Interface to Libsvm *)

open Lacaml.D

module Svm : sig

  (** {2 SVM problem and model} *)

  module Problem : sig
    type t (** Type of a SVM problem (training set). *)

    (** [create x y] constructs a problem from a feature matrix [x] and target
        vector [y]. Each row of [x] is a feature vector of a training
        instance. *)
    val create : x:mat -> y:vec -> t

    (** [create_k k y] constructs a problem from a matrix [k] and target vector
        [y]. The matrix [k] has to be of the following form:

        1 K(x1,x1) K(x1,x2) ... K(x1,xL)

        2 K(x2,x1) K(x2,x2) ... K(x2,xL)

        ...

        L K(xL,x1) K(xL,x2) ... K(xL,xL)

        where L denotes the number of training instances and K(x,y) is the
        precomputed kernel value of the two training instances x and y. *)
    val create_k : k:mat -> y:vec -> t

    (** [get_n_samples prob] @return the number of training samples. *)
    val get_n_samples : t -> int

    (** [get_n_feats prob] @return the number of features (attributes). *)
    val get_n_feats : t -> int

    (** [get_targets prob] @return the targets of training instances. *)
    val get_targets : t -> vec

    (** [load filename] loads a problem from the file [filename].
        @raise Failure if an error occured during parsing of [filename]. *)
    val load : string -> t

    (** [output prob oc] outputs the problem [prob] to an output channel [oc].
        NOTE: the function does not close the output channel. *)
    val output : t -> out_channel -> unit

    (** [save prob filename] saves the problem [prob] to the file [filename]. *)
    val save : t -> string -> unit

    (** [min_max_feats prob] @return the smallest and largest feature value for
        each column in the feature matrix. *)
    val min_max_feats : t -> [ `Min of vec ] * [ `Max of vec ]

    (** [scale ?lower ?upper prob min_feats max_feats] @return a linearly
        scaled problem where each feature (attribute) lies in the range
        \[[lower],[upper]\]. The default range is \[-1,1\]. *)
    val scale :
      ?lower:float -> ?upper:float
      -> t
      -> min_feats:vec -> max_feats:vec
      -> t

    (** [print prob] prints the internal representation of a problem.
        It is mainly used for debugging purposes. *)
    val print : t -> unit
  end

  module Model : sig
    type t (** Type of a SVM model. *)

    (** [get_svm_type model] @return the svm type that was used to train the
        given [model]. *)
    val get_svm_type : t -> [ `C_SVC | `NU_SVC | `ONE_CLASS | `EPSILON_SVR | `NU_SVR ]

    (** [get_n_classes model] @return the number of classes for a
        classification model or 2 for a regression or an one-class model. *)
    val get_n_classes : t -> int

    (** [get_labels model] @return the labels of a two or multi-class
        classification problem in a list.
        @raise Invalid_argument in the case of a regression or one-class model. *)
    val get_labels : t -> int list

    (** [get_n_sv model] @return the total number of support vectors. *)
    val get_n_sv : t -> int

    (** [get_svr_probability model] @return a positive value for a regression
        model with probability information. In the case of no probability
        information, 0 is returned.
        @raise Invalid_argument for non-regression models. *)
    val get_svr_probability : t -> float

    (** [save model filename] saves a model to the file [filename]. *)
    val save : t -> string -> unit

    (** [load filename] loads a model from the file [filename].
        @raise Failure if model could not be loaded. *)
    val load : string -> t
  end

  (** {2 SVM training} *)

  (** [train params problem] trains a SVM model on the given [problem] and
      parameters [params]:
      - [svm_type] - type of SVM classification/regression (default [C_SVC])
      - [kernel] - type of the SVM kernel (default [RBF])
      - [degree] - the exponent in the [POLY] kernel (default 3)
      - [gamma] - parameter for [POLY], [RBF] and [SIGMOID] kernel (default 0)
      - [coef0] - parameter for [POLY] and [SIGMOID] kernel (default 0)
      - [c] - the cost of constraints violation in [C_SVC], [EPSILON_SVR], and
      [NU_SVR] (default 1)
      - [nu] - the parameter in [NU_SVM], [NU_SVR] and [ONE_CLASS] (default 0.5)
      - [eps] - the epsilon in the epsilon-sensitive loss function of
      [EPSILON_SVR] (default 0.1)
      - [cachesize] - the size of the kernel cache in megabytes (default 100)
      - [tol] - the stopping criterion (default 1e-3)
      - [shrinking] - use [on] to conduct shrinking, otherwise [off] (default [on])
      - [probability] - if probability = true, then a model with probability
      information will be obtained (default false)
      - [weights] - weights to penalize classes (default = [])
      - [verbose] - if verbose = true, then train the SVM in verbose mode
      (default false)
      @return the trained model.
      @raise Failure if parameters are not feasible. *)
  val train :
    ?svm_type:[ `C_SVC | `NU_SVC | `ONE_CLASS | `EPSILON_SVR | `NU_SVR ]
    -> ?kernel:[
      | `LINEAR       (* u'*v *)
      | `POLY         (* (gamma*u'*v + coef0)^degree *)
      | `RBF          (* exp(-gamma*|u-v|^2) *)
      | `SIGMOID      (* tanh(gamma*u'*v + coef0) *)
      | `PRECOMPUTED  (* kernel values are stored in a file *)
    ]
    -> ?degree:int
    -> ?gamma:float
    -> ?coef0:float
    -> ?c:float
    -> ?nu:float
    -> ?eps:float
    -> ?cachesize:float
    -> ?tol:float
    -> ?shrinking:[ `on | `off ]
    -> ?probability:bool
    -> ?weights: (int * float) list
    -> ?verbose:bool
    -> Problem.t
    -> Model.t

  (** [cross_validation params problem n_folds] conducts n-fold
      cross-validation on the given [problem] and parameters [params].
      The parameters [params] are the same as in [train] above.
      @return vector with all predicted values (of all problem instances) in
      the validation process.
      @raise Failure if parameters are not feasible. *)
  val cross_validation :
    ?svm_type:[ `C_SVC | `NU_SVC | `ONE_CLASS | `EPSILON_SVR | `NU_SVR ]
    -> ?kernel:[
      | `LINEAR       (* u'*v *)
      | `POLY         (* (gamma*u'*v + coef0)^degree *)
      | `RBF          (* exp(-gamma*|u-v|^2) *)
      | `SIGMOID      (* tanh(gamma*u'*v + coef0) *)
      | `PRECOMPUTED  (* kernel values are stored in a file *)
    ]
    -> ?degree:int
    -> ?gamma:float
    -> ?coef0:float
    -> ?c:float
    -> ?nu:float
    -> ?eps:float
    -> ?cachesize:float
    -> ?tol:float
    -> ?shrinking:[ `on | `off ]
    -> ?probability:bool
    -> ?weights: (int * float) list
    -> ?verbose:bool
    -> n_folds:int
    -> Problem.t
    -> vec

  (** {2 SVM prediction} *)

  (** [predict_one model x] does classification or regression on a test vector
      [x] given a [model].
      For a classification model, the predicted class for [x] is returned.
      For a regression model, the function value of [x] is returned.
      For a one-class model, +1 or -1 is returned. *)
  val predict_one : Model.t -> x:vec -> float

  (** [predict model x] applies predict_one to each row of the matrix [x]. *)
  val predict : Model.t -> x:mat -> vec

  (** [predict_values model x] @return a matrix with decision values on a test
      vector [x]. *)
  val predict_values : Model.t -> x:vec -> float array array

  (** [predict_probability m x] does classification or regression on a test
      vector [x] based on a [model] with probability information.
      @raise Invalid_argument if the model does not support probability
      estimates. *)
  val predict_probability : Model.t -> x:vec -> float * float array

  (** [predict_from_file model filename] does classification or regression
      on the testing data given in [filename].
      @return a pair vectors containing the expected (true) values form the
      test file and the predicted ones computed from the given [model].
      @raise Failure if an error occured during parsing of [filename]. *)
  val predict_from_file :
    Model.t
    -> string
    -> ([ `Expected of vec ] * [ `Predicted of vec ])
end

module Stats : sig
  (** These functions compute several performance measures to compare the
      [predicted] values of a SVM model to the [expected] values of a
      given test data set. For more details, have a look on page 8 in the
      {{:http://www.csie.ntu.edu.tw/~cjlin/papers/libsvm.pdf } LIBSVM paper} *)

  (** [calc_n_correct expected predicted] @return the number of correctly
      predicted labels.
      @raise Invalid_argument if the vector dimensions do not match. *)
  val calc_n_correct : vec -> vec -> int

  (** [calc_accuracy expected predicted] @return the ratio (in percent) of
      correctly predicted labels to the number of all labels.
      @raise Invalid_argument if the vector dimensions do not match. *)
  val calc_accuracy : vec -> vec -> float

  (** [calc_mse expected predicted] @return the mean sum of squared errors.
      @raise Invalid_argument if the vector dimensions do not match. *)
  val calc_mse : vec -> vec -> float

  (** [calc_scc expected predicted] @return the squared correlation coefficient.
      @raise Invalid_argument if the vector dimensions do not match. *)
  val calc_scc : vec -> vec -> float
end
