Translating ProScript to OCaml
==============================

**Why build ProScript into OCaml?** 

tldr: ProScript can run in production *and* be formally verified with proof tools. But ProScript is JavaScript, and in production you may be using more than JavaScript.

Our original motivation came from reading [KBB2017]_ "Automated Verification for Secure Messaging Protocols
and Their Implementations: A Symbolic and Computational Approach" paper produced by the Prosecco research group.
We call the paper's algorithm the "Kobeissi, Bhargavan and Blanchet" (KBB2017) algorithm here and elsewhere
to both recognize the paper's authors and to avoid the use of the word "Signal" (which the Signal non-profit who
created the original algorithm has `protected with trademarks <https://github.com/libresignal/libresignal/issues/37#issuecomment-216975737>`_).
The KBB2017 algorithm describes how to integrate security algorithms like authenticated 1-on-1
conversation management with OCaml-based proof verification tools (CryptoVerif and ProVerif) and a
runtime deployment target of Javascript. They used the subset of Javascript called "ProScript" to automatically
generated models for ProVerify and CryptoVerif. In a real sense the OCaml basis was hidden.

You can use OCaml for much more than handling 1-on-1 conversations. So if you need multiple security
or privacy algorithms, writing those in a mixture of ProScript and/or OCaml and *delaying the conversion into your production languages*
(ex. OCaml can easily be converted into Javascript or integrated with C programs) make it relatively easy to verify proofs and
guarantee type safety between the algorithms. Said another way, why shoot yourself in the foot by not letting OCaml tooling
check all of your security and privacy data models?

There is `OCaml documentation <https://diskuv.github.io/dirsp-exchange/ocaml/dirsp-exchange-kbb2017/Dirsp_exchange_kbb2017/index.html>`_
that describes how to use the ProScript-generated OCaml KBB2017 libraries. But in this documentation, we describe the
*machine translation* of KBB2017 ProScript into OCaml, with some general pointers if you are using your own ProScript algorithm.
So **you can** use the structure of this code for integrating ProScript into your own OCaml libraries. Read on.

.. toctree::
    :maxdepth: 2
    :caption: Contents:
 
    PS2OCAML_GUIDE
    PS2OCAML_FAQS
  
.. [KBB2017]  Nadim Kobeissi, Karthikeyan Bhargavan, Bruno Blanchet.
    Automated Verification for Secure Messaging Protocols
    and Their Implementations: A Symbolic and Computational Approach.
    2nd IEEE European Symposium on Security and Privacy , Apr 2017, Paris, France.
    pp.435 - 450, 2017, <https://www.ieee-security.org/TC/EuroSP2017/>.
    <10.1109/EuroSP.2017.38>. <hal-01575923>
