pscl.js and the ProScript API
=============================

Using pscl.js
-------------

We do *not* use pscl.js in dirsp-exchange! The ProScript Cryptography Library (PSCL) is used as an API during ProVerif proof
testing, and we use it as an API
(see the OCaml library ``dirsp-proscript``). There is a ``dirsp-proscript-mirage`` that uses the Mirage crypto libraries,
which themselves use a correct-by-construction implementation from `fiat-crypto <https://github.com/mit-plv/fiat-crypto>`_
for its elliptic curve functions.

We decided to abandon pscl.js after trying to compare its output with ours. Some cautionary notes:

1. Comparing dirsp-exchange's output to sp.js + pscl.js is a bit of a fool's errand. After burning two days trying
   to match results, I realized that pscl.js' DH25519 was non-standard. In fact, pscl.js is not used for the security
   proofs (the ProVerif translation will replace references to PSCL functions like DH25519). See comment about DH25519 in
   https://github.com/Inria-Prosecco/proscript-messaging/issues/1 ; I had likewise ran test vectors through pscl.js
   and its DH25519 failed.
2. It was difficult to understand which parameters to pscl.js need to be hex-encoded and which ones don't. And it **really**
   matters in JavaScript. In node.js, the ``new Buffer(some_variable, 'hex')`` construction used frequently in pscl.js
   will silently return an empty buffer if the input is not hex. For example, supplying values without hex encoding
   to ``ProScript.crypto.ED25519.signature`` would result in a call to ``ProScript.crypto.ED25519Hash(sk)``,
   which in turn would return a SHA512 hash of an empty value because of the Buffer-based hex decoding. Yet the ProVerif code
   does not hex encode that ``sk`` parameter when calling the ``ProScript.crypto.ED25519.signature`` API.
3. Completely as a reaction to hex-encoding ambiguities in pscl.js, nothing is expected to be hex encoded in
   ``dirsp-proscript``. The ``dirsp-proscript`` API is just raw bytes. However, the hex encoders ``toBitstring`` and
   ``fromBitstring`` used in the higher-level KBB2017 algorithm (``ps/sp.js``) are still present.

So there is no need for pscl.js, and you won't get matching results because the kinda critical Diffie-Hellman
implementation in pscl.js is non-standard and perhaps broken.

*Sidebar: None of the above caution affects the KBB2017 proof*

Now that the cautionary preamble is out of the way ... here is how you would run sp.js + pscl.js.

You'll need node.js v10+ installed.

Then:

.. code-block:: bash

    npm install --save-dev window hexy
    npm install --save-dev @peculiar/webcrypto # only needed if node.js v14 or earlier
    node --experimental-repl-await # or 'nvm exec 16.0 node --experimental-repl-await' depending on your installation

Within ``node``:

.. code-block:: javascript

    // EITHER: this section is for node.js v10-v14
    const getRandomValues = await require('get-random-values')

    // OR: this section is for node.js v15+ which has built-in (more vetted) webcrypto support
    const { getRandomValues } = await require('crypto').webcrypto

    // THEN continue with the following ...

    // Make an approximation of the browser
    const Window = await require('window')
    const { createHash, createHmac, createCipheriv, createDecipheriv } = await require('crypto');
    const window = new Window()
    window.crypto = { getRandomValues }
    const NodeCrypto = { createHash, createHmac, createCipheriv, createDecipheriv }

    // Load the Javascript files
    const fs = await require("fs")
    const pscl_js = fs.readFileSync('./src-proscript/proscript-messaging/pscl/pscl.js', 'utf8')
    eval(pscl_js)
    const sp_js = fs.readFileSync('./src-proscript/proscript-messaging//ps/sp.js', 'utf8')
    eval(sp_js.replaceAll(/\bconst /g,"var "))

    // Now you have access to the ProScript Cryptographic Library (pscl.js)
    // and the 'Signal Protocol'/KBB2017 (sp.js)
    ProScript.crypto.random16Bytes()
    Type_key.construct()
    const { hexy } = await require('hexy')
    hexy(Type_key.construct())

    // You can also override the random functions so you can do a
    // repeatable comparison between JavaScript and OCaml
    const firstByteMd5 = function(s) { return NodeCrypto.createHash('md5').update(s).digest()[0] }
    ProScript.crypto.random12Bytes = function(id) { const fb = firstByteMd5(id); return Array.from({length: 12}, () => fb) }
    ProScript.crypto.random16Bytes = function(id) { const fb = firstByteMd5(id); return Array.from({length: 16}, () => fb) }
    ProScript.crypto.random32Bytes = function(id) { const fb = firstByteMd5(id); return Array.from({length: 32}, () => fb) }

You can simulate a conversation between Alice and Bob:

.. code-block:: javascript

    const P   = ProScript
    const C   = P.crypto
    const E   = P.crypto.ED25519
    const U   = UTIL
    const T   = TOPLEVEL
    const KEY = Type_key
    const MSG = Type_msg

    let aliceIdentityKey  = U.newIdentityKey ("alice-identity")
    let aliceSignedPreKey = U.newKeyPair     ("alice-signed-prekey")
    let bobIdentityKey    = U.newIdentityKey ("bob-identity")
    let bobSignedPreKey   = U.newKeyPair     ("bob-signed-prekey")

    let aliceIdentityKeyPub        = aliceIdentityKey.pub
    let aliceIdentityDHKeyPub      = U.getDHPublicKey (aliceIdentityKey.priv)
    let aliceSignedPreKeyPub       = aliceSignedPreKey.pub
    let aliceSignedPreKeySignature = E.signature (KEY.toBitstring(aliceSignedPreKeyPub), aliceIdentityKey.priv, aliceIdentityKeyPub)
    let bobIdentityKeyPub          = bobIdentityKey.pub
    let bobIdentityDHKeyPub        = U.getDHPublicKey(bobIdentityKey.priv)
    let bobSignedPreKeyPub         = bobSignedPreKey.pub
    let bobSignedPreKeySignature   = E.signature (KEY.toBitstring(bobSignedPreKeyPub  ), bobIdentityKey.priv,   bobIdentityKeyPub)

    let alicePreKey   = U.newKeyPair     ("alice-prekey")
    let bobPreKey     = U.newKeyPair     ("bob-prekey")

    let alicePreKeyPub = KEY.toBitstring (alicePreKey.pub)
    let alicePreKeyId  = 1 /* the Id of alicePreKey */
    let bobPreKeyPub   = KEY.toBitstring (bobPreKey.pub)
    let bobPreKeyId    = 1 /* the Id of bobPreKey   */

    let aliceSessionWithBob = T.newSession(
        aliceSignedPreKey,
        alicePreKey,
        KEY.toBitstring (bobIdentityKeyPub),
        KEY.toBitstring (bobIdentityDHKeyPub),
        KEY.toBitstring (bobSignedPreKeyPub),
        bobSignedPreKeySignature,
        KEY.toBitstring (bobPreKeyPub),
        bobPreKeyId
        )
    let bobSessionWithAlice = T.newSession(
        bobSignedPreKey,
        bobPreKey,
        KEY.toBitstring (aliceIdentityKeyPub),
        KEY.toBitstring (aliceIdentityDHKeyPub),
        KEY.toBitstring (aliceSignedPreKeyPub),
        aliceSignedPreKeySignature,
        KEY.toBitstring (alicePreKeyPub),
        alicePreKeyId
        )

    let aliceToBobMsg1        = "Hi Bob!"
    let aliceToBobSendOutput1 = T.send(
        aliceIdentityKey,
        aliceSessionWithBob,
        aliceToBobMsg1
    )
    console.log(util.format("Did Alice send successfully? %s\n", aliceToBobSendOutput1.output.valid))

    let bobFromAliceMsg2           = aliceToBobSendOutput1.output
    let bobFromAliceReceiveOutput2 = T.recv(
      bobIdentityKey,
      bobSignedPreKey,
      bobSessionWithAlice,
      bobFromAliceMsg2,
    )
    console.log(util.format("Did Bob receive successfully? %s\n", bobFromAliceReceiveOutput2.output.valid))
    /* this should work, but doesn't */

Suggested Improvements
----------------------

We don't control the development of ProScript. So here are the feature requests we would send to the ProScript team:

1. If ``pscl.js`` is going to be used by anyone, it needs to do a length check on the result of any Buffer conversion. In fact,
   node.js complains that "Buffer() is deprecated due to security and usability issues", so removing the code would be better.
2. Let's statically type or annotate which parameters are hex-encoded. It is _way_ too easy to make a mistake where you pass
   in a hex encoded input to a parameter that doesn't need hex encoding, or you hex encode something twice.

