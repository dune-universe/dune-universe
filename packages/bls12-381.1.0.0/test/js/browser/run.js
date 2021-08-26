import('@dannywillems/rustc-bls12-381').then((m) => {
  window._RUSTC_BLS12_381 = m;
  import('./test.js').then((m) => {
    m.run();
  }).catch((e) => {
    console.log('Error while importing rustc-bls12-381 dep: ' + e);
  });
});
