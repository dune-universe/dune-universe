let get = () => {
  switch (Sys.getenv_opt("RHYTHM_PROJECT_ROOT")) {
  | Some(dir) => dir
  | None =>
    failwith(
      "Expected `RHYTHM_PROJECT_ROOT` environment variable to be set "
      ++ "before running tests.",
    )
  };
};
