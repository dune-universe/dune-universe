let projectDir = GetProjectRoot.get();

include Rely.Make({
  let config =
    Rely.TestFrameworkConfig.initialize({
      snapshotDir:
        Filename.(
          projectDir
          |> (dir => Filename.concat(dir, "tests"))
          |> (dir => Filename.concat(dir, "__snapshots__"))
        ),
      projectDir: projectDir,
    });
});
