let _ =
  Sys.chdir "testspace";
  FrameWork.Test.run_many Test_file.tests;
  FrameWork.Test.run_many Test_metapost.tests
