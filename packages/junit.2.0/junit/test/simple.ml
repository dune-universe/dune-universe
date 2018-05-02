(** Encode this example (from http://help.catchsoftware.com/display/ET/JUnit+Format):

<testsuites>
  <testsuite name="JUnitXmlReporter" errors="0" tests="0" failures="0"
             time="0" timestamp="2013-05-24T10:23:58" />
  <testsuite name="JUnitXmlReporter.constructor" errors="0" skipped="1"
             tests="3" failures="1" time="0.006" timestamp="2013-05-24T10:23:58">
    <properties>
      <property name="java.vendor" value="Sun Microsystems Inc." />
      <property name="compiler.debug" value="on" />
      <property name="project.jdk.classpath" value="jdk.classpath.1.6" />
    </properties>
    <testcase classname="JUnitXmlReporter.constructor"
              name="should default path to an empty string"
              time="0.006">
      <failure message="test failure">Assertion failed</failure>
    </testcase>
    <testcase classname="JUnitXmlReporter.constructor"
              name="should default consolidate to true"
              time="0">
      <skipped />
    </testcase>
    <testcase classname="JUnitXmlReporter.constructor"
              name="should default useDotNotation to true"
              time="0" />
  </testsuite>
</testsuites>

*)

let timestamp =
  match Ptime.of_date_time ((2013, 5, 24), ((10, 23, 58), 0)) with
  | Some t -> t
  | None -> assert false

let simple path =
  let junitXmlReporter = Junit.Testsuite.make ~timestamp ~name:"JUnitXmlReporter" () in
  let junitXmlReportConstructor =
    let properties =
      [
        Junit.Property.make ~name:"java.vendor" ~value:"Sun Microsystems Inc.";
        Junit.Property.make ~name:"compiler.debug" ~value:"on";
        Junit.Property.make ~name:"project.jdk.classpath" ~value:"jdk.classpath.1.6";
      ]
    in
    let testcases =
      [
        Junit.Testcase.failure
          ~name:"should default path to an empty string"
          ~classname:"JUnitXmlReporter.constructor"
          ~time:0.006
          ~message:"test failure"
          ~typ:"not equal"
          "Assertion failed";
        Junit.Testcase.skipped
          ~name:"should default consolidate to true"
          ~classname:"JUnitXmlReporter.constructor"
          ~time:0.;
        Junit.Testcase.pass
          ~name:"should default useDotNotation to true"
          ~classname:"JUnitXmlReporter.constructor"
          ~time:0.;
      ]
    in
    Junit.Testsuite.make ~timestamp ~name:"JUnitXmlReporter.constructor" ()
    |> Junit.Testsuite.add_testcases testcases
    |> Junit.Testsuite.add_properties properties
  in
  let report = Junit.make [junitXmlReporter; junitXmlReportConstructor] in
  match path with
  | None ->
    let xml_report = Junit.to_xml report in
    Format.printf "%a\n" (Tyxml.Xml.pp ()) xml_report
  | Some path ->
    Junit.to_file report path

let () =
  let path =
    if Array.length Sys.argv > 1 then
      Some (Sys.argv.(1))
    else
      None
  in
  simple path
