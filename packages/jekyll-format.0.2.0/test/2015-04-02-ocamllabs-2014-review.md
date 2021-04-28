---
layout: post
title: Reviewing the second year of OCaml Labs in 2014
author: Anil Madhavapeddy
date: 2014-04-24 17:14:07
tags: ocamllabs,ocaml
source: http://www.cl.cam.ac.uk/projects/ocamllabs/news/index.html
source_name: OCaml Labs
---

<a href="http://www.cl.cam.ac.uk/projects/ocamllabs/"><img style="padding: 15px; float:right;" src="{{site.url}}/images/ocl14rev/lab_main.jpg" /></a>
The [OCaml Labs](http://ocaml.io) initiative within the [Cambridge Computer Laboratory](http://www.cl.cam.ac.uk)
is now just over two years old, and it is time for an update about our  activities
since the last update at the [end of 2013](http://www.cl.cam.ac.uk/projects/ocamllabs/news/index.html#Dec 2013) and [2012](http://anil.recoil.org/2012/10/19/announcing-ocaml-labs.html).

The theme of our group was not to be pure research, but rather a hybrid group that takes on
some of the load of day-to-day OCaml maintenance from [INRIA](http://caml.inria.fr/),
as well as help grow the wider community and meet our own research agendas around
topics such as [unikernels](https://queue.acm.org/detail.cfm?id=2566628).
To this end, all of our projects have been highly collaborative, often involving colleagues
from [OCamlPro](http://ocamlpro.com), [INRIA](http://caml.inria.fr/),
[Jane Street](http://janestreet.com), [Lexifi](http://lexifi.com) and [Citrix](http://citrix.com).

This post covers our progress in tooling, the compiler and language, community efforts, research projects and concludes with our priorities for 2015.

## Tooling

At the start of 2014, we had just helped to release
[OPAM 1.1.1](http://opam.ocaml.org/blog/opam-1-1-1-released/) with our colleagues
at [OCamlPro](http://ocamlpro.com), and serious OCaml users had just started moving
over to using it. 

Our overall goal at OCaml Labs is to deliver a modular set of of development tools around
OCaml that we dub the *OCaml Platform*.  The remainder of 2014 was thus spent polishing
this nascent OPAM release into a solid base (both as a command-line tool and as a library)
that we could use as the basis for documentation, testing and build infrastructure, all
the while making sure that bigger OCaml projects continued to migrate over to it.
Things have been busy; here are the highlights of this effort.

### OPAM

The central [OPAM repository](https://github.com/ocaml/opam-repository) that 
contains the package descriptions has grown tremendously in 2014, with over
280 contributors committing almost 10000 changesets across 3800 
[pull requests](https://github.com/ocaml/opam-repository/pulls) on GitHub.
The front line of incoming testing has been continuous integration by the wonderful
[Travis CI](http://travis-ci.org/ocaml/opam-repository), who also
granted us access to their experimental [MacOS X](http://docs.travis-ci.com/user/osx-ci-environment/) 
build pool.  The OPAM package team also to expanded to give David Sheets, Jeremy Yallop, 
Peter Zotov and Damien Doligez commit rights, and they have all been busily triaging new packages
as they come in.

Several large projects such as [Xapi](http://xapi-project.github.io/), [Ocsigen](http://ocsigen.org) and
our own [MirageOS](http://openmirage.org) switched over to using OPAM
for day-to-day development, as well as prolific individual developers such as 
[Daniel Buenzli](http://erratique.ch) and [Markus Mottl](http://ocaml.info/).
[Jane Street](https://blogs.janestreet.com/category/ocaml/) continued to send
regular [monthly updates](https://github.com/ocaml/opam-repository/pulls?utf8=%E2%9C%93&q=is%3Apr+author%3Adiml+) 
of their Core/Async suite, and releases appeared from the 
[Facebook](https://github.com/ocaml/opam-repository/pull/3570) open-source team
as well (who develop [Hack](https://code.facebook.com/posts/264544830379293/hack-a-new-programming-language-for-hhvm/),
[Flow](https://github.com/facebook/flow) and [Pfff](https://github.com/facebook/pfff) in OCaml).

<table width="90%" align="center">
<tr>
<td width="30%" class="bimg"><a href="{{site.url}}/images/ocl14rev/opam12-contributors-mar14.png"><img border="0" src="{{site.url}}/images/ocl14rev/opam12-contributors-mar14.png" width="220" /></a><br />Number of unique contributors to the central OPAM package repository.</td>
<td width="30%" class="bimg"><a href="{{site.url}}/images/ocl14rev/opam12-packages-mar14.png"><img border="0" src="{{site.url}}/images/ocl14rev/opam12-packages-mar14.png" width="220" /></a><br />Total number of unique packages (including multiple versions of the same package).</td>
<td width="30%" class="bimg"><a href="{{site.url}}/images/ocl14rev/opam12-unique-packages-mar14.png"><img border="0" src="{{site.url}}/images/ocl14rev/opam12-unique-packages-mar14.png" width="220" /></a><br />Total packages with multiple versions coalesced so you can see new package growth.</td>
</tr>
</table>


We used feedback from the users to smooth away many of the rough edges, with:

- a redesigned [development workflow](http://opam.ocaml.org/blog/opam-1-2-pin/) that
  lets developers quickly grab a development version of a library  recompile all 
  dependent packages automatically, and quickly publish results to GitHub.
- binary distributions for common OS distributions via their 
  [native packaging](https://github.com/ocaml/opam/wiki/Distributions), as well 
  as [0install](http://opam.ocaml.org/blog/0install-intro/) and 
  [Vagrant boxes](https://github.com/mirage/mirage-vagrant-vms).
- a unified way of cloning the source of any package via `opam source`.  This
  handles any supported OPAM archive, including Git, Mercurial or Darcs remotes. 
- a richer package metadata, including source code, development archives and
  bug report URLs.

These changes were all incorporated into the [OPAM 1.2](http://opam.ocaml.org/blog/opam-1-2-0-release/),
along with backwards compatibility shims to keep the old 1.1 metadata format working until
the migration is complete.  The 1.2.x series has been a solid and usable
development manager, and last week's release of [OPAM 1.2.1](http://opam.ocaml.org/blog/opam-1-2-1-release/) has further polished the core scripting engine.

#### Platform Blog
 
One of the more notable developments during 2014 was the 
[adoption of OPAM](http://coq-blog.clarus.me/use-opam-for-coq.html) further up the 
ecosystem by the [Coq](https://coq.inria.fr/) theorem prover.  This broadening of the
community prompted us to create an [official OPAM blog](http://opam.ocaml.org) to give
us a central place for new and tips, and we've had posts about
[XenServer](http://opam.ocaml.org/blog/opam-in-xenserver/) developments, 
the [Merlin IDE tool](http://opam.ocaml.org/blog/turn-your-editor-into-an-ocaml-ide/)
and the modern [UTop](http://opam.ocaml.org/blog/about-utop/) interactive REPL.
If you are using OPAM in an interesting or production capacity, please do
[get in touch](https://github.com/ocaml/platform-blog/issues) so that we can work
with you to write about it for the wider community.

The goal of the blog is also to start bringing together the various
components that form the OCaml Platform.  These are designed to be
modular tools (so that you can pick and choose which ones are necessary
for your particular use of OCaml).  There are more details available
from the OCaml Workshop presentation at ICFP 2014 ([abstract](https://ocaml.org/meetings/ocaml/2014/ocaml2014_7.pdf),
[slides](https://ocaml.org/meetings/ocaml/2014/ocl-platform-2014-slides.pdf),
[video](https://www.youtube.com/watch?v=jxhtpQ5nJHg&list=UUP9g4dLR7xt6KzCYntNqYcw)).

#### Onboarding New Users

OPAM has also been adopted now by [several](http://harvard.edu) [big](http://cornell.edu) [universities](http://princeton.edu)
(including [us at Cambridge](http://www.cl.cam.ac.uk/teaching/1415/L28/)!)
for undergraduate and graduate Computer Science courses.  The demands increased for
an out-of-the-box solution that makes it as easy possible for new users to
get started with minimum hassle.
We created a [dedicated teaching list](http://lists.ocaml.org/listinfo/teaching)
to aid collaboration, and a list of [teaching resources on ocaml.org](http://ocaml.org/learn/teaching-ocaml.html)
and supported several initiatives in collaboration with
[Louis Gesbert](https://github.com/AltGr) at OCamlPro, as usual with OPAM development).

The easiest way to make things "just work" are via regular binary builds of the
latest releases of OCaml and OPAM on Debian, Ubuntu, CentOS and Fedora, via
[Ubuntu PPAs](http://launchpad.net/~avsm) 
and the [OpenSUSE Build Service](https://build.opensuse.org/package/show/home:ocaml/opam) repositories.
Our industrial collaborators from Citrix, [Jon Ludlam](http://jon.recoil.org) 
and [Dave Scott](http://dave.recoil.org) began an 
[upstreaming initiative](http://lists.ocaml.org/pipermail/opam-devel/2015-January/000910.html) to Fedora
and sponsored the creation of a [CentOS SIG](http://lists.centos.org/pipermail/centos-devel/2014-November/012375.html)
to ensure that binary packages remain up-to-date.  We also contribute to the hardworking packagers
on MacOS X, Debian, FreeBSD, NetBSD and OpenBSD where possible as well to ensure that
binary builds are well rounded out.  Richard Mortier also assembled [Vagrant boxes](https://github.com/mirage/mirage-vagrant-vms)
that contain OCaml, for use with VirtualBox.

<a href="{{site.url}}/images/ocl14rev/opam-in-nice.jpg"><img style="padding-left: 15px; float:right;" alt="Louis cooks us dinner in Nice at our OPAM developers summit" src="{{site.url}}/images/ocl14rev/opam-in-nice-thumb.jpg" /></a>
Within OPAM itself, we applied polish to the handling of [external dependencies](https://github.com/ocaml/opam-depext)
to automate checking that the system libraries required by OPAM are present.  Two emerging tools that should
help further in 2015 are the [opam-user-setup](https://github.com/OCamlPro/opam-user-setup) and
[OPAM-in-a-box](https://github.com/ocaml/opam/issues/1035) plugins that automate first-time configuration.
These last two are primarily developed at OCamlPro, with design input and support from OCaml Labs.

We do have a lot of work left to do with making the new user experience really
seamless, and help is *very* welcome from anyone who is interested.  It often helps
to get the perspective of a newcomer to find out where the stumbling blocks are, and
we value any such advice.  Just mail [opam-devel@lists.ocaml.org](mailto:opam-devel@lists.ocaml.org)
with your thoughts, or [create an issue](https://github.com/ocaml/opam/issues) on how we
can improve.  A particularly good example of such an initiative was started by 
Jordan Walke, who prototyped [CommonML](https://github.com/jordwalke/CommonML) with
a NodeJS-style development workflow, and 
[wrote up](http://lists.ocaml.org/pipermail/opam-devel/2015-February/000975.html) his 
design document for the mailing list. (Your questions or ideas do not need to be as
well developed as Jordan's prototype!)

### Testing Packages

<a href="http://travis-ci.org"><img style="width:160px; padding-left: 15px; float:right;" src="{{site.url}}/images/travis-mascot-200px.png" /></a>
The public Travis CI testing does come with some limitations, since it only
checks that the latest package sets install, but not if any transitive
dependencies fail due to interface changes.  It also doesn't test all the
optional dependency combinations due to the 50 minute time limit.

We expanded the OPAM repository testing in several ways to get around this:

* **Individual Repositories:** Thomas Gazagnaire built 
  [centralised Travis scripts](http://opam.ocaml.org/blog/opam-1-2-travisci/) 
  that can be used on any OCaml GitHub repository to easily test code before it is
  released into OPAM.  These scripts are sourced from a central [repository](https://github.com/ocaml/ocaml-travisci-skeleton)
  and support external, optional and reverse dependency checking across multiple revisions of the
  compiler.  For instance, it just needs [one file](https://github.com/mirage/ocaml-cohttp/blob/master/.travis.yml) to test
  all the supported permutations of the [CoHTTP](https://github.com/mirage/ocaml-cohttp) library.

* **Bulk Builds**: Damien Doligez and I independently started doing large-scale bulk builds of
  the repository to ensure that a single snapshot of the package repository can automatically
  build as many packages as possible.  My implementation used the [Docker](http://docker.com)
  container manager to spawn off 1000s of package builds in parallel and commit the results into a filesystem
  This required building a [Dockerfile eDSL](http://avsm.github.io/ocaml-dockerfile), and the results
  are now online at <https://opam.ocaml.org/builds>.

* **OCamlot**: An ongoing piece of infrastructure work is to take the bulk build logs (which are
  around 7GB per daily run), and to store and render them using our [Irmin](http://irmin.io) Git store. 
  Expect to see more around this soon; it has the awesome feature of letting any developer clone
  the build logs for their project locally, to make triage of foreign operating systems as simple 
  as possible.

#### Language Evolution

This ability to do unattended builds of the package repository has also improved the decision
making process within the core compiler team.  Since we now have a large (3000+ package) corpus of
OCaml code, it became a regular occurrence in the 4.02 development cycle to 
"[ask OPAM](http://anil.recoil.org/2014/04/08/grepping-every-known-ocaml-package-source.html)" whether 
a particular feature or new syntax would break any existing code.  This in turn provides an
incentive for commercial users to provide representative samples of their code; for instance, the
Jane Street Core releases in OPAM (with their very modular style) act as an open-source canary
without needing access to any closed source code.

One good example in 2014 was the decoupling of the [Camlp4](http://en.wikipedia.org/wiki/Camlp4) 
macro preprocessor from the main OCaml
distribution.  Since Camlp4 has been used for over a decade and there are some very commonly
used syntax extensions such as [type_conv](https://github.com/janestreet/type_conv), a simple
removal would break a lot of packages.  We used OPAM to perform a gradual movement that most
users hopefully never noticed by the time OCaml 4.02 was released.  First, we added a 
[dummy package](https://github.com/ocaml/opam-repository/pull/2558) in OPAM for earlier versions 
of the compiler that had Camlp4 built-in, and then used the OPAM constraint engine to compile it
as an external tool for the newer compiler revisions.  Then we just had to triage the bulk build
logs to find build failures from packages
that were missing a Camlp4 dependency, and [add them](https://github.com/ocaml/opam-repository/pulls?utf8=%E2%9C%93&q=camlp4+requires+is%3Apr+) to the package metadata.

#### GitHub Integration

An interesting [comment](https://twitter.com/vincenthz/status/563108158907097089) from Vincent Hanquez
about OPAM is that "OCaml's OPAM is a post-GitHub design".  This is very true, as much of the
workflow for pinning `git://` URLs emerged out of being early adopters of GitHub for hosting the
MirageOS.  OCaml Labs supported two pieces of infrastructure integration around GitHub in 2014:

* OPAM has a compiler switch feature that lets you run simultaneous OCaml installations and
  swap between them easily. I used my [GitHub API bindings](https://github.com/avsm/ocaml-github)
  to regularly convert every GitHub pull request 
  into [a custom compiler switch](http://anil.recoil.org/2014/03/25/ocaml-github-and-opam.html).
  This lets users reporting bugs try out a patched compiler almost immediately upon a fix becoming
  available.

* The motivation behind this feature was our collaborator Gabriel Scherer's 
  [experiment](http://gallium.inria.fr/blog/patch-review-on-github/) to enable patch review of
  OCaml on GitHub, alongside the venerable [Mantis bug tracker](http://caml.inria.fr/mantis/view_all_bug_page.php).
  We supported this via adding Travis CI support to the main compiler, and also helped to migrate a
  number of support libraries to GitHub, such as [camlp4](https://github.com/ocaml/camlp4).  These
  can all be found on the [ocaml](https://github.com/ocaml) organisation on GitHub.

### Codoc Documentation

<iframe style="float: right; padding-left: 15px" width="200" height="150" src="https://www.youtube.com/embed/jxhtpQ5nJHg?rel=0" frameborder="0" allowfullscreen></iframe> 
Leo White, David Sheets, Amir Chaudhry and Thomas Gazagnaire led the charge 
to build a modern documentation generator for OCaml, and 
[published](http://lists.ocaml.org/pipermail/platform/2015-February/000539.html)
an *alpha* version of [codoc 0.2.0](https://github.com/dsheets/codoc) after a lot of work throughout 2014.
In the 2014 OCaml workshop presentation 
([abstract](http://ocaml.org/meetings/ocaml/2014/ocaml2014_7.pdf),
[slides](http://ocaml.org/meetings/ocaml/2014/ocl-platform-2014-slides.pdf),
[video](https://www.youtube.com/watch?v=jxhtpQ5nJHg&list=UUP9g4dLR7xt6KzCYntNqYcw)),
we mentioned the "module wall" for documentation and this attempts to fix it.
To try it out, simply follow the directions in the README on that repository,
or [browse some samples](http://dsheets.github.io/codoc) of the current,
default output of the tool. Please do bear in mind codoc and its constituent
libraries are still under heavy development and are *not* feature complete, but
we're gathering [feedback](https://github.com/dsheets/codoc/issues) from early
adopters.

`codoc`'s aim is to provide a widely useful set of tools for generating OCaml
documentation. In particular, we are striving to:

1. Cover all of OCaml's language features
2. Provide accurate name resolution and linking
3. Support cross-linking between different packages
4. Expose interfaces to the components we've used to build `codoc`
5. Provide a magic-free command-line interface to the tool itself
6. Reduce external dependencies and default integration with other tools

We haven't yet achieved all of these at all levels of our tool stack but are
getting close, and the patches are all under discussion for integration into
the mainstream OCaml compiler.
`codoc` 0.2.0 is usable today (if a little rough in some areas like default CSS),
and there is a [blog post](http://opam.ocaml.org/blog/codoc-0-2-0-released/) that outlines the architecture of the new system to
make it easier to understand the design decisions that went into it.

### Community Governance

As the amount of infrastructure built around the [ocaml.org](http://ocaml.org) domain grows (e.g. mailing lists, file hosting, bulk building), it is important to establish a governance framework to ensure that it is being used as best needed by the wider OCaml community.

Amir Chaudhry took a good look at how other language communities organise themself, 
and began putting together a succinct 
[governance framework](http://amirchaudhry.com/towards-governance-framework-for-ocamlorg/) 
to capture how the community around `ocaml.org` operates, and how to quickly resolve any conflicts 
that may arise in the future.  He took care to ensure it had a well-defined scope, is 
simple and self-contained, and (crucially) documents the current reality.  The result 
of this work is circulating privately through all the existing volunteers for a first 
round of feedback, and will go live in the next few months as a living document 
that explains how our community operates.

### Assemblage

One consequence of OCaml's age (close to twenty years old now) is that the tools built
around the compiler have evolved fairly independently.  While OPAM now handles the high-level
package management, there is quite a complex ecosystem of other components that are complex
for new users to get to grips with: [OASIS](http://github.com/ocaml/oasis),
[ocamlfind](http://projects.camlcity.org/projects/findlib.html),
[ocamlbuild](https://ocaml.org/learn/tutorials/ocamlbuild/), and
[Merlin](https://github.com/the-lambda-church/merlin) to name a few.
Each of these components (while individually stable) have their own metadata and
namespace formats, further compounding the lack of cohesion of the tools.

Thomas Gazagnaire and Daniel Buenzli embarked on an effort to build an eDSL
that unifies OCaml package descriptions, with the short-term aim of generating the
support files required by the various support tools, and the long-term goal of
being the integration point for the build, test and documentation generation
lifecycle of an OCaml/OPAM package.  This prototype, dubbed [Assemblage](https://github.com/samoht/assemblage)
has gone through several iterations and [design discussions](https://github.com/samoht/assemblage/labels/design)
over the summer of 2014.  Daniel has since been splitting out portions of it
into the [Bos](http://erratique.ch/software/bos) OS interaction library.

Assemblage is not released officially yet, but we are committed to resuming work
on it this summer when Daniel visits again, with the intention of unifying much
of our workflow through this tool.   If you are interested in build and packaging
systems, now is the time to [make your opinion known](https://github.com/samoht/assemblage)!


## Core Compiler

We also spent time in 2014 working on the core OCaml language and compiler, with our work
primarily led by Jeremy Yallop and Leo White.  These efforts were not looking to make any
radical changes in the core language; instead, we generally opted for evolutionary
changes that either polish rough edges in the language (such as open type and handler cases),
or new features that fit into the ML style of building programs.

### New Features in 4.02.0

The OCaml 4.02 series was primarily developed and [released](https://ocaml.org/releases/4.02.html) in 2014.
The [ChangeLog](http://caml.inria.fr/pub/distrib/ocaml-4.02/notes/Changes) generated much
[user excitement](https://blogs.janestreet.com/ocaml-4-02-everything-else/),
and we were also pleased to have contributed several language improvements.

#### Handler Cases and exceptional syntax

OCaml's `try` and `match` constructs are good at dealing with exceptions
and values respectively, but neither constructs can handle both values and exceptions.
Jeremy Yallop investigated 
[how to handle success](http://ocamllabs.github.io/compiler-hacking/2014/02/04/handler-case.html#match-exception)
more elegantly, and an elegant unified syntax emerged.  A simple example is that
of a stream iterator that uses exceptions for control flow:

    let rec iter_stream f s =
      match (try Some (MyStream.get s) with End_of_stream -> None) with
      | None -> ()
      | Some (x, s') -> f x; iter_stream f s'

This code is not only verbose, but it also has to allocate an `option` value to ensure
that the `iter_stream` calls remains tail recursive.  The new syntax in OCaml 4.02
allows the above to be rewritten succinctly:

    let rec iter_stream f s =
      match MyStream.get s with
      | (x, s') -> f x; iter_stream f s'
      | exception End_of_stream -> ()

Read more about the background of this feature in Jeremy's 
[blog post](http://ocamllabs.github.io/compiler-hacking/2014/02/04/handler-case.html#match-exception), the associated discussion in the [upstream Mantis bug](http://caml.inria.fr/mantis/view.php?id=6318),
and the final [manual page](http://caml.inria.fr/pub/docs/manual-ocaml/extn.html#sec245) in the OCaml 4.02 release.
For an example of its use in a real library, see the Jane Street [usage](https://github.com/janestreet/sexplib/blob/1bd69553/lib/conv.ml#L213-L215) in the [s-expression](https://github.com/janestreet/sexplib) handling library (which they use widely to reify arbitrary OCaml values and exceptions).

#### Open Extensible Types

A long-standing trick to build [universal containers](https://blogs.janestreet.com/rethinking-univ/) 
in OCaml has been to encode them using the exception `exn` type.
There is a similar concept of a [universal type](http://mlton.org/UniversalType) in Standard ML,
and they were described in the "[Open Data Types and Open Functions](http://www.andres-loeh.de/OpenDatatypes.pdf)"
paper by Andres Löh and Ralf Hinze in 2006.

Leo White designed, implemented and upstreamed support for 
[extensible variant types](http://caml.inria.fr/pub/docs/manual-ocaml/extn.html#sec246) in 
OCaml 4.02. Extensible variant types are variant types that can be extended with new variant constructors.
They can be defined as follows:

    type attr = ..

    type attr += Str of string

    type attr +=
      | Int of int
      | Float of float

Pattern matching on an extensible variant type requires a default case to handle unknown 
variant constructors, just as is required for pattern matching on exceptions (extensible
types use the exception memory representation at runtime).

With this feature added, the OCaml `exn` type simply becomes a special case of open extensible
types. Exception constructors can be declared using the type extension syntax:

        type exn += Exc of int

You can read more about the discussion behind open extensible types in the upstream
[Mantis bug](http://caml.inria.fr/mantis/view.php?id=5584).  If you'd like to see another
example of their use, they have been adopted by the latest releases of the 
Jane Street Core libraries in the 
[Type_equal](https://github.com/janestreet/core_kernel/blob/43ee3eef/lib/type_equal.ml#L64)
module.

### Modular Implicits

<iframe style="float:right; padding-left: 15px" width="200" height="150" src="https://www.youtube.com/embed/3wVUXTd4WNc?rel=0" frameborder="0" allowfullscreen></iframe>
A common criticism of OCaml is its lack of support for ad-hoc polymorphism. 
The classic example of this is OCaml's separate addition operators for 
integers (`+`) and floating-point numbers (`+.`).
Another example is the need for type-specific printing functions (`print_int`, `print_string`, etc.)
rather than a single `print` function which works across multiple types.

Taking inspiration from Scala's 
[implicits](http://docs.scala-lang.org/tutorials/tour/implicit-parameters.html) and 
[Modular Type Classes](http://www.mpi-sws.org/~dreyer/papers/mtc/main-long.pdf) by Dreyer *et al.*,
Leo White designed a system for ad-hoc polymorphism in OCaml based on using modules as
type-directed implicit parameters.  The design not only supports implicit modules, but also 
implicit functors (that is, modules parameterised by other module types) to permit the expression
of generic modular implicits in exactly the same way that functors are used to build abstract
data structures.

Frederic Bour joined us as a summer intern and dove straight
into the implementation, resulting in an 
[online demo](http://andrewray.github.io/iocamljs/modimp_show.html) and ML Workshop presentation 
([abstract](https://sites.google.com/site/mlworkshoppe/modular-implicits.pdf?attredirects=0),
 [video](https://www.youtube.com/watch?v=3wVUXTd4WNc) and
 [paper](http://www.lpw25.net/ml2014.pdf)).
Another innovation in how we've been trialling this feature is the use of Andy Ray's
[IOCamlJS](https://andrewray.github.io/iocamljs/)
to publish an interactive, online notebook that is fully hosted in the browser.  You can follow the
examples of modular implicits [online](https://andrewray.github.io/iocamljs/modimp_show.html),
or try them out on your own computer via an OPAM switch:

    opam switch 4.02.0+modular-implicits
    eval `opam config env`
    opam install utop 
    utop

Some of the early feedback on modular implicits from industrial users was interesting.
Jane Street commented that although this would be a big usability leap, it would be dangerous to
lose control over exactly what goes into the implicit environment (i.e. the programmer should always
know what `(a + b)` represents by locally reasoning about the code).  The current design thus follows
the ML discipline of maintaining explicit control over the namespace, with any ambiguities in resolving
an implicit module type resulting in a type error.

### Multicore

<iframe style="float:right; padding-left:10px" width="200" height="150" src="https://www.youtube.com/embed/FzmQTC_X5R4?rel=0" frameborder="0" allowfullscreen></iframe>

In addition to ad-hoc polymorphism, support for parallel execution on multicore CPUs is undoubtedly
the most common feature request for OCaml.  This has been high on our list after improving tooling 
support, and Stephen Dolan and Leo White made solid progress in 2014 on the core runtime plumbing
required.

Stephen initially added [thread-local support](https://github.com/stedolan/ocaml) to the 
OCaml compiler.  This design avoided the need to make the entire OCaml runtime preemptive
(and thus a huge patch) by allocating thread-local state per core.

We are now deep into the design and implementation of the programming abstractions built
over these low-level primitives.  One exciting aspect of our implementation is much of the scheduling
logic for multicore OCaml can be written in (single-threaded) OCaml, making the design very
flexible with respect to [heterogenous hardware](http://kcsrk.info/papers/mmscc_marc12.pdf) 
and [variable IPC performance](http://fable.io).

To get feedback on the overall design of multicore OCaml, we presented at OCaml 2014
([slides](http://www.cl.cam.ac.uk/~sd601/papers/multicore_slides.pdf),
 [video](https://www.youtube.com/watch?v=FzmQTC_X5R4) and
 [abstract](https://ocaml.org/meetings/ocaml/2014/ocaml2014_1.pdf)), and 
 Stephen visited INRIA to consult with the development team and Arthur Chargueraud (the author of [PASL](http://www.chargueraud.org/softs/pasl/)). 
Towards the end of the year, [KC Sivaramakrishnan](http://kcsrk.info/) finished his PhD
studies at Purdue and joined
our OCaml Labs group.  He is the author of [MultiMlton](http://multimlton.cs.purdue.edu/mML/Welcome.html),
and is now driving the completion of the OCaml multicore work along with Stephen Dolan, Leo White and
Mark Shinwell. Stay tuned for updates from us when there is more to show later this year!

### Ctypes: a Modular Foreign Function Interface

<a href="http://github.com/ocamllabs/ocaml-ctypes"><img style="width:180px; padding: 15px; float:right;" src="{{site.url}}/images/ocl14rev/c.png" /></a>
The [Ctypes](https://github.com/ocamllabs/ocaml-ctypes) library started as an experiment with
GADTs by Jeremy Yallop, and has since ballooned in a robust, comprehensive library
for safely interacting with the OCaml foreign function interface.   The first release
came out in time to be included in 
[Real World OCaml](https://realworldocaml.org/v1/en/html/foreign-function-interface.html) 
in lieu of the low-level FFI (which I was not particularly enamoured with having to explain
in a tight page limit).

Throughout 2014, Jeremy expanded support for a number of features requested by users
(both industrial and academic) who adopted the library in preference to manually writing
C code to interface with the runtime, and issued several updated 
[releases](https://github.com/ocamllabs/ocaml-ctypes/releases).

#### C Stub Generation

The first release of Ctypes required the use of [libffi](https://sourceware.org/libffi/)
to dynamically load shared libraries and dynamically construct function call stack
frames whenever a foreign function is called.  While this works for simple libraries,
it cannot cover *all* usecases, since interfacing with C demands an understanding
of `struct` memory layout, C preprocessor macros, and other platform-dependent quirks
which are more easily dealt with by invoking a C compiler.  Finally, the performance
of a `libffi`-based API will necessarily be slower than writing direct C stub code.

While many other language FFIs provide separate libraries for dynamic and static
FFI libraries, we decided to have a go at building a *modular* version of Ctypes
that could handle both cases from a single description of the foreign function interface.
The result (dubbed "Cmeleon") remained surprisingly succinct and usable, and now
covers almost every use of the OCaml foreign function interface.  We 
submitted a paper to [ICFP 2015](http://icfpconference.org/2015) dubbed
"[A modular foreign function interface](http://anil.recoil.org/papers/drafts/2015-cmeleon-icfp-draft1.pdf)"
that describes it in detail.  Here is a highlight of how simple a generic binding looks:

    module Bindings(F : FOREIGN) = struct
      open F
      let gettimeofday = foreign "gettimeofday"
         (ptr timeval @-> ptr timezone @-> returning int)
    end

The `FOREIGN` module type completely abstracts the details of whether or not dynamic
or static binding is used, and handles C complexities such as computing the struct
layout on the local machine architecture.

#### Inverse Stubs

The other nice result from functorising the foreign function interface emerged
when we tried to *invert* the FFI and serve a C interface from OCaml code (for
example, by compiling the OCaml code as a 
[shared library](http://caml.inria.fr/pub/docs/manual-ocaml/intfc.html)).  This
would let us begin swapping out C libraries that we [don't trust](http://openssl.org)
with [safer equivalents](https://github.com/mirage/ocaml-tls) written in OCaml.

You can see an [example](https://github.com/yallop/ocaml-ctypes-inverted-stubs-example)
 of how inverted stubs work via a simple C XML parsing exposed
from the [Xmlm](http://erratique.ch/software/xmlm) library.  We can define a C `struct`
by:

    (* Define a struct of callbacks (C function pointers) *)
    let handlers : [`handlers] structure typ = structure "handlers"
    let (--) s f = field handlers s (funptr f)
     let on_data      = "on_data"      -- (string @-> returning void)
     let on_start_tag = "on_start_tag" -- (string @-> string @-> returning void)
     let on_end_tag   = "on_end_tag"   -- (void @-> returning void)
     let on_dtd       = "on_dtd"       -- (string @-> returning void) 
     let on_error     = "on_error"     -- (int @-> int @-> string @-> returning void)
    let () = seal handlers

and then expose this via C functions:

    module Stubs(I : Cstubs_inverted.INTERNAL) = struct
      (* Expose the type 'struct handlers' to C. *)
      let () = I.structure handlers

      (* We expose just a single function to C.  The first argument is a (pointer
         to a) struct of callbacks, and the second argument is a string
         representing a filename to parse. *)
      let () = I.internal "parse_xml" 
         (ptr handlers @-> string @-> returning void) parse
    end

You can find the full source code to these snippets on the 
[ocaml-ctypes-inverted-stubs-example](https://github.com/yallop/ocaml-ctypes-inverted-stubs-example)
repository on GitHub.

We'll be exploring this aspect of Ctypes further in 2015 for SSL/TLS with
David Kaloper and Hannes Mehnert, and Microsoft Research has generously funded a 
[PhD studentship](http://research.microsoft.com/en-us/collaboration/global/phd_projects2015.aspx) 
to facilitate the work.

#### Community Contributions

Ctypes benefited enormously from several external contributions from the OCaml
community.  From a portability perspective, A. Hauptmann contributed
[Windows support](https://github.com/ocamllabs/ocaml-ctypes/pull/190), and
Thomas Leonard added [Xen support](https://github.com/ocamllabs/ocaml-ctypes/pull/231)
to allow Ctypes bindings to work with [MirageOS unikernels](http://openmirage.org) (which opens
up the intriguing possibility of accessing shared libraries across virtual machine boundaries
in the future).  C language support was fleshed out by Edwin Torok contributing
[typedef support](https://github.com/ocamllabs/ocaml-ctypes/pull/238),
Ramkumar Ramachandra adding [C99 bools](https://github.com/ocamllabs/ocaml-ctypes/pull/220) 
and Peter Zotov integrating [native strings](https://github.com/ocamllabs/ocaml-ctypes/pull/143).

The winner of "most enthusiastic use of OCaml Labs code" goes to [Thomas Braibant](https://github.com/braibant)
of [Cryptosense](http://cryptosense.com/the-team/), who used *every* feature of the
Ctypes library (consider multi-threaded, inverted, staged and marshalled bindings) in their
effort to [hack the hackers](http://www.economist.com/news/science-and-technology/21647269-automating-search-loopholes-software-hacking-hackers).  David Sheets comes a close second with his implementation of the
[FUSE binary protocol](https://github.com/dsheets/profuse), parameterised by version quirks.

If you're using Ctypes, we would love to hear about your particular use.  A search on
GitHub and OPAM reveals over 20 projects using it already, including industrial use
at [Cryptosense](http://cryptosense.com) and [Jane Street](http://ocaml.janestreet.com),
and ports to Windows, *BSD, MacOS X and even iPhone and Android.  There's a
[getting started](https://github.com/ocamllabs/ocaml-ctypes/wiki) guide, and
a [mailing list](http://lists.ocaml.org/listinfo/ctypes) available.

## Community and Teaching Efforts

In addition to the online community building, we also participated in a number of conferences
and face-to-face events to promote education about functional programming.

### Conferences and Talks

<a href="{{site.url}}/images/ocl14rev/qcon-unikernel-talk.jpg"><img style="width:160px; padding-left: 15px; float:right;" src="{{site.url}}/images/ocl14rev/qcon-unikernel-talk-thumb.jpg" /></a>
There has been a huge growth in the number of quality conferences in recent years, making 
it tough to choose which ones to attend.
[ICFP](http://icfpconference.org) is the academic meeting point that predates most of them, 
and we [participated extensively](http://anil.recoil.org/2014/08/31/ocaml-labs-at-icfp-2014.html) in 2014 via talks, tutorials and a [keynote](https://www.youtube.com/watch?v=UEIHfXLMtwA) at the Haskell Symposium.  
I also served on the [program committee](http://icfpconference.org/icfp2014/) and 
[industrial relations chair](http://anil.recoil.org/2015/02/18/icfp15-call-for-sponsorships.html) 
and took over as the steering committee 
chair of [CUFP](http://cufp.org).  Jeremy Yallop, Thomas Gazagnaire and Leo White all 
served program committees on workshops, with Jeremy also chairing this year's ML Workshop.

Outside of academic conferences, we participated in a number of non-academic conferences such
as [QCon](https://qconsf.com/), [OSCON](http://oscon.com), [CCC](http://ccc.de), 
[New Directions in OS](https://operatingsystems.io/), [FunctionalConf](http://functionalconf.com),
[FPX](https://skillsmatter.com/conferences/1819-functional-programming-exchange) and [FOSDEM](https://fosdem.org/2014/). 
The vast majority of these talks were about the MirageOS, and slides can be found at [decks.openmirage.org](http://decks.openmirage.org).

#### The 2048 Browser Game

Yaron Minsky and I have run OCaml tutorials for ICFP for 
[a](http://cufp.org/2011/t3-building-functional-os.html) 
[few](http://cufp.org/2013/t2-yaron-minsky-anil-madhavapeddy-ocaml-tutorial.html)
[years](http://cufp.org/2012/t1-real-world-ocaml-anil-madhavapeddy-university-c.html), and 
we finally hung up our boots in favour of a new crowd.

Jeremy Yallop and Leo White stepped up to the mark with their ICFP/CUFP 2014 
[Introduction to OCaml](http://cufp.org/2014/t7-leo-white-introduction-to-ocaml.html)
tutorial, which had the additional twist of being taught entirely in a web browser
by virtue of using the [js_of_ocaml](http://ocsigen.org/js_of_ocaml) and [IOCamlJS](http://andrewray.github.io/iocamljs/).
They decided that a good practical target was the popular [2048](http://gabrielecirulli.github.io/2048/) game that
has wasted many programmer hours here at OCaml Labs.  They [hacked on it](https://github.com/ocamllabs/2048-tutorial) over the summertime, assisted by our visitor Daniel Buenzli who also released useful libraries such as
[Vg](http://erratique.ch/software/vg), [React](http://erratique.ch/software/react), [Useri](http://erratique.ch/software/useri), and [Gg](http://erratique.ch/software/gg).

The end result is satisfyingly [playable online](http://ocamllabs.github.io/2048-tutorial/), with the source code available at [ocamllabs/2048-tutorial](https://github.com/ocamllabs/2048-tutorial).

Thomas Gazagnaire got invited to Bangalore for [Functional Conf](http://functionalconf.com/) later in the year, and he extended the [interactive tutorial notebook](http://gazagnaire.org/fuconf14/) and also ran an OCaml tutorial to a packed room.  We were very happy to support the first functional programming conference in India, and hope to see many more such events spring up!  Amir Chaudhry then went to Belgium to [FOSDEM 2015](https://fosdem.org/2015/) where he showed off [the 2048 game running as an ARM unikernel](http://amirchaudhry.com/unikernel-arm-demo-fosdem/) to a crowd of attendees at the Xen booth.

### Graduate Teaching

<a href="{{site.url}}/images/ocl14rev/l23.jpg"><img style="width:160px; padding-left: 15px; float:right;" src="{{site.url}}/images/ocl14rev/l23-thumb.jpg" /></a>
Jeremy Yallop and Leo White (with assistance from Alan Mycroft and myself) also led the design of
a new graduate course on [Advanced Functional Programming](http://www.cl.cam.ac.uk/teaching/1415/L28/)
at the Computer Laboratory.  This ran in the [Lent Term](http://en.wikipedia.org/wiki/Lent_term) and
was over-subscribed by three times the number who pre-registered (due to a number of PhD students
and our collaborators from [Citrix](http://citrix.com) also attending).

The course materials are [freely available online](http://www.cl.cam.ac.uk/teaching/1415/L28/materials.html)
and cover the theory behind functional programming, and then move onto type inference, abstraction and
parametricity, GADTs, rows, monads, and staging.  We will be running this again in future years, and the
lecture materials are already proving useful to 
[answer mailing list questions](https://sympa.inria.fr/sympa/arc/caml-list/2015-04/msg00001.html).

### Mentoring Beginners

We also had the pleasure of mentoring up-and-coming functional programmers via several outreach programs,
both face-to-face and remote.

#### Cambridge Compiler Hacking

<a href="{{site.url}}/images/ocl14rev/compiler-hacking-dsyme.jpg"><img style="width:160px; padding-left: 15px; float:right;" src="{{site.url}}/images/ocl14rev/compiler-hacking-dsyme-thumb.jpg" /></a>
We started the [Cambridge Compiler Hacking](http://ocamllabs.github.io/compiler-hacking/) sessions
in a small way towards the end of 2013 in order to provide a local, friendly place to assist people
who wanted to dip their toes into the unnecessarily mysterious world of programming language hacking.
The plan was simple: provide drinks, pizza, network and a [bug list of varying difficulty](https://github.com/ocamllabs/compiler-hacking/wiki) for attendees to choose from and work on for the evening, with mentoring from the
experienced OCaml contributors.

We continued this bi-monthly tradition in 2014, with a regular attendance of 15-30 people, and
even cross-pollinated communities with our local F# and Haskell colleagues.  We rotated locations
from the Cambridge Computer Laboratory to Citrix, Makespace, and the new Cambridge Postdoc Centre.
We posted some [highlights](http://ocamllabs.github.io/compiler-hacking/2014/06/24/highlights-from-recent-sessions.html)
from sessions towards the start of the year, and are very happy with how it's going.  There
has even been uptake of the bug list across the water in France, thanks to Gabriel Scherer.

In 2015, we'd like to branch out further and host some sessions in London. If you have a suggestion
for a venue or theme, please [get in touch](http://lists.ocaml.org/listinfo/cam-compiler-hacking)!

#### Summer Programs

There has been a laudable rise in summer programs designed to encourage diversity in 
our community, and we of course leap at the opportunity to participate in these when we find them.

* The [GNOME Outreach Program](https://gnome.org/opw/) (now also known as [Outreachy](https://www.gnome.org/outreachy/))
  had one funded place for Xen and MirageOS.  [Mindy Preston](http://www.somerandomidiot.com/) did a spectacular
  [blog series](http://www.somerandomidiot.com/blog/categories/ocaml/) about her experiences and motivations
  behind learning OCaml.
* The [Google Summer of Code 2014](https://www.google-melange.com/) also had us [participating](http://openmirage.org/blog/applying-for-gsoc2014) via MirageOS, 
  and [Jyotsna Prakash](https://github.com/moonlightdrive) took on the challenging job of building
  OCaml bindings for Amazon EC2, also detailed on [her blog](https://1000hippos.wordpress.com/).
* Amir Chaudhry began the [Mirage Pioneer Projects](https://github.com/mirage/mirage-www/wiki/Pioneer-Projects)
  initiative to give beginners an easier onramp, and this has taken off very effectively as a way
  to advertise interesting projects for beginners at varying levels of difficulties.

Our own students also had the chance to participate in such workshops to get out of Cambridge
in the summer!
[Heidi Howard](http://hh360.user.srcf.net/blog/) liveblogged her experiences at the [PLMW](http://www.syslog.cl.cam.ac.uk/2015/01/14/programming-languages-mentoring-workshop-plmw/) workshop in Mumbai.
Meanwhile, [David Sheets](https://github.com/dsheets) got to travel to the slightly less exotic London to [liveblog OSIO](http://www.syslog.cl.cam.ac.uk/2014/11/25/new-directions-in-operating-systems/), and Leonhard Markert covered [ICFP 2014](http://www.syslog.cl.cam.ac.uk/2014/09/05/ocaml-2014/) as a student volunteer.

### Blogging and Online Activities

<a href="{{site.url}}/images/ocl14rev/jeremy-rwo.jpg"><img style="width:160px; padding-left: 15px; float:right;" src="{{site.url}}/images/ocl14rev/jeremy-rwo-thumb.jpg" /></a>
Our [blog roll](http://www.cl.cam.ac.uk/projects/ocamllabs/blogs/) maintains the ongoing stream of
activity from the OCaml Labs crew, but there were some particular highlights throughout 2014.

* [Thomas Leonard](http://roscidus.com/blog/) began writing about his experiences with switching
  his [0install](http://0install.net) installation system from 
  [Python to OCaml](http://roscidus.com/blog/blog/2014/06/06/python-to-ocaml-retrospective/)
  and [what you gain with OCaml](http://roscidus.com/blog/blog/2014/02/13/ocaml-what-you-gain/).
  This series led to a bunch of interesting feedback on social networking sites, and Thomas
  joined the group full-time to work on our research into 
  [unikernels](http://roscidus.com/blog/blog/2015/01/21/securing-the-unikernel/).
* [Magnus Skjegstad](http://www.skjegstad.com/) returned from Norway to Cambridge to work on 
   MirageOS, and came up with some [crazy experiements](http://www.skjegstad.com/blog/2015/03/25/mirageos-vm-per-url-experiment/),
   as well as helping to build [Vagrant images](http://www.skjegstad.com/blog/2015/01/19/mirageos-xen-virtualbox/) of the OCaml development environment.
* [Amir Chaudhry](http://amirchaudhry.com) began his quest to 
  [port his website](http://amirchaudhry.com/writing-planet-in-pure-ocaml/) website
  to a [Jekyll unikernel](http://amirchaudhry.com/from-jekyll-to-unikernel-in-fifty-lines/).
* The [Mirage 2.0 release](http://openmirage.org/blog/announcing-mirage-20-release) in the summer of 2014
  saw a slew of blogs posts about the [surge](http://openmirage.org/blog/2014-in-review) in MirageOS activity.

It wasn't all just blogging though, and Jeremy Yallop and Leo White in particular participated in some
epic OCaml [bug threads](http://caml.inria.fr/mantis/view.php?id=5528) about new features, and
[explanations](https://sympa.inria.fr/sympa/arc/caml-list/2015-02/msg00150.html) about OCaml semantics
on the mailing list.

Amir Chaudhry also continued to curate and develop the content on the [ocaml.org](http://ocaml.org)
website with our external collaborators [Ashish Agarwal](), [Christophe Troestler]() and [Phillippe Wang]().
Notably, it is now the recommended site for OCaml (with the [INRIA site](http://caml.inria.fr)
being infrequently updated), and also hosts the [ACM OCaml Workshop](https://ocaml.org/meetings/)
pages.  One addition that highlighted the userbase of OCaml in the teaching community came from
building a [map of all of the universities](https://ocaml.org/learn/teaching-ocaml.html) where the
language is taught, and this was Yan Shvartzshnaider's [first contribution](http://yansnotes.blogspot.co.uk/2014/11/good-news-everyone-ocamlorg-teaching.html) to the site.

### Visitors and Interns

<a href="{{site.url}}/images/ocl14rev/ocl-pub.jpg"><img style="width:160px; padding-left: 15px; float:right;" src="{{site.url}}/images/ocl14rev/ocl-pub-thumb.jpg" /></a>
Finally, a really important part of any community is hanging out with each other to chat over ideas in a friendly environment.
As usual, we had a very steady stream of visitors and interns throughout 2014 to facilitate this.

Frederic Bour, Benjamin Farinier and Matthieu Journault joined us as summer interns from their respective universities in France as part of their Masters programs.  Frederic worked on modular implicits and [gave a great talk](https://www.irill.org/videos/oups-december-2014/Modular_implicits) at the OCaml Users group.  Benjamin and Matthieu worked on Irmin data structures and complexity (and [merge-queues](https://github.com/mirage/merge-queues) and [merge-ropes](https://github.com/mirage/merge-ropes)), and Benjamin had his paper on "[Mergeable Persistent Data Structures](http://anil.recoil.org/papers/2015-jfla-irmin.pdf)" accepted to [JFLA 2015](http://jfla.inria.fr/2015/), while Matthieu's work on efficient algorithms for synchronising Irmin DAGs is being integrated into the upstream source code.

Daniel Buenzli repeated his visit from 2013 and spent a productive summer with us, commenting on almost every project we're working on.  In his own words (edited for brevity):

> I started by implementing and releasing [Uucp](http://erratique.ch/software/uucp), a library to provide efficient access to a selection of the  properties of the latest Unicode Character database (UCD). [...] As a side effect of the previous point I took time to write an absolute [minimal introduction to Unicode](http://erratique.ch/software/uucp/doc/Uucp.html#uminimal). [...]
> Since I was in this Unicode business I took the opportunity to propose a [31 loc patch to the standard library](https://github.com/ocaml/ocaml/pull/80) for a type to represent Unicode scalar values (an Unicode character to be imprecise) to improve interoperability.
>
> The usual yearly update to OpenGL was announced at the Siggraph conference. This prompted me to update the ctypes-based [tgls library](http://erratique.ch/software/tgls) for supporting the latest entry point of OpenGL 4.5 and OpenGL ES 3.1. Since the bindings are automatically generated from the OpenGL XML registry the work is not too involved but there's always the odd function signature you don't/can't handle automatically yet.
>
> Spend quite a bit (too much) time on [useri](http://erratique.ch/software/useri), a small multi-platform abstraction for setting up a drawing surface and gather user input (*not* usury) as [React](http://erratique.ch/software/react) events. Useri started this winter as a layer on top of SDL to implement a [CT scan app](http://erratique.ch/log/2014-05-18) and it felt like this could be the basis for adding interactivity and animation to Vg/Vz visualizations – js viz libraries simply rely on the support provided by the browser or SVG support but Vg/Vz strives for backend independence and clear separations of concern (up to which limit remains an open question). Unfortunately I couldn't bring it to a release and got a little bit lost in browser compatibility issues and trying to reconcile what browser and SDL give us in terms of functionality and way of operating, so that a maximum of client code can be shared among the supported platforms. But despite this non-release it still managed to be useful in some way, see the next point.
>
> Helped Jeremy and Leo to implement the rendering and interaction for their ICFP tutorial [2048 js_of_ocaml implementation](https://github.com/ocamllabs/2048-tutorial). This featured the use of Gg, Vg, Useri and React and I was quite pleased with the result (despite some performance problems in certain browsers, but hey composable rendering and animation without a single assignement in client code). It's nice to see that all these pains at trying to design good APIs eventually fit together [...]

A couple of visitors joined us from sunny [Morocco](http://github.com/mirleft), where Hannes Mehnert and David Kaloper had gone to work on a clean-slate TLS stack.  They found the [MirageOS](http://openmirage.org) effort online, and got in touch about visiting.  After a very fun summer of hacking, their stack is now the standard TLS option in MirageOS and resulted in the [Bitcoin Pinata challenge](http://amirchaudhry.com/bitcoin-pinata/) being issued!  Hannes and David have since moved to Cambridge to work on this stack full-time in 2015, but the internships served as a great way for everyone to get to know each other.

<a href="{{site.url}}/images/ocl14rev/christs-dinner.jpg"><img style="padding-left: 15px; float:right;" src="{{site.url}}/images/ocl14rev/christs-dinner-thumb.jpg" /></a>
We also had the pleasure of visits from several of our usually remote collaborators. [Christophe Troestler](https://github.com/Chris00), [Yaron Minsky](http://ocaml.janestreet.com), [Jeremie Diminio](http://github.com/diml) and [Andy Ray](https://github.com/andrewray) all visited for the annual OCaml Labs [review meeting](https://gist.github.com/avsm/18450004ae19c2facf7a) in Christ's College.
There were also many academic talks from foreign visitors in our [SRG seminar series](http://talks.cam.ac.uk/show/archive/8316), ranging from [Uday Khedkar](http://www.cse.iitb.ac.in/~uday/) from IIT to [Oleg Kiselyov](http://okmij.org/ftp/) deliver multiple talks on staging and optimisation (as well as making a celebrity appearance at the compiler hacking session, and [Yaron Minsky](http://ocaml.janestreet.com) delivering an Emacs-driven departmental seminar on his experiences with [Incremental](http://talks.cam.ac.uk/talk/index/51144) computation.

## Research Efforts

The OCaml Labs are of course based in the Cambridge Computer Laboratory, where 
our day job is to do academic research.  Balancing the demands of open source 
coding, community efforts and top-tier research has be a tricky one, but an effort
that has been worthwhile.

<a href="{{site.url}}/images/ocl14rev/nsdi-deadline.jpg"><img style="width:160px; padding-left: 15px; float:right;" src="{{site.url}}/images/ocl14rev/nsdi-deadline-thumb.jpg" /></a>
Our research efforts are broadly unchanged [from 2013](http://www.cl.cam.ac.uk/projects/ocamllabs/news/index.html#Dec%202013)
(it takes time to craft good ideas!), and this will not be an exhaustive recap.  Instead, we'll summarise
them here and point to our [papers](http://www.cl.cam.ac.uk/projects/ocamllabs/papers/index.html) that describe
the work in detail.

* The [MirageOS](http://openmirage.org) really found its own feet in 2014, with a 
  [summer 2.0 release](http://openmirage.org/blog/announcing-mirage-20-release) and
  an extensive [end-of-year recap](http://openmirage.org/blog/2014-in-review).
  The most notable thing has been how well the MirageOS research work has melded
  with the core OCaml Labs efforts, since much of it has been constructing good
  quality OCaml libraries to plug holes in the ecosystem.  It also served to make
  us use OPAM on a day-to-day basis for our own work, thus creating an effective
  feedback loop between open-source and research.

* In the [Trilogy2](http://trilogy2.it.uc3m.es/) and [UCN](http://usercentricnetworking.eu/)
  EU projects, we built out MirageOS features such as the
  [Jitsu](http://anil.recoil.org/papers/2015-nsdi-jitsu.pdf) toolstack for the
  "just-in-time" summoning of unikernels in response to DNS requests.  This paper
  will be presented next month at UlSENIX [NSDI](https://www.usenix.org/conference/nsdi15/).
  It also drove the development of the [ARMv7 port](http://openmirage.org/blog/introducing-xen-minios-arm),
  an architecture for which OCaml has an excellent native code generator, as well
  as more experimental forays into [BitCoin incentive schemes](http://arxiv.org/abs/1412.4638)
  for distributed systems.  

<iframe style="float:right; padding-left: 15px" width="200" height="150" src="https://www.youtube.com/embed/DSzvFwIVm5s?rel=0" frameborder="0" allowfullscreen></iframe>

* The [Irmin](http://irmin.io) Git-like branchable store created by Thomas Gazagnaire matured,
  with Dave Scott [prototyping](https://www.youtube.com/watch?v=DSzvFwIVm5s) a complex
  port of the [XenStore](http://wiki.xen.org/wiki/XenStore) database to Irmin, thus 
  letting us show off [debugging systems with Git](http://decks.openmirage.org/xendevsummit14#/).
  We had a paper accepted on some early datastructures accepted at [JFLA](http://anil.recoil.org/papers/2015-jfla-irmin.pdf),
  and Thomas Leonard is building the JavaScript backend for running in-browser,
  while Yan Schvartzshnaider is experimenting with [graph processing](http://yansnotes.blogspot.co.uk/2015/01/work-summary-ocaml-labs.html) over the DAG representation for privacy-friendly queries.
  KC is investigating how to adapt his PLDI 2015 paper on [Quelea](http://kcsrk.info/papers/quelea_pldi15.pdf) into using
  Irmin as a backend as well.


* The [Higher](https://github.com/ocamllabs/higher) kinded polymorphism library written
  by Jeremy Yallop and Leo White was published in [FLOPS 2014](http://www.lpw25.net/flops2014.pdf),
  forming a basis for building more complex use-cases that need the flexibility of higher
  kinded types without requiring functorising code.

<a href="{{site.url}}/images/ocl14rev/scotty.jpg"><img style="width:160px; padding-left: 15px; float:right;" src="{{site.url}}/images/ocl14rev/scotty-thumb.jpg" /></a>

Our long standing research into [personal online privacy](http://nymote.org) 
  led to our next system target that uses unikernels: the [Databox](http://arxiv.org/abs/1501.04737) paper
  outlines the architecture, and was covered in the [Guardian](http://www.theguardian.com/technology/2015/feb/01/control-personal-data-databox-end-user-agreement) newspaper.  Jon Crowcroft led the establishment of the Cambridge wing of the [Microsoft Cloud Computing Research Center](http://www.mccrc.eu/about-us) to consider the legal aspect of things, and so we have made forays outside of technology into considering the implications of [region-specific clouds](http://www.cl.cam.ac.uk/techreports/UCAM-CL-TR-863.pdf) as well.

Some of the most exciting work done in the group as part of the [REMS](http://rems.io) 
and [NaaS](http://www.naas-project.org/) projects came towards the end of 2014 and start of 2015, with
multiple submissions going into top conferences.  Unfortunately, due to most of them
being double blind reviewed, we cannot link to the papers yet.  Keep an eye on the
blog and [published paper set](http://www.cl.cam.ac.uk/projects/ocamllabs/papers/index.html),
or ask us directly about what's been going on!

## Priorities for 2015

As spring breaks and the weather (almost) becomes bearable again, we're setting our work priorities
for the remainder of the year. 

* **Tooling Cohesion**: The entire core team is focussed on fusing together
  the individual tools that have been created last year into a cohesive OCaml Platform release that
  covers the lifecycle of documentation, testing and build.
  This is being managed by Amir Chaudhry.  OPAM remains at the heart of this strategy,
  and Louis Gesbert and Thomas Gazagnaire have settled on the 
  [OPAM 1.3 roadmap](https://github.com/ocaml/opam/wiki/1.3-Roadmap) ([summary](http://lists.ocaml.org/pipermail/opam-devel/2015-February/000940.html)).

* **Multicore**:  [KC Sivaramakrishnan](kcsrk.info) has joined the core OCaml Labs
  fulltime to drive the multicore work into a publically testable form. Leo White recently 
  departed after many productive years in Cambridge to head into a career in industry 
  (but still remains very much involved with OCaml development!).

* **Language Evolution**: Jeremy Yallop continues to drive our efforts on staged programming,
  modular implicits, and a macro system for OCaml, all of which are key features that
  make building complex, reliable systems more tractable than ever.

I'd like to thank the [entire team](http://www.cl.cam.ac.uk/projects/ocamllabs/people/index.html)
and wider community for a wonderfully enjoyable 2014 and start of 2015, and am very thankful
to the funding and support from Jane Street, Citrix, British Telecom, RCUK, EPSRC, DARPA and
the EU FP7 that made it all possible.
As always, please feel free to contact any of us directly with questions, or reach out to
me [personally](mailto:avsm2@cl.cam.ac.uk) with any queries, concerns or bars of chocolate
as encouragement.
