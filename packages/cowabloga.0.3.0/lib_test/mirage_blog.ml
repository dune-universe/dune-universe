open Mirage_people
open Cowabloga
open Date
open Blog

let entries = Entry.([
    { updated    = date (2010, 10, 11, 15, 0);
      authors    = [anil];
      subject    = "Self-hosting Mirage website";
      body       = "/blog/welcome.md";
      permalink  = "self-hosting-mirage-website";
    };
    { updated    = date (2011, 04, 11, 15, 0);
      authors    = [anil];
      subject    = "A Spring Wiki Cleaning";
      body       = "/blog/spring-cleaning.md";
      permalink  = "spring-cleaning";
    };
    { updated    = date (2011, 09, 29, 11, 10);
      authors    = [anil];
      subject    = "An Outing to CUFP 2011";
      body       = "/blog/an-outing-to-cufp.md";
      permalink  = "an-outing-to-cufp";
    };
    { updated    = date (2012, 02, 29, 11, 10);
      authors    = [mort];
      subject    = "Connected Cloud Control: OpenFlow in Mirage";
      body       = "/blog/announcing-mirage-openflow.md";
      permalink  = "announcing-mirage-openflow";
    };
    { updated    = date (2012, 9, 12, 0, 0);
      authors    = [dave];
      subject    = "Building a \"xenstore stub domain\" with Mirage";
      body       = "/blog/xenstore-stub.md";
      permalink  = "xenstore-stub-domain";
    };
    { updated    = date (2012, 10, 17, 17, 30);
      authors    = [anil];
      subject    = "Breaking up is easy to do (with OPAM)";
      body       = "/blog/breaking-up-with-opam.md";
      permalink  = "breaking-up-is-easy-with-opam";
    };
    { updated    = date (2013, 05, 20, 16, 20);
      authors    = [anil];
      subject    = "The road to a developer preview at OSCON 2013";
      body       = "/blog/the-road-to-a-dev-release.md";
      permalink  = "the-road-to-a-dev-release";
    };
    { updated    = date (2013, 07, 18, 11, 20);
      authors    = [dave];
      subject    = "Creating Xen block devices with Mirage";
      body       = "/blog/xen-block-devices-with-mirage.md";
      permalink  = "xen-block-devices-with-mirage";
    };
  ])
