(* Some file for which we'd like to compute a swhid , it's taken from FreeDink *)
let content =
  "/**\n\
  \ * Base class for Tar and Bzip\n\n\
  \ * Copyright (C) 2004  Andrew Reading\n\
  \ * Copyright (C) 2005, 2006  Dan Walma\n\
  \ * Copyright (C) 2008  Sylvain Beucler\n\n\
  \ * This file is part of GNU FreeDink\n\n\
  \ * GNU FreeDink is free software; you can redistribute it and/or\n\
  \ * modify it under the terms of the GNU General Public License as\n\
  \ * published by the Free Software Foundation; either version 3 of the\n\
  \ * License, or (at your option) any later version.\n\n\
  \ * GNU FreeDink is distributed in the hope that it will be useful, but\n\
  \ * WITHOUT ANY WARRANTY; without even the implied warranty of\n\
  \ * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU\n\
  \ * General Public License for more details.\n\n\
  \ * You should have received a copy of the GNU General Public License\n\
  \ * along with this program.  If not, see\n\
  \ * <http://www.gnu.org/licenses/>.\n\
  \ */\n\n\
   #include \"DFile.hpp\"\n\n\
   #include <wx/filename.h>\n\n\
   DFile::DFile(wxString &szFile)\n\
   {\n\
  \  if(szFile.IsEmpty())\n\
  \    {\n\
  \      mFilePath = wxFileName::CreateTempFileName(_T(\"DFArc2\"));\n\
  \    }\n\
  \  else\n\
  \    {\n\
  \      mFilePath = szFile;\n\
  \    }\n\
   } \n"

let swhid = Swhid.Compute.content_identifier content

let () =
  match swhid with
  | None -> Format.eprintf "invalid ID :S@."
  | Some swhid -> Format.printf "ID is: `%a`@." Swhid.Pp.identifier swhid
