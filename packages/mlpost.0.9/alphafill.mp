% taken from http://wwwmathlabo.univ-poitiers.fr/~phan/metalpha.html
%
picture alphapict_; alphapict_=nullpicture;
color fillcolor; fillcolor=red;
fgalpha := 0.5; % usual alpha parameter
bgalpha:= 1; % alpha parameter with respect to the background

vardef alphafill expr c =
  alphapict_ := nullpicture;
  alphafill_(currentpicture, c);
  addto currentpicture also alphapict_;
enddef;

def alphafill_(expr p, c) =
  begingroup
    save p_, xmax_, xmin_, ymax_, ymin_; picture p_;
    p_ = nullpicture;
    (xmin_, ymin_) = llcorner c; (xmax_, ymax_) = urcorner c;
    addto p_ contour c withcolor bgalpha[background, fillcolor];
    for p__ within p:
      numeric xmin__, xmax__, ymin__, ymax__;
      (xmin__, ymin__) = llcorner p__; (xmax__, ymax__) = urcorner p__;
      if (xmax__<= xmin_) or (xmin__ >= xmax_):
      else:
	if (ymax__<= ymin_) or (ymin__ >= ymax_):
	else:
	  if (not clipped p__) and (not bounded p__):
	    addto p_ also p__ withcolor
	    fgalpha[(redpart p__, greenpart p__, bluepart p__),
	    fillcolor];
	  else:
	    begingroup save alphapict_;
	      picture alphapict_; alphapict_ = nullpicture;
	      alphafill_(p__, pathpart p__);
	      addto p_ also alphapict_;
	    endgroup;
	  fi
	fi
      fi
    endfor
    clip p_ to c;
    addto alphapict_ also p_;
  endgroup;
enddef;

