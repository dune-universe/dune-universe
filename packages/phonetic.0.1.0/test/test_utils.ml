type data =
  { input : string
  ; double_metaphone : string
  ; soundex : string
  }

let data =
  [ { input = "ALLERTON" ; double_metaphone = "ALRT" ; soundex = "A463" }
  ; { input = "Acton" ; double_metaphone = "AKTN" ; soundex = "A235" }
  ; { input = "Adams" ; double_metaphone = "ATMS" ; soundex = "A352" }
  ; { input = "Aggar" ; double_metaphone = "AKR" ; soundex = "A260" }
  ; { input = "Ahl" ; double_metaphone = "AL" ; soundex = "A400" }
  ; { input = "Aiken" ; double_metaphone = "AKN" ; soundex = "A250" }
  ; { input = "Alan" ; double_metaphone = "ALN" ; soundex = "A450" }
  ; { input = "Alcock" ; double_metaphone = "ALKK" ; soundex = "A422" }
  ; { input = "Alden" ; double_metaphone = "ALTN" ; soundex = "A435" }
  ; { input = "Aldham" ; double_metaphone = "ALTM" ; soundex = "A435" }
  ; { input = "Allen" ; double_metaphone = "ALN" ; soundex = "A450" }
  ; { input = "Allerton" ; double_metaphone = "ALRT" ; soundex = "A463" }
  ; { input = "Alsop" ; double_metaphone = "ALSP" ; soundex = "A421" }
  ; { input = "Alwein" ; double_metaphone = "ALN" ; soundex = "A450" }
  ; { input = "Ambler" ; double_metaphone = "AMPL" ; soundex = "A514" }
  ; { input = "Andevill" ; double_metaphone = "ANTF" ; soundex = "A531" }
  ; { input = "Andrews" ; double_metaphone = "ANTR" ; soundex = "A536" }
  ; { input = "Andreyco" ; double_metaphone = "ANTR" ; soundex = "A536" }
  ; { input = "Andriesse" ; double_metaphone = "ANTR" ; soundex = "A536" }
  ; { input = "Angier" ; double_metaphone = "ANJ" ; soundex = "A526" }
  ; { input = "Annabel" ; double_metaphone = "ANPL" ; soundex = "A514" }
  ; { input = "Anne" ; double_metaphone = "AN" ; soundex = "A500" }
  ; { input = "Anstye" ; double_metaphone = "ANST" ; soundex = "A523" }
  ; { input = "Appling" ; double_metaphone = "APLN" ; soundex = "A145" }
  ; { input = "Apuke" ; double_metaphone = "APK" ; soundex = "A120" }
  ; { input = "Arnold" ; double_metaphone = "ARNL" ; soundex = "A654" }
  ; { input = "Ashby" ; double_metaphone = "AXP" ; soundex = "A210" }
  ; { input = "Astwood" ; double_metaphone = "ASTT" ; soundex = "A233" }
  ; { input = "Atkinson" ; double_metaphone = "ATKN" ; soundex = "A325" }
  ; { input = "Audley" ; double_metaphone = "ATL" ; soundex = "A340" }
  ; { input = "Austin" ; double_metaphone = "ASTN" ; soundex = "A235" }
  ; { input = "Avenal" ; double_metaphone = "AFNL" ; soundex = "A154" }
  ; { input = "Ayer" ; double_metaphone = "AR" ; soundex = "A600" }
  ; { input = "Ayot" ; double_metaphone = "AT" ; soundex = "A300" }
  ; { input = "Babbitt" ; double_metaphone = "PPT" ; soundex = "" }
  ; { input = "Bachelor" ; double_metaphone = "PXLR" ; soundex = "" }
  ; { input = "Bachelour" ; double_metaphone = "PXLR" ; soundex = "" }
  ; { input = "Bailey" ; double_metaphone = "PL" ; soundex = "" }
  ; { input = "Baivel" ; double_metaphone = "PFL" ; soundex = "" }
  ; { input = "Baker" ; double_metaphone = "PKR" ; soundex = "" }
  ; { input = "Baldwin" ; double_metaphone = "PLTN" ; soundex = "" }
  ; { input = "Balsley" ; double_metaphone = "PLSL" ; soundex = "" }
  ; { input = "Barber" ; double_metaphone = "PRPR" ; soundex = "" }
  ; { input = "Barker" ; double_metaphone = "PRKR" ; soundex = "" }
  ; { input = "Barlow" ; double_metaphone = "PRL" ; soundex = "" }
  ; { input = "Barnard" ; double_metaphone = "PRNR" ; soundex = "" }
  ; { input = "Barnes" ; double_metaphone = "PRNS" ; soundex = "" }
  ; { input = "Barnsley" ; double_metaphone = "PRNS" ; soundex = "" }
  ; { input = "Barouxis" ; double_metaphone = "PRKS" ; soundex = "" }
  ; { input = "Bartlet" ; double_metaphone = "PRTL" ; soundex = "" }
  ; { input = "Basley" ; double_metaphone = "PSL" ; soundex = "" }
  ; { input = "Basset" ; double_metaphone = "PST" ; soundex = "" }
  ; { input = "Bassett" ; double_metaphone = "PST" ; soundex = "" }
  ; { input = "Batchlor" ; double_metaphone = "PXLR" ; soundex = "" }
  ; { input = "Bates" ; double_metaphone = "PTS" ; soundex = "" }
  ; { input = "Batson" ; double_metaphone = "PTSN" ; soundex = "" }
  ; { input = "Bayes" ; double_metaphone = "PS" ; soundex = "" }
  ; { input = "Bayley" ; double_metaphone = "PL" ; soundex = "" }
  ; { input = "Beale" ; double_metaphone = "PL" ; soundex = "" }
  ; { input = "Beauchamp" ; double_metaphone = "PXMP" ; soundex = "" }
  ; { input = "Beauclerc" ; double_metaphone = "PKLR" ; soundex = "" }
  ; { input = "Beech" ; double_metaphone = "PK" ; soundex = "" }
  ; { input = "Beers" ; double_metaphone = "PRS" ; soundex = "" }
  ; { input = "Beke" ; double_metaphone = "PK" ; soundex = "" }
  ; { input = "Belcher" ; double_metaphone = "PLXR" ; soundex = "" }
  ; { input = "Benjamin" ; double_metaphone = "PNJM" ; soundex = "" }
  ; { input = "Benningham" ; double_metaphone = "PNNK" ; soundex = "" }
  ; { input = "Bereford" ; double_metaphone = "PRFR" ; soundex = "" }
  ; { input = "Bergen" ; double_metaphone = "PRJN" ; soundex = "" }
  ; { input = "Berkeley" ; double_metaphone = "PRKL" ; soundex = "" }
  ; { input = "Berry" ; double_metaphone = "PR" ; soundex = "" }
  ; { input = "Besse" ; double_metaphone = "PS" ; soundex = "" }
  ; { input = "Bessey" ; double_metaphone = "PS" ; soundex = "" }
  ; { input = "Bessiles" ; double_metaphone = "PSLS" ; soundex = "" }
  ; { input = "Bigelow" ; double_metaphone = "PJL" ; soundex = "" }
  ; { input = "Bigg" ; double_metaphone = "PK" ; soundex = "" }
  ; { input = "Bigod" ; double_metaphone = "PKT" ; soundex = "" }
  ; { input = "Billings" ; double_metaphone = "PLNK" ; soundex = "" }
  ; { input = "Bimper" ; double_metaphone = "PMPR" ; soundex = "" }
  ; { input = "Binker" ; double_metaphone = "PNKR" ; soundex = "" }
  ; { input = "Birdsill" ; double_metaphone = "PRTS" ; soundex = "" }
  ; { input = "Bishop" ; double_metaphone = "PXP" ; soundex = "" }
  ; { input = "Black" ; double_metaphone = "PLK" ; soundex = "" }
  ; { input = "Blagge" ; double_metaphone = "PLK" ; soundex = "" }
  ; { input = "Blake" ; double_metaphone = "PLK" ; soundex = "" }
  ; { input = "Blanck" ; double_metaphone = "PLNK" ; soundex = "" }
  ; { input = "Bledsoe" ; double_metaphone = "PLTS" ; soundex = "" }
  ; { input = "Blennerhasset" ; double_metaphone = "PLNR" ; soundex = "" }
  ; { input = "Blessing" ; double_metaphone = "PLSN" ; soundex = "" }
  ; { input = "Blewett" ; double_metaphone = "PLT" ; soundex = "" }
  ; { input = "Bloctgoed" ; double_metaphone = "PLKT" ; soundex = "" }
  ; { input = "Bloetgoet" ; double_metaphone = "PLTK" ; soundex = "" }
  ; { input = "Bloodgood" ; double_metaphone = "PLTK" ; soundex = "" }
  ; { input = "Blossom" ; double_metaphone = "PLSM" ; soundex = "" }
  ; { input = "Blount" ; double_metaphone = "PLNT" ; soundex = "" }
  ; { input = "Bodine" ; double_metaphone = "PTN" ; soundex = "" }
  ; { input = "Bodman" ; double_metaphone = "PTMN" ; soundex = "" }
  ; { input = "BonCoeur" ; double_metaphone = "PNKR" ; soundex = "" }
  ; { input = "Bond" ; double_metaphone = "PNT" ; soundex = "" }
  ; { input = "Boscawen" ; double_metaphone = "PSKN" ; soundex = "" }
  ; { input = "Bosworth" ; double_metaphone = "PSR0" ; soundex = "" }
  ; { input = "Bouchier" ; double_metaphone = "PX" ; soundex = "" }
  ; { input = "Bowne" ; double_metaphone = "PN" ; soundex = "" }
  ; { input = "Bradbury" ; double_metaphone = "PRTP" ; soundex = "" }
  ; { input = "Bradder" ; double_metaphone = "PRTR" ; soundex = "" }
  ; { input = "Bradford" ; double_metaphone = "PRTF" ; soundex = "" }
  ; { input = "Bradstreet" ; double_metaphone = "PRTS" ; soundex = "" }
  ; { input = "Braham" ; double_metaphone = "PRHM" ; soundex = "" }
  ; { input = "Brailsford" ; double_metaphone = "PRLS" ; soundex = "" }
  ; { input = "Brainard" ; double_metaphone = "PRNR" ; soundex = "" }
  ; { input = "Brandish" ; double_metaphone = "PRNT" ; soundex = "" }
  ; { input = "Braun" ; double_metaphone = "PRN" ; soundex = "" }
  ; { input = "Brecc" ; double_metaphone = "PRK" ; soundex = "" }
  ; { input = "Brent" ; double_metaphone = "PRNT" ; soundex = "" }
  ; { input = "Brenton" ; double_metaphone = "PRNT" ; soundex = "" }
  ; { input = "Briggs" ; double_metaphone = "PRKS" ; soundex = "" }
  ; { input = "Brigham" ; double_metaphone = "PRM" ; soundex = "" }
  ; { input = "Brobst" ; double_metaphone = "PRPS" ; soundex = "" }
  ; { input = "Brome" ; double_metaphone = "PRM" ; soundex = "" }
  ; { input = "Bronson" ; double_metaphone = "PRNS" ; soundex = "" }
  ; { input = "Brooks" ; double_metaphone = "PRKS" ; soundex = "" }
  ; { input = "Brouillard" ; double_metaphone = "PRLR" ; soundex = "" }
  ; { input = "Brown" ; double_metaphone = "PRN" ; soundex = "" }
  ; { input = "Browne" ; double_metaphone = "PRN" ; soundex = "" }
  ; { input = "Brownell" ; double_metaphone = "PRNL" ; soundex = "" }
  ; { input = "Bruley" ; double_metaphone = "PRL" ; soundex = "" }
  ; { input = "Bryant" ; double_metaphone = "PRNT" ; soundex = "" }
  ; { input = "Brzozowski" ; double_metaphone = "PRSS" ; soundex = "" }
  ; { input = "Buide" ; double_metaphone = "PT" ; soundex = "" }
  ; { input = "Bulmer" ; double_metaphone = "PLMR" ; soundex = "" }
  ; { input = "Bunker" ; double_metaphone = "PNKR" ; soundex = "" }
  ; { input = "Burden" ; double_metaphone = "PRTN" ; soundex = "" }
  ; { input = "Burge" ; double_metaphone = "PRJ" ; soundex = "" }
  ; { input = "Burgoyne" ; double_metaphone = "PRKN" ; soundex = "" }
  ; { input = "Burke" ; double_metaphone = "PRK" ; soundex = "" }
  ; { input = "Burnett" ; double_metaphone = "PRNT" ; soundex = "" }
  ; { input = "Burpee" ; double_metaphone = "PRP" ; soundex = "" }
  ; { input = "Bursley" ; double_metaphone = "PRSL" ; soundex = "" }
  ; { input = "Burton" ; double_metaphone = "PRTN" ; soundex = "" }
  ; { input = "Bushnell" ; double_metaphone = "PXNL" ; soundex = "" }
  ; { input = "Buss" ; double_metaphone = "PS" ; soundex = "" }
  ; { input = "Buswell" ; double_metaphone = "PSL" ; soundex = "" }
  ; { input = "Butler" ; double_metaphone = "PTLR" ; soundex = "" }
  ; { input = "Calkin" ; double_metaphone = "KLKN" ; soundex = "" }
  ; { input = "Canada" ; double_metaphone = "KNT" ; soundex = "" }
  ; { input = "Canmore" ; double_metaphone = "KNMR" ; soundex = "" }
  ; { input = "Canney" ; double_metaphone = "KN" ; soundex = "" }
  ; { input = "Capet" ; double_metaphone = "KPT" ; soundex = "" }
  ; { input = "Card" ; double_metaphone = "KRT" ; soundex = "" }
  ; { input = "Carman" ; double_metaphone = "KRMN" ; soundex = "" }
  ; { input = "Carpenter" ; double_metaphone = "KRPN" ; soundex = "" }
  ; { input = "Cartwright" ; double_metaphone = "KRTR" ; soundex = "" }
  ; { input = "Casey" ; double_metaphone = "KS" ; soundex = "" }
  ; { input = "Catterfield" ; double_metaphone = "KTRF" ; soundex = "" }
  ; { input = "Ceeley" ; double_metaphone = "SL" ; soundex = "" }
  ; { input = "Chambers" ; double_metaphone = "XMPR" ; soundex = "" }
  ; { input = "Champion" ; double_metaphone = "XMPN" ; soundex = "" }
  ; { input = "Chapman" ; double_metaphone = "XPMN" ; soundex = "" }
  ; { input = "Chase" ; double_metaphone = "XS" ; soundex = "" }
  ; { input = "Cheney" ; double_metaphone = "XN" ; soundex = "" }
  ; { input = "Chetwynd" ; double_metaphone = "XTNT" ; soundex = "" }
  ; { input = "Chevalier" ; double_metaphone = "XFL" ; soundex = "" }
  ; { input = "Chillingsworth" ; double_metaphone = "XLNK" ; soundex = "" }
  ; { input = "Christie" ; double_metaphone = "KRST" ; soundex = "" }
  ; { input = "Chubbuck" ; double_metaphone = "XPK" ; soundex = "" }
  ; { input = "Church" ; double_metaphone = "XRX" ; soundex = "" }
  ; { input = "Clark" ; double_metaphone = "KLRK" ; soundex = "" }
  ; { input = "Clarke" ; double_metaphone = "KLRK" ; soundex = "" }
  ; { input = "Cleare" ; double_metaphone = "KLR" ; soundex = "" }
  ; { input = "Clement" ; double_metaphone = "KLMN" ; soundex = "" }
  ; { input = "Clerke" ; double_metaphone = "KLRK" ; soundex = "" }
  ; { input = "Clibben" ; double_metaphone = "KLPN" ; soundex = "" }
  ; { input = "Clifford" ; double_metaphone = "KLFR" ; soundex = "" }
  ; { input = "Clivedon" ; double_metaphone = "KLFT" ; soundex = "" }
  ; { input = "Close" ; double_metaphone = "KLS" ; soundex = "" }
  ; { input = "Clothilde" ; double_metaphone = "KL0L" ; soundex = "" }
  ; { input = "Cobb" ; double_metaphone = "KP" ; soundex = "" }
  ; { input = "Coburn" ; double_metaphone = "KPRN" ; soundex = "" }
  ; { input = "Coburne" ; double_metaphone = "KPRN" ; soundex = "" }
  ; { input = "Cocke" ; double_metaphone = "KK" ; soundex = "" }
  ; { input = "Coffin" ; double_metaphone = "KFN" ; soundex = "" }
  ; { input = "Coffyn" ; double_metaphone = "KFN" ; soundex = "" }
  ; { input = "Colborne" ; double_metaphone = "KLPR" ; soundex = "" }
  ; { input = "Colby" ; double_metaphone = "KLP" ; soundex = "" }
  ; { input = "Cole" ; double_metaphone = "KL" ; soundex = "" }
  ; { input = "Coleman" ; double_metaphone = "KLMN" ; soundex = "" }
  ; { input = "Collier" ; double_metaphone = "KL" ; soundex = "" }
  ; { input = "Compton" ; double_metaphone = "KMPT" ; soundex = "" }
  ; { input = "Cone" ; double_metaphone = "KN" ; soundex = "" }
  ; { input = "Cook" ; double_metaphone = "KK" ; soundex = "" }
  ; { input = "Cooke" ; double_metaphone = "KK" ; soundex = "" }
  ; { input = "Cooper" ; double_metaphone = "KPR" ; soundex = "" }
  ; { input = "Copperthwaite" ; double_metaphone = "KPR0" ; soundex = "" }
  ; { input = "Corbet" ; double_metaphone = "KRPT" ; soundex = "" }
  ; { input = "Corell" ; double_metaphone = "KRL" ; soundex = "" }
  ; { input = "Corey" ; double_metaphone = "KR" ; soundex = "" }
  ; { input = "Corlies" ; double_metaphone = "KRLS" ; soundex = "" }
  ; { input = "Corneliszen" ; double_metaphone = "KRNL" ; soundex = "" }
  ; { input = "Cornelius" ; double_metaphone = "KRNL" ; soundex = "" }
  ; { input = "Cornwallis" ; double_metaphone = "KRNL" ; soundex = "" }
  ; { input = "Cosgrove" ; double_metaphone = "KSKR" ; soundex = "" }
  ; { input = "Count of Brionne" ; double_metaphone = "KNTF" ; soundex = "" }
  ; { input = "Covill" ; double_metaphone = "KFL" ; soundex = "" }
  ; { input = "Cowperthwaite" ; double_metaphone = "KPR0" ; soundex = "" }
  ; { input = "Cowperwaite" ; double_metaphone = "KPRT" ; soundex = "" }
  ; { input = "Crane" ; double_metaphone = "KRN" ; soundex = "" }
  ; { input = "Creagmile" ; double_metaphone = "KRKM" ; soundex = "" }
  ; { input = "Crew" ; double_metaphone = "KR" ; soundex = "" }
  ; { input = "Crispin" ; double_metaphone = "KRSP" ; soundex = "" }
  ; { input = "Crocker" ; double_metaphone = "KRKR" ; soundex = "" }
  ; { input = "Crockett" ; double_metaphone = "KRKT" ; soundex = "" }
  ; { input = "Crosby" ; double_metaphone = "KRSP" ; soundex = "" }
  ; { input = "Crump" ; double_metaphone = "KRMP" ; soundex = "" }
  ; { input = "Cunningham" ; double_metaphone = "KNNK" ; soundex = "" }
  ; { input = "Curtis" ; double_metaphone = "KRTS" ; soundex = "" }
  ; { input = "Cutha" ; double_metaphone = "K0" ; soundex = "" }
  ; { input = "Cutter" ; double_metaphone = "KTR" ; soundex = "" }
  ; { input = "D'Aubigny" ; double_metaphone = "TPN" ; soundex = "" }
  ; { input = "DAVIS" ; double_metaphone = "TFS" ; soundex = "" }
  ; { input = "Dabinott" ; double_metaphone = "TPNT" ; soundex = "" }
  ; { input = "Dacre" ; double_metaphone = "TKR" ; soundex = "" }
  ; { input = "Daggett" ; double_metaphone = "TKT" ; soundex = "" }
  ; { input = "Danvers" ; double_metaphone = "TNFR" ; soundex = "" }
  ; { input = "Darcy" ; double_metaphone = "TRS" ; soundex = "" }
  ; { input = "Davis" ; double_metaphone = "TFS" ; soundex = "" }
  ; { input = "Dawn" ; double_metaphone = "TN" ; soundex = "" }
  ; { input = "Dawson" ; double_metaphone = "TSN" ; soundex = "" }
  ; { input = "Day" ; double_metaphone = "T" ; soundex = "" }
  ; { input = "Daye" ; double_metaphone = "T" ; soundex = "" }
  ; { input = "DeGrenier" ; double_metaphone = "TKRN" ; soundex = "" }
  ; { input = "Dean" ; double_metaphone = "TN" ; soundex = "" }
  ; { input = "Deekindaugh" ; double_metaphone = "TKNT" ; soundex = "" }
  ; { input = "Dennis" ; double_metaphone = "TNS" ; soundex = "" }
  ; { input = "Denny" ; double_metaphone = "TN" ; soundex = "" }
  ; { input = "Denton" ; double_metaphone = "TNTN" ; soundex = "" }
  ; { input = "Desborough" ; double_metaphone = "TSPR" ; soundex = "" }
  ; { input = "Despenser" ; double_metaphone = "TSPN" ; soundex = "" }
  ; { input = "Deverill" ; double_metaphone = "TFRL" ; soundex = "" }
  ; { input = "Devine" ; double_metaphone = "TFN" ; soundex = "" }
  ; { input = "Dexter" ; double_metaphone = "TKST" ; soundex = "" }
  ; { input = "Dillaway" ; double_metaphone = "TL" ; soundex = "" }
  ; { input = "Dimmick" ; double_metaphone = "TMK" ; soundex = "" }
  ; { input = "Dinan" ; double_metaphone = "TNN" ; soundex = "" }
  ; { input = "Dix" ; double_metaphone = "TKS" ; soundex = "" }
  ; { input = "Doggett" ; double_metaphone = "TKT" ; soundex = "" }
  ; { input = "Donahue" ; double_metaphone = "TNH" ; soundex = "" }
  ; { input = "Dorfman" ; double_metaphone = "TRFM" ; soundex = "" }
  ; { input = "Dorris" ; double_metaphone = "TRS" ; soundex = "" }
  ; { input = "Dow" ; double_metaphone = "T" ; soundex = "" }
  ; { input = "Downey" ; double_metaphone = "TN" ; soundex = "" }
  ; { input = "Downing" ; double_metaphone = "TNNK" ; soundex = "" }
  ; { input = "Dowsett" ; double_metaphone = "TST" ; soundex = "" }
  ; { input = "Duck?" ; double_metaphone = "TK" ; soundex = "" }
  ; { input = "Dudley" ; double_metaphone = "TTL" ; soundex = "" }
  ; { input = "Duffy" ; double_metaphone = "TF" ; soundex = "" }
  ; { input = "Dunn" ; double_metaphone = "TN" ; soundex = "" }
  ; { input = "Dunsterville" ; double_metaphone = "TNST" ; soundex = "" }
  ; { input = "Durrant" ; double_metaphone = "TRNT" ; soundex = "" }
  ; { input = "Durrin" ; double_metaphone = "TRN" ; soundex = "" }
  ; { input = "Dustin" ; double_metaphone = "TSTN" ; soundex = "" }
  ; { input = "Duston" ; double_metaphone = "TSTN" ; soundex = "" }
  ; { input = "Eames" ; double_metaphone = "AMS" ; soundex = "" }
  ; { input = "Early" ; double_metaphone = "ARL" ; soundex = "" }
  ; { input = "Easty" ; double_metaphone = "AST" ; soundex = "" }
  ; { input = "Ebbett" ; double_metaphone = "APT" ; soundex = "" }
  ; { input = "Eberbach" ; double_metaphone = "APRP" ; soundex = "" }
  ; { input = "Eberhard" ; double_metaphone = "APRR" ; soundex = "" }
  ; { input = "Eddy" ; double_metaphone = "AT" ; soundex = "" }
  ; { input = "Edenden" ; double_metaphone = "ATNT" ; soundex = "" }
  ; { input = "Edwards" ; double_metaphone = "ATRT" ; soundex = "" }
  ; { input = "Eglinton" ; double_metaphone = "AKLN" ; soundex = "" }
  ; { input = "Eliot" ; double_metaphone = "ALT" ; soundex = "" }
  ; { input = "Elizabeth" ; double_metaphone = "ALSP" ; soundex = "" }
  ; { input = "Ellis" ; double_metaphone = "ALS" ; soundex = "" }
  ; { input = "Ellison" ; double_metaphone = "ALSN" ; soundex = "" }
  ; { input = "Ellot" ; double_metaphone = "ALT" ; soundex = "" }
  ; { input = "Elny" ; double_metaphone = "ALN" ; soundex = "" }
  ; { input = "Elsner" ; double_metaphone = "ALSN" ; soundex = "" }
  ; { input = "Emerson" ; double_metaphone = "AMRS" ; soundex = "" }
  ; { input = "Empson" ; double_metaphone = "AMPS" ; soundex = "" }
  ; { input = "Est" ; double_metaphone = "AST" ; soundex = "" }
  ; { input = "Estabrook" ; double_metaphone = "ASTP" ; soundex = "" }
  ; { input = "Estes" ; double_metaphone = "ASTS" ; soundex = "" }
  ; { input = "Estey" ; double_metaphone = "AST" ; soundex = "" }
  ; { input = "Evans" ; double_metaphone = "AFNS" ; soundex = "" }
  ; { input = "Fallowell" ; double_metaphone = "FLL" ; soundex = "" }
  ; { input = "Farnsworth" ; double_metaphone = "FRNS" ; soundex = "" }
  ; { input = "Feake" ; double_metaphone = "FK" ; soundex = "" }
  ; { input = "Feke" ; double_metaphone = "FK" ; soundex = "" }
  ; { input = "Fellows" ; double_metaphone = "FLS" ; soundex = "" }
  ; { input = "Fettiplace" ; double_metaphone = "FTPL" ; soundex = "" }
  ; { input = "Finney" ; double_metaphone = "FN" ; soundex = "" }
  ; { input = "Fischer" ; double_metaphone = "FXR" ; soundex = "" }
  ; { input = "Fisher" ; double_metaphone = "FXR" ; soundex = "" }
  ; { input = "Fisk" ; double_metaphone = "FSK" ; soundex = "" }
  ; { input = "Fiske" ; double_metaphone = "FSK" ; soundex = "" }
  ; { input = "Fletcher" ; double_metaphone = "FLXR" ; soundex = "" }
  ; { input = "Folger" ; double_metaphone = "FLKR" ; soundex = "" }
  ; { input = "Foliot" ; double_metaphone = "FLT" ; soundex = "" }
  ; { input = "Folyot" ; double_metaphone = "FLT" ; soundex = "" }
  ; { input = "Fones" ; double_metaphone = "FNS" ; soundex = "" }
  ; { input = "Fordham" ; double_metaphone = "FRTM" ; soundex = "" }
  ; { input = "Forstner" ; double_metaphone = "FRST" ; soundex = "" }
  ; { input = "Fosten" ; double_metaphone = "FSTN" ; soundex = "" }
  ; { input = "Foster" ; double_metaphone = "FSTR" ; soundex = "" }
  ; { input = "Foulke" ; double_metaphone = "FLK" ; soundex = "" }
  ; { input = "Fowler" ; double_metaphone = "FLR" ; soundex = "" }
  ; { input = "Foxwell" ; double_metaphone = "FKSL" ; soundex = "" }
  ; { input = "Fraley" ; double_metaphone = "FRL" ; soundex = "" }
  ; { input = "Franceys" ; double_metaphone = "FRNS" ; soundex = "" }
  ; { input = "Franke" ; double_metaphone = "FRNK" ; soundex = "" }
  ; { input = "Frascella" ; double_metaphone = "FRSL" ; soundex = "" }
  ; { input = "Frazer" ; double_metaphone = "FRSR" ; soundex = "" }
  ; { input = "Fredd" ; double_metaphone = "FRT" ; soundex = "" }
  ; { input = "Freeman" ; double_metaphone = "FRMN" ; soundex = "" }
  ; { input = "French" ; double_metaphone = "FRNX" ; soundex = "" }
  ; { input = "Freville" ; double_metaphone = "FRFL" ; soundex = "" }
  ; { input = "Frey" ; double_metaphone = "FR" ; soundex = "" }
  ; { input = "Frick" ; double_metaphone = "FRK" ; soundex = "" }
  ; { input = "Frier" ; double_metaphone = "FR" ; soundex = "" }
  ; { input = "Froe" ; double_metaphone = "FR" ; soundex = "" }
  ; { input = "Frorer" ; double_metaphone = "FRRR" ; soundex = "" }
  ; { input = "Frost" ; double_metaphone = "FRST" ; soundex = "" }
  ; { input = "Frothingham" ; double_metaphone = "FR0N" ; soundex = "" }
  ; { input = "Fry" ; double_metaphone = "FR" ; soundex = "" }
  ; { input = "Gaffney" ; double_metaphone = "KFN" ; soundex = "" }
  ; { input = "Gage" ; double_metaphone = "KJ" ; soundex = "" }
  ; { input = "Gallion" ; double_metaphone = "KLN" ; soundex = "" }
  ; { input = "Gallishan" ; double_metaphone = "KLXN" ; soundex = "" }
  ; { input = "Gamble" ; double_metaphone = "KMPL" ; soundex = "" }
  ; { input = "Garbrand" ; double_metaphone = "KRPR" ; soundex = "" }
  ; { input = "Gardner" ; double_metaphone = "KRTN" ; soundex = "" }
  ; { input = "Garrett" ; double_metaphone = "KRT" ; soundex = "" }
  ; { input = "Gassner" ; double_metaphone = "KSNR" ; soundex = "" }
  ; { input = "Gater" ; double_metaphone = "KTR" ; soundex = "" }
  ; { input = "Gaunt" ; double_metaphone = "KNT" ; soundex = "" }
  ; { input = "Gayer" ; double_metaphone = "KR" ; soundex = "" }
  ; { input = "Gerken" ; double_metaphone = "KRKN" ; soundex = "" }
  ; { input = "Gerritsen" ; double_metaphone = "KRTS" ; soundex = "" }
  ; { input = "Gibbs" ; double_metaphone = "KPS" ; soundex = "" }
  ; { input = "Giffard" ; double_metaphone = "JFRT" ; soundex = "" }
  ; { input = "Gilbert" ; double_metaphone = "KLPR" ; soundex = "" }
  ; { input = "Gill" ; double_metaphone = "KL" ; soundex = "" }
  ; { input = "Gilman" ; double_metaphone = "KLMN" ; soundex = "" }
  ; { input = "Glass" ; double_metaphone = "KLS" ; soundex = "" }
  ; { input = "Goddard\\Gifford" ; double_metaphone = "KTRT" ; soundex = "" }
  ; { input = "Godfrey" ; double_metaphone = "KTFR" ; soundex = "" }
  ; { input = "Godwin" ; double_metaphone = "KTN" ; soundex = "" }
  ; { input = "Goodale" ; double_metaphone = "KTL" ; soundex = "" }
  ; { input = "Goodnow" ; double_metaphone = "KTN" ; soundex = "" }
  ; { input = "Gorham" ; double_metaphone = "KRM" ; soundex = "" }
  ; { input = "Goseline" ; double_metaphone = "KSLN" ; soundex = "" }
  ; { input = "Gott" ; double_metaphone = "KT" ; soundex = "" }
  ; { input = "Gould" ; double_metaphone = "KLT" ; soundex = "" }
  ; { input = "Grafton" ; double_metaphone = "KRFT" ; soundex = "" }
  ; { input = "Grant" ; double_metaphone = "KRNT" ; soundex = "" }
  ; { input = "Gray" ; double_metaphone = "KR" ; soundex = "" }
  ; { input = "Green" ; double_metaphone = "KRN" ; soundex = "" }
  ; { input = "Griffin" ; double_metaphone = "KRFN" ; soundex = "" }
  ; { input = "Grill" ; double_metaphone = "KRL" ; soundex = "" }
  ; { input = "Grim" ; double_metaphone = "KRM" ; soundex = "" }
  ; { input = "Grisgonelle" ; double_metaphone = "KRSK" ; soundex = "" }
  ; { input = "Gross" ; double_metaphone = "KRS" ; soundex = "" }
  ; { input = "Guba" ; double_metaphone = "KP" ; soundex = "" }
  ; { input = "Gybbes" ; double_metaphone = "KPS" ; soundex = "" }
  ; { input = "Haburne" ; double_metaphone = "HPRN" ; soundex = "" }
  ; { input = "Hackburne" ; double_metaphone = "HKPR" ; soundex = "" }
  ; { input = "Haddon?" ; double_metaphone = "HTN" ; soundex = "" }
  ; { input = "Haines" ; double_metaphone = "HNS" ; soundex = "" }
  ; { input = "Hale" ; double_metaphone = "HL" ; soundex = "" }
  ; { input = "Hall" ; double_metaphone = "HL" ; soundex = "" }
  ; { input = "Hallet" ; double_metaphone = "HLT" ; soundex = "" }
  ; { input = "Hallock" ; double_metaphone = "HLK" ; soundex = "" }
  ; { input = "Halstead" ; double_metaphone = "HLST" ; soundex = "" }
  ; { input = "Hammond" ; double_metaphone = "HMNT" ; soundex = "" }
  ; { input = "Hance" ; double_metaphone = "HNS" ; soundex = "" }
  ; { input = "Handy" ; double_metaphone = "HNT" ; soundex = "" }
  ; { input = "Hanson" ; double_metaphone = "HNSN" ; soundex = "" }
  ; { input = "Harasek" ; double_metaphone = "HRSK" ; soundex = "" }
  ; { input = "Harcourt" ; double_metaphone = "HRKR" ; soundex = "" }
  ; { input = "Hardy" ; double_metaphone = "HRT" ; soundex = "" }
  ; { input = "Harlock" ; double_metaphone = "HRLK" ; soundex = "" }
  ; { input = "Harris" ; double_metaphone = "HRS" ; soundex = "" }
  ; { input = "Hartley" ; double_metaphone = "HRTL" ; soundex = "" }
  ; { input = "Harvey" ; double_metaphone = "HRF" ; soundex = "" }
  ; { input = "Harvie" ; double_metaphone = "HRF" ; soundex = "" }
  ; { input = "Harwood" ; double_metaphone = "HRT" ; soundex = "" }
  ; { input = "Hathaway" ; double_metaphone = "H0" ; soundex = "" }
  ; { input = "Haukeness" ; double_metaphone = "HKNS" ; soundex = "" }
  ; { input = "Hawkes" ; double_metaphone = "HKS" ; soundex = "" }
  ; { input = "Hawkhurst" ; double_metaphone = "HKRS" ; soundex = "" }
  ; { input = "Hawkins" ; double_metaphone = "HKNS" ; soundex = "" }
  ; { input = "Hawley" ; double_metaphone = "HL" ; soundex = "" }
  ; { input = "Heald" ; double_metaphone = "HLT" ; soundex = "" }
  ; { input = "Helsdon" ; double_metaphone = "HLST" ; soundex = "" }
  ; { input = "Hemenway" ; double_metaphone = "HMN" ; soundex = "" }
  ; { input = "Hemmenway" ; double_metaphone = "HMN" ; soundex = "" }
  ; { input = "Henck" ; double_metaphone = "HNK" ; soundex = "" }
  ; { input = "Henderson" ; double_metaphone = "HNTR" ; soundex = "" }
  ; { input = "Hendricks" ; double_metaphone = "HNTR" ; soundex = "" }
  ; { input = "Hersey" ; double_metaphone = "HRS" ; soundex = "" }
  ; { input = "Hewes" ; double_metaphone = "HS" ; soundex = "" }
  ; { input = "Heyman" ; double_metaphone = "HMN" ; soundex = "" }
  ; { input = "Hicks" ; double_metaphone = "HKS" ; soundex = "" }
  ; { input = "Hidden" ; double_metaphone = "HTN" ; soundex = "" }
  ; { input = "Higgs" ; double_metaphone = "HKS" ; soundex = "" }
  ; { input = "Hill" ; double_metaphone = "HL" ; soundex = "" }
  ; { input = "Hills" ; double_metaphone = "HLS" ; soundex = "" }
  ; { input = "Hinckley" ; double_metaphone = "HNKL" ; soundex = "" }
  ; { input = "Hipwell" ; double_metaphone = "HPL" ; soundex = "" }
  ; { input = "Hobart" ; double_metaphone = "HPRT" ; soundex = "" }
  ; { input = "Hoben" ; double_metaphone = "HPN" ; soundex = "" }
  ; { input = "Hoffmann" ; double_metaphone = "HFMN" ; soundex = "" }
  ; { input = "Hogan" ; double_metaphone = "HKN" ; soundex = "" }
  ; { input = "Holmes" ; double_metaphone = "HLMS" ; soundex = "" }
  ; { input = "Hoo" ; double_metaphone = "H" ; soundex = "" }
  ; { input = "Hooker" ; double_metaphone = "HKR" ; soundex = "" }
  ; { input = "Hopcott" ; double_metaphone = "HPKT" ; soundex = "" }
  ; { input = "Hopkins" ; double_metaphone = "HPKN" ; soundex = "" }
  ; { input = "Hopkinson" ; double_metaphone = "HPKN" ; soundex = "" }
  ; { input = "Hornsey" ; double_metaphone = "HRNS" ; soundex = "" }
  ; { input = "Houckgeest" ; double_metaphone = "HKJS" ; soundex = "" }
  ; { input = "Hough" ; double_metaphone = "H" ; soundex = "" }
  ; { input = "Houstin" ; double_metaphone = "HSTN" ; soundex = "" }
  ; { input = "How" ; double_metaphone = "H" ; soundex = "" }
  ; { input = "Howe" ; double_metaphone = "H" ; soundex = "" }
  ; { input = "Howland" ; double_metaphone = "HLNT" ; soundex = "" }
  ; { input = "Hubner" ; double_metaphone = "HPNR" ; soundex = "" }
  ; { input = "Hudnut" ; double_metaphone = "HTNT" ; soundex = "" }
  ; { input = "Hughes" ; double_metaphone = "HS" ; soundex = "" }
  ; { input = "Hull" ; double_metaphone = "HL" ; soundex = "" }
  ; { input = "Hulme" ; double_metaphone = "HLM" ; soundex = "" }
  ; { input = "Hume" ; double_metaphone = "HM" ; soundex = "" }
  ; { input = "Hundertumark" ; double_metaphone = "HNTR" ; soundex = "" }
  ; { input = "Hundley" ; double_metaphone = "HNTL" ; soundex = "" }
  ; { input = "Hungerford" ; double_metaphone = "HNKR" ; soundex = "" }
  ; { input = "Hunt" ; double_metaphone = "HNT" ; soundex = "" }
  ; { input = "Hurst" ; double_metaphone = "HRST" ; soundex = "" }
  ; { input = "Husbands" ; double_metaphone = "HSPN" ; soundex = "" }
  ; { input = "Hussey" ; double_metaphone = "HS" ; soundex = "" }
  ; { input = "Husted" ; double_metaphone = "HSTT" ; soundex = "" }
  ; { input = "Hutchins" ; double_metaphone = "HXNS" ; soundex = "" }
  ; { input = "Hutchinson" ; double_metaphone = "HXNS" ; soundex = "" }
  ; { input = "Huttinger" ; double_metaphone = "HTNK" ; soundex = "" }
  ; { input = "Huybertsen" ; double_metaphone = "HPRT" ; soundex = "" }
  ; { input = "Iddenden" ; double_metaphone = "ATNT" ; soundex = "" }
  ; { input = "Ingraham" ; double_metaphone = "ANKR" ; soundex = "" }
  ; { input = "Ives" ; double_metaphone = "AFS" ; soundex = "" }
  ; { input = "Jackson" ; double_metaphone = "JKSN" ; soundex = "" }
  ; { input = "Jacob" ; double_metaphone = "JKP" ; soundex = "" }
  ; { input = "Jans" ; double_metaphone = "JNS" ; soundex = "" }
  ; { input = "Jenkins" ; double_metaphone = "JNKN" ; soundex = "" }
  ; { input = "Jewett" ; double_metaphone = "JT" ; soundex = "" }
  ; { input = "Jewitt" ; double_metaphone = "JT" ; soundex = "" }
  ; { input = "Johnson" ; double_metaphone = "JNSN" ; soundex = "" }
  ; { input = "Jones" ; double_metaphone = "JNS" ; soundex = "" }
  ; { input = "Josephine" ; double_metaphone = "JSFN" ; soundex = "" }
  ; { input = "Judd" ; double_metaphone = "JT" ; soundex = "" }
  ; { input = "June" ; double_metaphone = "JN" ; soundex = "" }
  ; { input = "Kamarowska" ; double_metaphone = "KMRS" ; soundex = "" }
  ; { input = "Kay" ; double_metaphone = "K" ; soundex = "" }
  ; { input = "Kelley" ; double_metaphone = "KL" ; soundex = "" }
  ; { input = "Kelly" ; double_metaphone = "KL" ; soundex = "" }
  ; { input = "Keymber" ; double_metaphone = "KMPR" ; soundex = "" }
  ; { input = "Keynes" ; double_metaphone = "KNS" ; soundex = "" }
  ; { input = "Kilham" ; double_metaphone = "KLM" ; soundex = "" }
  ; { input = "Kim" ; double_metaphone = "KM" ; soundex = "" }
  ; { input = "Kimball" ; double_metaphone = "KMPL" ; soundex = "" }
  ; { input = "King" ; double_metaphone = "KNK" ; soundex = "" }
  ; { input = "Kinsey" ; double_metaphone = "KNS" ; soundex = "" }
  ; { input = "Kirk" ; double_metaphone = "KRK" ; soundex = "" }
  ; { input = "Kirton" ; double_metaphone = "KRTN" ; soundex = "" }
  ; { input = "Kistler" ; double_metaphone = "KSTL" ; soundex = "" }
  ; { input = "Kitchen" ; double_metaphone = "KXN" ; soundex = "" }
  ; { input = "Kitson" ; double_metaphone = "KTSN" ; soundex = "" }
  ; { input = "Klett" ; double_metaphone = "KLT" ; soundex = "" }
  ; { input = "Kline" ; double_metaphone = "KLN" ; soundex = "" }
  ; { input = "Knapp" ; double_metaphone = "NP" ; soundex = "" }
  ; { input = "Knight" ; double_metaphone = "NT" ; soundex = "" }
  ; { input = "Knote" ; double_metaphone = "NT" ; soundex = "" }
  ; { input = "Knott" ; double_metaphone = "NT" ; soundex = "" }
  ; { input = "Knox" ; double_metaphone = "NKS" ; soundex = "" }
  ; { input = "Koeller" ; double_metaphone = "KLR" ; soundex = "" }
  ; { input = "La Pointe" ; double_metaphone = "LPNT" ; soundex = "" }
  ; { input = "LaPlante" ; double_metaphone = "LPLN" ; soundex = "" }
  ; { input = "Laimbeer" ; double_metaphone = "LMPR" ; soundex = "" }
  ; { input = "Lamb" ; double_metaphone = "LMP" ; soundex = "" }
  ; { input = "Lambertson" ; double_metaphone = "LMPR" ; soundex = "" }
  ; { input = "Lancto" ; double_metaphone = "LNKT" ; soundex = "" }
  ; { input = "Landry" ; double_metaphone = "LNTR" ; soundex = "" }
  ; { input = "Lane" ; double_metaphone = "LN" ; soundex = "" }
  ; { input = "Langendyck" ; double_metaphone = "LNJN" ; soundex = "" }
  ; { input = "Langer" ; double_metaphone = "LNKR" ; soundex = "" }
  ; { input = "Langford" ; double_metaphone = "LNKF" ; soundex = "" }
  ; { input = "Lantersee" ; double_metaphone = "LNTR" ; soundex = "" }
  ; { input = "Laquer" ; double_metaphone = "LKR" ; soundex = "" }
  ; { input = "Larkin" ; double_metaphone = "LRKN" ; soundex = "" }
  ; { input = "Latham" ; double_metaphone = "LTM" ; soundex = "" }
  ; { input = "Lathrop" ; double_metaphone = "L0RP" ; soundex = "" }
  ; { input = "Lauter" ; double_metaphone = "LTR" ; soundex = "" }
  ; { input = "Lawrence" ; double_metaphone = "LRNS" ; soundex = "" }
  ; { input = "Leach" ; double_metaphone = "LK" ; soundex = "" }
  ; { input = "Leager" ; double_metaphone = "LKR" ; soundex = "" }
  ; { input = "Learned" ; double_metaphone = "LRNT" ; soundex = "" }
  ; { input = "Leavitt" ; double_metaphone = "LFT" ; soundex = "" }
  ; { input = "Lee" ; double_metaphone = "L" ; soundex = "" }
  ; { input = "Leete" ; double_metaphone = "LT" ; soundex = "" }
  ; { input = "Leggett" ; double_metaphone = "LKT" ; soundex = "" }
  ; { input = "Leland" ; double_metaphone = "LLNT" ; soundex = "" }
  ; { input = "Leonard" ; double_metaphone = "LNRT" ; soundex = "" }
  ; { input = "Lester" ; double_metaphone = "LSTR" ; soundex = "" }
  ; { input = "Lestrange" ; double_metaphone = "LSTR" ; soundex = "" }
  ; { input = "Lethem" ; double_metaphone = "L0M" ; soundex = "" }
  ; { input = "Levine" ; double_metaphone = "LFN" ; soundex = "" }
  ; { input = "Lewes" ; double_metaphone = "LS" ; soundex = "" }
  ; { input = "Lewis" ; double_metaphone = "LS" ; soundex = "" }
  ; { input = "Lincoln" ; double_metaphone = "LNKL" ; soundex = "" }
  ; { input = "Lindsey" ; double_metaphone = "LNTS" ; soundex = "" }
  ; { input = "Linher" ; double_metaphone = "LNR" ; soundex = "" }
  ; { input = "Lippet" ; double_metaphone = "LPT" ; soundex = "" }
  ; { input = "Lippincott" ; double_metaphone = "LPNK" ; soundex = "" }
  ; { input = "Lockwood" ; double_metaphone = "LKT" ; soundex = "" }
  ; { input = "Loines" ; double_metaphone = "LNS" ; soundex = "" }
  ; { input = "Lombard" ; double_metaphone = "LMPR" ; soundex = "" }
  ; { input = "Long" ; double_metaphone = "LNK" ; soundex = "" }
  ; { input = "Longespee" ; double_metaphone = "LNJS" ; soundex = "" }
  ; { input = "Look" ; double_metaphone = "LK" ; soundex = "" }
  ; { input = "Lounsberry" ; double_metaphone = "LNSP" ; soundex = "" }
  ; { input = "Lounsbury" ; double_metaphone = "LNSP" ; soundex = "" }
  ; { input = "Louthe" ; double_metaphone = "L0" ; soundex = "" }
  ; { input = "Loveyne" ; double_metaphone = "LFN" ; soundex = "" }
  ; { input = "Lowe" ; double_metaphone = "L" ; soundex = "" }
  ; { input = "Ludlam" ; double_metaphone = "LTLM" ; soundex = "" }
  ; { input = "Lumbard" ; double_metaphone = "LMPR" ; soundex = "" }
  ; { input = "Lund" ; double_metaphone = "LNT" ; soundex = "" }
  ; { input = "Luno" ; double_metaphone = "LN" ; soundex = "" }
  ; { input = "Lutz" ; double_metaphone = "LTS" ; soundex = "" }
  ; { input = "Lydia" ; double_metaphone = "LT" ; soundex = "" }
  ; { input = "Lynne" ; double_metaphone = "LN" ; soundex = "" }
  ; { input = "Lyon" ; double_metaphone = "LN" ; soundex = "" }
  ; { input = "MacAlpin" ; double_metaphone = "MKLP" ; soundex = "" }
  ; { input = "MacBricc" ; double_metaphone = "MKPR" ; soundex = "" }
  ; { input = "MacCrinan" ; double_metaphone = "MKRN" ; soundex = "" }
  ; { input = "MacKenneth" ; double_metaphone = "MKN0" ; soundex = "" }
  ; { input = "MacMael nam Bo" ; double_metaphone = "MKML" ; soundex = "" }
  ; { input = "MacMurchada" ; double_metaphone = "MKMR" ; soundex = "" }
  ; { input = "Macomber" ; double_metaphone = "MKMP" ; soundex = "" }
  ; { input = "Macy" ; double_metaphone = "MS" ; soundex = "" }
  ; { input = "Magnus" ; double_metaphone = "MNS" ; soundex = "" }
  ; { input = "Mahien" ; double_metaphone = "MHN" ; soundex = "" }
  ; { input = "Malmains" ; double_metaphone = "MLMN" ; soundex = "" }
  ; { input = "Malory" ; double_metaphone = "MLR" ; soundex = "" }
  ; { input = "Mancinelli" ; double_metaphone = "MNSN" ; soundex = "" }
  ; { input = "Mancini" ; double_metaphone = "MNSN" ; soundex = "" }
  ; { input = "Mann" ; double_metaphone = "MN" ; soundex = "" }
  ; { input = "Manning" ; double_metaphone = "MNNK" ; soundex = "" }
  ; { input = "Manter" ; double_metaphone = "MNTR" ; soundex = "" }
  ; { input = "Marion" ; double_metaphone = "MRN" ; soundex = "" }
  ; { input = "Marley" ; double_metaphone = "MRL" ; soundex = "" }
  ; { input = "Marmion" ; double_metaphone = "MRMN" ; soundex = "" }
  ; { input = "Marquart" ; double_metaphone = "MRKR" ; soundex = "" }
  ; { input = "Marsh" ; double_metaphone = "MRX" ; soundex = "" }
  ; { input = "Marshal" ; double_metaphone = "MRXL" ; soundex = "" }
  ; { input = "Marshall" ; double_metaphone = "MRXL" ; soundex = "" }
  ; { input = "Martel" ; double_metaphone = "MRTL" ; soundex = "" }
  ; { input = "Martha" ; double_metaphone = "MR0" ; soundex = "" }
  ; { input = "Martin" ; double_metaphone = "MRTN" ; soundex = "" }
  ; { input = "Marturano" ; double_metaphone = "MRTR" ; soundex = "" }
  ; { input = "Marvin" ; double_metaphone = "MRFN" ; soundex = "" }
  ; { input = "Mary" ; double_metaphone = "MR" ; soundex = "" }
  ; { input = "Mason" ; double_metaphone = "MSN" ; soundex = "" }
  ; { input = "Maxwell" ; double_metaphone = "MKSL" ; soundex = "" }
  ; { input = "Mayhew" ; double_metaphone = "MH" ; soundex = "" }
  ; { input = "McAllaster" ; double_metaphone = "MKLS" ; soundex = "" }
  ; { input = "McAllister" ; double_metaphone = "MKLS" ; soundex = "" }
  ; { input = "McConnell" ; double_metaphone = "MKNL" ; soundex = "" }
  ; { input = "McFarland" ; double_metaphone = "MKFR" ; soundex = "" }
  ; { input = "McIlroy" ; double_metaphone = "MSLR" ; soundex = "" }
  ; { input = "McNair" ; double_metaphone = "MKNR" ; soundex = "" }
  ; { input = "McNair-Landry" ; double_metaphone = "MKNR" ; soundex = "" }
  ; { input = "McRaven" ; double_metaphone = "MKRF" ; soundex = "" }
  ; { input = "Mead" ; double_metaphone = "MT" ; soundex = "" }
  ; { input = "Meade" ; double_metaphone = "MT" ; soundex = "" }
  ; { input = "Meck" ; double_metaphone = "MK" ; soundex = "" }
  ; { input = "Melton" ; double_metaphone = "MLTN" ; soundex = "" }
  ; { input = "Mendenhall" ; double_metaphone = "MNTN" ; soundex = "" }
  ; { input = "Mering" ; double_metaphone = "MRNK" ; soundex = "" }
  ; { input = "Merrick" ; double_metaphone = "MRK" ; soundex = "" }
  ; { input = "Merry" ; double_metaphone = "MR" ; soundex = "" }
  ; { input = "Mighill" ; double_metaphone = "ML" ; soundex = "" }
  ; { input = "Miller" ; double_metaphone = "MLR" ; soundex = "" }
  ; { input = "Milton" ; double_metaphone = "MLTN" ; soundex = "" }
  ; { input = "Mohun" ; double_metaphone = "MHN" ; soundex = "" }
  ; { input = "Montague" ; double_metaphone = "MNTK" ; soundex = "" }
  ; { input = "Montboucher" ; double_metaphone = "MNTP" ; soundex = "" }
  ; { input = "Moore" ; double_metaphone = "MR" ; soundex = "" }
  ; { input = "Morrel" ; double_metaphone = "MRL" ; soundex = "" }
  ; { input = "Morrill" ; double_metaphone = "MRL" ; soundex = "" }
  ; { input = "Morris" ; double_metaphone = "MRS" ; soundex = "" }
  ; { input = "Morton" ; double_metaphone = "MRTN" ; soundex = "" }
  ; { input = "Moton" ; double_metaphone = "MTN" ; soundex = "" }
  ; { input = "Muir" ; double_metaphone = "MR" ; soundex = "" }
  ; { input = "Mulferd" ; double_metaphone = "MLFR" ; soundex = "" }
  ; { input = "Mullins" ; double_metaphone = "MLNS" ; soundex = "" }
  ; { input = "Mulso" ; double_metaphone = "MLS" ; soundex = "" }
  ; { input = "Munger" ; double_metaphone = "MNKR" ; soundex = "" }
  ; { input = "Munt" ; double_metaphone = "MNT" ; soundex = "" }
  ; { input = "Murchad" ; double_metaphone = "MRXT" ; soundex = "" }
  ; { input = "Murdock" ; double_metaphone = "MRTK" ; soundex = "" }
  ; { input = "Murray" ; double_metaphone = "MR" ; soundex = "" }
  ; { input = "Muskett" ; double_metaphone = "MSKT" ; soundex = "" }
  ; { input = "Myers" ; double_metaphone = "MRS" ; soundex = "" }
  ; { input = "Myrick" ; double_metaphone = "MRK" ; soundex = "" }
  ; { input = "NORRIS" ; double_metaphone = "NRS" ; soundex = "" }
  ; { input = "Nayle" ; double_metaphone = "NL" ; soundex = "" }
  ; { input = "Newcomb" ; double_metaphone = "NKMP" ; soundex = "" }
  ; { input = "Newcomb(e" ; double_metaphone = "NKMP" ; soundex = "" }
  ; { input = "Newkirk" ; double_metaphone = "NKRK" ; soundex = "" }
  ; { input = "Newton" ; double_metaphone = "NTN" ; soundex = "" }
  ; { input = "Niles" ; double_metaphone = "NLS" ; soundex = "" }
  ; { input = "Noble" ; double_metaphone = "NPL" ; soundex = "" }
  ; { input = "Noel" ; double_metaphone = "NL" ; soundex = "" }
  ; { input = "Northend" ; double_metaphone = "NR0N" ; soundex = "" }
  ; { input = "Norton" ; double_metaphone = "NRTN" ; soundex = "" }
  ; { input = "Nutter" ; double_metaphone = "NTR" ; soundex = "" }
  ; { input = "Odding" ; double_metaphone = "ATNK" ; soundex = "" }
  ; { input = "Odenbaugh" ; double_metaphone = "ATNP" ; soundex = "" }
  ; { input = "Ogborn" ; double_metaphone = "AKPR" ; soundex = "" }
  ; { input = "Oppenheimer" ; double_metaphone = "APNM" ; soundex = "" }
  ; { input = "Otis" ; double_metaphone = "ATS" ; soundex = "" }
  ; { input = "Oviatt" ; double_metaphone = "AFT" ; soundex = "" }
  ; { input = "PRUST?" ; double_metaphone = "PRST" ; soundex = "" }
  ; { input = "Paddock" ; double_metaphone = "PTK" ; soundex = "" }
  ; { input = "Page" ; double_metaphone = "PJ" ; soundex = "" }
  ; { input = "Paine" ; double_metaphone = "PN" ; soundex = "" }
  ; { input = "Paist" ; double_metaphone = "PST" ; soundex = "" }
  ; { input = "Palmer" ; double_metaphone = "PLMR" ; soundex = "" }
  ; { input = "Park" ; double_metaphone = "PRK" ; soundex = "" }
  ; { input = "Parker" ; double_metaphone = "PRKR" ; soundex = "" }
  ; { input = "Parkhurst" ; double_metaphone = "PRKR" ; soundex = "" }
  ; { input = "Parrat" ; double_metaphone = "PRT" ; soundex = "" }
  ; { input = "Parsons" ; double_metaphone = "PRSN" ; soundex = "" }
  ; { input = "Partridge" ; double_metaphone = "PRTR" ; soundex = "" }
  ; { input = "Pashley" ; double_metaphone = "PXL" ; soundex = "" }
  ; { input = "Pasley" ; double_metaphone = "PSL" ; soundex = "" }
  ; { input = "Patrick" ; double_metaphone = "PTRK" ; soundex = "" }
  ; { input = "Pattee" ; double_metaphone = "PT" ; soundex = "" }
  ; { input = "Patten" ; double_metaphone = "PTN" ; soundex = "" }
  ; { input = "Pawley" ; double_metaphone = "PL" ; soundex = "" }
  ; { input = "Payne" ; double_metaphone = "PN" ; soundex = "" }
  ; { input = "Peabody" ; double_metaphone = "PPT" ; soundex = "" }
  ; { input = "Peake" ; double_metaphone = "PK" ; soundex = "" }
  ; { input = "Pearson" ; double_metaphone = "PRSN" ; soundex = "" }
  ; { input = "Peat" ; double_metaphone = "PT" ; soundex = "" }
  ; { input = "Pedersen" ; double_metaphone = "PTRS" ; soundex = "" }
  ; { input = "Percy" ; double_metaphone = "PRS" ; soundex = "" }
  ; { input = "Perkins" ; double_metaphone = "PRKN" ; soundex = "" }
  ; { input = "Perrine" ; double_metaphone = "PRN" ; soundex = "" }
  ; { input = "Perry" ; double_metaphone = "PR" ; soundex = "" }
  ; { input = "Peson" ; double_metaphone = "PSN" ; soundex = "" }
  ; { input = "Peterson" ; double_metaphone = "PTRS" ; soundex = "" }
  ; { input = "Peyton" ; double_metaphone = "PTN" ; soundex = "" }
  ; { input = "Phinney" ; double_metaphone = "FN" ; soundex = "" }
  ; { input = "Pickard" ; double_metaphone = "PKRT" ; soundex = "" }
  ; { input = "Pierce" ; double_metaphone = "PRS" ; soundex = "" }
  ; { input = "Pierrepont" ; double_metaphone = "PRPN" ; soundex = "" }
  ; { input = "Pike" ; double_metaphone = "PK" ; soundex = "" }
  ; { input = "Pinkham" ; double_metaphone = "PNKM" ; soundex = "" }
  ; { input = "Pitman" ; double_metaphone = "PTMN" ; soundex = "" }
  ; { input = "Pitt" ; double_metaphone = "PT" ; soundex = "" }
  ; { input = "Pitts" ; double_metaphone = "PTS" ; soundex = "" }
  ; { input = "Plantagenet" ; double_metaphone = "PLNT" ; soundex = "" }
  ; { input = "Platt" ; double_metaphone = "PLT" ; soundex = "" }
  ; { input = "Platts" ; double_metaphone = "PLTS" ; soundex = "" }
  ; { input = "Pleis" ; double_metaphone = "PLS" ; soundex = "" }
  ; { input = "Pleiss" ; double_metaphone = "PLS" ; soundex = "" }
  ; { input = "Plisko" ; double_metaphone = "PLSK" ; soundex = "" }
  ; { input = "Pliskovitch" ; double_metaphone = "PLSK" ; soundex = "" }
  ; { input = "Plum" ; double_metaphone = "PLM" ; soundex = "" }
  ; { input = "Plume" ; double_metaphone = "PLM" ; soundex = "" }
  ; { input = "Poitou" ; double_metaphone = "PT" ; soundex = "" }
  ; { input = "Pomeroy" ; double_metaphone = "PMR" ; soundex = "" }
  ; { input = "Poretiers" ; double_metaphone = "PRTR" ; soundex = "" }
  ; { input = "Pote" ; double_metaphone = "PT" ; soundex = "" }
  ; { input = "Potter" ; double_metaphone = "PTR" ; soundex = "" }
  ; { input = "Potts" ; double_metaphone = "PTS" ; soundex = "" }
  ; { input = "Powell" ; double_metaphone = "PL" ; soundex = "" }
  ; { input = "Pratt" ; double_metaphone = "PRT" ; soundex = "" }
  ; { input = "Presbury" ; double_metaphone = "PRSP" ; soundex = "" }
  ; { input = "Priest" ; double_metaphone = "PRST" ; soundex = "" }
  ; { input = "Prindle" ; double_metaphone = "PRNT" ; soundex = "" }
  ; { input = "Prior" ; double_metaphone = "PRR" ; soundex = "" }
  ; { input = "Profumo" ; double_metaphone = "PRFM" ; soundex = "" }
  ; { input = "Purdy" ; double_metaphone = "PRT" ; soundex = "" }
  ; { input = "Purefoy" ; double_metaphone = "PRF" ; soundex = "" }
  ; { input = "Pury" ; double_metaphone = "PR" ; soundex = "" }
  ; { input = "Quinter" ; double_metaphone = "KNTR" ; soundex = "" }
  ; { input = "Rachel" ; double_metaphone = "RXL" ; soundex = "" }
  ; { input = "Rand" ; double_metaphone = "RNT" ; soundex = "" }
  ; { input = "Rankin" ; double_metaphone = "RNKN" ; soundex = "" }
  ; { input = "Ravenscroft" ; double_metaphone = "RFNS" ; soundex = "" }
  ; { input = "Raynsford" ; double_metaphone = "RNSF" ; soundex = "" }
  ; { input = "Reakirt" ; double_metaphone = "RKRT" ; soundex = "" }
  ; { input = "Reaves" ; double_metaphone = "RFS" ; soundex = "" }
  ; { input = "Reeves" ; double_metaphone = "RFS" ; soundex = "" }
  ; { input = "Reichert" ; double_metaphone = "RXRT" ; soundex = "" }
  ; { input = "Remmele" ; double_metaphone = "RML" ; soundex = "" }
  ; { input = "Reynolds" ; double_metaphone = "RNLT" ; soundex = "" }
  ; { input = "Rhodes" ; double_metaphone = "RTS" ; soundex = "" }
  ; { input = "Richards" ; double_metaphone = "RXRT" ; soundex = "" }
  ; { input = "Richardson" ; double_metaphone = "RXRT" ; soundex = "" }
  ; { input = "Ring" ; double_metaphone = "RNK" ; soundex = "" }
  ; { input = "Roberts" ; double_metaphone = "RPRT" ; soundex = "" }
  ; { input = "Robertson" ; double_metaphone = "RPRT" ; soundex = "" }
  ; { input = "Robson" ; double_metaphone = "RPSN" ; soundex = "" }
  ; { input = "Rodie" ; double_metaphone = "RT" ; soundex = "" }
  ; { input = "Rody" ; double_metaphone = "RT" ; soundex = "" }
  ; { input = "Rogers" ; double_metaphone = "RKRS" ; soundex = "" }
  ; { input = "Ross" ; double_metaphone = "RS" ; soundex = "" }
  ; { input = "Rosslevin" ; double_metaphone = "RSLF" ; soundex = "" }
  ; { input = "Rowland" ; double_metaphone = "RLNT" ; soundex = "" }
  ; { input = "Ruehl" ; double_metaphone = "RL" ; soundex = "" }
  ; { input = "Russell" ; double_metaphone = "RSL" ; soundex = "" }
  ; { input = "Ruth" ; double_metaphone = "R0" ; soundex = "" }
  ; { input = "Ryan" ; double_metaphone = "RN" ; soundex = "" }
  ; { input = "Rysse" ; double_metaphone = "RS" ; soundex = "" }
  ; { input = "Sadler" ; double_metaphone = "STLR" ; soundex = "" }
  ; { input = "Salmon" ; double_metaphone = "SLMN" ; soundex = "" }
  ; { input = "Salter" ; double_metaphone = "SLTR" ; soundex = "" }
  ; { input = "Salvatore" ; double_metaphone = "SLFT" ; soundex = "" }
  ; { input = "Sanders" ; double_metaphone = "SNTR" ; soundex = "" }
  ; { input = "Sands" ; double_metaphone = "SNTS" ; soundex = "" }
  ; { input = "Sanford" ; double_metaphone = "SNFR" ; soundex = "" }
  ; { input = "Sanger" ; double_metaphone = "SNKR" ; soundex = "" }
  ; { input = "Sargent" ; double_metaphone = "SRJN" ; soundex = "" }
  ; { input = "Saunders" ; double_metaphone = "SNTR" ; soundex = "" }
  ; { input = "Schilling" ; double_metaphone = "XLNK" ; soundex = "" }
  ; { input = "Schlegel" ; double_metaphone = "XLKL" ; soundex = "" }
  ; { input = "Scott" ; double_metaphone = "SKT" ; soundex = "" }
  ; { input = "Sears" ; double_metaphone = "SRS" ; soundex = "" }
  ; { input = "Segersall" ; double_metaphone = "SJRS" ; soundex = "" }
  ; { input = "Senecal" ; double_metaphone = "SNKL" ; soundex = "" }
  ; { input = "Sergeaux" ; double_metaphone = "SRJ" ; soundex = "" }
  ; { input = "Severance" ; double_metaphone = "SFRN" ; soundex = "" }
  ; { input = "Sharp" ; double_metaphone = "XRP" ; soundex = "" }
  ; { input = "Sharpe" ; double_metaphone = "XRP" ; soundex = "" }
  ; { input = "Sharply" ; double_metaphone = "XRPL" ; soundex = "" }
  ; { input = "Shatswell" ; double_metaphone = "XTSL" ; soundex = "" }
  ; { input = "Shattack" ; double_metaphone = "XTK" ; soundex = "" }
  ; { input = "Shattock" ; double_metaphone = "XTK" ; soundex = "" }
  ; { input = "Shattuck" ; double_metaphone = "XTK" ; soundex = "" }
  ; { input = "Shaw" ; double_metaphone = "X" ; soundex = "" }
  ; { input = "Sheldon" ; double_metaphone = "XLTN" ; soundex = "" }
  ; { input = "Sherman" ; double_metaphone = "XRMN" ; soundex = "" }
  ; { input = "Shinn" ; double_metaphone = "XN" ; soundex = "" }
  ; { input = "Shirford" ; double_metaphone = "XRFR" ; soundex = "" }
  ; { input = "Shirley" ; double_metaphone = "XRL" ; soundex = "" }
  ; { input = "Shively" ; double_metaphone = "XFL" ; soundex = "" }
  ; { input = "Shoemaker" ; double_metaphone = "XMKR" ; soundex = "" }
  ; { input = "Short" ; double_metaphone = "XRT" ; soundex = "" }
  ; { input = "Shotwell" ; double_metaphone = "XTL" ; soundex = "" }
  ; { input = "Shute" ; double_metaphone = "XT" ; soundex = "" }
  ; { input = "Sibley" ; double_metaphone = "SPL" ; soundex = "" }
  ; { input = "Silver" ; double_metaphone = "SLFR" ; soundex = "" }
  ; { input = "Simes" ; double_metaphone = "SMS" ; soundex = "" }
  ; { input = "Sinken" ; double_metaphone = "SNKN" ; soundex = "" }
  ; { input = "Sinn" ; double_metaphone = "SN" ; soundex = "" }
  ; { input = "Skelton" ; double_metaphone = "SKLT" ; soundex = "" }
  ; { input = "Skiffe" ; double_metaphone = "SKF" ; soundex = "" }
  ; { input = "Skotkonung" ; double_metaphone = "SKTK" ; soundex = "" }
  ; { input = "Slade" ; double_metaphone = "SLT" ; soundex = "" }
  ; { input = "Slye" ; double_metaphone = "SL" ; soundex = "" }
  ; { input = "Smedley" ; double_metaphone = "SMTL" ; soundex = "" }
  ; { input = "Smith" ; double_metaphone = "SM0" ; soundex = "" }
  ; { input = "Snow" ; double_metaphone = "SN" ; soundex = "" }
  ; { input = "Soole" ; double_metaphone = "SL" ; soundex = "" }
  ; { input = "Soule" ; double_metaphone = "SL" ; soundex = "" }
  ; { input = "Southworth" ; double_metaphone = "S0R0" ; soundex = "" }
  ; { input = "Sowles" ; double_metaphone = "SLS" ; soundex = "" }
  ; { input = "Spalding" ; double_metaphone = "SPLT" ; soundex = "" }
  ; { input = "Spark" ; double_metaphone = "SPRK" ; soundex = "" }
  ; { input = "Spencer" ; double_metaphone = "SPNS" ; soundex = "" }
  ; { input = "Sperry" ; double_metaphone = "SPR" ; soundex = "" }
  ; { input = "Spofford" ; double_metaphone = "SPFR" ; soundex = "" }
  ; { input = "Spooner" ; double_metaphone = "SPNR" ; soundex = "" }
  ; { input = "Sprague" ; double_metaphone = "SPRK" ; soundex = "" }
  ; { input = "Springer" ; double_metaphone = "SPRN" ; soundex = "" }
  ; { input = "St. Clair" ; double_metaphone = "STKL" ; soundex = "" }
  ; { input = "St. Claire" ; double_metaphone = "STKL" ; soundex = "" }
  ; { input = "St. Leger" ; double_metaphone = "STLJ" ; soundex = "" }
  ; { input = "St. Omer" ; double_metaphone = "STMR" ; soundex = "" }
  ; { input = "Stafferton" ; double_metaphone = "STFR" ; soundex = "" }
  ; { input = "Stafford" ; double_metaphone = "STFR" ; soundex = "" }
  ; { input = "Stalham" ; double_metaphone = "STLM" ; soundex = "" }
  ; { input = "Stanford" ; double_metaphone = "STNF" ; soundex = "" }
  ; { input = "Stanton" ; double_metaphone = "STNT" ; soundex = "" }
  ; { input = "Star" ; double_metaphone = "STR" ; soundex = "" }
  ; { input = "Starbuck" ; double_metaphone = "STRP" ; soundex = "" }
  ; { input = "Starkey" ; double_metaphone = "STRK" ; soundex = "" }
  ; { input = "Starkweather" ; double_metaphone = "STRK" ; soundex = "" }
  ; { input = "Stearns" ; double_metaphone = "STRN" ; soundex = "" }
  ; { input = "Stebbins" ; double_metaphone = "STPN" ; soundex = "" }
  ; { input = "Steele" ; double_metaphone = "STL" ; soundex = "" }
  ; { input = "Stephenson" ; double_metaphone = "STFN" ; soundex = "" }
  ; { input = "Stevens" ; double_metaphone = "STFN" ; soundex = "" }
  ; { input = "Stoddard" ; double_metaphone = "STTR" ; soundex = "" }
  ; { input = "Stodder" ; double_metaphone = "STTR" ; soundex = "" }
  ; { input = "Stone" ; double_metaphone = "STN" ; soundex = "" }
  ; { input = "Storey" ; double_metaphone = "STR" ; soundex = "" }
  ; { input = "Storrada" ; double_metaphone = "STRT" ; soundex = "" }
  ; { input = "Story" ; double_metaphone = "STR" ; soundex = "" }
  ; { input = "Stoughton" ; double_metaphone = "STFT" ; soundex = "" }
  ; { input = "Stout" ; double_metaphone = "STT" ; soundex = "" }
  ; { input = "Stow" ; double_metaphone = "ST" ; soundex = "" }
  ; { input = "Strong" ; double_metaphone = "STRN" ; soundex = "" }
  ; { input = "Strutt" ; double_metaphone = "STRT" ; soundex = "" }
  ; { input = "Stryker" ; double_metaphone = "STRK" ; soundex = "" }
  ; { input = "Stuckeley" ; double_metaphone = "STKL" ; soundex = "" }
  ; { input = "Sturges" ; double_metaphone = "STRJ" ; soundex = "" }
  ; { input = "Sturgess" ; double_metaphone = "STRJ" ; soundex = "" }
  ; { input = "Sturgis" ; double_metaphone = "STRJ" ; soundex = "" }
  ; { input = "Suevain" ; double_metaphone = "SFN" ; soundex = "" }
  ; { input = "Sulyard" ; double_metaphone = "SLRT" ; soundex = "" }
  ; { input = "Sutton" ; double_metaphone = "STN" ; soundex = "" }
  ; { input = "Swain" ; double_metaphone = "SN" ; soundex = "" }
  ; { input = "Swayne" ; double_metaphone = "SN" ; soundex = "" }
  ; { input = "Swayze" ; double_metaphone = "SS" ; soundex = "" }
  ; { input = "Swift" ; double_metaphone = "SFT" ; soundex = "" }
  ; { input = "Taber" ; double_metaphone = "TPR" ; soundex = "" }
  ; { input = "Talcott" ; double_metaphone = "TLKT" ; soundex = "" }
  ; { input = "Tarne" ; double_metaphone = "TRN" ; soundex = "" }
  ; { input = "Tatum" ; double_metaphone = "TTM" ; soundex = "" }
  ; { input = "Taverner" ; double_metaphone = "TFRN" ; soundex = "" }
  ; { input = "Taylor" ; double_metaphone = "TLR" ; soundex = "" }
  ; { input = "Tenney" ; double_metaphone = "TN" ; soundex = "" }
  ; { input = "Thayer" ; double_metaphone = "0R" ; soundex = "" }
  ; { input = "Thember" ; double_metaphone = "0MPR" ; soundex = "" }
  ; { input = "Thomas" ; double_metaphone = "TMS" ; soundex = "" }
  ; { input = "Thompson" ; double_metaphone = "TMPS" ; soundex = "" }
  ; { input = "Thorne" ; double_metaphone = "0RN" ; soundex = "" }
  ; { input = "Thornycraft" ; double_metaphone = "0RNK" ; soundex = "" }
  ; { input = "Threlkeld" ; double_metaphone = "0RLK" ; soundex = "" }
  ; { input = "Throckmorton" ; double_metaphone = "0RKM" ; soundex = "" }
  ; { input = "Thwaits" ; double_metaphone = "0TS" ; soundex = "" }
  ; { input = "Tibbetts" ; double_metaphone = "TPTS" ; soundex = "" }
  ; { input = "Tidd" ; double_metaphone = "TT" ; soundex = "" }
  ; { input = "Tierney" ; double_metaphone = "TRN" ; soundex = "" }
  ; { input = "Tilley" ; double_metaphone = "TL" ; soundex = "" }
  ; { input = "Tillieres" ; double_metaphone = "TLRS" ; soundex = "" }
  ; { input = "Tilly" ; double_metaphone = "TL" ; soundex = "" }
  ; { input = "Tisdale" ; double_metaphone = "TSTL" ; soundex = "" }
  ; { input = "Titus" ; double_metaphone = "TTS" ; soundex = "" }
  ; { input = "Tobey" ; double_metaphone = "TP" ; soundex = "" }
  ; { input = "Tooker" ; double_metaphone = "TKR" ; soundex = "" }
  ; { input = "Towle" ; double_metaphone = "TL" ; soundex = "" }
  ; { input = "Towne" ; double_metaphone = "TN" ; soundex = "" }
  ; { input = "Townsend" ; double_metaphone = "TNSN" ; soundex = "" }
  ; { input = "Treadway" ; double_metaphone = "TRT" ; soundex = "" }
  ; { input = "Trelawney" ; double_metaphone = "TRLN" ; soundex = "" }
  ; { input = "Trinder" ; double_metaphone = "TRNT" ; soundex = "" }
  ; { input = "Tripp" ; double_metaphone = "TRP" ; soundex = "" }
  ; { input = "Trippe" ; double_metaphone = "TRP" ; soundex = "" }
  ; { input = "Trott" ; double_metaphone = "TRT" ; soundex = "" }
  ; { input = "True" ; double_metaphone = "TR" ; soundex = "" }
  ; { input = "Trussebut" ; double_metaphone = "TRSP" ; soundex = "" }
  ; { input = "Tucker" ; double_metaphone = "TKR" ; soundex = "" }
  ; { input = "Turgeon" ; double_metaphone = "TRJN" ; soundex = "" }
  ; { input = "Turner" ; double_metaphone = "TRNR" ; soundex = "" }
  ; { input = "Tuttle" ; double_metaphone = "TTL" ; soundex = "" }
  ; { input = "Tyler" ; double_metaphone = "TLR" ; soundex = "" }
  ; { input = "Tylle" ; double_metaphone = "TL" ; soundex = "" }
  ; { input = "Tyrrel" ; double_metaphone = "TRL" ; soundex = "" }
  ; { input = "Ua Tuathail" ; double_metaphone = "AT0L" ; soundex = "" }
  ; { input = "Ulrich" ; double_metaphone = "ALRX" ; soundex = "" }
  ; { input = "Underhill" ; double_metaphone = "ANTR" ; soundex = "" }
  ; { input = "Underwood" ; double_metaphone = "ANTR" ; soundex = "" }
  ; { input = "Unknown" ; double_metaphone = "ANKN" ; soundex = "" }
  ; { input = "Valentine" ; double_metaphone = "FLNT" ; soundex = "" }
  ; { input = "Van Egmond" ; double_metaphone = "FNKM" ; soundex = "" }
  ; { input = "Van der Beek" ; double_metaphone = "FNTR" ; soundex = "" }
  ; { input = "Vaughan" ; double_metaphone = "FKN" ; soundex = "" }
  ; { input = "Vermenlen" ; double_metaphone = "FRMN" ; soundex = "" }
  ; { input = "Vincent" ; double_metaphone = "FNSN" ; soundex = "" }
  ; { input = "Volentine" ; double_metaphone = "FLNT" ; soundex = "" }
  ; { input = "Wagner" ; double_metaphone = "AKNR" ; soundex = "" }
  ; { input = "Waite" ; double_metaphone = "AT" ; soundex = "" }
  ; { input = "Walker" ; double_metaphone = "ALKR" ; soundex = "" }
  ; { input = "Walter" ; double_metaphone = "ALTR" ; soundex = "" }
  ; { input = "Wandell" ; double_metaphone = "ANTL" ; soundex = "" }
  ; { input = "Wandesford" ; double_metaphone = "ANTS" ; soundex = "" }
  ; { input = "Warbleton" ; double_metaphone = "ARPL" ; soundex = "" }
  ; { input = "Ward" ; double_metaphone = "ART" ; soundex = "" }
  ; { input = "Warde" ; double_metaphone = "ART" ; soundex = "" }
  ; { input = "Ware" ; double_metaphone = "AR" ; soundex = "" }
  ; { input = "Wareham" ; double_metaphone = "ARHM" ; soundex = "" }
  ; { input = "Warner" ; double_metaphone = "ARNR" ; soundex = "" }
  ; { input = "Warren" ; double_metaphone = "ARN" ; soundex = "" }
  ; { input = "Washburne" ; double_metaphone = "AXPR" ; soundex = "" }
  ; { input = "Waterbury" ; double_metaphone = "ATRP" ; soundex = "" }
  ; { input = "Watson" ; double_metaphone = "ATSN" ; soundex = "" }
  ; { input = "WatsonEllithorpe" ; double_metaphone = "ATSN" ; soundex = "" }
  ; { input = "Watts" ; double_metaphone = "ATS" ; soundex = "" }
  ; { input = "Wayne" ; double_metaphone = "AN" ; soundex = "" }
  ; { input = "Webb" ; double_metaphone = "AP" ; soundex = "" }
  ; { input = "Weber" ; double_metaphone = "APR" ; soundex = "" }
  ; { input = "Webster" ; double_metaphone = "APST" ; soundex = "" }
  ; { input = "Weed" ; double_metaphone = "AT" ; soundex = "" }
  ; { input = "Weeks" ; double_metaphone = "AKS" ; soundex = "" }
  ; { input = "Wells" ; double_metaphone = "ALS" ; soundex = "" }
  ; { input = "Wenzell" ; double_metaphone = "ANSL" ; soundex = "" }
  ; { input = "West" ; double_metaphone = "AST" ; soundex = "" }
  ; { input = "Westbury" ; double_metaphone = "ASTP" ; soundex = "" }
  ; { input = "Whatlocke" ; double_metaphone = "ATLK" ; soundex = "" }
  ; { input = "Wheeler" ; double_metaphone = "ALR" ; soundex = "" }
  ; { input = "Whiston" ; double_metaphone = "ASTN" ; soundex = "" }
  ; { input = "White" ; double_metaphone = "AT" ; soundex = "" }
  ; { input = "Whitman" ; double_metaphone = "ATMN" ; soundex = "" }
  ; { input = "Whiton" ; double_metaphone = "ATN" ; soundex = "" }
  ; { input = "Whitson" ; double_metaphone = "ATSN" ; soundex = "" }
  ; { input = "Wickes" ; double_metaphone = "AKS" ; soundex = "" }
  ; { input = "Wilbur" ; double_metaphone = "ALPR" ; soundex = "" }
  ; { input = "Wilcotes" ; double_metaphone = "ALKT" ; soundex = "" }
  ; { input = "Wilkinson" ; double_metaphone = "ALKN" ; soundex = "" }
  ; { input = "Willets" ; double_metaphone = "ALTS" ; soundex = "" }
  ; { input = "Willett" ; double_metaphone = "ALT" ; soundex = "" }
  ; { input = "Willey" ; double_metaphone = "AL" ; soundex = "" }
  ; { input = "Williams" ; double_metaphone = "ALMS" ; soundex = "" }
  ; { input = "Williston" ; double_metaphone = "ALST" ; soundex = "" }
  ; { input = "Wilson" ; double_metaphone = "ALSN" ; soundex = "" }
  ; { input = "Wimes" ; double_metaphone = "AMS" ; soundex = "" }
  ; { input = "Winch" ; double_metaphone = "ANX" ; soundex = "" }
  ; { input = "Winegar" ; double_metaphone = "ANKR" ; soundex = "" }
  ; { input = "Wing" ; double_metaphone = "ANK" ; soundex = "" }
  ; { input = "Winsley" ; double_metaphone = "ANSL" ; soundex = "" }
  ; { input = "Winslow" ; double_metaphone = "ANSL" ; soundex = "" }
  ; { input = "Winthrop" ; double_metaphone = "AN0R" ; soundex = "" }
  ; { input = "Wise" ; double_metaphone = "AS" ; soundex = "" }
  ; { input = "Wood" ; double_metaphone = "AT" ; soundex = "" }
  ; { input = "Woodbridge" ; double_metaphone = "ATPR" ; soundex = "" }
  ; { input = "Woodward" ; double_metaphone = "ATRT" ; soundex = "" }
  ; { input = "Wooley" ; double_metaphone = "AL" ; soundex = "" }
  ; { input = "Woolley" ; double_metaphone = "AL" ; soundex = "" }
  ; { input = "Worth" ; double_metaphone = "AR0" ; soundex = "" }
  ; { input = "Worthen" ; double_metaphone = "AR0N" ; soundex = "" }
  ; { input = "Worthley" ; double_metaphone = "AR0L" ; soundex = "" }
  ; { input = "Wright" ; double_metaphone = "RT" ; soundex = "" }
  ; { input = "Wyer" ; double_metaphone = "AR" ; soundex = "" }
  ; { input = "Wyere" ; double_metaphone = "AR" ; soundex = "" }
  ; { input = "Wynkoop" ; double_metaphone = "ANKP" ; soundex = "" }
  ; { input = "Yarnall" ; double_metaphone = "ARNL" ; soundex = "" }
  ; { input = "Yeoman" ; double_metaphone = "AMN" ; soundex = "" }
  ; { input = "Yorke" ; double_metaphone = "ARK" ; soundex = "" }
  ; { input = "Young" ; double_metaphone = "ANK" ; soundex = "" }
  ; { input = "ab Wennonwen" ; double_metaphone = "APNN" ; soundex = "" }
  ; { input = "ap Llewellyn" ; double_metaphone = "APLL" ; soundex = "" }
  ; { input = "ap Lorwerth" ; double_metaphone = "APLR" ; soundex = "" }
  ; { input = "d'Angouleme" ; double_metaphone = "TNKL" ; soundex = "" }
  ; { input = "de Audeham" ; double_metaphone = "TTHM" ; soundex = "" }
  ; { input = "de Bavant" ; double_metaphone = "TPFN" ; soundex = "" }
  ; { input = "de Beauchamp" ; double_metaphone = "TPXM" ; soundex = "" }
  ; { input = "de Beaumont" ; double_metaphone = "TPMN" ; soundex = "" }
  ; { input = "de Bolbec" ; double_metaphone = "TPLP" ; soundex = "" }
  ; { input = "de Braiose" ; double_metaphone = "TPRS" ; soundex = "" }
  ; { input = "de Braose" ; double_metaphone = "TPRS" ; soundex = "" }
  ; { input = "de Briwere" ; double_metaphone = "TPRR" ; soundex = "" }
  ; { input = "de Cantelou" ; double_metaphone = "TKNT" ; soundex = "" }
  ; { input = "de Cherelton" ; double_metaphone = "TXRL" ; soundex = "" }
  ; { input = "de Cherleton" ; double_metaphone = "TXRL" ; soundex = "" }
  ; { input = "de Clare" ; double_metaphone = "TKLR" ; soundex = "" }
  ; { input = "de Claremont" ; double_metaphone = "TKLR" ; soundex = "" }
  ; { input = "de Clifford" ; double_metaphone = "TKLF" ; soundex = "" }
  ; { input = "de Colville" ; double_metaphone = "TKLF" ; soundex = "" }
  ; { input = "de Courtenay" ; double_metaphone = "TKRT" ; soundex = "" }
  ; { input = "de Fauconberg" ; double_metaphone = "TFKN" ; soundex = "" }
  ; { input = "de Forest" ; double_metaphone = "TFRS" ; soundex = "" }
  ; { input = "de Gai" ; double_metaphone = "TK" ; soundex = "" }
  ; { input = "de Grey" ; double_metaphone = "TKR" ; soundex = "" }
  ; { input = "de Guernons" ; double_metaphone = "TKRN" ; soundex = "" }
  ; { input = "de Haia" ; double_metaphone = "T" ; soundex = "" }
  ; { input = "de Harcourt" ; double_metaphone = "TRKR" ; soundex = "" }
  ; { input = "de Hastings" ; double_metaphone = "TSTN" ; soundex = "" }
  ; { input = "de Hoke" ; double_metaphone = "TK" ; soundex = "" }
  ; { input = "de Hooch" ; double_metaphone = "TK" ; soundex = "" }
  ; { input = "de Hugelville" ; double_metaphone = "TJLF" ; soundex = "" }
  ; { input = "de Huntingdon" ; double_metaphone = "TNTN" ; soundex = "" }
  ; { input = "de Insula" ; double_metaphone = "TNSL" ; soundex = "" }
  ; { input = "de Keynes" ; double_metaphone = "TKNS" ; soundex = "" }
  ; { input = "de Lacy" ; double_metaphone = "TLS" ; soundex = "" }
  ; { input = "de Lexington" ; double_metaphone = "TLKS" ; soundex = "" }
  ; { input = "de Lusignan" ; double_metaphone = "TLSN" ; soundex = "" }
  ; { input = "de Manvers" ; double_metaphone = "TMNF" ; soundex = "" }
  ; { input = "de Montagu" ; double_metaphone = "TMNT" ; soundex = "" }
  ; { input = "de Montault" ; double_metaphone = "TMNT" ; soundex = "" }
  ; { input = "de Montfort" ; double_metaphone = "TMNT" ; soundex = "" }
  ; { input = "de Mortimer" ; double_metaphone = "TMRT" ; soundex = "" }
  ; { input = "de Morville" ; double_metaphone = "TMRF" ; soundex = "" }
  ; { input = "de Morvois" ; double_metaphone = "TMRF" ; soundex = "" }
  ; { input = "de Neufmarche" ; double_metaphone = "TNFM" ; soundex = "" }
  ; { input = "de Odingsells" ; double_metaphone = "TTNK" ; soundex = "" }
  ; { input = "de Odyngsells" ; double_metaphone = "TTNK" ; soundex = "" }
  ; { input = "de Percy" ; double_metaphone = "TPRS" ; soundex = "" }
  ; { input = "de Pierrepont" ; double_metaphone = "TPRP" ; soundex = "" }
  ; { input = "de Plessetis" ; double_metaphone = "TPLS" ; soundex = "" }
  ; { input = "de Porhoet" ; double_metaphone = "TPRT" ; soundex = "" }
  ; { input = "de Prouz" ; double_metaphone = "TPRS" ; soundex = "" }
  ; { input = "de Quincy" ; double_metaphone = "TKNS" ; soundex = "" }
  ; { input = "de Ripellis" ; double_metaphone = "TRPL" ; soundex = "" }
  ; { input = "de Ros" ; double_metaphone = "TRS" ; soundex = "" }
  ; { input = "de Salisbury" ; double_metaphone = "TSLS" ; soundex = "" }
  ; { input = "de Sanford" ; double_metaphone = "TSNF" ; soundex = "" }
  ; { input = "de Somery" ; double_metaphone = "TSMR" ; soundex = "" }
  ; { input = "de St. Hilary" ; double_metaphone = "TSTL" ; soundex = "" }
  ; { input = "de St. Liz" ; double_metaphone = "TSTL" ; soundex = "" }
  ; { input = "de Sutton" ; double_metaphone = "TSTN" ; soundex = "" }
  ; { input = "de Toeni" ; double_metaphone = "TTN" ; soundex = "" }
  ; { input = "de Tony" ; double_metaphone = "TTN" ; soundex = "" }
  ; { input = "de Umfreville" ; double_metaphone = "TMFR" ; soundex = "" }
  ; { input = "de Valognes" ; double_metaphone = "TFLN" ; soundex = "" }
  ; { input = "de Vaux" ; double_metaphone = "TF" ; soundex = "" }
  ; { input = "de Vere" ; double_metaphone = "TFR" ; soundex = "" }
  ; { input = "de Vermandois" ; double_metaphone = "TFRM" ; soundex = "" }
  ; { input = "de Vernon" ; double_metaphone = "TFRN" ; soundex = "" }
  ; { input = "de Vexin" ; double_metaphone = "TFKS" ; soundex = "" }
  ; { input = "de Vitre" ; double_metaphone = "TFTR" ; soundex = "" }
  ; { input = "de Wandesford" ; double_metaphone = "TNTS" ; soundex = "" }
  ; { input = "de Warenne" ; double_metaphone = "TRN" ; soundex = "" }
  ; { input = "de Westbury" ; double_metaphone = "TSTP" ; soundex = "" }
  ; { input = "di Saluzzo" ; double_metaphone = "TSLS" ; soundex = "" }
  ; { input = "fitz Alan" ; double_metaphone = "FTSL" ; soundex = "" }
  ; { input = "fitz Geoffrey" ; double_metaphone = "FTSJ" ; soundex = "" }
  ; { input = "fitz Herbert" ; double_metaphone = "FTSR" ; soundex = "" }
  ; { input = "fitz John" ; double_metaphone = "FTSJ" ; soundex = "" }
  ; { input = "fitz Patrick" ; double_metaphone = "FTSP" ; soundex = "" }
  ; { input = "fitz Payn" ; double_metaphone = "FTSP" ; soundex = "" }
  ; { input = "fitz Piers" ; double_metaphone = "FTSP" ; soundex = "" }
  ; { input = "fitz Randolph" ; double_metaphone = "FTSR" ; soundex = "" }
  ; { input = "fitz Richard" ; double_metaphone = "FTSR" ; soundex = "" }
  ; { input = "fitz Robert" ; double_metaphone = "FTSR" ; soundex = "" }
  ; { input = "fitz Roy" ; double_metaphone = "FTSR" ; soundex = "" }
  ; { input = "fitz Scrob" ; double_metaphone = "FTSS" ; soundex = "" }
  ; { input = "fitz Walter" ; double_metaphone = "FTSL" ; soundex = "" }
  ; { input = "fitz Warin" ; double_metaphone = "FTSR" ; soundex = "" }
  ; { input = "fitz Williams" ; double_metaphone = "FTSL" ; soundex = "" }
  ; { input = "la Zouche" ; double_metaphone = "LSX" ; soundex = "" }
  ; { input = "le Botiller" ; double_metaphone = "LPTL" ; soundex = "" }
  ; { input = "le Despenser" ; double_metaphone = "LTSP" ; soundex = "" }
  ; { input = "le deSpencer" ; double_metaphone = "LTSP" ; soundex = "" }
  ; { input = "of Allendale" ; double_metaphone = "AFLN" ; soundex = "" }
  ; { input = "of Angouleme" ; double_metaphone = "AFNK" ; soundex = "" }
  ; { input = "of Anjou" ; double_metaphone = "AFNJ" ; soundex = "" }
  ; { input = "of Aquitaine" ; double_metaphone = "AFKT" ; soundex = "" }
  ; { input = "of Aumale" ; double_metaphone = "AFML" ; soundex = "" }
  ; { input = "of Bavaria" ; double_metaphone = "AFPF" ; soundex = "" }
  ; { input = "of Boulogne" ; double_metaphone = "AFPL" ; soundex = "" }
  ; { input = "of Brittany" ; double_metaphone = "AFPR" ; soundex = "" }
  ; { input = "of Brittary" ; double_metaphone = "AFPR" ; soundex = "" }
  ; { input = "of Castile" ; double_metaphone = "AFKS" ; soundex = "" }
  ; { input = "of Chester" ; double_metaphone = "AFXS" ; soundex = "" }
  ; { input = "of Clermont" ; double_metaphone = "AFKL" ; soundex = "" }
  ; { input = "of Cologne" ; double_metaphone = "AFKL" ; soundex = "" }
  ; { input = "of Dinan" ; double_metaphone = "AFTN" ; soundex = "" }
  ; { input = "of Dunbar" ; double_metaphone = "AFTN" ; soundex = "" }
  ; { input = "of England" ; double_metaphone = "AFNK" ; soundex = "" }
  ; { input = "of Essex" ; double_metaphone = "AFSK" ; soundex = "" }
  ; { input = "of Falaise" ; double_metaphone = "AFFL" ; soundex = "" }
  ; { input = "of Flanders" ; double_metaphone = "AFFL" ; soundex = "" }
  ; { input = "of Galloway" ; double_metaphone = "AFKL" ; soundex = "" }
  ; { input = "of Germany" ; double_metaphone = "AFKR" ; soundex = "" }
  ; { input = "of Gloucester" ; double_metaphone = "AFKL" ; soundex = "" }
  ; { input = "of Heristal" ; double_metaphone = "AFRS" ; soundex = "" }
  ; { input = "of Hungary" ; double_metaphone = "AFNK" ; soundex = "" }
  ; { input = "of Huntington" ; double_metaphone = "AFNT" ; soundex = "" }
  ; { input = "of Kiev" ; double_metaphone = "AFKF" ; soundex = "" }
  ; { input = "of Kuno" ; double_metaphone = "AFKN" ; soundex = "" }
  ; { input = "of Landen" ; double_metaphone = "AFLN" ; soundex = "" }
  ; { input = "of Laon" ; double_metaphone = "AFLN" ; soundex = "" }
  ; { input = "of Leinster" ; double_metaphone = "AFLN" ; soundex = "" }
  ; { input = "of Lens" ; double_metaphone = "AFLN" ; soundex = "" }
  ; { input = "of Lorraine" ; double_metaphone = "AFLR" ; soundex = "" }
  ; { input = "of Louvain" ; double_metaphone = "AFLF" ; soundex = "" }
  ; { input = "of Mercia" ; double_metaphone = "AFMR" ; soundex = "" }
  ; { input = "of Metz" ; double_metaphone = "AFMT" ; soundex = "" }
  ; { input = "of Meulan" ; double_metaphone = "AFML" ; soundex = "" }
  ; { input = "of Nass" ; double_metaphone = "AFNS" ; soundex = "" }
  ; { input = "of Normandy" ; double_metaphone = "AFNR" ; soundex = "" }
  ; { input = "of Ohningen" ; double_metaphone = "AFNN" ; soundex = "" }
  ; { input = "of Orleans" ; double_metaphone = "AFRL" ; soundex = "" }
  ; { input = "of Poitou" ; double_metaphone = "AFPT" ; soundex = "" }
  ; { input = "of Polotzk" ; double_metaphone = "AFPL" ; soundex = "" }
  ; { input = "of Provence" ; double_metaphone = "AFPR" ; soundex = "" }
  ; { input = "of Ringelheim" ; double_metaphone = "AFRN" ; soundex = "" }
  ; { input = "of Salisbury" ; double_metaphone = "AFSL" ; soundex = "" }
  ; { input = "of Saxony" ; double_metaphone = "AFSK" ; soundex = "" }
  ; { input = "of Scotland" ; double_metaphone = "AFSK" ; soundex = "" }
  ; { input = "of Senlis" ; double_metaphone = "AFSN" ; soundex = "" }
  ; { input = "of Stafford" ; double_metaphone = "AFST" ; soundex = "" }
  ; { input = "of Swabia" ; double_metaphone = "AFSP" ; soundex = "" }
  ; { input = "of Tongres" ; double_metaphone = "AFTN" ; soundex = "" }
  ; { input = "of the Tributes" ; double_metaphone = "AF0T" ; soundex = "" }
  ; { input = "unknown" ; double_metaphone = "ANKN" ; soundex = "" }
  ; { input = "van der Gouda" ; double_metaphone = "FNTR" ; soundex = "" }
  ; { input = "von Adenbaugh" ; double_metaphone = "FNTN" ; soundex = "" }
  ; { input = "ARCHITure" ; double_metaphone = "ARKT" ; soundex = "" }
  ; { input = "Arnoff" ; double_metaphone = "ARNF" ; soundex = "" }
  ; { input = "Arnow" ; double_metaphone = "ARN" ; soundex = "" }
  ; { input = "DANGER" ; double_metaphone = "TNJR" ; soundex = "" }
  ; { input = "Jankelowicz" ; double_metaphone = "JNKL" ; soundex = "" }
  ; { input = "MANGER" ; double_metaphone = "MNJR" ; soundex = "" }
  ; { input = "McClellan" ; double_metaphone = "MKLL" ; soundex = "" }
  ; { input = "McHugh" ; double_metaphone = "MK" ; soundex = "" }
  ; { input = "McLaughlin" ; double_metaphone = "MKLF" ; soundex = "" }
  ; { input = "ORCHEStra" ; double_metaphone = "ARKS" ; soundex = "" }
  ; { input = "ORCHID" ; double_metaphone = "ARKT" ; soundex = "" }
  ; { input = "Pierce" ; double_metaphone = "PRS" ; soundex = "" }
  ; { input = "RANGER" ; double_metaphone = "RNJR" ; soundex = "" }
  ; { input = "Schlesinger" ; double_metaphone = "XLSN" ; soundex = "" }
  ; { input = "Uomo" ; double_metaphone = "AM" ; soundex = "" }
  ; { input = "Vasserman" ; double_metaphone = "FSRM" ; soundex = "" }
  ; { input = "Wasserman" ; double_metaphone = "ASRM" ; soundex = "" }
  ; { input = "Womo" ; double_metaphone = "AM" ; soundex = "" }
  ; { input = "Yankelovich" ; double_metaphone = "ANKL" ; soundex = "" }
  ; { input = "accede" ; double_metaphone = "AKST" ; soundex = "" }
  ; { input = "accident" ; double_metaphone = "AKST" ; soundex = "" }
  ; { input = "adelsheim" ; double_metaphone = "ATLS" ; soundex = "" }
  ; { input = "aged" ; double_metaphone = "AJT" ; soundex = "" }
  ; { input = "ageless" ; double_metaphone = "AJLS" ; soundex = "" }
  ; { input = "agency" ; double_metaphone = "AJNS" ; soundex = "" }
  ; { input = "aghast" ; double_metaphone = "AKST" ; soundex = "" }
  ; { input = "agio" ; double_metaphone = "AJ" ; soundex = "" }
  ; { input = "agrimony" ; double_metaphone = "AKRM" ; soundex = "" }
  ; { input = "album" ; double_metaphone = "ALPM" ; soundex = "" }
  ; { input = "alcmene" ; double_metaphone = "ALKM" ; soundex = "" }
  ; { input = "alehouse" ; double_metaphone = "ALHS" ; soundex = "" }
  ; { input = "antique" ; double_metaphone = "ANTK" ; soundex = "" }
  ; { input = "artois" ; double_metaphone = "ART" ; soundex = "" }
  ; { input = "automation" ; double_metaphone = "ATMX" ; soundex = "" }
  ; { input = "bacchus" ; double_metaphone = "PKS" ; soundex = "" }
  ; { input = "bacci" ; double_metaphone = "PX" ; soundex = "" }
  ; { input = "bajador" ; double_metaphone = "PJTR" ; soundex = "" }
  ; { input = "bellocchio" ; double_metaphone = "PLX" ; soundex = "" }
  ; { input = "bertucci" ; double_metaphone = "PRTX" ; soundex = "" }
  ; { input = "biaggi" ; double_metaphone = "PJ" ; soundex = "" }
  ; { input = "bough" ; double_metaphone = "P" ; soundex = "" }
  ; { input = "breaux" ; double_metaphone = "PR" ; soundex = "" }
  ; { input = "broughton" ; double_metaphone = "PRTN" ; soundex = "" }
  ; { input = "cabrillo" ; double_metaphone = "KPRL" ; soundex = "" }
  ; { input = "caesar" ; double_metaphone = "SSR" ; soundex = "" }
  ; { input = "cagney" ; double_metaphone = "KKN" ; soundex = "" }
  ; { input = "campbell" ; double_metaphone = "KMPL" ; soundex = "" }
  ; { input = "carlisle" ; double_metaphone = "KRLL" ; soundex = "" }
  ; { input = "carlysle" ; double_metaphone = "KRLL" ; soundex = "" }
  ; { input = "chemistry" ; double_metaphone = "KMST" ; soundex = "" }
  ; { input = "chianti" ; double_metaphone = "KNT" ; soundex = "" }
  ; { input = "chorus" ; double_metaphone = "KRS" ; soundex = "" }
  ; { input = "cough" ; double_metaphone = "KF" ; soundex = "" }
  ; { input = "czerny" ; double_metaphone = "SRN" ; soundex = "" }
  ; { input = "deffenbacher" ; double_metaphone = "TFNP" ; soundex = "" }
  ; { input = "dumb" ; double_metaphone = "TM" ; soundex = "" }
  ; { input = "edgar" ; double_metaphone = "ATKR" ; soundex = "" }
  ; { input = "edge" ; double_metaphone = "AJ" ; soundex = "" }
  ; { input = "filipowicz" ; double_metaphone = "FLPT" ; soundex = "" }
  ; { input = "focaccia" ; double_metaphone = "FKX" ; soundex = "" }
  ; { input = "gallegos" ; double_metaphone = "KLKS" ; soundex = "" }
  ; { input = "gambrelli" ; double_metaphone = "KMPR" ; soundex = "" }
  ; { input = "geithain" ; double_metaphone = "K0N" ; soundex = "" }
  ; { input = "ghiradelli" ; double_metaphone = "JRTL" ; soundex = "" }
  ; { input = "ghislane" ; double_metaphone = "JLN" ; soundex = "" }
  ; { input = "gough" ; double_metaphone = "KF" ; soundex = "" }
  ; { input = "hartheim" ; double_metaphone = "HR0M" ; soundex = "" }
  ; { input = "heimsheim" ; double_metaphone = "HMSM" ; soundex = "" }
  ; { input = "hochmeier" ; double_metaphone = "HKMR" ; soundex = "" }
  ; { input = "hugh" ; double_metaphone = "H" ; soundex = "" }
  ; { input = "hunger" ; double_metaphone = "HNKR" ; soundex = "" }
  ; { input = "hungry" ; double_metaphone = "HNKR" ; soundex = "" }
  ; { input = "island" ; double_metaphone = "ALNT" ; soundex = "" }
  ; { input = "isle" ; double_metaphone = "AL" ; soundex = "" }
  ; { input = "jose" ; double_metaphone = "HS" ; soundex = "" }
  ; { input = "laugh" ; double_metaphone = "LF" ; soundex = "" }
  ; { input = "mac caffrey" ; double_metaphone = "MKFR" ; soundex = "" }
  ; { input = "mac gregor" ; double_metaphone = "MKRK" ; soundex = "" }
  ; { input = "pegnitz" ; double_metaphone = "PNTS" ; soundex = "" }
  ; { input = "piskowitz" ; double_metaphone = "PSKT" ; soundex = "" }
  ; { input = "queen" ; double_metaphone = "KN" ; soundex = "" }
  ; { input = "raspberry" ; double_metaphone = "RSPR" ; soundex = "" }
  ; { input = "resnais" ; double_metaphone = "RSN" ; soundex = "" }
  ; { input = "rogier" ; double_metaphone = "RJ" ; soundex = "" }
  ; { input = "rough" ; double_metaphone = "RF" ; soundex = "" }
  ; { input = "san jacinto" ; double_metaphone = "SNHS" ; soundex = "" }
  ; { input = "schenker" ; double_metaphone = "XNKR" ; soundex = "" }
  ; { input = "schermerhorn" ; double_metaphone = "XRMR" ; soundex = "" }
  ; { input = "schmidt" ; double_metaphone = "XMT" ; soundex = "" }
  ; { input = "schneider" ; double_metaphone = "XNTR" ; soundex = "" }
  ; { input = "school" ; double_metaphone = "SKL" ; soundex = "" }
  ; { input = "schooner" ; double_metaphone = "SKNR" ; soundex = "" }
  ; { input = "schrozberg" ; double_metaphone = "XRSP" ; soundex = "" }
  ; { input = "schulman" ; double_metaphone = "XLMN" ; soundex = "" }
  ; { input = "schwabach" ; double_metaphone = "XPK" ; soundex = "" }
  ; { input = "schwarzach" ; double_metaphone = "XRSK" ; soundex = "" }
  ; { input = "smith" ; double_metaphone = "SM0" ; soundex = "" }
  ; { input = "snider" ; double_metaphone = "SNTR" ; soundex = "" }
  ; { input = "succeed" ; double_metaphone = "SKST" ; soundex = "" }
  ; { input = "sugarcane" ; double_metaphone = "XKRK" ; soundex = "" }
  ; { input = "svobodka" ; double_metaphone = "SFPT" ; soundex = "" }
  ; { input = "tagliaro" ; double_metaphone = "TKLR" ; soundex = "" }
  ; { input = "thames" ; double_metaphone = "TMS" ; soundex = "" }
  ; { input = "theilheim" ; double_metaphone = "0LM" ; soundex = "" }
  ; { input = "thomas" ; double_metaphone = "TMS" ; soundex = "" }
  ; { input = "thumb" ; double_metaphone = "0M" ; soundex = "" }
  ; { input = "tichner" ; double_metaphone = "TXNR" ; soundex = "" }
  ; { input = "tough" ; double_metaphone = "TF" ; soundex = "" }
  ; { input = "umbrella" ; double_metaphone = "AMPR" ; soundex = "" }
  ; { input = "vilshofen" ; double_metaphone = "FLXF" ; soundex = "" }
  ; { input = "von schuller" ; double_metaphone = "FNXL" ; soundex = "" }
  ; { input = "wachtler" ; double_metaphone = "AKTL" ; soundex = "" }
  ; { input = "wechsler" ; double_metaphone = "AKSL" ; soundex = "" }
  ; { input = "weikersheim" ; double_metaphone = "AKRS" ; soundex = "" }
  ; { input = "zhao" ; double_metaphone = "J" ; soundex = "" }
  ]

