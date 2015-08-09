structure Excluded =
struct

  (* Does not contain any power words. Some are subsequences of a de bruijn sequence
     so they are encoded pretty efficiently (some have a prefix like "hog" in order to
     put the piece in a place where it doesn't immediately crash).

     Others are from powerball.txt, which is shotgun-style guesses (pastes from the
     lovecraft wiki, etc.)
     *)
  val excluded =
    ["hog1exkexsextexuexwexxfffyff2ffaffgffhffiffjff4fflffmffnffoff ff5ffdffqffrffvffzff1ffkffsfftffuffwffxfyyfy2fyafygfyhfyifyjfy4fylfymfynfyofy fy5fydfyqfyrfyvfyzfy1fykfysfytfyufywfyxf2yf22f2af2gf2hf2if2jf24f2lf2mf2nf2of2 f25f2df2qf2rf2vf2zf21f2kf2sf2tf2uf2wf2xfayfa2faafagfahfaifajfa4falfamfanfaofa fa5fadfaqfarfavfazfa1fakfasfatfaufawfaxfgyfg2fgafggfghfgifgjfg4fglfgmfgnfgofg fg5fgdfgqfgrfgvfgzfg1fgkfgsfgtfgufgwfgxfhyfh2fhafhgfhhfhifhjfh4fhlfhmfhnfhofh fh5fh",
     "4bblbbmbbnbbobb bb5bbdbbqbbrbbvbbzbb1bbkbbsbbtbbubbwbbxbccbcebcfbcybc2bcabcgbchbcibcjbc4bclbcmbcnbcobc bc5bcdbcqbcrbcvbczbc1bckbcsbctbcubcwbcxbecbeebefbeybe2beabegbehbeibejbe4belbembenbeobe be5bedbeqberbevbezbe1bekbesbetbeubewbexbfcbfebffbfybf2bfabfgbfhbfibfjbf4bflbfmbfnbfobf bf5bfdbfqbfrbfvbfzbf1bfkbfsbftbfubfwbfxbycbyebyfbyyby2byabygbyhbyibyjby4bylbymbynbyoby by5by",
     "hog1fxkfxsfxtfxufxwfxxyyy2yyayygyyhyyiyyjyy4yylyymyynyyoyy yy5yydyyqyyryyvyyzyy1yykyysyytyyuyywyyxy22y2ay2gy2hy2iy2jy24y2ly2my2ny2oy2 y25y2dy2qy2ry2vy2zy21y2ky2sy2ty2uy2wy2xya2yaayagyahyaiyajya4yalyamyanyaoya ya5yadyaqyaryavyazya1yakyasyatyauyawyaxyg2ygayggyghygiygjyg4yglygmygnygoyg yg5ygdygqygrygvygzyg1ygkygsygtyguygwygxyh2yhayhgyhhyhiyhjyh4yhlyhmyhnyhoyh yh5yh",
     "hogdbyqbyrbyvbyzby1bykbysbytbyubywbyxb2cb2eb2fb2yb22b2ab2gb2hb2ib2jb24b2lb2mb2nb2ob2 b25b2db2qb2rb2vb2zb21b2kb2sb2tb2ub2wb2xbacbaebafbayba2baabagbahbaibajba4balbambanbaoba ba5badbaqbarbavbazba1bakbasbatbaubawbaxbgcbgebgfbgybg2bgabggbghbgibgjbg4bglbgmbgnbgobg bg5bgdbgqbgrbgvbgzbg1bgkbgsbgtbgubgwbgxbhcbhebhfbhybh2bhabhgbhhbhibhjbh4bhlbhmbhnbhobh",
     "ohi hi5hidhiqhirhivhizhi1hikhishithiuhiwhixhjihjjhj4hjlhjmhjnhjohj hj5hjdhjqhjrhjvhjzhj1hjkhjshjthjuhjwhjxh4ih4j",
     "hog1gxkgxsgxtgxugxwgxxhhhihhjhh4hhlhhmhhnhhohh hh5hhdhhqhhrhhvhhzhh1hhkhhshhthhuhhwhhxhiihijhi4hilhimhinhi",
     "hog1bxkbxsbxtbxubxwbxxccceccfccycc2ccaccgcchcciccjcc4cclccmccnccocc cc5ccdccqccrccvcczcc1cckccscctccuccwccxceecefceyce2ceacegcehceicejce4celcemcenceoce ce5cedceqcercevcezce1cekcescetceucewcexcfecffcfycf2cfacfgcfhcficfjcf4cflcfmcfncfocf cf5cfdcfqcfrcfvcfzcf1cfkcfscftcfucfwcfxcyecyfcyycy2cyacygcyhcyicyjcy4cylcymcyncyocy cy5cydcyqcyrcyvcy",
     "hog1bxkbxsbxtbxubxwbxxccceccfccycc2ccaccgcchcciccjcc4cclccmccnccocc cc5ccdccqccrccvcczcc1cckccscctccuccwccxceecefceyce2ceacegcehceicejce4celcemcenceoce ce5cedceqcercevcezce1cekcescetceucewcexcfecffcfycf2cfacfgcfhcficfjcf4cflcfmcfncfocf cf5cfdcfqcfrcfvcfzcf1cfkcfscftcfucfwcfxcyecyfcyycy2cyacygcyhcyicyjcy4cylcymcyncyocy cy5cydcyqcyrcyvcy",
     "hogzcy1cykcyscytcyucywcyxc2ec2fc2yc22c2ac2gc2hc2ic2jc24c2lc2mc2nc2oc2 c25c2dc2qc2rc2vc2zc21c2kc2sc2tc2uc2wc2xcaecafcayca2caacagcahcaicajca4calcamcancaoca ca5cadcaqcarcavcazca1cakcascatcaucawcaxcgecgfcgycg2cgacggcghcgicgjcg4cglcgmcgncgocg cg5cgdcgqcgrcgvcgzcg1cgkcgscgtcgucgwcgxchechfchych2chachgchhchichjch4chlc",
     "abholosabhothaiueb gnshalalalaalithlai tyyammutsebaapocolothothayi'igaylithbasatanbastbyatiscthaeghya",
     "cthaatcoinchenncoatlicuecluluclooloocrom cruachclearedcighuluchaugnar faugncathuluazathothbigger scorescthugha'ymnarcthulhucthullucthulucthyllactogghacyaeghaeyroix",
     "cxaxukluthcynothoglysei'lordzewadythalladygradveahtehserrordagondaolothderlethdhuminfinalsgaloisghadamonghatanothoahoneycombhoggghastalyksebekscathachkaalutkthanid",
     "klosmiebhyxkathulukassogthakthulhukthulhutktulukulhukutulamkutunluulocksmh'ithrhakutuluph'nglui",
     "backusbanzaibenthicbigbootebuckaroobuckaroo banzaicharonc'thulhucockeconwayerisghisguthghrothgilman houseglaakigleethgloon",
     (* during search for power word in powerball phrase *)
     "xalafuxctholxexanothwhorfinwatsonlamportmormomotheryhagniyhoundehyidhrayigyomagn'thoparker",
     "watermelonvorvadossultharvoltiyigtulushugguathulhu regiothu thuthe worm that gnaws in the nightthe nameless mistthe hydrasthanee",
     "eighth dimensioneihortemdallgobogeggod of the red fluxgolothessistashaiodhouellebecqgtuhanaihoare",
     "charybdishaumeahoney beesjanai'ngok'nar'stk'tulukaajh'kaalbhkasparovliskovlloigorhnarqulovecraftlythaliaarwassam'nagalah", (* pb_789 *)
     "tharapithiaraphanasuanthanaroasmolenskhopcroftsoon!star motherrlim shaikorthzindarak", (* pb_5a0 *)
     "ovytonvothuumorryxoornolkothogham waitenyogthanycteliosnycramanyaghogguamril thorionmother of pusmordiggianpluto", (* pb_d6b *)
     "summanussmallberriesth'ryghthe green godyorithxislanyxxirdnethxa'lighavulthoomysbaddadenythogthavthyarilopsmnomquahz'toggua", (* pb_ea7 *)
     "makemakemappo no ryujinarkhammichelmilnermlandothdarknessmynoghranugpharolothuyeg", (* pb_90d *)
     "shlithnethshathakshaklatalsfatlicllpvarunaserioussedna", (* pb_753 *)
     "xoxiigghuaxotliswarogsk'taishterotshavalyoth", (* pb_c5d *)
     "nctolhunctosaphrases of powerragnallaprintposeidonplanetxplanet xpimoa cthulhusaa'itiirokonrivestscyllasathogguayaya", (* pb_2ff *)
     "yaggdytharhogogrh'thullareal soonzathogh'chtelegothinpescazehiretezhar", (* pb_c0d *)
     "amon gorlothaphoom zhahazhorra thaadaeduubbo sathla", (* pb_9a5 *)
     "shuy nihlsho gathsheb tethshabbith kasedmelluqrhan tegoths'tya yg'nallenoth yidikyibb tstll", (* pb_dac *)
     "ptar axtlanreal soon!kaunuzothraandaii b'nkyog saphagzxtyos", (* pb_3a0 *)
     "juk shabblu kthungyr korathnssu ghahnbut'ulls hr'hervile octhuitloxopetlvolgna gathuitzilcapacyad thaddag", (* pb_f33 *)
     "lord john whorfin barbara liskov robin milner ron rivest ronald rivest sir tony hoare john conway john cocke", (* submittyfa9 *)
     "etaoin shrdluflux capacitord wavegi hoveggigawattsgog hoorgol gorothasdfgreat scottyegg ha", (* pb_516 *)
     "great scott!groth golkagorthaiogh yaic'thluhypnos", (* pb_8a9 *)
     "allons yatlach nachabad wolfbalrogbugg shashdoctor whogeronimoidh yaajohn backusmonkey boymoriamr fusionpandoricariver songq'yth azrobin minlerzoth ommog", (* pb_3ac *)
     "fly you foolsfly you fools!nautilustardisthe doctorzhar and lloigorzvilpogghuaunder the sea", (* pb_5a5 *)
     "no matter where you go there you areremember no matter where you go there you are", (* pb_c03 *)
     ""]

end
