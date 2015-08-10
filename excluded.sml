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
     "chaimchoose wiselyemetgolemwatermelons", (* pb_e67 *)
     "'twas brillig and the slithy tovesall mimsy were the borogovesand the mome raths outgrabe42!don't panicnorthot", (* pb_13 *)
     "abracadabraavada kedavracallooh!hocus pocusbadwolfvogon", (* pb_efb *)
     "oh no! not again!oh no not againthe cloud thingpresto", (* pb_e13 *)
     "as2h2atlastbig bootybigboo tayconstancedid gyre and gimble in the wabefalconer madandon't panic!laugh aformless spawnlaughalowell observatorynew horizonvenetiaithaquaremember no matter where you go there you are!", (* pb_45 *)
     "what do you get when you multiply six by ninevoilazaphod beeblebrox", (* pb_5d2 *)
     "hp lovecrafthplovecraftso long and thanks for all the fishzeus", (* pb_7d8 *)
     "hogzymal", (* pb_6d2 *)
     "hoghoghogothkkartho", (* pb_82c *)
     "galois connectionsinternational conference on functional programminginternational conference on functional programming 2015international conference on functional programming!zymal", (* pb_d9d *)
     "hogxyzzy", (* pb_262 *)
     "blue dandelionbashful incendiaryanning blue skullcandid", (* pb_133, pre "case nightmare green" *)
     "club zeroapocalypse codexcobracobweb mazecode bluecode redconcrete junglecult of the bound variabledown on the farmemocumphang", (* pb_133, post "case nightmare green" *)
     "magic circle of safetyoccintocculuso frabjous day!old dreamerinternational conference on functional programming!percival lowellhowardgreen limemortllghgod game redemocumcallay!", (* pb_4b7 *)
     "ia cthulhugod game violetgod game silvergod game rediddqdploverjohnny prince", (* pb_a5a *)
     "magic circle of safetyoccintocculuso frabjous day!old dreamerjennifer morguescorpion starekag'naruplughpluto koboldhziulquoigmnzhahjabberwockteapotthe concrete jungletowel", (* pb_38e *)
     "god game bluedante eastevariste galoisgod game greendevil's reefdouglas adamsgod game rainbowia cthulhu!idclipialdagorth", (* pb_d43 *)
     "acm sigplanangletonannihilation scoreanning blackgod game black", (* pb_a77 *)
     "the jennifer morguethe annihilation scorethank yousim sala bimeppirfonthe fuller memorandumshemhamforashshazampleasejoshikazam", (* pb_8fa *)
     "aajayealakazamblorbajji majji la tarajjisorryglulxwelcomewalla walla washington", (* pb_23f *)
     "on ilka moor bah't'atbrookwoodon ilka mor bah't 'aton ilkla moor baht 'at", (* pb_de6 *)
     "on ilkla moor baht'aton ilkley moor baht'atschloss neuschwansteinthe rhesus chart", (* pb_2e *)
     "lllllllldeep seven", (* pbwjl_31e *)
     "bob howardcase nightmare rainbowget lamph.p. lovecrafth.p.lovecrafthello aviatorhumans", (* pb_5d1 *)     
     "alison chaoare we cool yetcontainment breachredactedjantar mantar jadu mantar", (* pb_240 *)
     "the factorythe global occult coalitionbenthic treaty", (* pb_ff3 *)
     "hoghoghogpxinlurgashno matter where you go... there you are!", (* pb_c77 *)
     "hoghoggod game purple", (* pb_bdf *)
     "hoghogleslie lamport", (* pb_d01 *)
     "hoghogngirrth'luthe chaos insurgencyquyagenkrangjabberwockyycnagnnissszburneyfortytwoob'mbuoztalunfrotz", (* pb_289 problem 20 *)
     "hogthe atrocity archivestulzschathe deep onesthe fifth churchdoctor wondertainmentequoidcquluicfppimpf", (* pb_f6b problem 20 *)
     "hoghoguuddlrlrbarhesus positive epsilonbaoht z'uqqa moggvancouvermaginot blue starsscpkurpannga", (* pb_c08 problem 20 *)
     "hoghoghoghoggurathnakagur'la yasupercalifragilisticexpialidociousatrocity archives", (* pb_103 problem 20 *)
     "blue dandelionanning blue skullcobweb mazecode bluecode redgod game silvergod game violetinternational conference on functional programmingd'endrrahgod game greenl'chaim", (* pb_84f, problem 24, post-"blue hades" *)
     "hogmkultrateapot baron tyburnpsuchawrlshaurash hoovertimequaoaridkfa", (* pb_7a9, problem 20 *)
     "hoghoghogvenetia burneyy'golonacup up down down left right left right b a startyou're welcomethe apocalypse codexy'llablue peacock", (* pb_b95, problem 20 *)
     "hoghogzorkmidremember no matter where you go... there you are!the black queen", (* pb_571, problem 20 *)
     "hoghasturbeware the jabberwock my son!", (* pb_63f, problem 20 *)
     "hoghoghogppthe church of the broken god", (* pb_e9a, problem 24 *)
     "hoghogsecure contain protectuvhash", (* pb_2b6, problem 20 *)
     "hoghogyug siturathyhashtur", (* pb_94b, problem 20 *)
     "hoghoghogppvancouver", (* pb_738, problem 24 *)
     "hoghoghoghogidspispopdvhuzompha", (* pb_20c, problem 20 *)
     "ppppquachil uttaus", (* pb_469, problem 24 *)
     "hogbbbbbbbbbbbqwertyuiop", (* pb_ee9, problem 20 *)
     "ppppquantum computing", (* pb_135, problem 24 *)
     "hoghoghoghogppppopen sesame", (* pb_754, problem 20 *)
     "pppzstylzhemghi", (* pb_321, problem 24 *)
     "elfgo back to the shadow!hello worldjehovahequestrian red sirloinb'gnu thunjhvh", (* pb_d5a *)
     "pppppzushakon", (* pb_f96 *)
     "yahwehyhwhviburhog leslie lamport!!!", (* pb_c34 *)
     ""]

end
