module Extensible.DataSets where

sirInfectedData :: [Int]
sirInfectedData = [1,6,26,73,222,293,258,236,191,124,69,26,11,4]

nnLogDataY :: [Bool]
nnLogDataY = [True, False, False, True, True, True, False, True, False, False, False, False, False, True, True, False, False, False, True, False, True, True, True, True, False, False, False, True, False, False, True, False, True, True, False, False, False, False, False, True, False, False, True, False, False, False, True, False, True, False, False, False, False, False, True, False, True, True, True, True, False, True, True, False, False, True, False, False, True, False, False, False, False, True, True, True, False, False, True, False, False, False, False, False, False, False, False, False, False, True, True, False, True, True, False, False, False, False, True, True, False, False, False, True, True, True, False, False, True, False, False, False, False, False, True, False, False, True, False, True, True, True, True, False, False, True, False, True, True, False, False, True, False, True, True, False, True, False, False, True, True, True, False, False, True, True, False, True, False, True, False, False, True, False, True, False, False, True, False, True, True, True, False, False, False, False, False, True, True, False, True, False, False, False, True, False, False, False, False, True, True, True, False, False, True, True, False, False, True, False, True, False, False, False, True, False, False, True, False, True, True, True, True, False, True, True, False, False, True, False, True, False, True, True, False, False, False, True, False, True, True, True, False, True, False, False, False, True, False, False, True, True, False, True, True, False, False, True, False, False, True, False, False, False, False, False, False, True, True, True, False, True, False, True, False, True, False, False, True, True, False, True, False, False, False, False, False, False, True, True, True, False, False, True, False, True, True, True, False, True, False, False, False, True, True, True, False, True, False, True, True, False, False, True, True, True, False, False, True, False, True, False, True, False, False, False, True, False, True, False, True, True, True, False, True, False, False, False, True, True, False, False, False, True, False, False, False, True, False, False, True, False, True, False, False, True, True, True, True, True, False, True, True, False, False, True, True, True, False, True, True, False, False, False, False, False, True, True, False, False, False, True, True, False, True, True, False, True, False, False, True, False, True, True, False, True, False, False, False, True, False, False, True, True, False, True, False, True, True, True, True, False, False, True, False, True, False, False, True, True, True, True, True, False, False, True, True, True, True, True, True, False, False, False, True, False, True, True, True, True, False, True, True, False, False, True, True, False, False, True, True, False, True, False, True, True, True, False, True, True, False, False, False, True, False, False, True, False, True, True, True, True, False, False, True, False, True, True, True, False, False, False, False, True, False, True, False, True, False, True, True, True, True, False, False, False, False, True, True, False, True, True, False, False, False, False, False, False, False, True, False, True, True, True, True, True, False, True, True, False]

nnLogDataX :: [(Double, Double)]
nnLogDataX = [(0.6311214683497496, -0.28960983587619304), (-0.17797762815378357, -1.0869588469179012), (-1.9256117267852344, 0.8413071805546803), (-1.2469575158282271, 0.7487330718662089), (0.28875033022801255, -0.97955327030011), (-1.153902571404388, 1.1757630649635904), (-0.37473367625331344, -0.6472544788037583), (1.36731798221196, -0.5844525244640739), (-0.34121358821594, -0.9749470745542601), (1.426492844760193, -0.8178562192171703), (-0.30618992713627874, -1.3550405769447407), (0.7520883371080749, -1.39705413664288), (0.6968477938932797, -0.6654627490053368), (1.3697621572738505, -0.767573049855639), (-1.3060856767938485, 1.3401830902742635), (0.17112589508349277, -0.9141360717662991), (0.1250426587703931, 0.9273653406809335), (-1.564185490057876, 0.4877574066224553), (0.7381387941829012, 0.9544868269566315), (-0.5754351926059786, 0.7098705787098063), (-0.42226924969940105, 1.7257009051106653), (1.559172721193746, -1.0958711470474871), (0.9667637768670136, -0.8001654128024904), (-1.2551737293023741, 0.49652220677816544), (0.11869579558798131, 0.8669582793773627), (1.3851305654710673, 0.17867440145116462), (0.8518427482630644, -1.26114442371491), (1.8880187618351085, -0.5450656919145256), (-1.6021739225969804, 0.5372366972313338), (-1.6343052790333068, -5.7009285960334285e-05), (0.017416461432979608, 1.3228490733643359), (-1.3743693112110411, 0.7972476848343152), (-0.3195400092868223, -0.1969638344588431), (-0.3303829384467706, 0.1691302673136519), (0.489404944229022, 0.3189009178219242), (0.31409828193292977, 0.36735984436977054), (0.42946551905838, -1.55727370446633), (-0.7305473454208614, 0.8243271091327827), (-0.435558607155793, 1.4531308863249823), (1.324834607542703, -0.1593143910240347), (0.7057566943411484, -1.1332256925372461), (0.738157714245218, -1.1480012053755058), (0.5832904336682114, 0.6347689378625129), (1.1319884298311618, -1.5493719887972954), (0.29588538383175933, -0.7856182705475614), (0.21122314638592263, -0.7030709654551467), (1.8528916839914513, -1.032557501689527), (-1.3417072287728786, 0.6245010942228848), (1.596516268176805, 0.4072198463647604), (0.2832108848809793, 0.18309290358918096), (-1.8425917950224866, -0.1663283444768519), (0.24553832034264508, -0.8537759760019967), (-0.18592475108967452, 1.7253548163746601), (-0.5167087586341418, 0.09362624684057984), (1.3196062217285507, -1.217451857197506), (-0.4707571474608543, 1.2550976565063758), (0.8031417214778478, -1.0917016615728719), (0.40418052019502826, -0.14198706529315833), (1.4365553352600886, -0.8011518064585305), (-0.1384223104689684, -0.7052216001629945), (-0.6843904216016845, 1.270413689295779), (-0.21871112026610356, -1.113087860498957), (0.42193932755490676, 1.4954431147427447), (-0.6901707155679737, 0.8599105149960939), (0.09448134965820648, 0.399075960871884), (-1.278896697772233, -0.7679997352696134), (0.22267874118577227, 1.3495971118952996), (-0.9407289293196044, 0.9936962512532177), (0.7848649103115045, -1.3529659878293963), (-0.10994830911056117, 1.5174704510505623), (-0.0933790471000936, -1.2910903125202686), (0.44753631697038104, 0.6411822555752746), (0.6070547753883605, -1.275926607862663), (0.135952482219554, 0.964402563447361), (0.5923434138530741, -1.4169087612779707), (0.5024502464769652, -0.03316645516692798), (1.5067122621139495, -0.9586821263661853), (-0.24678150103750449, -0.5602009268372522), (0.6286019315136466, 0.48566210543009436), (0.8211754841680127, -0.9533815477422458), (0.022443183417891553, 0.9768990936884032), (0.06854980906239289, 0.184845772814779), (1.8382492720662291, 0.11950387288471374), (1.2677998762387193, -1.3419462519202012), (-1.9497004389836559, -0.19554399394210176), (-0.056511078111367886, 0.5741769214728738), (-2.0268416228713932, 0.34258287163128376), (-0.42562483150445685, -0.4476229362081409), (-0.9587975813698298, 1.3572647040724792), (2.124951811756866, 0.5680770076568733), (-2.042591414936162, 0.6536441471894003), (-1.4932028675127915, -0.04499034527035607), (0.48736175493428, -1.0605175132238778), (0.546348347034563, 0.6390243218330033), (-0.7801290345865766, -0.1898272926830457), (-0.9102286100695389, 1.4014144009977638), (1.789991472298173, 0.043203817016176205), (0.9884847274695423, -1.2524906201022081), (-1.6875288275956537, 0.5484926516116805), (-0.011920320086478888, 1.2343522127831787), (-0.3695287086710493, 1.3687279775696026), (0.6074633436532593, 1.162219705659753), (-0.06163849352867019, 0.36424328267587436), (1.5740366705857367, -1.692354167028319), (-0.45477442194560413, -0.5895556394020192), (-0.6024615332937792, 1.6755596134488477), (1.3316872805852067, 0.056544431857253476), (0.961841225961403, 0.07869224313044847), (-1.410680322270569, 0.6845943657687735), (-1.0735643159397315, 1.4841985887788876), (0.144159700625438, 1.3404980415503667), (-0.1931642232989139, 1.1628382482234285), (0.6331576199117693, -0.5121377559297371), (0.9176037179408928, -0.41303297791668897), (-0.07664769026626947, -1.2591451281709418), (-0.44910371645602465, 1.656840622770707), (-1.2804295175791698, 0.9456792071670342), (-1.8848711508356006, 0.007252985131971699), (0.25205321372751566, 0.12681734378858647), (-0.012783618807834674, -0.5376175507250043), (-0.856949004109301, 1.1672337969377844), (1.578901198956067, -0.08103035749590255), (-0.1803248037824348, -1.1557202763159027), (0.1489984614010066, 1.073541946478792), (-1.667175643743927, 0.21214728853676243), (1.7749749938875041, 0.9968907982010777), (0.6158763497216847, 1.5264242790296345), (-0.6313214091296473, 1.3572572532424239), (-0.0898555900157588, -0.5929442108820188), (1.2937571559120442, -0.5425001983716419), (0.8311052832198869, -1.9092711941530485), (-0.4298694034022312, 0.6734587307049696), (1.781148235353765, -0.32360644976136427), (0.07742427158571803, 1.02403485637836), (0.4153467751711281, -0.6612975377771163), (1.3263048499523442, -0.9556974529110144), (1.5718680952347686, -0.1489184272721593), (-0.11318738731648707, -1.014599489155529), (-1.6788473801496282, -0.32330739348314086), (-0.23177844132158282, 1.2777372055165346), (0.15605376086053388, -0.1077144275306466), (1.7818376093645942, 0.1504625645651211), (0.005617069369470083, -1.1068416931397498), (-1.573943246244794, -0.09279959630401463), (0.5164658473189351, 0.0702366756133234), (-0.9389410249646065, 1.1321416923786702), (1.0078047710484832, -1.2837175091226372), (0.1743559709698879, 1.0448375910592502), (-1.7411760152522957, 0.1406637795680133), (-0.04273032067349001, 1.5457940428531798), (1.8976745196829017, -0.006720776566672287), (-0.9747075331571693, 1.384660285479797), (-0.3992947337132801, 1.5818320525796512), (1.6648476542246007, -0.546074882746082), (0.8804911662298648, -1.8647836074884026), (1.7778057779842427, 0.607589425787672), (-0.6285411813260845, 1.5387394746213439), (-0.625187172600356, 1.6460384739006093), (-1.2830343713635037, 1.296878682060551), (0.2865012748506953, -1.525109986923553), (0.3173719651846082, -0.17310756116471335), (-0.8371552698976674, 0.9182261741112604), (-0.08113540979830851, -1.0758419548644977), (1.0508381907731874, -1.4886279275108616), (1.1159750190458602, -0.8015641759403564), (1.051895705556892, -0.8926144076583101), (1.250101259617212, -0.9701995327248344), (-1.934897987766495, -0.12153726606322672), (1.7961614283229894, -0.8921081298272776), (-1.602044046252546, 0.6887700165264444), (1.67655883700709, 0.5488749710208128), (-0.935376968679363, 1.8990531173152172), (-0.3683215012637515, 0.04100325687563592), (-1.5892163799133372, 0.740190473839353), (-1.0057999792986239, 1.778444047805054), (-1.4061505292291312, 0.4307537478270723), (-0.5180718170861416, -1.3360744566545153), (1.0337317244931095, -1.0033371892462377), (0.2993721230732864, 1.0163709215086336), (-1.250771699459192, 0.8398482162041785), (1.5065830961772249, -0.5384209897444039), (1.3874720048672404, 0.1390105795091866), (-0.8985439441959021, 1.1702874645853774), (-1.5484042178344202, 0.6436010674800554), (-0.3788930803786776, -1.3491247214628974), (-0.5780862886846454, 1.5180080670079694), (0.9952809237577608, -1.6246243443084945), (0.71325891430425, -0.024843777040790074), (0.8787213737168644, -1.9193457591953698), (-0.5150103245828983, 1.1845375280558723), (0.4418290870780123, -1.4780642572879354), (0.6291029531964146, -1.59302435156941), (0.23146386969566074, 0.7019668774953759), (-0.13562673272234996, -0.7564523031815377), (0.5688880698430819, -1.1126825928796469), (-1.6252210317640605, -0.6840997978606514), (-0.9631671116827136, 1.5604806016253103), (-0.8909490970132952, 1.7709564225738874), (1.7414972415563166, -0.6743423446532472), (-0.08459212703528737, -1.8276101638361362), (-0.963720441378335, 1.060402724071973), (-0.3251572802067912, 1.9120014771335911), (1.6803239147900813, -1.1180467387315702), (0.3860206246298902, -1.4957069624970567), (-1.8780484226411867, 0.17205093430892981), (1.7145128101874836, 0.09279286262863941), (-1.335970822234295, 1.391858023872504), (-0.23827412554398947, -0.612609260974782), (1.3954463082426416, -1.0186445793812162), (0.3848325764666854, -0.013282377394845746), (0.016028321722095708, 1.6145352041275578), (-1.4553548975761077, 0.7879191717548809), (1.4490848360961432, 0.30554237698376085), (-0.21563829447419147, -1.2111977407159094), (0.9891291749338558, -1.32773492648767), (0.3142867118767592, -0.11359470950122497), (-0.4366167335984767, 1.306666236272482), (-0.3778844930291226, -0.9316636394454322), (1.3782260847075125, -0.9448575846049628), (0.5707774506741279, -1.34655671561507), (-1.3045915132232841, -1.1101534246749083), (0.10933937723801805, 1.7354758275483733), (-0.7063526850794056, 1.5997597618288626), (-1.3682066916555946, 1.1562063344097069), (1.0068488962211533, -0.9354653121861097), (-1.4787936732688105, 0.4748236925640353), (-1.4727214597222047, 0.19797719423017096), (1.4845989740092198, -0.9637287515812748), (1.3965301387469, -0.7631619493125347), (0.002931952452385018, 0.8823256454048309), (1.484212700491603, 0.2753905797139164), (0.4107647474258507, -1.2505049496339353), (1.0736737900255358, -1.8927926931235382), (-1.2496759546586937, 1.4195586061395638), (-0.90142016993168, 1.1681787125817988), (-0.14486251985269247, 1.4384248282531171), (-0.08160172762384278, -1.6147735447938933), (-0.6617555158082606, 0.2961951017852211), (0.6923729404255281, -1.8340531685778607), (1.4752820566093157, 0.0603781902489377), (0.7894376845738272, -0.5100827950065904), (-0.37155549957946776, 1.0241855932204829), (-1.307220345621983, 0.23600941838591868), (-0.45669909382915763, -0.663891536056515), (-0.8460416892424767, -0.11127504448165121), (0.14184301243745145, -1.403464328906825), (1.7828778181243714, 0.015449596134292146), (1.5789963480185407, -0.9330461339167856), (-0.6491776925406133, 1.5789397919709887), (-0.9765178205602586, 1.061026429709095), (-1.2046282341575414, -0.3372280737605144), (-1.8953259166301557, -0.21646293818695805), (-0.5683434499008713, 0.8693987361194637), (-1.3394563167210425, -0.9993600892167863), (-1.4118442164797713, 1.0131505859557577), (0.5899180144824372, 0.3812969162311781), (0.04444994043676115, -0.7148505999041475), (0.5716602323866549, 0.8416410543541853), (-0.6279370587282384, -0.7022711025325071), (1.258498648409362, -0.9520497857992486), (0.29446956536370666, -1.429554248253014), (-0.13406086252306756, -0.7752712440657673), (0.8299241897510693, -1.9046849683792444), (1.1844482390197182, -0.8013108305013421), (-0.4584762141117457, -0.6482575281506344), (-0.813028967319166, 0.1974040567778523), (0.2661954744829101, -0.8004168659239231), (-1.0024364246453117, 0.08880172558117164), (1.2086946573462272, -0.4070768951683332), (1.5795001776816515, -0.7728549530516364), (0.24026464759440083, -1.0282251448897917), (0.19249976564148172, 1.0416647528898098), (-0.3515382362158594, -0.7757208648183291), (0.2988017610617829, -0.07574470962937573), (-0.36230730735602745, 0.5133461450467788), (-0.08300719361759618, 0.633074775302685), (-0.08741154034001804, -1.7807925237820839), (0.19830098689139955, 0.5374763635379809), (0.2808070272403551, 1.3500673281383972), (1.3809988932093353, 0.015751738269243976), (1.1072225115763097, -0.3591228562304688), (1.5499882689032505, 0.06682818765898775), (-1.3603690208318284, 0.6361673277885833), (0.31625263221904604, 0.8099411215177909), (-0.7422427792608859, -0.04970701687922182), (0.8262696999186226, 0.40065867193662646), (-0.8238718615965077, 1.4916584941634157), (-0.28623387213986956, 0.9343423379331662), (-0.3030462911848741, 1.8884601106203038), (-1.5306748888216162, 0.7865846459516553), (-0.8279504375649369, 1.651889177988776), (-1.63885124235709, 0.20566867126651628), (0.23533735952001444, 0.788932548247166), (-0.028739225948771235, -2.121123269434025), (-1.1952251154384312, 0.32928151630573343), (-0.49615688123795065, -0.5395955195948785), (0.510763743415433, 0.23881664186938525), (0.3430762526857643, 0.7224316360287154), (0.9829313375275619, -1.2191784599590068), (0.9964889783486299, -1.7216020412933144), (1.5294770981339856, 0.1564998590398343), (0.42477534546088436, 0.8854979736288868), (1.4323529321562727, 0.6946575837740324), (-2.0712524393484735, -0.8113700976249334), (-0.06982150749364467, -0.8842618990505581), (1.2858578887586096, -1.5737621338927974), (0.019714242156580363, -1.4061430583063153), (-0.27063466288760496, 1.7233390312149692), (0.9405508347828292, -1.7344637745160556), (1.3401091780320031, -1.2939053447314113), (1.0605070063270061, -1.3296931387559108), (-0.02538403086036955, -0.977260489021146), (-0.6684440666580471, 1.6117233337593546), (0.15897306015561177, -1.6661108172643033), (-1.4152145764639108, 0.5281840932581694), (0.6266138226767657, 0.2270771398745063), (-0.23796546473408398, 0.23094285037938303), (-0.27912830512415543, 0.8460430432698193), (0.3668390799199857, -0.9726834799217161), (-0.18702861233256152, 1.6485313910500954), (-0.18516232108512737, -1.8382123294980903), (0.9403936245931201, -2.3208020321239644), (-0.919582094047216, 1.3669937293451213), (0.7876875014389292, -1.3692866079982187), (1.7389711022975292, 0.4265000331620348), (1.240826495928704, -0.9698729392558068), (1.5855955415992253, -0.2890764785506776), (1.429826571691139, -0.8064831969326565), (0.9797245054974668, -1.281023561888107), (1.6049043571992203, -0.21819094271622944), (-1.1687776455522758, 0.3895012448357548), (0.9301630382180919, -1.2828718430366541), (0.17706404285072633, 0.2781079448947822), (0.8129620202502149, -1.2218340027683874), (-1.2245331644087407, 1.2909419440619576), (0.014803689235960276, 0.7557223912560797), (0.05010697492015836, 0.8356363463531978), (0.3496280501994755, 0.42309473104345846), (0.3415658519167312, -1.724398712302698), (-0.5832243372037518, -0.21091958967654095), (-0.3042534645925362, 1.1652655402724073), (0.12107786238926331, -1.22397928469408), (0.13715339603346494, -1.2121900562752712), (0.537625891635701, -0.5442335958776283), (-0.956060444143122, 1.0195968176347026), (-0.051022964756233496, -1.401890922466634), (-1.2277574459177436, 1.7028254226500317), (1.9232551341850164, -0.07385150355332146), (-1.1916697665180793, 0.5989567968288118), (-2.0334886162985235, -0.1633578649654595), (-0.6755302496489786, -0.15884183718258502), (1.2650953255714896, -0.20214722213043163), (-1.1129898323243896, 1.3985131595930198), (-0.25684268078341427, 0.3397712027968041), (1.3096143396121276, -0.5043280294723391), (0.30025148581464667, 0.1983137370407054), (0.18430300218486426, -1.2591013067189167), (0.8761911151826095, -1.4896606265480588), (-1.8806941501787602, -0.5859098666292754), (0.3883543009984025, -1.4227828293502436), (0.7389289305443981, -1.4034154549636721), (0.27486838626220295, 1.7161771913812631), (0.5205229505178632, 1.1640337711856932), (-0.05733376240345521, 1.182415471088372), (-0.8036192399611195, 0.8585358840952165), (1.8624286999768667, -0.32259995642132544), (0.3533563906612574, 0.8064651946004041), (-1.0042122237938607, 1.9071239134680675), (-1.0500330714874762, 1.0397241685901486), (1.1834549428736632, -1.0740279341154797), (-1.4414348147241567, 1.0328828210274688), (0.645652152901135, -0.7764058689032133), (0.7250733820150521, 0.5993312556000018), (-0.08932469501818725, 1.1937955477492939), (1.2980814652994102, -1.829209651605702), (-0.8189727038980811, 0.6073951863575997), (0.25559203565296224, 0.3201504983900358), (-0.6106440046925314, 1.1567239234703806), (-1.5393770553671393, 0.11912334651920649), (-1.5396181134133868, 0.07769044640120124), (1.5350567616796087, 0.43643338697801276), (-1.0675777956022294, 1.4266033989535178), (1.3123310501369272, 0.3606671178201283), (0.051888232738256594, 1.5125683292724257), (0.015153727894639939, -1.1029464932214401), (0.03515903045009693, 1.1696771412364593), (-0.46533989912310464, -0.6705040224814358), (-1.653051107366923, 0.16168364794047085), (-0.414129070613347, 0.7915896496264762), (0.43906396256526065, 0.005444004573742193), (0.6966270512266242, 0.23804300227633748), (-0.696942784552838, 1.526825318007369), (-1.759680860491844, 0.9317878206565037), (2.0624570582595894, -0.36253355824369915), (-0.40284585375731136, 0.010358795027719024), (0.43567711107049184, -0.094991094622028), (1.325239299651492, -1.141930760810375), (1.851164716493272, -0.025930585570482705), (1.1352555234201331, -1.6013450192012455), (0.9235779520074252, 0.09779128512744326), (-0.3413899927594983, -0.5543291653745347), (0.6001531634120095, -1.4582531854069287), (-0.32839961699421155, 0.08698615429199862), (1.7066561262270308, -1.0204966288216242), (-0.08989743493778905, 1.0853187979473615), (0.09448048023801692, 0.7127722380421331), (0.24510021229529352, 0.9031188932451021), (0.152937952608442, -1.8317513911737635), (1.6917981540882028, -0.9320236067165407), (-0.7063736796090746, 1.0227178888501705), (0.38347147748588833, -0.24527113087556027), (0.35514571467473444, -1.468921838253296), (-0.8044918684451691, 1.4030824681820726), (1.7714543331526098, 0.07292061223496901), (0.3224453390008763, -0.6338918141397419), (0.04519445079141701, 1.612575302702938), (-0.44237801845439434, -0.859282938179067), (1.766010079400999, -0.772197957470287), (0.6291056962786994, -0.9394786567337848), (1.5734729673916994, 0.3529815872374604), (-0.5180672410674314, 0.18906241008682664), (-1.195570953891375, 1.1925176932360726), (1.7415672384210976, -1.1301274732859692), (-0.460757016967466, -0.0013363287447427925), (0.4178251275755493, 0.5402936952676909), (-1.079766958413481, 2.1020222469289047), (-1.3632913747126096, 1.1681453068183132), (-1.7255493156020105, 0.4854862783567333), (-0.11190071455002164, -0.3538922344270711), (1.9153279963349024, 0.14743989336757768), (0.36705334579933596, -0.5543546302089704), (-1.5253248446247716, 0.5783717993475024), (-0.6419696857949004, 0.23466080264615782), (0.9513483412991122, -1.6016802422146788), (-0.6508155020907884, 0.7331168590581703), (1.055551050596449, -0.5364054518205255), (-0.3412606049693436, -1.1186031472969002), (1.38079317481943, -1.2150016983737726), (-1.4575776636870221, 0.7926452287032449), (0.39213201315961443, -1.2453462681177074), (-0.2591359100928893, -0.2727306412917646), (-1.8001966977054118, 0.5220732611981254), (-1.8363667572811988, 0.0006423103054833411), (-1.46928407582687, -0.4135408208433472), (-0.4807866232436311, 0.3954772063055595), (-1.650151774289476, -0.08640180533580788), (1.5128972206866191, 0.8142228265027812), (1.01345363379515, -1.2636174072709674), (-0.25620513847754767, 1.69904028851683), (0.2174430073618891, 1.0368916235369388), (-0.2702855097716985, 1.134148977346839), (-1.7684394766827585, -1.1266739974626807), (0.41319220583634114, -1.6452093842245024), (-0.2462634352279909, -0.8145233006235882), (0.7687030682128755, 0.1681368286616103), (-0.4709487463937355, -0.2203214926075974), (1.660396400723322, -1.250945724225583), (-1.81811316849441, 0.43605548441396064), (-1.839210638748143, -0.14544687706803267), (0.4614991341773884, -0.4544583902936971), (0.08536758891838686, -1.0006968416654884), (0.4940117338288988, 0.33459355603986146), (-1.414836699325635, 0.5371173770370103), (1.272628159442155, 0.5188564742893288), (1.0518350089228674, 0.006899004571879951), (-1.2444879033595841, 0.4049862922734269), (1.5157769936483338, -0.0605531844174656), (1.7542982388480752, -0.22355374114652093), (1.116323792743214, -1.418970623852686), (-1.2420566520170113, 0.17526794263288645), (-1.3102485489101918, 1.574340700919391), (1.1603448151492015, -1.6739784615584943), (0.02672746329475923, 0.20325443823781045), (0.12476535318944346, -1.0098217848527646), (0.23896451243577768, -1.240948897546935), (1.2723514970421823, -1.5894871748091413), (1.8263193781780107, -0.37819364524797233), (-0.261717590284612, -0.8398760292070804), (-0.1481608178186157, -0.287652841873308), (1.3750891071932427, -1.1503948166289006), (-0.5276787312274871, 0.40972626218243874), (-0.5804932665004289, 1.246032963014639), (1.1850930442780176, -0.9465518558655727), (-0.9942610410763489, 1.5852154613620941), (1.407929110408779, 0.8662032269944928), (1.6749551499108628, 0.4579770741986378), (1.0045092563686164, -0.8953690399206445), (-0.21256441925002043, -0.784758125767126), (-1.7217811059644976, -0.5249006417067439), (1.1212484360623152, -1.3247529639486284), (0.35552381649556847, -1.4172789247411839), (-1.0634450912598228, 1.5104427106473681), (-1.4661540680435365, 0.27909108012631145), (-1.8379115257680791, -0.5641859883898434), (-0.16718956915539254, 0.9318528778813367), (-0.7932009219040662, -0.10190156963000156), (-0.39287152947841314, -0.47803230863257934), (-0.03968029515462099, -0.6435828564485073), (-1.542462776015882, 1.2244775524193776), (-0.24457915690762305, 1.4684312561418291)]