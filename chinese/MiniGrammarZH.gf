concrete MiniGrammarZH of MiniGrammar = open MiniResZH, Prelude in {


  lincat
    Utt = {s : Str} ;
    Adv = Adverb ;
    Pol = {s : Str ; p : Polarity} ;
    S  = {s : Str} ;
    Cl = {s : Polarity => Str} ;
    VP = {v : Verb ; c : Agr => Str ; isAux : Bool; adv: Str} ;
    AP = Adjective ;
    CN = Noun ;
    NP = Pronoun ;
    Pron = Pronoun ;
    Det  = Determiner ;
    Conj = {s : Str} ;
    Prep = {s : Str} ;
    V = Verb ;
    V2 = Verb2 ;
    A = Adjective ;
    N = Noun ;
    PN = ProperName ;

  lin
    UttS s = s ;
    
    UttNP np = {s = np.s ! Nom} ;

    UsePresCl pol cl = {
      s = pol.s ++ cl.s ! pol.p ;
      } ;
      
    PredVP np vp = {
      s = \\p =>
         np.s ! Nom  ++
         vp.adv ++
         case p of {
	   --Pos => vp.v.s ! np.a.n ! np.a.p ;
     Pos => vp.v.s ;
	   Neg => case vp.isAux of {
	     --True  => vp.v.s ! np.a.n ! np.a.p ++ "不" ;
       True  => vp.v.s ++ "不" ;
	     --False => ( np.a.n ! np.a.p ++ "没有" ++ vp.v.s) | ( np.a.n ! np.a.p ++ vp.v.s)
       False => (  "没有" ++ vp.v.s) | (  vp.v.s)
	     }
	   } ++
	 vp.c ! np.a ;
      } ;
      
    UseV v = {
      v = v ;
      c = \\_ => [] ;
      isAux = False ;
      adv = "";
      } ;
    
    ComplV2 v2 np = {
      v = v2 ;
      c = \\_ => v2.c.prep ++ np.s ! v2.c.c ;
      isAux = False ;
      adv = "";
      } ;
      
    UseAP ap = {
      v = {s=""} ;
      c = \\a => ap.s ;
      isAux = True ;
      adv = "";
      } ;
      
    AdvVP vp adv = vp ** {
      c = \\a => vp.c ! a ; 
      adv = adv.s;
      } ;
      
    DetCN det cn = {
      s = \\c => det.s ++ cn.s ! det.n ;
--      a = {g = cn.g ; n = det.n} ;
      a = {p = P3 ; n = det.n} ;
      } ;
      
    UsePN pn = {s = \\c => pn.s ; a = {p = P3; g = pn.g ; n = Sg}} ;
    
    UsePron p = p ;
    
    MassNP cn = {
      s = \\c => cn.s ! Sg ;
--     a = {g = cn.g ; n = Sg} ;
      a = {p = P3 ; n = Sg} ;
      } ;
    
    a_Det = mkDeterminer (pre {_ => "一"}) Sg ; -- Too complex to fix: Quantifier in Chinese has hundred of forms that can not be interpreted by "one sth"
    
    aPl_Det = mkDeterminer "" Pl ;
    
    the_Det = mkDeterminer "这" Sg ;
    
    thePl_Det = mkDeterminer "这些" Pl ;
    
    UseN n = n ;
    
    AdjCN ap cn = {
      s = \\n => ap.s ++ cn.s ! n ;
    --  g = cn.g ;
      } ;

    PositA a = a ;

    PrepNP prep np = {s = prep.s ++ np.s ! Nom } ;

    CoordS conj a b = {s = a.s ++ conj.s ++ b.s} ;
    
    PPos  = {s = "" ; p = Pos} ;
    PNeg  = {s = "" ; p = Neg} ;

    and_Conj = {s = "和"} ;
    or_Conj = {s = "或"} ;

    every_Det = mkDeterminer "每一" Sg ;

    in_Prep = {s = "在" } ;
    on_Prep = {s = "在" } ;
    with_Prep = {s = "和" } ;

    i_Pron = mkPronoun "我" "我" Masc Sg P1;
    youSg_Pron = mkPronoun "你" "你" Fem Sg P2;
    he_Pron = mkPronoun "他"  "他" Masc Sg P3;
    she_Pron = mkPronoun "她"  "她" Fem Sg P3;
    we_Pron = mkPronoun "我们"  "我们" Masc Pl P1;
    youPl_Pron = mkPronoun "你们" "你们" Masc Pl P2;
    theymas_Pron = mkPronoun "他们" "他们" Masc Pl P3;
    theyfem_Pron = mkPronoun "她们" "她们" Fem Pl P3;
}
