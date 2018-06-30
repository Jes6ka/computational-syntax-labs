concrete LinguisticsChi of Linguistics =

 open
   SyntaxChi,
   SymbolicChi,
   ParadigmsChi
 in {

lincat
   Statement = Utt ;
   Rule = S ;
   Language = NP ;
   Category = CN ;
   ParameterType = CN ;
   ParameterValue = NP ;
   [ParameterType] = {bare,indef : NP} ;
   [ParameterValue] = NP ;
   [Category] = NP ;
   Word = NP ;

lin
   ruleStatement rule = mkUtt rule ;
   languageRuleStatement language rule = mkUtt (mkS (SyntaxChi.mkAdv in_Prep language) rule) ;

   categoryInflectedRule category types = mkS (mkCl (mkNP aPl_Det category) (mkV2 (mkV "活用") on_Prep) types.bare) ;
   categoryNotInflectedRule category = mkS negativePol (mkCl (mkNP aPl_Det category) (mkV "活用")) ;
  
   categoryInherentRule category types = mkS (mkCl (mkNP aPl_Det category) have_V2 types.indef) ;

   parameterTypeRule ptype pvalues = mkS (mkCl (mkNP thePl_Det ptype) pvalues) ;
   parameterValueRule pvalues ptype = mkS (mkCl pvalues ptype) ;
  
   wordFormRule pvalue category wordlemma wordform = mkS (mkCl (mkNP pvalue (SyntaxChi.mkAdv possess_Prep (mkNP the_Det (mkCN category wordlemma)))) wordform) ;
   wordInherentRule ptype category word pvalue = mkS (mkCl (mkNP (mkNP the_Det ptype) (SyntaxChi.mkAdv possess_Prep (mkNP the_Det (mkCN category word)))) pvalue) ;

   wordCategoryRule word category = mkS (mkCl word category) ;
   syntaxCombinationRule category categories = mkS (mkCl (mkNP a_Det category) can_VV (mkVP (passiveVP (mkV2 "组合"))(SyntaxChi.mkAdv from_Prep categories))) ;

   --agreementRule cat1 cat2 ptypes = mkS (mkCl (mkNP the_Det cat1)(mkVP (mkComp (SyntaxChi.mkAdv in_Prep ptypes.bare)) (mkVP (mkV2 (mkV "继承了")) (mkNP the_Det cat2)))) ;
   --agreementRule cat1 cat2 ptypes = mkS (mkCl (mkNP the_Det cat1) (mkVP (mkAdV (in_Prep.prepPre ++ ptypes.bare.s ++ in_Prep.prepPost)) (mkVP (mkV2 (mkV "继承了")) (mkNP the_Det cat2)))) ;
   orderBeforeRule cat1 cat2 = mkS (mkCl (mkNP the_Det cat1) (mkVP (passiveVP (mkV2 "放在")) (SyntaxChi.mkAdv before_Prep (mkNP the_Det cat2) ))) ;
   orderAfterRule cat1 cat2 = mkS (mkCl (mkNP the_Det cat1) (mkVP (passiveVP (mkV2 "放在")) (SyntaxChi.mkAdv  after_Prep (mkNP the_Det cat2)))) ;

   stringWord string = symb ("//" ++ string.s ++ "//") ; 
   
   englishLanguage = mkNP (mkPN "英语") ;
   swedishLanguage = mkNP (mkPN "瑞典语") ;
   latinLanguage = mkNP (mkPN "拉丁语") ;
   
   nounCategory = mkCN (mkN "名词") ;
 
   verbCategory = mkCN (mkN "动词") ;
   adjectiveCategory = mkCN (mkN "形容词") ;
   adverbCategory = mkCN (mkN "副词") ;
   sentenceCategory = mkCN (mkN "句子") ;
   nounPhraseCategory = mkCN (mkN "名词短语") ;
   verbPhraseCategory = mkCN (mkN "动词短语") ;
   transitiveVerbCategory = mkCN (mkA "及物") (mkN "动词") ;
  
   pronounCategory = mkCN (mkN "代词");
   determinerCategory = mkCN (mkN "定语");
   relativePronounCategory = mkCN (mkA "关系") (mkN "代词");
   interrogativePronounCategory = mkCN (mkA "疑问") (mkN "代词");
   conjunctionCategory = mkCN (mkN "连词");
   subjunctionCategory = mkCN (mkN "增补");
   particleCategory = mkCN (mkN "小品词");
   articleCategory = mkCN (mkN "冠词");
   prepositionCategory = mkCN (mkN "介词");
   interjectionCategory = mkCN (mkN "叹词");

   utteranceCategory = mkCN (mkN "语句");
   questionCategory = mkCN (mkN "问句");
   relativeClauseCategory = mkCN (mkA "关系") (mkN "从句");
   adjectivalPhraseCategory = mkCN (mkA "形容词") (mkN "短语");
   adverbialPhraseCategory = mkCN (mkA "副词") (mkN "短语");

   numberParameterType = mkCN (mkN "数字") ;
   caseParameterType = mkCN (mkN "格") ;
   genderParameterType = mkCN (mkN "性别") ;
   personParameterType = mkCN (mkN "人称") ;
   tenseParameterType = mkCN (mkN "时态") ;

   singularParameterValue   = mkParameterValue "单数" ;
   pluralParameterValue     = mkParameterValue "复数" ;
   
   nominativeParameterValue = mkParameterValue "主格" ;
   genitiveParameterValue   = mkParameterValue "所有格" ;
   accusativeParameterValue = mkParameterValue "宾格" ;

   masculineParameterValue = mkParameterValue "阳性" ;
   feminineParameterValue  = mkParameterValue "阴性" ;
   neuterParameterValue    = mkParameterValue "中性" ;

   firstParameterValue  = mkParameterValue (mkCN (mkA "第一") (mkN "人称")) ;
   secondParameterValue = mkParameterValue (mkCN (mkA "第二") (mkN "人称")) ;
   thirdParameterValue  = mkParameterValue (mkCN (mkA "第三") (mkN "人称")) ;
   
   presentParameterValue = mkParameterValue "现在式" ;
   pastParameterValue = mkParameterValue "过去式" ;
   futureParameterValue = mkParameterValue "將来式" ;
  
   indicativeParameterValue  = mkParameterValue "指示词" ;
   conjunctiveParameterValue  = mkParameterValue "连词" ;
   imperativeParameterValue  = mkParameterValue "命令词" ;
   participleParameterValue  = mkParameterValue "分词" ;
   infinitiveParameterValue  = mkParameterValue "不定词" ;

   BaseCategory c = mkNP a_Det c ; --　c means category
   ConsCategory c cs = mkNP and_Conj (mkNP a_Det c) cs ; --cs stand for a list of s
   
   BaseParameterValue pv = pv ;  --parameter value
   ConsParameterValue pv pvs = mkNP and_Conj pv pvs ;

   BaseParameterType pt = {bare = mkNP pt ; indef = mkNP a_Det pt} ;
   ConsParameterType pt pts = {bare = mkNP and_Conj (mkNP pt) pts.bare ; indef = mkNP and_Conj (mkNP a_Det pt) pts.indef} ;

oper
   mkParameterValue = overload {
     mkParameterValue : Str -> NP = \str -> (mkNP the_Det (mkN str)) ;
     mkParameterValue : CN -> NP = \cn -> (mkNP the_Det cn) ;
     } ;
  --   
 --  in_Prep = overload {
   --  in_Prep : Str ->  in_Prep = \str -> {advType : ResChi.AdvType; lock_Prep : {};
     --            prepPost : Str; prepPre : Str}
 
     --mkAdV : CN -> AdV = \cn -> (mkAdV in_Prep cn) ;
  --   } ;
}